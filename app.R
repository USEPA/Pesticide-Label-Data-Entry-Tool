library(shiny)
library(shinyWidgets)
library(bslib)
library(readxl)
library(DT)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(shinyvalidate)

## For publishing directly from GitHub to Posit
# library(rsconnect)
# rsconnect::writeManifest()
# Freeform Unit Entry added 6.3.2026
# Smart upload added 6/5/26

# ---------------- CONFIG ----------------
workbook_path <- "data/templates/UST_Active Ingredient (PC Code) UST Report_Template_2.2026.xlsx"

# ---------------- Helpers ----------------
`%||%` <- function(x, y) { if (is.null(x) || length(x) == 0) y else x }
idsafe <- function(x) gsub("[^A-Za-z0-9_]", "_", x)
collapse_multi <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (length(x) == 0) return(NA_character_)
  x <- as.character(x); x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = "; ")
}
split_multi <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x <- as.character(x[1]); if (!nzchar(x)) return(character(0))
  strsplit(x, ";\\s*")[[1]]
}
extract_number <- function(x) {
  s <- as.character(x %||% "")
  num <- stringr::str_extract(s, "-?\\d+(?:\\.\\d+)?(?:[eE][+-]?\\d+)?")
  suppressWarnings(as.numeric(num))
}

# ---------------- Read vocab from workbook ----------------
read_vocab_col <- function(path, sheet, col_name) {
  dat <- suppressMessages(read_excel(path, sheet = sheet))
  if (!col_name %in% names(dat)) return(character(0))
  vals <- dat[[col_name]]
  vals <- as.character(vals)
  vals <- trimws(vals)
  vals <- vals[!is.na(vals) & nzchar(vals)]
  sort(unique(vals))
}
read_vocab_range <- function(path, sheet, range) {
  dat <- suppressMessages(read_excel(path, sheet = sheet, range = range, col_names = FALSE))
  vals <- unlist(dat, use.names = FALSE)
  vals <- as.character(vals)
  vals <- trimws(vals)
  vals <- vals[!is.na(vals) & nzchar(vals)]
  sort(unique(vals))
}
build_vocab <- function(path) {
  list(
    # Product-level
    "Physical Form"                   = read_vocab_col(path, "Physical Form", "Term"),
    "Product-level PPE"               = read_vocab_col(path, "PPE", "Short Form Terms"),
    "RUP"                             = c("No", "Yes"),
    # Scenario-level
    "Crop Use Site"                   = read_vocab_col(path, "Crop Use Sites", "Label"),
    "Non Crop Use Site"               = read_vocab_col(path, "Non Crop Use Sites", "Label"),
    "Location"                        = read_vocab_col(path, "Location", "Label"),
    "App Target"                      = read_vocab_col(path, "App. Target", "Label"),
    "App Type"                        = read_vocab_col(path, "App. Type", "Label"),
    "App Equipment Type"              = read_vocab_range(path, "App. Equipment", "G2:G8"),
    "App Timing (Site)"               = read_vocab_col(path, "App Timing (Site Status)", "Label"),
    "App Timing (Pest)"               = read_vocab_col(path, "App Timing (Pest)", "Label"),
    "ASABE Droplet Size"              = read_vocab_col(path, "ASABE Droplet Size", "Label"),
    "Buffered Area (Term)"            = read_vocab_col(path, "Buffered Area", "Term"),
    "Pollinator Protection Statement" = read_vocab_col(path, "Pollinator Protection", "Label"),
    "Soil Type Restrictions"          = read_vocab_col(path, "Soil Type", "Label"),
    "Site-Level ALLOWED Geographic Area"    = read_vocab_col(path, "Geographic Area", "Label"),
    "Site-Level PROHIBITED Geographic Area" = read_vocab_col(path, "Geographic Area", "Label")
  )
}

# ---------------- Input builders ----------------
make_input <- function(field_label, type = c("text","numeric","pick"), choices = NULL, prefix = "", multiple = FALSE) {
  type <- match.arg(type)
  input_id <- paste0(prefix, idsafe(field_label))
  if (type == "pick") {
    selectizeInput(
      inputId = input_id, label = field_label,
      choices = choices %||% character(0),
      multiple = multiple,
      options = list(
        create = TRUE,
        placeholder = "Type or pick…",
        openOnFocus = TRUE,
        maxOptions = 10000,
        dropdownParent = "body",
        plugins = list("remove_button")
      ),
      width = "100%"
    )
  } else if (type == "numeric") {
    numericInput(input_id, field_label, value = NA_real_, width = "100%")
  } else {
    textInput(input_id, field_label, value = "", width = "100%")
  }
}

# ---------- Unit choices ----------
weight_units <- c("lb", "oz", "kg", "g")
volume_units <- c("gal", "qt", "L", "mL", "fl oz")
area_units   <- c("ac", "ha")

scenario_area_rate_allowed <- list(
  "Min Diluent Quantity (Gal Spray Soln per Acre)" = list(allow_weight = FALSE, allow_volume = TRUE),
  "Product Max App Rate/Area"                      = list(allow_weight = TRUE,  allow_volume = TRUE),
  "AI Max Rate/App"                                = list(allow_weight = TRUE,  allow_volume = FALSE),
  "Product Max Rate/Year"                          = list(allow_weight = TRUE,  allow_volume = TRUE),
  "Product Max Rate/Crop Cycle"                    = list(allow_weight = TRUE,  allow_volume = TRUE),
  "AI Max Rate/Year"                               = list(allow_weight = TRUE,  allow_volume = FALSE),
  "AI Max Rate/Cycle"                              = list(allow_weight = TRUE,  allow_volume = FALSE)
)

unit_choices_for_field <- function(field) {
  allow <- scenario_area_rate_allowed[[field]] %||% list(allow_weight = TRUE, allow_volume = TRUE)
  unique(c(
    if (isTRUE(allow$allow_weight)) weight_units else character(0),
    if (isTRUE(allow$allow_volume)) volume_units else character(0)
  ))
}

# ---------- UI: numeric + numerator-unit + area-unit (open selectize) ----------
make_area_rate_input <- function(field_label,
                                 default_num_unit = "lb",
                                 default_area_unit = "ac",
                                 prefix = "scen__",
                                 allow_weight = TRUE,
                                 allow_volume = TRUE) {
  input_id      <- paste0(prefix, idsafe(field_label))
  numunit_id    <- paste0(input_id, "__numunit")
  areaunit_id   <- paste0(input_id, "__areaunit")
  num_choices   <- c(if (allow_weight) weight_units else character(0),
                     if (allow_volume) volume_units else character(0))
  num_choices   <- unique(num_choices)
  
  if (!default_num_unit %in% num_choices) {
    default_num_unit <- if (allow_weight) "lb" else if (allow_volume) "gal" else num_choices[1]
  }
  if (!default_area_unit %in% area_units) default_area_unit <- "ac"
  
  tagList(
    tags$label(class = "form-label", `for` = input_id, field_label),
    div(class = "ust-rate-row mb-3",
        div(class = "ust-numeric",
            numericInput(input_id, label = NULL, value = NA_real_, width = "100%")),
        div(class = "ust-units",
            div(class = "ust-unit",
                selectizeInput(
                  numunit_id, label = NULL, choices = num_choices,
                  selected = default_num_unit, width = NULL,
                  options = list(
                    create = TRUE, createOnBlur = TRUE, persist = TRUE, selectOnTab = TRUE,
                    dropdownParent = "body"
                  )
                )
            ),
            span(class = "sep", "/"),
            div(class = "ust-unit",
                selectizeInput(
                  areaunit_id, label = NULL, choices = area_units,
                  selected = default_area_unit, width = NULL,
                  options = list(
                    create = TRUE, createOnBlur = TRUE, persist = TRUE, selectOnTab = TRUE,
                    dropdownParent = "body"
                  )
                )
            )
        )
    )
  )
}

# ---------- Parser to restore units (preserves custom units) ----------
parse_rate_units <- function(x, default_num = "lb", default_area = "ac") {
  s <- as.character(x %||% "")
  rest <- sub("^\\s*-?\\d+(?:\\.\\d+)?(?:[eE][+-]?\\d+)?\\s*", "", s, perl = TRUE)
  parts <- strsplit(rest, "/", fixed = TRUE)[[1]]
  numu  <- trimws(if (length(parts) >= 1) parts[1] else default_num)
  areau <- trimws(if (length(parts) >= 2) parts[2] else default_area)
  numu <- sub("\\s+ai\\b.*$", "", numu, perl = TRUE)
  if (!nzchar(numu))  numu  <- default_num
  if (!nzchar(areau)) areau <- default_area
  list(num = numu, area = areau)
}

# ---------------- Field lists ----------------
product_fields <- c(
  "EPA Registration Number",
  "AI Name",
  "PC Code",
  "Co-Formulated AI",
  "Physical Form",
  "% AI",
  "AI Concentration (Or Product Density if liquid)",
  "RUP",
  "Product-level PPE"
)

scenario_fields <- c(
  "Crop Use Site","Non Crop Use Site",
  "Location","App Target","App Type","App Equipment Type",
  "App Timing (Site)","App Timing (Pest)",
  "Min Diluent Quantity (Gal Spray Soln per Acre)",
  "Product Max App Rate/Area",
  "AI Max Rate/App","Max # App/Year","Max # App/Crop Cycle",
  "Product Max Rate/Year","Product Max Rate/Crop Cycle",
  "AI Max Rate/Year","AI Max Rate/Cycle",
  "Max Number of Seasons/Crop Cycles per year","RTI (d)","REI (H)","PHI (d)","PGI (d)","PSI (d)",
  "ASABE Droplet Size","Max Release Height","Max Wind Speed (mph)",
  "Buffered Area (ft)","Buffered Area (Term)",
  "Site-Level ALLOWED Geographic Area","Site-Level PROHIBITED Geographic Area",
  "Soil Type Restrictions",
  "Pollinator Protection Statement"
)

scenario_picklist_fields <- c(
  "Crop Use Site","Non Crop Use Site",
  "Location","App Target","App Type","App Equipment Type",
  "App Timing (Site)","App Timing (Pest)",
  "ASABE Droplet Size","Buffered Area (Term)",
  "Pollinator Protection Statement","Soil Type Restrictions",
  "Site-Level ALLOWED Geographic Area","Site-Level PROHIBITED Geographic Area"
)

scenario_numeric_fields <- c("Buffered Area (ft)")

scenario_area_rate_fields <- c(
  "Min Diluent Quantity (Gal Spray Soln per Acre)",
  "Product Max App Rate/Area",
  "AI Max Rate/App",
  "Product Max Rate/Year",
  "Product Max Rate/Crop Cycle",
  "AI Max Rate/Year",
  "AI Max Rate/Cycle"
)

scenario_area_rate_defaults <- list(
  "Min Diluent Quantity (Gal Spray Soln per Acre)" = list(num = "gal",   area = "ac"),
  "Product Max App Rate/Area"                      = list(num = "gal",   area = "ac"),
  "AI Max Rate/App"                                = list(num = "lb",    area = "ac"),
  "Product Max Rate/Year"                          = list(num = "fl oz", area = "ac"),
  "Product Max Rate/Crop Cycle"                    = list(num = "fl oz", area = "ac"),
  "AI Max Rate/Year"                               = list(num = "lb",    area = "ac"),
  "AI Max Rate/Cycle"                              = list(num = "lb",    area = "ac")
)

scenario_textarea_label <- "Other Site/Scenario Specific Restrictions & Limitations"
scenario_textarea_id    <- "scen__Other_Site_Scenario_Specific_Restrictions_Limitations"

# ---------------- UI ----------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #resizable {
        resize: vertical;
        overflow: auto;
        border: 1px solid #ddd;
        padding: 10px;
        height: auto;
      }
    ")),
    tags$style(HTML("
      table.dataTable thead th {
        font-size: 11px;
        color: #333;
      }
    ")),
    tags$style(HTML("
    /* Row layout for numeric + units */
    .ust-rate-row { display:flex; align-items:center; gap:6px; flex-wrap: nowrap; }
    .ust-rate-row .ust-numeric { flex: 1 1 auto; min-width: 0; }
    .ust-rate-row .ust-units { display:flex; align-items:center; gap:6px; flex: 0 0 auto; }

    /* Shiny forces input containers to 100% width; override for our unit inputs */
    .ust-units .shiny-input-container { width: auto !important; display: inline-block; }

    .ust-units .shiny-input-container { width: auto !important; display: inline-block; }
    /* First unit (numerator) */
    .ust-units .ust-unit:first-of-type .selectize-control { width: auto !important; min-width: 60px; }
    /* Second unit (area) */
    .ust-units .ust-unit:last-of-type .selectize-control  { width: auto !important; min-width: 60px; }
    .ust-units .selectize-control .selectize-input { width: auto; white-space: nowrap; }

    .ust-units .sep { padding: 0 2px; color: #555; }

    .selectize-control.single .selectize-input,
    .selectize-control.single .selectize-input.input-active {
      min-height: calc(2.25rem + 2px);
      padding: .375rem .75rem;
      line-height: 1.5;
      box-sizing: border-box;
    }
    .selectize-control.single .selectize-input > input {
      height: 1.5rem;
    }
  "))
  ),
  tags$head(
    tags$style(HTML("
    /* Let the caret render outside the input box */
    .ust-units .selectize-control.single .selectize-input {
      overflow: visible;           /* default is hidden; allow caret to overflow */
      padding-right: .75rem;       /* remove the extra right padding selectize reserves */
      position: relative;
    }
  
    .ust-units .selectize-control.single .selectize-input:after {
      right: -12px !important;     /* negative pushes it outside */
      border-top-color: #6c757d;   /* subtle color */
      opacity: 0.9;
    }

   
    .ust-units .selectize-control.single .selectize-input.dropdown-active:after {
      border-width: 0 6px 6px 6px;
      border-color: transparent transparent #6c757d transparent;
      margin-top: -1px;
    }

    /* Ensure unit inputs don't stretch full width */
    .ust-units .shiny-input-container { width: auto !important; display: inline-block; }
    .ust-units .selectize-control       { width: auto !important; min-width: 120px; }
  "))
  ),
  
  tags$style(type = "text/css", "
    .navbar-brand { color: #000000 !important; font-weight: bold !important; }
  "),
  tags$head(tags$link(rel = "icon", type = "image/png", href = "PLDET_icon.png")),
  div(
    id = "resizable",
    navbarPage(
      title = div(
        tags$img(src = "PLDET_icon.png", height = "30px", style = "vertical-align: middle;left-margin:1px"),
        "Pesticide Label Data Entry Tool"
      ),
      id = "navbar",
      tabPanel(
        "Product-Level", value = "product",
        div(
          class = "hdr",
          h4("Product-Level"),
          br(),
          fluidRow(
            column(
              4,
              actionButton("reload", "Reload workbook", icon = icon("redo")),
              actionButton("clear_prod", "Clear form", icon = icon("eraser"), class = "ms-1"),
              actionButton("add_prod", "Add row", class = "btn-primary ms-1", icon = icon("plus")),
              hr(),
              uiOutput("product_form_col1")
            ),
            column(4, uiOutput("product_form_col2")),
            column(4, uiOutput("product_form_col3"))
          )
        )
      ),
      tabPanel(
        "Scenario-Level", value = "scenario",
        div(
          class = "hdr",
          fluidRow(
            column(
              4,
              h4("Scenario-Level"),
              uiOutput("current_product_ui"),
              br(),
              actionButton("relink_selected", "Relink selected scenarios", class = "btn-outline-secondary", icon = icon("sync-alt")),
              actionButton("clear_scenario", "Clear form", icon = icon("eraser")),
              actionButton("add_scen", "Add row", class = "btn-primary", icon = icon("plus")),
              hr(),
              uiOutput("scenario_form_col1")
            ),
            column(4, uiOutput("scenario_form_col2")),
            column(4, uiOutput("scenario_form_col3"))
          )
        )
      )
    )
  ),
  fluidRow(
    column(
      12,
      tags$hr(style = "border-top: 2px solid #333; margin-top: 10px;"),
      tabsetPanel(
        id = "data_tables",
        tabPanel(
          "Product-Level Table", value = "product",
          div(class = "mb-2", style = "margin-top:10px",
              actionButton("del_prod", "Delete selected", icon = icon("remove"), class = "btn-danger me-2")),
          div(style = "margin-top:5px", DTOutput("tbl_prod")),
          downloadButton("dl_prod", "Download product-level CSV")
        ),
        tabPanel(
          "Scenario-Level Table", value = "scenario",
          div(class = "mb-2", style = "margin-top:10px",
              actionButton("clone_to_form", "Load selected to form", icon = icon("sign-in-alt"), class = "btn-secondary me-2"),
              actionButton("dup_scen", "Duplicate selected", icon = icon("copy"), class = "btn-outline-secondary me-2"),
              actionButton("del_scen", "Delete selected", icon = icon("remove"), class = "btn-danger me-2")),
          div(style = "margin-top:5px", DTOutput("tbl_scen")),
          div(style = "float: left;", downloadButton("dl_scen", "Download scenario-level CSV"))
        )
      )
    )
  )
)


# ---------------- Server ----------------
server <- function(input, output, session) {
  vocab <- reactiveVal(NULL)
  
  # Empty schemas
  make_empty_prod <- function() {
    cols <- c("Product_ID", product_fields)
    as_tibble(setNames(rep(list(character()), length(cols)), cols))
  }
  make_empty_scen <- function() {
    cols <- c("Product_ID", product_fields, scenario_fields, scenario_textarea_label)
    as_tibble(setNames(rep(list(character()), length(cols)), cols))
  }
  
  prod_dat <- reactiveVal(make_empty_prod())
  scen_dat <- reactiveVal(make_empty_scen())
  
  # Load vocab
  observe({
    validate(need(file.exists(workbook_path),
                  paste("Workbook not found. Check path:\n", workbook_path)))
    vocab(build_vocab(workbook_path))
  })
  observeEvent(input$reload, {
    vocab(build_vocab(workbook_path))
    showNotification("Workbook reloaded.", type = "message")
  })
  
  # Diagnostics for empty picklists
  observeEvent(vocab(), {
    cu <- length(vocab()[["Crop Use Site"]] %||% character(0))
    ncu <- length(vocab()[["Non Crop Use Site"]] %||% character(0))
    if (cu == 0 || ncu == 0) {
      showNotification(
        sprintf("Picklist empty? Crop Use Site = %d items, Non Crop Use Site = %d items. Check sheet/column names.", cu, ncu),
        type = "warning", duration = 7
      )
    }
  }, once = TRUE)
  
  # ----- Product form (3 columns) -----
  output$product_form_col1 <- renderUI({
    req(vocab())
    tagList(
      make_input("EPA Registration Number", "text", prefix = "prod__"),
      make_input("AI Name", "text", prefix = "prod__"),
      make_input("PC Code", "text", prefix = "prod__")
    )
  })
  output$product_form_col2 <- renderUI({
    req(vocab())
    tagList(
      make_input("Co-Formulated AI", "text", prefix = "prod__"),
      make_input("Physical Form", "pick", choices = vocab()[["Physical Form"]], prefix = "prod__", multiple = TRUE),
      make_input("% AI", "text", prefix = "prod__")
    )
  })
  output$product_form_col3 <- renderUI({
    req(vocab())
    tagList(
      make_input("AI Concentration (Or Product Density if liquid)", "text", prefix = "prod__"),
      make_input("RUP", "pick", choices = vocab()[["RUP"]], prefix = "prod__", multiple = FALSE),
      make_input("Product-level PPE", "pick", choices = vocab()[["Product-level PPE"]], prefix = "prod__", multiple = TRUE)
    )
  })
  
  # Linked product dropdown for scenarios
  product_choices <- reactive({
    pd <- prod_dat()
    if (nrow(pd) == 0) return(setNames(character(0), character(0)))
    s <- function(x) ifelse(is.na(x) | x == "", "", as.character(x))
    ai  <- s(pd$`AI Name`)
    epa <- if ("EPA Registration Number" %in% names(pd)) s(pd$`EPA Registration Number`) else ""
    lbl <- ifelse(nzchar(epa) & nzchar(ai),
                  paste0(ai, " (EPA Reg #", epa, ")"),
                  ifelse(nzchar(ai), ai, ifelse(nzchar(epa), paste0("EPA Reg #", epa), "")))
    lbl[!nzchar(lbl)] <- paste0("Product ", pd$Product_ID)
    setNames(pd$Product_ID, lbl)
  })
  output$current_product_ui <- renderUI({
    ch <- product_choices()
    default_sel <- if (length(ch)) unname(tail(ch, 1)) else NULL
    selectInput("current_product", "Linked product", choices = ch, selected = default_sel, width = "260px")
  })
  
  # ----- Scenario form (3 columns) -----
  output$scenario_form_col1 <- renderUI({
    req(vocab())
    tagList(
      make_input("Crop Use Site", "pick", choices = vocab()[["Crop Use Site"]], prefix = "scen__", multiple = TRUE),
      make_input("Non Crop Use Site", "pick", choices = vocab()[["Non Crop Use Site"]], prefix = "scen__", multiple = TRUE),
      make_input("Location", "pick", choices = vocab()[["Location"]], prefix = "scen__", multiple = TRUE),
      make_input("App Target", "pick", choices = vocab()[["App Target"]], prefix = "scen__", multiple = TRUE),
      make_input("App Type", "pick", choices = vocab()[["App Type"]], prefix = "scen__", multiple = TRUE),
      make_input("App Equipment Type", "pick", choices = vocab()[["App Equipment Type"]], prefix = "scen__", multiple = TRUE),
      make_input("App Timing (Site)", "pick", choices = vocab()[["App Timing (Site)"]], prefix = "scen__", multiple = TRUE),
      make_input("App Timing (Pest)", "pick", choices = vocab()[["App Timing (Pest)"]], prefix = "scen__", multiple = TRUE),
      make_area_rate_input(
        "Min Diluent Quantity (Gal Spray Soln per Acre)",
        default_num_unit  = scenario_area_rate_defaults[["Min Diluent Quantity (Gal Spray Soln per Acre)"]]$num,
        default_area_unit = scenario_area_rate_defaults[["Min Diluent Quantity (Gal Spray Soln per Acre)"]]$area,
        prefix = "scen__", allow_weight = FALSE, allow_volume = TRUE
      ),
      make_area_rate_input(
        "Product Max App Rate/Area",
        default_num_unit  = scenario_area_rate_defaults[["Product Max App Rate/Area"]]$num,
        default_area_unit = scenario_area_rate_defaults[["Product Max App Rate/Area"]]$area,
        prefix = "scen__", allow_weight = TRUE, allow_volume = TRUE
      )
    )
  })
  output$scenario_form_col2 <- renderUI({
    req(vocab())
    tagList(
      make_area_rate_input(
        "AI Max Rate/App",
        default_num_unit  = scenario_area_rate_defaults[["AI Max Rate/App"]]$num,
        default_area_unit = scenario_area_rate_defaults[["AI Max Rate/App"]]$area,
        prefix = "scen__", allow_weight = TRUE, allow_volume = FALSE
      ),
      make_input("Max # App/Year", "text", prefix = "scen__"),
      make_input("Max # App/Crop Cycle", "text", prefix = "scen__"),
      make_area_rate_input(
        "Product Max Rate/Year",
        default_num_unit  = scenario_area_rate_defaults[["Product Max Rate/Year"]]$num,
        default_area_unit = scenario_area_rate_defaults[["Product Max Rate/Year"]]$area,
        prefix = "scen__", allow_weight = TRUE, allow_volume = TRUE
      ),
      make_area_rate_input(
        "Product Max Rate/Crop Cycle",
        default_num_unit  = scenario_area_rate_defaults[["Product Max Rate/Crop Cycle"]]$num,
        default_area_unit = scenario_area_rate_defaults[["Product Max Rate/Crop Cycle"]]$area,
        prefix = "scen__", allow_weight = TRUE, allow_volume = TRUE
      ),
      make_area_rate_input(
        "AI Max Rate/Year",
        default_num_unit  = scenario_area_rate_defaults[["AI Max Rate/Year"]]$num,
        default_area_unit = scenario_area_rate_defaults[["AI Max Rate/Year"]]$area,
        prefix = "scen__", allow_weight = TRUE, allow_volume = FALSE
      ),
      make_area_rate_input(
        "AI Max Rate/Cycle",
        default_num_unit  = scenario_area_rate_defaults[["AI Max Rate/Cycle"]]$num,
        default_area_unit = scenario_area_rate_defaults[["AI Max Rate/Cycle"]]$area,
        prefix = "scen__", allow_weight = TRUE, allow_volume = FALSE
      ),
      make_input("Max Number of Seasons/Crop Cycles per year", "text", prefix = "scen__"),
      make_input("RTI (d)", "text", prefix = "scen__"),
      make_input("REI (H)", "text", prefix = "scen__")
    )
  })
  output$scenario_form_col3 <- renderUI({
    req(vocab())
    tagList(
      make_input("PHI (d)", "text", prefix = "scen__"),
      make_input("PGI (d)", "text", prefix = "scen__"),
      make_input("PSI (d)", "text", prefix = "scen__"),
      make_input("ASABE Droplet Size", "pick", choices = vocab()[["ASABE Droplet Size"]], prefix = "scen__", multiple = TRUE),
      make_input("Buffered Area (ft)", "numeric", prefix = "scen__"),
      make_input("Buffered Area (Term)", "pick", choices = vocab()[["Buffered Area (Term)"]], prefix = "scen__", multiple = TRUE),
      make_input("Pollinator Protection Statement", "pick", choices = vocab()[["Pollinator Protection Statement"]], prefix = "scen__", multiple = TRUE),
      make_input("Soil Type Restrictions", "pick", choices = vocab()[["Soil Type Restrictions"]], prefix = "scen__", multiple = TRUE),
      make_input("Site-Level ALLOWED Geographic Area", "pick", choices = vocab()[["Site-Level ALLOWED Geographic Area"]], prefix = "scen__", multiple = TRUE),
      make_input("Site-Level PROHIBITED Geographic Area", "pick", choices = vocab()[["Site-Level PROHIBITED Geographic Area"]], prefix = "scen__", multiple = TRUE),
      make_input("Max Release Height", "text", prefix = "scen__"),
      make_input("Max Wind Speed (mph)", "text", prefix = "scen__"),
      textAreaInput(inputId = scenario_textarea_id, label = scenario_textarea_label, value = "", rows = 4, resize = "vertical", width = "100%")
    )
  })
  
  # ----- Validation -----
  iv <- shinyvalidate::InputValidator$new()
  session$onFlushed(function() {
    for (f in scenario_area_rate_fields) {
      id <- paste0("scen__", idsafe(f))
      iv$add_rule(id, function(value) {
        if (is.null(value) || is.na(value)) return(NULL)
        if (!is.numeric(value)) return("Must be a number")
        if (value < 0) return("Must be ≥ 0")
        NULL
      })
    }
    for (f in scenario_numeric_fields) {
      id <- paste0("scen__", idsafe(f))
      iv$add_rule(id, function(value) {
        if (is.null(value) || is.na(value)) return(NULL)
        if (!is.numeric(value)) return("Must be a number")
        if (value < 0) return("Must be ≥ 0")
        NULL
      })
    }
    iv$enable()
  }, once = TRUE)
  
  # ----- Collectors -----
  collect_row <- function(input, fields, prefix = "") {
    ids <- paste0(prefix, idsafe(fields))
    vals <- map(ids, ~ input[[.x]])
    vals <- map_chr(vals, collapse_multi)
    tibble(!!!setNames(vals, fields))
  }
  collect_scenario_row <- function(input, fields, prefix,
                                   area_rate_fields,
                                   area_rate_defaults,
                                   numeric_fields) {
    vals <- vector("list", length(fields))
    names(vals) <- fields
    for (f in fields) {
      id <- paste0(prefix, idsafe(f))
      if (f %in% area_rate_fields) {
        val <- input[[id]]
        if (is.null(val) || is.na(val)) {
          vals[[f]] <- NA_character_
        } else {
          numu  <- input[[paste0(id, "__numunit")]]  %||% area_rate_defaults[[f]]$num
          areau <- input[[paste0(id, "__areaunit")]] %||% area_rate_defaults[[f]]$area
          vals[[f]] <- sprintf("%s %s/%s", val, numu, areau)
        }
      } else if (f %in% numeric_fields) {
        val <- input[[id]]
        vals[[f]] <- if (is.null(val) || is.na(val)) NA_character_ else as.character(val)
      } else {
        v <- input[[id]]
        if (is.null(v)) {
          vals[[f]] <- NA_character_
        } else if (is.character(v)) {
          vals[[f]] <- if (length(v) > 1) paste(v, collapse = "; ") else if (nzchar(v)) v else NA_character_
        } else {
          vals[[f]] <- as.character(v)
        }
      }
    }
    tibble::as_tibble(vals)
  }
  
  # ----- Product actions -----
  observeEvent(input$add_prod, {
    new_row <- collect_row(input, product_fields, prefix = "prod__")
    next_id_num <- nrow(prod_dat()) + 1
    new_row <- tibble::add_column(new_row, Product_ID = sprintf("P%03d", next_id_num), .before = 1)
    prod_dat(dplyr::bind_rows(prod_dat(), new_row))
    updateSelectInput(session, "current_product",
                      choices = product_choices(),
                      selected = sprintf("P%03d", next_id_num))
    showNotification("Product-level row added.", type = "message")
  })
  
  # ----- Scenario actions -----
  observeEvent(input$add_scen, {
    tryCatch({
      pd <- prod_dat()
      if (nrow(pd) == 0) {
        showNotification("Please add a product first (Product-level > Add row).", type = "warning")
        return()
      }
      cur_prod <- input$current_product
      if (is.null(cur_prod) || !nzchar(cur_prod) || !any(pd$Product_ID == cur_prod)) {
        showNotification("Please select a linked product in the 'Linked product' dropdown.", type = "warning")
        return()
      }
      if (!iv$is_valid()) {
        iv$enable(); iv$show()
        showNotification("Please correct the highlighted scenario fields, then try again.", type = "error")
        return()
      }
      scen_row <- collect_scenario_row(
        input, scenario_fields, prefix = "scen__",
        area_rate_fields   = scenario_area_rate_fields,
        area_rate_defaults = scenario_area_rate_defaults,
        numeric_fields     = scenario_numeric_fields
      )
      other <- input[[scenario_textarea_id]]
      scen_row[[scenario_textarea_label]] <- if (is.null(other) || !nzchar(other)) NA_character_ else other
      
      prod_row <- pd %>%
        dplyr::filter(Product_ID == cur_prod) %>%
        dplyr::select(Product_ID, dplyr::all_of(product_fields))
      if (nrow(prod_row) != 1) {
        showNotification(sprintf("Expected 1 product row for '%s', found %d.", cur_prod, nrow(prod_row)), type = "error")
        return()
      }
      new_row <- dplyr::bind_cols(prod_row[1, , drop = FALSE], scen_row)
      sd_new <- dplyr::bind_rows(scen_dat(), new_row)
      scen_dat(sd_new)
      showNotification(sprintf("Scenario-level row added. Total scenarios: %d", nrow(sd_new)), type = "message")
    }, error = function(e) {
      showNotification(paste("Failed to add scenario:", conditionMessage(e)), type = "error", duration = 8)
    })
  })
  
  observeEvent(input$relink_selected, {
    pd <- prod_dat()
    sd <- scen_dat()
    cur_prod <- input$current_product
    sel <- input$tbl_scen_rows_selected
    if (nrow(pd) == 0 || is.null(cur_prod) || !any(pd$Product_ID == cur_prod)) {
      showNotification("Select a product to relink to.", type = "warning")
      return()
    }
    if (nrow(sd) == 0) {
      showNotification("No scenario rows to relink.", type = "warning")
      return()
    }
    if (length(sel) == 0) {
      showNotification("Select one or more scenario rows in the table to relink.", type = "warning")
      return()
    }
    pd_sel <- pd %>%
      dplyr::filter(Product_ID == cur_prod) %>%
      dplyr::select(Product_ID, dplyr::all_of(product_fields))
    cols_to_update <- c("Product_ID", product_fields)
    for (col in cols_to_update) {
      if (!col %in% names(sd)) sd[[col]] <- NA_character_
      sd[sel, col] <- pd_sel[[col]][1]
    }
    sd <- sd %>% dplyr::select(Product_ID, dplyr::all_of(product_fields), dplyr::everything())
    scen_dat(sd)
    showNotification(sprintf("Relinked %d selected scenario row(s) to the chosen product.", length(sel)), type = "message")
  })
  
  observeEvent(input$dup_scen, {
    sel <- input$tbl_scen_rows_selected
    if (length(sel) == 0) {
      showNotification("Select one or more scenario rows to duplicate.", type = "warning")
      return()
    }
    sd <- scen_dat()
    copies <- sd[sel, , drop = FALSE]
    scen_dat(dplyr::bind_rows(sd, copies))
    showNotification(sprintf("Duplicated %d scenario row(s).", length(sel)), type = "message")
  })
  
  observeEvent(input$clone_to_form, {
    sel <- input$tbl_scen_rows_selected
    if (length(sel) != 1) {
      showNotification("Select exactly one scenario row to load into the form.", type = "warning")
      return()
    }
    sd <- scen_dat()
    row <- sd[sel, , drop = FALSE]
    if ("Product_ID" %in% names(row)) {
      updateSelectInput(session, "current_product", choices = product_choices(), selected = row$Product_ID[1])
    }
    for (nm in scenario_picklist_fields) {
      id <- paste0("scen__", idsafe(nm))
      vals <- split_multi(row[[nm]][1] %||% "")
      ch <- vocab()[[nm]] %||% character(0)
      missing <- setdiff(vals, ch)
      ch <- c(ch, missing)
      try(updateSelectizeInput(session, id, choices = ch, selected = vals), silent = TRUE)
    }
    for (nm in scenario_numeric_fields) {
      id <- paste0("scen__", idsafe(nm))
      val_num <- extract_number(row[[nm]][1] %||% "")
      try(updateNumericInput(session, id, value = val_num), silent = TRUE)
    }
    for (nm in scenario_area_rate_fields) {
      base_id     <- paste0("scen__", idsafe(nm))
      numunit_id  <- paste0(base_id, "__numunit")
      areaunit_id <- paste0(base_id, "__areaunit")
      raw <- row[[nm]][1] %||% ""
      val_num <- extract_number(raw)
      units <- parse_rate_units(raw,
                                default_num  = scenario_area_rate_defaults[[nm]]$num,
                                default_area = scenario_area_rate_defaults[[nm]]$area)
      num_choices  <- unique(c(unit_choices_for_field(nm), units$num))
      area_choices <- unique(c(area_units, units$area))
      try(updateNumericInput(session, base_id, value = val_num), silent = TRUE)
      try(updateSelectizeInput(session, numunit_id, choices = num_choices, selected = units$num),  silent = TRUE)
      try(updateSelectizeInput(session, areaunit_id, choices = area_choices, selected = units$area), silent = TRUE)
    }
    text_fields <- setdiff(scenario_fields, c(scenario_picklist_fields, scenario_numeric_fields, scenario_area_rate_fields))
    for (nm in text_fields) {
      id <- paste0("scen__", idsafe(nm))
      val <- row[[nm]][1] %||% ""
      try(updateTextInput(session, id, value = val), silent = TRUE)
    }
    if (scenario_textarea_label %in% names(row)) {
      try(updateTextAreaInput(session, scenario_textarea_id, value = row[[scenario_textarea_label]][1] %||% ""), silent = TRUE)
    }
    showNotification("Scenario loaded into form. Edit fields and click 'Add row' to save as a new scenario.", type = "message")
  })
  
  # ----- Delete selected -----
  observeEvent(input$del_prod, {
    pd <- prod_dat()
    sel <- input$tbl_prod_rows_selected
    if (length(sel) == 0) {
      showNotification("Select one or more product rows to delete.", type = "warning")
      return()
    }
    ids_to_delete <- pd$Product_ID[sel]
    sd <- scen_dat()
    n_linked <- if ("Product_ID" %in% names(sd)) sum(sd$Product_ID %in% ids_to_delete) else 0
    showModal(modalDialog(
      title = "Confirm deletion",
      size = "m",
      tagList(
        tags$p(sprintf("You are about to delete %d product row(s).", length(sel))),
        if (n_linked > 0) tags$p(sprintf("There are %d linked scenario row(s) referencing these products.", n_linked)),
        checkboxInput("delete_linked", "Also delete linked scenario rows (cascade delete)", value = n_linked > 0)
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_products", "Delete", class = "btn-danger")
      ),
      easyClose = FALSE
    ))
  })
  observeEvent(input$confirm_delete_products, {
    removeModal()
    pd <- prod_dat()
    sd <- scen_dat()
    sel <- input$tbl_prod_rows_selected
    if (length(sel) == 0) return()
    ids_to_delete <- pd$Product_ID[sel]
    if (isTRUE(input$delete_linked) && "Product_ID" %in% names(sd)) {
      sd <- sd %>% dplyr::filter(!(Product_ID %in% ids_to_delete))
      scen_dat(sd)
    }
    pd <- pd[-sel, , drop = FALSE]
    prod_dat(pd)
    updateSelectInput(session, "current_product", choices = product_choices(), selected = unname(tail(product_choices(), 1)))
    showNotification(
      sprintf("Deleted %d product row(s)%s.", length(sel), if (isTRUE(input$delete_linked)) " and linked scenarios" else ""),
      type = "message"
    )
  })
  
  observeEvent(input$del_scen, {
    sel <- input$tbl_scen_rows_selected
    if (length(sel) == 0) {
      showNotification("Select one or more scenario rows to delete.", type = "warning")
      return()
    }
    sd <- scen_dat()
    sd <- sd[-sel, , drop = FALSE]
    scen_dat(sd)
    showNotification(sprintf("Deleted %d scenario row(s).", length(sel)), type = "message")
  })
  
  # ----- Tables -----
  output$tbl_prod <- DT::renderDT({
    dat <- prod_dat()
    req(ncol(dat) > 0)
    df <- as.data.frame(dat)
    prod_idx <- which(names(df) == "Product_ID")
    opts <- list(pageLength = 25, scrollX = TRUE, searching = FALSE, lengthChange = FALSE)
    if (length(prod_idx) == 1) {
      opts$columnDefs <- list(list(visible = FALSE, targets = prod_idx - 1))
    }
    DT::datatable(df, options = opts, rownames = FALSE, selection = "multiple")
  })
  output$tbl_scen <- renderDT({
    dat <- scen_dat()
    req(ncol(dat) > 0)
    df <- as.data.frame(dat)
    prod_idx <- which(names(df) == "Product_ID")
    opts <- list(pageLength = 25, scrollX = TRUE, searching = FALSE, lengthChange = FALSE)
    if (length(prod_idx) == 1) {
      opts$columnDefs <- list(list(visible = FALSE, targets = prod_idx - 1))
    }
    DT::datatable(df, options = opts, rownames = FALSE, selection = "multiple")
  })
  
  # ----- Downloads -----
  output$dl_prod <- downloadHandler(
    filename = function() paste0("product_entries_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(prod_dat(), file, na = "")
  )
  output$dl_scen <- downloadHandler(
    filename = function() paste0("scenario_entries_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(scen_dat(), file, na = "")
  )
  
  # ---- Clear forms ----
  observeEvent(input$clear_prod, {
    product_text_fields <- c(
      "EPA Registration Number","PC Code","AI Name",
      "Co-Formulated AI","% AI",
      "AI Concentration (Or Product Density if liquid)"
    )
    lapply(product_text_fields, function(field) {
      id <- paste0("prod__", idsafe(field))
      updateTextInput(session, id, value = "")
    })
    updateSelectizeInput(session, "prod__Physical_Form", selected = character(0))
    updateSelectizeInput(session, "prod__Product_level_PPE", selected = character(0))
    updateSelectizeInput(session, "prod__RUP", selected = NULL)
  })
  observeEvent(input$clear_scenario, {
    scenario_text_fields <- c(
      "Max # App/Year", "Max # App/Crop Cycle",
      "Max Number of Seasons/Crop Cycles per year",
      "RTI (d)", "REI (H)", "PHI (d)", "PGI (d)", "PSI (d)",
      "Max Release Height", "Max Wind Speed (mph)"
    )
    lapply(scenario_text_fields, function(field) {
      id <- paste0("scen__", idsafe(field))
      updateTextInput(session, id, value = "")
    })
    scenario_select_fields <- c(
      "Crop Use Site", "Non Crop Use Site", "Location", "App Target",
      "App Type", "App Equipment Type", "App Timing (Site)",
      "App Timing (Pest)", "ASABE Droplet Size", "Buffered Area (Term)",
      "Pollinator Protection Statement", "Soil Type Restrictions",
      "Site-Level ALLOWED Geographic Area", "Site-Level PROHIBITED Geographic Area"
    )
    lapply(scenario_select_fields, function(field) {
      id <- paste0("scen__", idsafe(field))
      updateSelectizeInput(session, id, selected = character(0))
    })
    lapply(c("Buffered Area (ft)"), function(field) {
      id <- paste0("scen__", idsafe(field))
      updateNumericInput(session, id, value = NA_real_)
    })
    for (f in scenario_area_rate_fields) {
      base_id     <- paste0("scen__", idsafe(f))
      numunit_id  <- paste0(base_id, "__numunit")
      areaunit_id <- paste0(base_id, "__areaunit")
      updateNumericInput(session, base_id, value = NA_real_)
      num_choices <- unit_choices_for_field(f)
      updateSelectizeInput(session, numunit_id,  choices = num_choices,
                           selected = scenario_area_rate_defaults[[f]]$num)
      updateSelectizeInput(session, areaunit_id, choices = area_units,
                           selected = scenario_area_rate_defaults[[f]]$area)
    }
    updateTextAreaInput(session, "scen__Other_Site_Scenario_Specific_Restrictions_Limitations", value = "")
  })
}

shinyApp(ui, server)
