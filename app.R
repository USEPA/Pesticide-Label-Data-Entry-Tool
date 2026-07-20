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
library(shinythemes)

# - Manifest 
#rsconnect::writeManifest()

# ---------------- CONFIG ----------------
workbook_path <- "data/templates/UST_Active Ingredient (PC Code) UST Report_Template_active.xlsx"
workbook_name <- "UST_Active Ingredient (PC Code) UST Report_Template_active.xlsx"

# Define the bslib theme if you want to apply one
theme <- bs_theme(version = 5)

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
# Coerce uploaded data to the expected schema (all character columns, correct order)
ensure_expected_columns <- function(df, expected_labels) {
  # Keep only expected columns
  df <- dplyr::select(df, dplyr::any_of(expected_labels))
  # Add any missing expected columns as NA_character_
  missing <- setdiff(expected_labels, names(df))
  for (m in missing) df[[m]] <- NA_character_
  # Reorder to expected order and coerce all to character
  df <- dplyr::select(df, dplyr::all_of(expected_labels))
  df <- dplyr::mutate(df, dplyr::across(dplyr::everything(), as.character))
  df
}

# ---------------- Read vocab from workbook ----------------
read_vocab_col <- function(path, sheet, col_name) {
  dat <- suppressMessages(readxl::read_excel(path, sheet = sheet))
  if (!col_name %in% names(dat)) return(character(0))
  vals <- dat[[col_name]]
  vals <- as.character(vals)
  vals <- trimws(vals)
  vals <- vals[!is.na(vals) & nzchar(vals)]
  sort(unique(vals))
}
read_vocab_range <- function(path, sheet, range) {
  dat <- suppressMessages(readxl::read_excel(path, sheet = sheet, range = range, col_names = FALSE))
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
    "Specific App Equipment"          = read_vocab_col(path, "App. Equipment", "Specific Application Equipment"),
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
make_input <- function(field_label, type = c("text", "numeric", "pick"), choices = NULL, prefix = "", multiple = FALSE, placeholder = "Type or pick…") {
  type <- match.arg(type)
  input_id <- paste0(prefix, idsafe(field_label))
  if (type == "pick") {
    selectizeInput(
      inputId = input_id, label = field_label,
      choices = choices %||% character(0),
      multiple = multiple,
      options = list(
        create = TRUE,
        placeholder = placeholder, #<-now customizable
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
volume_units <- c("gal", "qt", "L", "mL", "fl oz", "mg %", "ppm", "ppb")
area_units   <- c("ac", "ha", "seed", "animal", "kg", "CWT", "feet (linear)", "linear feet of depth", "sanitizer no area needed", "target concentration no area needed")

scenario_area_rate_allowed <- list(
  "Min Diluent Quantity (Gal Spray Soln per Acre)" = list(allow_weight = FALSE, allow_volume = TRUE),
  "Product Max Rate/App"                           = list(allow_weight = TRUE,  allow_volume = TRUE),
  "AI Max Rate/App"                                = list(allow_weight = TRUE,  allow_volume = FALSE),
  "Product Max Rate/Year"                          = list(allow_weight = TRUE,  allow_volume = TRUE),
  "Product Max Rate/Crop Cycle"                    = list(allow_weight = TRUE,  allow_volume = TRUE),
  "AI Max Rate/Year"                               = list(allow_weight = TRUE,  allow_volume = FALSE),
  "AI Max Rate/Crop Cycle"                         = list(allow_weight = TRUE,  allow_volume = FALSE)
)

unit_choices_for_field <- function(field) {
  allow <- scenario_area_rate_allowed[[field]] %||% list(allow_weight = TRUE, allow_volume = TRUE)
  unique(c(
    if (isTRUE(allow$allow_weight)) weight_units else character(0),
    if (isTRUE(allow$allow_volume)) volume_units else character(0)
  ))
}

# ---------- UI: numeric + numerator-unit + area-unit ----------
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

# ---------- Parser to restore units ----------
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
  "AI Concentration",
  "RUP",
  "Product-level PPE"
)

scenario_fields <- c(
  "Crop Use Site","Non Crop Use Site",
  "Location","App Target","App Type","App Equipment Type","Specific App Equipment",
  "App Timing (Site)","App Timing (Pest)",
  "Min Diluent Quantity (Gal Spray Soln per Acre)",
  "Product Max Rate/App",
  "AI Max Rate/App","Max # App/Year","Max # App/Crop Cycle",
  "Product Max Rate/Year","Product Max Rate/Crop Cycle",
  "AI Max Rate/Year","AI Max Rate/Cycle",
  "Max Number of Seasons/Crop Cycles per year","RTI (days)","REI (hours)","PHI (days)","PGI (days)","PSI (days)",
  "ASABE Droplet Size","Max Release Height (ft)","Max Wind Speed (mph)",
  "Buffered Area (ft)","Buffered Area (Term)",
  "Site-Level ALLOWED Geographic Area","Site-Level PROHIBITED Geographic Area",
  "Soil Type Restrictions",
  "Pollinator Protection Statement",
  "Other Site/Scenario Specific Restrictions & Limitations"
)

scenario_picklist_fields <- c(
  "Crop Use Site","Non Crop Use Site",
  "Location","App Target","App Type","App Equipment Type","Specific App Equipment",
  "App Timing (Site)","App Timing (Pest)",
  "ASABE Droplet Size","Buffered Area (Term)",
  "Pollinator Protection Statement","Soil Type Restrictions",
  "Site-Level ALLOWED Geographic Area","Site-Level PROHIBITED Geographic Area"
)

scenario_numeric_fields <- c("Buffered Area (ft)")

scenario_area_rate_fields <- c(
  "Min Diluent Quantity (Gal Spray Soln per Acre)",
  "Product Max Rate/App",
  "AI Max Rate/App",
  "Product Max Rate/Year",
  "Product Max Rate/Crop Cycle",
  "AI Max Rate/Year",
  "AI Max Rate/Cycle"
)

scenario_area_rate_defaults <- list(
  "Min Diluent Quantity (Gal Spray Soln per Acre)" = list(num = "gal",   area = "ac"),
  "Product Max Rate/App"                      = list(num = "gal",   area = "ac"),
  "AI Max Rate/App"                                = list(num = "lb",    area = "ac"),
  "Product Max Rate/Year"                          = list(num = "fl oz", area = "ac"),
  "Product Max Rate/Crop Cycle"                    = list(num = "fl oz", area = "ac"),
  "AI Max Rate/Year"                               = list(num = "lb",    area = "ac"),
  "AI Max Rate/Cycle"                              = list(num = "lb",    area = "ac")
)

# ---------------- UI ----------------

my_theme <- bs_theme(
  version = 5,
  "font-size-base" = "0.85rem", 
  "line-height-base" = "1.3"
) |> 
  bs_add_rules(c(
    ":root {",
    "  --bsb-sidebar-width: 200px; bsb-sidebar-width-md: 200px;",
    "  --bs-body-font-size: 0.85rem;",
    "}"
  ))

ui <- page_fillable(
  theme = my_theme,
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "PLDET_icon.png"),
    tags$style(HTML("
 .bslib-full-screen-enter {
      bottom: auto !important;
      top: 1px !important;
      right: 1px !important;
      width: 28px !important;
      height: 28px !important;
      min-width: 28px !important;
      min-height: 28px !important;
      padding: 0 !important;
      display: inline-flex !important;
      align-items: center !important;
      justify-content: center !important;
      border-radius: 50% !important;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1) !important;
    }
    .bslib-full-screen-enter svg {
      transform: scale(0.75) !important;
    }
    .resizable-card {
      transition: height 0.3s ease;
    }
    .horizontal-resize-card, .vertical-resize-card {
      transition: width 0.3s ease, height 0.3s ease; /* Ensure smooth transition */
    }
    #horizontal-resize-btn, #vertical-resize-btn {
      cursor: pointer;
      background-color: transparent;
      border: none;
      font-size: 1.2em;
      padding: 5px;
      color: gray;
    }
    .sub-card {
      border: none; /* Removes border around the entire sub-card if needed */
    }
    .sub-card .card-header {
      border-bottom: 1px solid white; /* Changes the header-bottom border to white */
    }
    .sub-card .card-body {
      border-top: 1px solid white; /* Ensures body-top border is white to match header */
    }
    /* If using borders inside card-body regions */
    .sub-card .card-footer {
      border-top: 1px solid white; /* Change footer-top border to match */
    }
    .ust-rate-row { 
      display: flex; 
      align-items: center; 
      gap: 6px; 
      flex-wrap: nowrap; 
    }
    .ust-rate-row .ust-numeric { 
      flex: 1 1 auto; 
      min-width: 70px; 
    }
    .ust-rate-row .ust-units { 
      display: flex; 
      align-items: center; 
      gap: 6px; 
      flex: 0 0 auto; 
      white-space: nowrap; 
    }
    .ust-units .shiny-input-container { 
      width: auto !important; 
      display: inline-block; 
    }
    .ust-units .ust-unit .selectize-control { 
      width: auto !important; 
      min-width: 75px; /* Ensure dropdowns remain visible */
    }
    .ust-units .ust-unit:first-of-type .selectize-control { 
      min-width: 50px; 
    }
    .ust-units .ust-unit:last-of-type .selectize-control { 
      min-width: 50px; 
    }
    .selectize-control.single .selectize-input:after {
      top: auto !important;
      bottom: 3px !important;
      right: 3px !important;
    }
   "))
  ),
  
  layout_columns(
    col_widths=12,
    gap="0px",
    
    card(
      full_screen=TRUE,
      height="65vh",
      class = "vertical-resize-card",
      card_header(
        fluidRow(
          column(3, tags$strong("UST Data Entry Tool")),
          column(6, tags$strong("Template:", style = "display: inline;"),
                 uiOutput("notebook_path_display", inline = TRUE)),
          column(3, tags$a(
            href = "https://github.com/USEPA/Pesticide-Label-Data-Entry-Tool/raw/refs/heads/main/data/templates/UST_Active%20Ingredient%20(PC%20Code)%20UST%20Report_Template_active.xlsx",
            "Template File: For reference and definitions", 
            target = "_blank"
          ))
        )
      ),
      card_body(
        layout_columns(
          gap = "0px",
          
          div(
            style = "display: flex;", 
            id = "card-container",
            
            # First card
            card(
              class = "horizontal-resize-card sub-card",
              style = "width: 15%;", 
              card_header("Product-Level Inputs"),
              card_body(
                uiOutput("product_form")
              )
            ),
            
            # Second card with toggle button in header
            card(
              class = "horizontal-resize-card sub-card",
              style = "width: 85%;",
              card_header(
                fluidRow(
                  column(4,
                         "Use-Level Inputs",
                         actionButton(
                           inputId = "horizontal-resize-btn",
                           label = NULL,
                           icon = icon("arrow-left"),
                           class = "btn-link",
                           style = "margin-left: auto;" # Align right in header
                         )),
                  column(4,),
                  column(4,))
              ),
              card_body(
                fluidRow(
                  column(3,
                         h5("Use Site Descriptors"),
                         tags$div(style = "height: 5px;"),
                         uiOutput("scenario_use_site_col1"),
                         uiOutput("scenario_use_site_col2")),
                  column(3,
                         h5("Rate Descriptors"),
                         uiOutput("scenario_rate_col1"),
                         uiOutput("scenario_rate_col2")),
                  column(3,
                         h5("Restrictions"),
                         uiOutput("scenario_restrictions_col1")),
                  column(3,
                         uiOutput("scenario_restrictions_col2"))
                )
              )
            )
          )
        )
      ),
      card_footer(
        fluidRow(
          column(4, actionButton("add_entry", "Add row", class = "btn-sm btn-primary", icon = icon("plus"))),
          column(4),
          column(4, style = "text-align: right;", 
                 actionButton("clear_use_only", "Clear Use", class = "btn-sm", icon = icon("eraser")),
                 actionButton("clear_all", "Clear All", class = "btn-sm", icon = icon("eraser")))
        )
      )
    ),
    card(
      full_screen=TRUE,
      height="35vh",
      class = "vertical-resize-card",
      card_header(
        fluidRow(
          column(4, "Data Display", 
                 actionButton("vertical-resize-btn", label = icon("arrow-up"), class = "btn-link")),
          column(4),
          column(4, style = "text-align: right;")
        )
      ),
      card_body(
        div(DTOutput("tbl_scen"), style = "font-size: 85%;")
      ),
      card_footer(
        fluidRow(
          column(4,
                 downloadButton("dl_scen", "Download CSV", class = "btn-sm"),
                 actionButton("upload_scen", "Upload CSV", icon = icon("upload"), class = "btn-sm btn-secondary")),
          column(4, style = "text-align: center;",
                 actionButton("clone_to_form", "Load selected to Data Entry", class = "btn-sm", icon = icon("sign-in-alt")),
                 actionButton("dup_scen", "Duplicate selected", class = "btn-sm", icon = icon("copy"))),
          column(4, style = "text-align: right;",
                 actionButton("del_scen", "Delete selected", class = "btn-sm", icon = icon("remove")))
        )
      )
    )
  ),
  
  # JavaScript for vertical resizing
  tags$script(HTML("
    document.getElementById('vertical-resize-btn').addEventListener('click', function() {
      var card1 = document.querySelectorAll('.vertical-resize-card')[0];
      var card2 = document.querySelectorAll('.vertical-resize-card')[1];
      var buttonIcon = document.querySelector('#vertical-resize-btn i');

      if (card1.style.height !== '35vh') { // Check if it's not already in minimized state
        card1.style.height = '35vh'; // small size for Data Entry
        card2.style.height = '65vh'; // large size for Data Display
        buttonIcon.classList.remove('fa-arrow-up'); // Change icon to down arrow
        buttonIcon.classList.add('fa-arrow-down');
      } else {
        card1.style.height = '65vh'; // revert to original size for Data Entry
        card2.style.height = '35vh'; // revert to original size for Data Display
        buttonIcon.classList.remove('fa-arrow-down'); // Change icon to up arrow
        buttonIcon.classList.add('fa-arrow-up');
      }
    });
  ")),
  
  # JavaScript for horizontal resizing
  tags$script(HTML("
    document.getElementById('horizontal-resize-btn').addEventListener('click', function() {
      var cards = document.querySelectorAll('#card-container .horizontal-resize-card'); // Select cards inside container
      var buttonIcon = document.querySelector('#horizontal-resize-btn i');

      if (cards[0].style.width !== '0%') { // Check non-contracted state
        cards[0].style.width = '0%'; // Contract first card
        cards[1].style.width = '100%'; // Expand second card
        buttonIcon.classList.remove('fa-arrow-left');
        buttonIcon.classList.add('fa-arrow-right');
      } else {
        cards[0].style.width = '15%'; // Expand first card
        cards[1].style.width = '85%'; // Contract second card
        buttonIcon.classList.remove('fa-arrow-right');
        buttonIcon.classList.add('fa-arrow-left');
      }
    });
  ")),
  tags$style(HTML("
  /* Ensure there's no margin or padding between the subcards and the container */
  #card-container {
    margin: 0;
    padding: 0;
  }

  .sub-card {
    margin: 0;
    padding: 0;
    border: none; /* Optional: remove borders for tighter layout */
  }
"))
)

# ---------------- Server ----------------
server <- function(input, output, session) {
  vocab <- reactiveVal(NULL)
  upload_buffer <- reactiveVal(NULL)
  
  # Empty schema: table stores BOTH product + scenario fields (no Product_ID)
  make_empty_scen <- function() {
    cols <- c(product_fields, scenario_fields)
    tibble::as_tibble(setNames(rep(list(character()), length(cols)), cols))
  }
  scen_dat <- reactiveVal(make_empty_scen())
  
  # ---- Upload modal (combined table) ----
  show_upload_modal <- function() {
    modalDialog(
      fileInput("file_upload_scenario", "Choose CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_upload_scenario", "Upload", class = "btn-primary")
      ),
      easyClose = FALSE,
      size = "m",
      title = "Upload to Table"
    )
  }
  
  observeEvent(input$upload_scen, {
    upload_buffer(NULL)  # clear any previous buffer
    showModal(modalDialog(
      fileInput(
        "file_upload_scenario",
        "Choose CSV File",
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_upload_scenario", "Upload", class = "btn-primary")
      ),
      easyClose = FALSE,
      size = "m",
      title = "Upload to Table"
    ))
  })
  
  observeEvent(input$confirm_upload_scenario, {
    req(input$file_upload_scenario)
    removeModal()
    
    expected_labels <- c(product_fields, scenario_fields)
    
    # Read as all-character
    data <- tryCatch({
      readr::read_csv(
        input$file_upload_scenario$datapath,
        col_types = readr::cols(.default = readr::col_character()),
        show_col_types = FALSE, progress = FALSE
      )
    }, error = function(e) NULL)
    
    if (is.null(data)) {
      showNotification("Failed to read file.", type = "error")
      return()
    }
    
    # Header validation using idsafe
    expected_fields <- purrr::map_chr(expected_labels, idsafe)
    uploaded_fields <- purrr::map_chr(names(data), idsafe)
    if (!all(expected_fields %in% uploaded_fields)) {
      showNotification("File format does not match expected fields. It should contain all required columns.",
                       type = "error")
      return()
    }
    
    # Coerce to expected schema
    data <- ensure_expected_columns(data, expected_labels)
    upload_buffer(data)
    
    showModal(modalDialog(
      title = "File Uploaded Successfully",
      selectInput("upload_mode_scen", "Choose an option:", choices = c("Append", "Replace")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("commit_upload_scen", "Commit Changes", class = "btn-success")
      ),
      easyClose = FALSE
    ))
  })
  
  observeEvent(input$commit_upload_scen, {
    req(upload_buffer())
    expected_labels <- c(product_fields, scenario_fields)
    
    data <- ensure_expected_columns(upload_buffer(), expected_labels)
    upload_buffer(NULL)
    
    if (identical(input$upload_mode_scen, "Append")) {
      existing <- ensure_expected_columns(scen_dat(), expected_labels)
      scen_dat(dplyr::bind_rows(existing, data) |> dplyr::distinct())
    } else {
      scen_dat(data)
    }
    
    removeModal()
    showNotification("Data uploaded successfully.", type = "message")
  })
  
  # ---------- Load vocab
  observe({
    validate(need(file.exists(workbook_path),
                  paste("Workbook not found. Check path:\n", workbook_path)))
    vocab(build_vocab(workbook_path))
  })
  observeEvent(input$reload, {
    vocab(build_vocab(workbook_path))
    showNotification("Workbook reloaded.", type = "message")
  })
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
  
  # ---- Product form ----
  output$product_form <- renderUI({
    req(vocab())
    tagList(
      make_input("EPA Registration Number", "text", prefix = "prod__"),
      make_input("AI Name", "text", prefix = "prod__"),
      make_input("PC Code", "text", prefix = "prod__"),
      make_input("Co-Formulated AI", "pick", choices = NULL, prefix = "prod__", multiple = TRUE, placeholder = "Type each AI name and press enter"),
      make_input("Physical Form", "pick", choices = vocab()[["Physical Form"]], prefix = "prod__", multiple = TRUE),
      make_input("% AI", "text", prefix = "prod__"),
      make_input("AI Concentration", "text", prefix = "prod__"),
      make_input("RUP", "pick", choices = vocab()[["RUP"]], prefix = "prod__", multiple = FALSE),
      make_input("Product-level PPE", "pick", choices = vocab()[["Product-level PPE"]], prefix = "prod__", multiple = TRUE)
    )
  })
  
  # ---- Scenario use site columns ----
  output$scenario_use_site_col1<-renderUI({
    req(vocab())
    tagList(
      make_input("Crop Use Site", "pick", choices = vocab()[["Crop Use Site"]], prefix = "scen__", multiple = TRUE),
      make_input("Non Crop Use Site", "pick", choices = vocab()[["Non Crop Use Site"]], prefix = "scen__", multiple = TRUE)
    )
  })
  
  output$scenario_use_site_col2<-renderUI({
    req(vocab())
    tagList(
      make_input("Location", "pick", choices = vocab()[["Location"]], prefix = "scen__", multiple = TRUE),
      make_input("App Target", "pick", choices = vocab()[["App Target"]], prefix = "scen__", multiple = TRUE),
      make_input("App Type", "pick", choices = vocab()[["App Type"]], prefix = "scen__", multiple = TRUE),
      make_input("App Equipment Type", "pick", choices = vocab()[["App Equipment Type"]], prefix = "scen__", multiple = TRUE),
      make_input("Specific App Equipment", "pick", choices = vocab()[["Specific App Equipment"]], prefix = "scen__", multiple = TRUE),
      make_input("App Timing (Site)", "pick", choices = vocab()[["App Timing (Site)"]], prefix = "scen__", multiple = TRUE),
      make_input("App Timing (Pest)", "pick", choices = vocab()[["App Timing (Pest)"]], prefix = "scen__", multiple = TRUE, placeholder = "Use only if all crop stages is selected in app timing (site) field"),
      )
    
  })
  
  # ---- Scenario rate columns ----
  output$scenario_rate_col1<-renderUI({
    req(vocab())
    tagList(
      make_area_rate_input("Min Diluent Quantity (Gal Spray Soln per Acre)",
                           default_num_unit  = scenario_area_rate_defaults[["Min Diluent Quantity (Gal Spray Soln per Acre)"]]$num,
                           default_area_unit = scenario_area_rate_defaults[["Min Diluent Quantity (Gal Spray Soln per Acre)"]]$area,
                           prefix = "scen__", allow_weight = FALSE, allow_volume = TRUE
      ),
      make_area_rate_input(
        "Product Max Rate/App",
        default_num_unit  = scenario_area_rate_defaults[["Product Max Rate/App"]]$num,
        default_area_unit = scenario_area_rate_defaults[["Product Max Rate/App"]]$area,
        prefix = "scen__", allow_weight = TRUE, allow_volume = TRUE
      ),
      make_area_rate_input(
        "AI Max Rate/App",
        default_num_unit  = scenario_area_rate_defaults[["AI Max Rate/App"]]$num,
        default_area_unit = scenario_area_rate_defaults[["AI Max Rate/App"]]$area,
        prefix = "scen__", allow_weight = TRUE, allow_volume = FALSE
      ),
      make_input("Max # App/Year", "text", prefix = "scen__"),
      make_input("Max # App/Crop Cycle", "text", prefix = "scen__")
    )
  })
  
  output$scenario_rate_col2<-renderUI({
    req(vocab())
    tagList(
      make_area_rate_input(
        "Product Max Rate/Year",
        default_num_unit  = scenario_area_rate_defaults[["Product Max Rate/Year"]]$num,
        default_area_unit = scenario_area_rate_defaults[["Product Max Rate/Year"]]$area,
        prefix = "scen__", allow_weight = TRUE, allow_volume = TRUE
      ),
      make_area_rate_input(
        "AI Max Rate/Year",
        default_num_unit  = scenario_area_rate_defaults[["AI Max Rate/Year"]]$num,
        default_area_unit = scenario_area_rate_defaults[["AI Max Rate/Year"]]$area,
        prefix = "scen__", allow_weight = TRUE, allow_volume = FALSE
      ),
      make_area_rate_input(
        "Product Max Rate/Crop Cycle",
        default_num_unit  = scenario_area_rate_defaults[["Product Max Rate/Crop Cycle"]]$num,
        default_area_unit = scenario_area_rate_defaults[["Product Max Rate/Crop Cycle"]]$area,
        prefix = "scen__", allow_weight = TRUE, allow_volume = TRUE
      ),
      make_area_rate_input(
        "AI Max Rate/Cycle",
        default_num_unit  = scenario_area_rate_defaults[["AI Max Rate/Cycle"]]$num,
        default_area_unit = scenario_area_rate_defaults[["AI Max Rate/Cycle"]]$area,
        prefix = "scen__", allow_weight = TRUE, allow_volume = FALSE
      ),
      make_input("Max Number of Seasons/Crop Cycles per year", "text", prefix = "scen__")
    )
  })
  
  # ---- Scenario restrictions columns ----
  output$scenario_restrictions_col1 <- renderUI({
    req(vocab())
    tagList(
      make_input("RTI (days)", "text", prefix = "scen__"),
      make_input("REI (hours)", "text", prefix = "scen__"),
      make_input("PHI (days)", "text", prefix = "scen__"),
      make_input("PGI (days)", "text", prefix = "scen__"),
      make_input("PSI (days)", "text", prefix = "scen__"),
    )
  })
  
  
  output$scenario_restrictions_col2 <- renderUI({
    req(vocab())
    tagList(
      make_input("ASABE Droplet Size", "pick", choices = vocab()[["ASABE Droplet Size"]], prefix = "scen__", multiple = TRUE),
      make_input("Buffered Area (ft)", "numeric", prefix = "scen__"),
      make_input("Buffered Area (Term)", "pick", choices = vocab()[["Buffered Area (Term)"]], prefix = "scen__", multiple = TRUE),
      make_input("Max Release Height (ft)", "text", prefix = "scen__"),
      make_input("Max Wind Speed (mph)", "text", prefix = "scen__"),
      make_input("Site-Level ALLOWED Geographic Area", "pick", choices = vocab()[["Site-Level ALLOWED Geographic Area"]], prefix = "scen__", multiple = TRUE),
      make_input("Site-Level PROHIBITED Geographic Area", "pick", choices = vocab()[["Site-Level PROHIBITED Geographic Area"]], prefix = "scen__", multiple = TRUE),
      make_input("Soil Type Restrictions", "pick", choices = vocab()[["Soil Type Restrictions"]], prefix = "scen__", multiple = TRUE),
      make_input("Pollinator Protection Statement", "pick", choices = vocab()[["Pollinator Protection Statement"]], prefix = "scen__", multiple = TRUE),
      make_input("Other Site/Scenario Specific Restrictions & Limitations", "text", prefix = "scen__")
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
  
  # ----- Unified Add row (product + scenario) -----
  observeEvent(input$add_entry, {
    tryCatch({
      if (!iv$is_valid()) {
        iv$enable(); iv$show()
        showNotification("Please correct the highlighted fields, then try again.", type = "error")
        return()
      }
      # Scenario inputs
      scen_row <- collect_scenario_row(
        input, scenario_fields, prefix = "scen__",
        area_rate_fields   = scenario_area_rate_fields,
        area_rate_defaults = scenario_area_rate_defaults,
        numeric_fields     = scenario_numeric_fields
      )
      
      # Product inputs
      prod_row <- collect_row(input, product_fields, prefix = "prod__")
      
      # Combine and append
      new_row <- dplyr::bind_cols(prod_row, scen_row)
      scen_dat(dplyr::bind_rows(scen_dat(), new_row))
      showNotification(sprintf("Row added. Total entries: %d", nrow(scen_dat())), type = "message")
    }, error = function(e) {
      showNotification(paste("Failed to add row:", conditionMessage(e)), type = "error", duration = 8)
    })
  })
  
  # ----- Duplicate/Delete/Clone -----
  observeEvent(input$dup_scen, {
    sel <- input$tbl_scen_rows_selected
    if (length(sel) == 0) {
      showNotification("Select one or more rows to duplicate.", type = "warning")
      return()
    }
    sd <- scen_dat()
    copies <- sd[sel, , drop = FALSE]
    scen_dat(dplyr::bind_rows(sd, copies))
    showNotification(sprintf("Duplicated %d row(s).", length(sel)), type = "message")
  })
  
  observeEvent(input$del_scen, {
    sel <- input$tbl_scen_rows_selected
    if (length(sel) == 0) {
      showNotification("Select one or more rows to delete.", type = "warning")
      return()
    }
    sd <- scen_dat()
    sd <- sd[-sel, , drop = FALSE]
    scen_dat(sd)
    showNotification(sprintf("Deleted %d row(s).", length(sel)), type = "message")
  })
  
  observeEvent(input$clone_to_form, {
    sel <- input$tbl_scen_rows_selected
    if (length(sel) != 1) {
      showNotification("Select exactly one row to load into the form.", type = "warning")
      return()
    }
    
    sd <- scen_dat()
    row <- sd[sel, , drop = FALSE]
    
    # Populate product inputs
    for (nm in product_fields) {
      id <- paste0("prod__", idsafe(nm))
      val <- row[[nm]][1] %||% ""
      if (nm %in% c("Physical Form", "Product-level PPE")) {
        try(updateSelectizeInput(session, id, selected = split_multi(val)), silent = TRUE)
        
      } else if (nm == "Co-Formulated AI") {
        vals <- split_multi(val)
        try(updateSelectizeInput(session, id, choices = unique(vals), selected = vals), silent = TRUE)
        
      } else if (nm == "RUP") {
        try(updateSelectizeInput(session, id, selected = if (nzchar(val)) val else NULL), silent = TRUE)
      } else {
        try(updateTextInput(session, id, value = val), silent = TRUE)
      }
    }
    
    # Populate scenario picklists
    for (nm in scenario_picklist_fields) {
      id <- paste0("scen__", idsafe(nm))
      vals <- split_multi(row[[nm]][1] %||% "")
      ch <- vocab()[[nm]] %||% character(0)
      missing <- setdiff(vals, ch)
      ch <- c(ch, missing)
      try(updateSelectizeInput(session, id, choices = ch, selected = vals), silent = TRUE)
    }
    
    # Populate scenario text fields
    for (nm in setdiff(scenario_fields, c(scenario_picklist_fields, scenario_numeric_fields, scenario_area_rate_fields))) {
      id <- paste0("scen__", idsafe(nm))
      val <- as.character(row[[nm]][1] %||% "")
      try(updateTextInput(session, id, value = val), silent = TRUE)
    }
    
    # Numeric
    for (nm in scenario_numeric_fields) {
      id <- paste0("scen__", idsafe(nm))
      val_num <- extract_number(row[[nm]][1] %||% "")
      try(updateNumericInput(session, id, value = val_num), silent = TRUE)
    }
    
    # Area-rate (value + units)
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
    
    showNotification("Row loaded into form. Edit and click 'Add row' to save a new entry.", type = "message")
  })
  
  # ----- Table -----
  output$tbl_scen <- DT::renderDT({
    dat <- scen_dat()
    req(ncol(dat) > 0)
    df <- as.data.frame(dat)
    opts <- list(
      pageLength = 25,
      scrollX = TRUE,
      searching = FALSE,
      lengthChange = FALSE
    )
    DT::datatable(df, options = opts, rownames = FALSE, selection = "multiple")
  })
  
  # ----- Download -----
  output$dl_scen <- downloadHandler(
    filename = function() paste0("entries_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(scen_dat(), file, na = "")
  )
  
  # - Notebook path display
  observeEvent(workbook_path, {
    output$notebook_path_display <- renderUI({
      tags$span(workbook_name)
    })
  })
  
  # ---- Unified Clear form ----
  observeEvent(input$clear_all, {
    # Product
    product_text_fields <- c(
      "EPA Registration Number", "PC Code", "AI Name",
      "% AI",
      "AI Concentration"
    )
    lapply(product_text_fields, function(field) {
      id <- paste0("prod__", idsafe(field))
      updateTextInput(session, id, value = "")
    })
    updateSelectizeInput(session, "prod__Physical_Form", selected = character(0))
    updateSelectizeInput(session, "prod__Product_level_PPE", selected = character(0))
    updateSelectizeInput(session, "prod__Co_Formulated_AI", selected = character(0))
    updateSelectizeInput(session, "prod__RUP", selected = NULL)
    
    # Scenario texts
    scenario_text_fields <- c(
      "Max # App/Year", "Max # App/Crop Cycle",
      "Max Number of Seasons/Crop Cycles per year",
      "RTI (days)", "REI (hours)", "PHI (days)", "PGI (days)", "PSI (days)",
      "Max Release Height (ft)", "Max Wind Speed (mph)", "Other Site/Scenario Specific Restrictions & Limitations"
    )
    lapply(scenario_text_fields, function(field) {
      id <- paste0("scen__", idsafe(field))
      updateTextInput(session, id, value = "")
    })
    # Scenario picklists
    scenario_select_fields <- c(
      "Crop Use Site", "Non Crop Use Site", "Location", "App Target",
      "App Type", "App Equipment Type", "Specific App Equipment", 
      "App Timing (Site)", "App Timing (Pest)", "ASABE Droplet Size", "Buffered Area (Term)",
      "Pollinator Protection Statement", "Soil Type Restrictions",
      "Site-Level ALLOWED Geographic Area", "Site-Level PROHIBITED Geographic Area"
    )
    lapply(scenario_select_fields, function(field) {
      id <- paste0("scen__", idsafe(field))
      updateSelectizeInput(session, id, selected = character(0))
    })
    # Numerics
    updateNumericInput(session, "scen__Buffered_Area_ft_", value = NA_real_)
    # Area-rate: reset value + units
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
  })
  
  observeEvent(input$clear_use_only, {
    
    # Scenario texts
    scenario_text_fields <- c(
      "Max # App/Year", "Max # App/Crop Cycle",
      "Max Number of Seasons/Crop Cycles per year",
      "RTI (days)", "REI (hours)", "PHI (days)", "PGI (days)", "PSI (days)",
      "Max Release Height (ft)", "Max Wind Speed (mph)"
    )
    lapply(scenario_text_fields, function(field) {
      id <- paste0("scen__", idsafe(field))
      updateTextInput(session, id, value = "")
    })
    # Scenario picklists
    scenario_select_fields <- c(
      "Crop Use Site", "Non Crop Use Site", "Location", "App Target",
      "App Type", "App Equipment Type", "Specific App Equipment", 
      "App Timing (Site)", "App Timing (Pest)", "ASABE Droplet Size", "Buffered Area (Term)",
      "Pollinator Protection Statement", "Soil Type Restrictions",
      "Site-Level ALLOWED Geographic Area", "Site-Level PROHIBITED Geographic Area"
    )
    lapply(scenario_select_fields, function(field) {
      id <- paste0("scen__", idsafe(field))
      updateSelectizeInput(session, id, selected = character(0))
    })
    # Numerics
    updateNumericInput(session, "scen__Buffered_Area_ft_", value = NA_real_)
    # Area-rate: reset value + units
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
  })
  
}

shinyApp(ui, server)