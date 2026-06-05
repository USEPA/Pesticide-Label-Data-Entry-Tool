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
    .ust-rate-row { display:flex; align-items:center; gap:6px; flex-wrap: nowrap; }
    .ust-rate-row .ust-numeric { flex: 1 1 auto; min-width: 0; }
    .ust-rate-row .ust-units { display:flex; align-items:center; gap:6px; flex: 0 0 auto; }
    .ust-units .shiny-input-container { width: auto !important; display: inline-block; }
    .ust-units .shiny-input-container { width: auto !important; display: inline-block; }
    .ust-units .ust-unit:first-of-type .selectize-control { width: auto !important; min-width: 60px; }
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
    .selectize-control.single .selectize-input > input { height: 1.5rem; }
  "))
  ),
  tags$head(
    tags$style(HTML("
    .ust-units .selectize-control.single .selectize-input { overflow: visible; padding-right: .75rem; position: relative; }
    .ust-units .selectize-control.single .selectize-input:after { right: -12px !important; border-top-color: #6c757d; opacity: 0.9; }
    .ust-units .selectize-control.single .selectize-input.dropdown-active:after { border-width: 0 6px 6px 6px; border-color: transparent transparent #6c757d transparent; margin-top: -1px; }
    .ust-units .shiny-input-container { width: auto !important; display: inline-block; }
    .ust-units .selectize-control { width: auto !important; min-width: 120px; }
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
      div(class = "mb-2",
          actionButton("upload_any", "Smart Upload CSV", icon = icon("upload"), class = "btn-success"),
          tags$span(class = "ms-2 text-muted",
                    "Upload a CSV with product and/or scenario columns; auto-detect and link.")
      ),
      tabsetPanel(
        id = "data_tables",
        tabPanel(
          "Product-Level Table", value = "product",
          div(class = "mb-2", style = "margin-top:10px",
              actionButton("del_prod", "Delete selected", icon = icon("remove"), class = "btn-danger me-2")),
          div(
            style = "margin-top:5px",
            DTOutput("tbl_prod"),
            div(style = "float:left;",
                downloadButton("dl_prod", "Download product-level CSV"),
                actionButton("upload_prod", "Upload CSV", icon=icon("upload"),class = "btn-secondary ms-2"))
          )
        ),
        tabPanel(
          "Scenario-Level Table", value = "scenario",
          div(class = "mb-2", style = "margin-top:10px",
              actionButton("clone_to_form", "Load selected to form", icon = icon("sign-in-alt"), class = "btn-secondary me-2"),
              actionButton("dup_scen", "Duplicate selected", icon = icon("copy"), class = "btn-outline-secondary me-2"),
              actionButton("del_scen", "Delete selected", icon = icon("remove"), class = "btn-danger me-2")),
          div(
            style = "margin-top:5px",
            DTOutput("tbl_scen"),
            div(style = "float:left;",
                downloadButton("dl_scen", "Download scenario-level CSV"),
                actionButton("upload_scen", "Upload CSV", icon= icon("upload"), class = "btn-secondary ms-2"))
          )
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
  
  # Helper: coerce all columns to character
  as_char_df <- function(df) {
    if (is.null(df) || !ncol(df)) return(df)
    for (nm in names(df)) {
      if (!is.character(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
    }
    df
  }
  
  ## Function to show modal for file upload
  show_upload_modal <- function(table_type) {
    modalDialog(
      fileInput(
        paste0("file_upload_", table_type),
        "Choose CSV File",
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(paste0("confirm_upload_", table_type),
                     if (identical(table_type, "auto")) "Analyze" else "Upload",
                     class = "btn-primary")
      ),
      easyClose = FALSE,
      size = "m",
      title = if (identical(table_type, "auto")) "Smart Upload" else paste("Upload to", table_type, "Table")
    )
  }
  
  ## Observer to show upload modal for product
  observeEvent(input$upload_prod, {
    showModal(show_upload_modal("product"))
    observeEvent(input$confirm_upload_product, {
      removeModal()
      if (is.null(input$file_upload_product)) {
        showNotification("No file selected.", type = "error")
        return()
      }
      data <- tryCatch({
        read.csv(input$file_upload_product$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      }, error = function(e) {
        showNotification("Failed to read file.", type = "error")
        return(NULL)
      })
      if (is.null(data)) return()
      data <- as_char_df(data)
      
      required_fields <- map_chr(c("Product_ID", product_fields), idsafe)
      uploaded_fields <- map_chr(names(data), idsafe)
      if (!all(required_fields %in% uploaded_fields)) {
        showNotification("File format does not match product-level fields. It should contain all the required columns.", type = "error")
        return()
      }
      
      showModal(modalDialog(
        title = "File Uploaded Successfully",
        selectInput("upload_mode_prod", "Choose an option:", choices = c("Append", "Replace")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("commit_upload_prod", "Commit Changes", class = "btn-success")
        ),
        easyClose = FALSE
      ))
      
      observeEvent(input$commit_upload_prod, {
        if (input$upload_mode_prod == "Append") {
          merged_data <- distinct(dplyr::bind_rows(prod_dat(), data))
          prod_dat(merged_data)
        } else {
          prod_dat(data)
        }
        removeModal()
        showNotification("Data uploaded successfully.", type = "message")
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
  
  ## Observer to show upload modal for scenario
  observeEvent(input$upload_scen, {
    showModal(show_upload_modal("scenario"))
    observeEvent(input$confirm_upload_scenario, {
      removeModal()
      if (is.null(input$file_upload_scenario)) {
        showNotification("No file selected.", type = "error")
        return()
      }
      data <- tryCatch({
        read.csv(input$file_upload_scenario$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      }, error = function(e) {
        showNotification("Failed to read file.", type = "error")
        return(NULL)
      })
      if (is.null(data)) return()
      data <- as_char_df(data)
      
      expected_fields <- map_chr(c("Product_ID", product_fields, scenario_fields, scenario_textarea_label), idsafe)
      uploaded_fields <- map_chr(names(data), idsafe)
      if (!all(expected_fields %in% uploaded_fields)) {
        showNotification("File format does not match scenario-level fields. It should contain all the required columns.", type = "error")
        return()
      }
      
      showModal(modalDialog(
        title = "File Uploaded Successfully",
        selectInput("upload_mode_scen", "Choose an option:", choices = c("Append", "Replace")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("commit_upload_scen", "Commit Changes", class = "btn-success")
        ),
        easyClose = FALSE
      ))
      
      observeEvent(input$commit_upload_scen, {
        if (input$upload_mode_scen == "Append") {
          merged_data <- distinct(dplyr::bind_rows(scen_dat(), data))
          scen_dat(merged_data)
        } else {
          scen_dat(data)
        }
        removeModal()
        showNotification("Data uploaded successfully.", type = "message")
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
  
  # ---- Smart-upload helpers (safer across dplyr versions) ----
  standardize_colnames <- function(df) {
    canon <- c("Product_ID", product_fields, scenario_fields, scenario_textarea_label)
    canon_map <- setNames(canon, idsafe(canon))
    up <- idsafe(names(df))
    names(df) <- vapply(
      up,
      function(nm) if (!is.null(canon_map[[nm]])) canon_map[[nm]] else nm,
      character(1)
    )
    df
  }
  
  ensure_cols_order <- function(df, cols) {
    for (c in cols) {
      if (!c %in% names(df)) df[[c]] <- NA_character_
    }
    df[, cols, drop = FALSE]
  }
  
  detect_upload_kind <- function(df) {
    df <- standardize_colnames(df)
    has_prod <- all(c("Product_ID", product_fields) %in% names(df))
    has_prod_no_id <- all(product_fields %in% names(df)) && !("Product_ID" %in% names(df))
    has_any_scen <- any(scenario_fields %in% names(df))
    has_all_scen <- all(c("Product_ID", product_fields, scenario_fields, scenario_textarea_label) %in% names(df))
    if (has_all_scen) return("scenario_full")
    if (has_any_scen && all(product_fields %in% names(df))) {
      if ("Product_ID" %in% names(df)) return("scenario_partial_with_id")
      return("scenario_partial_no_id")
    }
    if (has_prod) return("product_only")
    if (has_prod_no_id) return("product_only_no_id")
    "unknown"
  }
  
  build_product_key <- function(df) {
    if (!nrow(df)) return(character(0))
    norm <- function(x) tolower(trimws(ifelse(is.na(x), "", as.character(x))))
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    if ("EPA Registration Number" %in% names(df) && any(nzchar(df[["EPA Registration Number"]]))) {
      key <- norm(df[["EPA Registration Number"]])
    } else if (all(c("AI Name", "PC Code") %in% names(df)) &&
               any(nzchar(df[["AI Name"]]) | nzchar(df[["PC Code"]]))) {
      key <- paste(norm(df[["AI Name"]]), norm(df[["PC Code"]]), sep = " | ")
    } else {
      pf <- intersect(product_fields, names(df))
      if (!length(pf)) pf <- names(df)
      key <- apply(df[, pf, drop = FALSE], 1, function(r) paste(norm(r), collapse = " | "))
    }
    ifelse(is.na(key) | !nzchar(key), paste0("ROWKEY_", seq_along(key)), key)
  }
  
  next_product_ids <- function(n) {
    pd <- prod_dat()
    existing_nums <- suppressWarnings(as.integer(sub("^P", "", pd$Product_ID)))
    maxn <- suppressWarnings(max(existing_nums, na.rm = TRUE))
    if (!is.finite(maxn)) maxn <- 0L
    sprintf("P%03d", seq.int(from = maxn + 1L, length.out = n))
  }
  
  # Avoid joins that create suffixed names; return character columns
  match_or_create_products <- function(prod_like_df) {
    prod_like_df <- standardize_colnames(prod_like_df)
    prod_only <- prod_like_df[, intersect(names(prod_like_df), product_fields), drop = FALSE] %>% distinct()
    if (nrow(prod_only) == 0) return(list(map = character(0), to_add = tibble()))
    
    prod_only$key <- build_product_key(prod_only)
    keys_uploaded <- unique(prod_only$key)
    
    existing <- prod_dat()
    existing$key <- build_product_key(existing[, product_fields, drop = FALSE])
    map_existing <- existing %>%
      dplyr::select(key, Product_ID) %>%
      dplyr::distinct()
    
    keys_new <- setdiff(keys_uploaded, map_existing$key)
    
    to_add <- tibble()
    key_to_id <- character(0)
    if (length(keys_new)) {
      new_src <- prod_only %>%
        dplyr::filter(key %in% keys_new) %>%
        dplyr::distinct(key, .keep_all = TRUE)
      new_ids <- next_product_ids(length(keys_new))
      key_to_id <- setNames(new_ids, keys_new)
      new_src$Product_ID <- unname(key_to_id[new_src$key])
      to_add <- new_src %>%
        ensure_cols_order(c("Product_ID", product_fields)) %>%
        dplyr::distinct(Product_ID, .keep_all = TRUE)
      to_add <- as_char_df(to_add)
    }
    
    map <- setNames(map_existing$Product_ID, map_existing$key)
    if (length(key_to_id)) map <- c(map, key_to_id)
    
    list(map = map, to_add = to_add)
  }
  
  fill_product_columns_from_master <- function(df_with_pid) {
    master <- prod_dat()[, c("Product_ID", product_fields), drop = FALSE]
    keep_other <- setdiff(names(df_with_pid), product_fields)
    out <- df_with_pid[, keep_other, drop = FALSE] %>% left_join(master, by = "Product_ID")
    front <- c("Product_ID", product_fields)
    rest  <- setdiff(names(out), front)
    out <- out[, c(front, rest), drop = FALSE]
    as_char_df(out)
  }
  
  # ---- Smart Upload: auto-detect product/scenario and link ----
  observeEvent(input$upload_any, {
    showModal(show_upload_modal("auto"))
    observeEvent(input$confirm_upload_auto, {
      removeModal()
      if (is.null(input$file_upload_auto)) {
        showNotification("No file selected.", type = "error")
        return()
      }
      df <- tryCatch({
        read.csv(input$file_upload_auto$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      }, error = function(e) {
        showNotification("Failed to read file.", type = "error")
        return(NULL)
      })
      if (is.null(df) || !nrow(df)) {
        showNotification("Empty or unreadable CSV.", type = "error")
        return()
      }
      
      df <- standardize_colnames(df)
      df <- as_char_df(df)
      kind <- detect_upload_kind(df)
      
      pending_prod <- NULL
      pending_scen <- NULL
      summary_lines <- c()
      
      if (kind %in% c("product_only", "product_only_no_id")) {
        prod_cols <- c("Product_ID", product_fields)
        df_prod <- df[, intersect(names(df), prod_cols), drop = FALSE]
        if (!"Product_ID" %in% names(df_prod)) df_prod$Product_ID <- NA_character_
        need_id <- which(is.na(df_prod$Product_ID) | !nzchar(df_prod$Product_ID))
        if (length(need_id)) {
          uni <- df_prod[need_id, product_fields, drop = FALSE] %>% distinct()
          new_ids <- next_product_ids(nrow(uni))
          uni$key <- build_product_key(uni)
          df_prod$key <- build_product_key(df_prod[, product_fields, drop = FALSE])
          id_map <- tibble(key = uni$key, Product_ID = new_ids)
          df_prod <- df_prod %>%
            left_join(id_map, by = "key", suffix = c("", ".new")) %>%
            mutate(Product_ID = ifelse(is.na(.data$Product_ID) | !nzchar(.data$Product_ID),
                                       .data$Product_ID.new, .data$Product_ID)) %>%
            select(-one_of(c("key", "Product_ID.new")))
        }
        df_prod <- df_prod %>%
          distinct(Product_ID, .keep_all = TRUE) %>%
          ensure_cols_order(c("Product_ID", product_fields))
        df_prod <- as_char_df(df_prod)
        
        pending_prod <- df_prod
        summary_lines <- c(summary_lines, sprintf("Detected product-only upload: %d product row(s).", nrow(df_prod)))
        
      } else if (kind %in% c("scenario_full", "scenario_partial_with_id", "scenario_partial_no_id")) {
        scen_cols_all <- c("Product_ID", product_fields, scenario_fields, scenario_textarea_label)
        df_scen <- df[, intersect(names(df), scen_cols_all), drop = FALSE]
        
        if (!"Product_ID" %in% names(df_scen)) {
          # No Product_ID in upload: match/create by key
          m <- match_or_create_products(df_scen)
          if (nrow(m$to_add)) {
            pending_prod <- bind_rows(pending_prod %||% make_empty_prod(), m$to_add)
            summary_lines <- c(summary_lines, sprintf("Created %d new product(s) from scenario upload.", nrow(m$to_add)))
          }
          keys <- build_product_key(df_scen[, product_fields, drop = FALSE])
          df_scen$Product_ID <- unname(m$map[keys])
        } else {
          # Product_ID present: respect it and ensure a matching product row exists
          prod_from_upload <- df_scen[, intersect(names(df_scen), c("Product_ID", product_fields)), drop = FALSE] %>%
            dplyr::distinct(Product_ID, .keep_all = TRUE)
          
          existing_pids <- prod_dat()$Product_ID %||% character(0)
          prod_to_add <- prod_from_upload %>%
            dplyr::filter(!.data$Product_ID %in% existing_pids) %>%
            ensure_cols_order(c("Product_ID", product_fields))
          
          if (nrow(prod_to_add)) {
            prod_to_add <- as_char_df(prod_to_add)
            pending_prod <- dplyr::bind_rows(pending_prod %||% make_empty_prod(), prod_to_add) %>%
              dplyr::distinct(Product_ID, .keep_all = TRUE)
            summary_lines <- c(summary_lines, sprintf("Detected %d new product(s) by Product_ID; will add.", nrow(prod_to_add)))
          }
        }
        
        # Remove scenarios that still lack a Product_ID after linking
        missing_pid <- which(is.na(df_scen$Product_ID) | !nzchar(df_scen$Product_ID))
        if (length(missing_pid)) {
          showNotification(sprintf("Skipping %d scenario row(s) that could not be linked to a product.", length(missing_pid)),
                           type = "warning", duration = 8)
          df_scen <- df_scen[-missing_pid, , drop = FALSE]
        }
        if (!nrow(df_scen)) {
          showNotification("No scenario rows remain after linking.", type = "error")
          return()
        }
        
        # Stage pending products (if any) so we can fill canonical product columns for preview
        staged_prod <- prod_dat()
        if (!is.null(pending_prod) && nrow(pending_prod)) {
          staged_prod <- staged_prod %>%
            bind_rows(pending_prod) %>%
            distinct(Product_ID, .keep_all = TRUE)
        }
        old_prod_dat <- prod_dat()
        prod_dat(staged_prod)  # temporarily stage for fill
        df_scen <- fill_product_columns_from_master(df_scen)
        prod_dat(old_prod_dat) # revert
        df_scen <- ensure_cols_order(df_scen, scen_cols_all)
        df_scen <- as_char_df(df_scen)
        
        pending_scen <- df_scen
        summary_lines <- c(summary_lines, sprintf("Detected scenario upload: %d scenario row(s).", nrow(df_scen)))
        
      } else {
        showNotification("Could not recognize CSV as product or scenario data. Check column names.", type = "error", duration = 8)
        return()
      }
      
      choices <- c()
      if (!is.null(pending_prod) && nrow(pending_prod)) choices <- c(choices, "Append products", "Replace products")
      if (!is.null(pending_scen) && nrow(pending_scen)) choices <- c(choices, "Append scenarios", "Replace scenarios")
      if (!length(choices)) {
        showNotification("Nothing to import.", type = "warning")
        return()
      }
      
      showModal(modalDialog(
        title = "Smart upload: review and commit",
        size = "m",
        tagList(
          tags$ul(lapply(summary_lines, tags$li)),
          checkboxGroupInput("smart_upload_actions", "Choose commit actions:",
                             choices = choices,
                             selected = if (any(grepl("^Append", choices))) choices[grepl("^Append", choices)] else choices),
          tags$small(class = "text-muted", "Tip: Append adds to existing rows; Replace overwrites the entire table.")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("commit_upload_any", "Commit", class = "btn-success")
        ),
        easyClose = FALSE
      ))
      
      observeEvent(input$commit_upload_any, {
        removeModal()
        
        if (!is.null(pending_prod) && nrow(pending_prod)) {
          pending_prod <- as_char_df(pending_prod)
          if ("Replace products" %in% input$smart_upload_actions) {
            prod_dat(pending_prod %>% distinct(Product_ID, .keep_all = TRUE))
          } else if ("Append products" %in% input$smart_upload_actions) {
            prod_dat(bind_rows(prod_dat(), pending_prod) %>% distinct(Product_ID, .keep_all = TRUE))
          }
        }
        
        if (!is.null(pending_scen) && nrow(pending_scen)) {
          df_scen_final <- fill_product_columns_from_master(pending_scen)
          df_scen_final <- as_char_df(df_scen_final)
          if ("Replace scenarios" %in% input$smart_upload_actions) {
            scen_dat(df_scen_final)
          } else if ("Append scenarios" %in% input$smart_upload_actions) {
            scen_dat(bind_rows(scen_dat(), df_scen_final) %>% distinct())
          }
        }
        
        showNotification("Smart upload completed.", type = "message")
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
  
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
    updateTabsetPanel(session, "data_tables", selected = "product")
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
      updateTabsetPanel(session, "data_tables", selected = "scenario")
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
  hidden_product_cols_in_scen <- c(
    "AI Name",
    "PC Code",
    "Co-Formulated AI",
    "Physical Form",
    "% AI",
    "AI Concentration (Or Product Density if liquid)",
    "RUP",
    "Product-level PPE"
  )
  output$tbl_scen <- DT::renderDT({
    dat <- scen_dat()
    req(ncol(dat) > 0)
    df <- as.data.frame(dat)
    cols_to_hide <- c("Product_ID", hidden_product_cols_in_scen)
    targets <- which(names(df) %in% cols_to_hide) - 1
    opts <- list(
      pageLength = 25,
      scrollX = TRUE,
      searching = FALSE,
      lengthChange = FALSE,
      columnDefs = list(list(visible = FALSE, targets = targets))
    )
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