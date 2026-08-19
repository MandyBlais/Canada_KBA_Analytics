# PART 2 - SHINY APP DASHBOARD INTERFACE (WEBGL ENHANCED VIA LEAFGL)

library(shiny)
library(shinydashboard)
library(leaflet)
library(leafgl) # WebGL hardware-accelerated polygon rendering
library(sf)
library(dplyr)
library(DT)
library(gfonts)
library(fresh)
library(plotly)

CACHE_PATH <- "data/cached_compiled_data.rds"

# Helper function to generate dropdown province/territory choices safely
get_province_choices <- function(kba_df) {
  base_provinces <- if (!is.null(kba_df)) {
    kba_cols <- tolower(colnames(kba_df))
    jur_idx <- match(TRUE, kba_cols %in% c("jurisdiction_en", "jurisdiction_fr", "jurisdiction_es", "jurisdiction"))
    if (!is.na(jur_idx)) sort(unique(kba_df[[jur_idx]])) else NULL
  } else NULL
  
  c("All", sort(unique(c(base_provinces, "Federal Offshore/Marine"))))
}

# Safe helper function to generate site choices
get_kba_choices <- function(kba_df) {
  if (!is.null(kba_df) && nrow(kba_df) > 0) {
    c("All", sort(unique(paste0(kba_df$kbasiteid, " - ", kba_df$nationalname))))
  } else {
    "All"
  }
}

# Helper function to define Critical Habitat plot colours
ch_status_map <- c(
  "0" = "NULL",
  "1" = "Extirpated",
  "2" = "Endangered",
  "3" = "Threatened",
  "4" = "Special Concern",
  "5" = "No Status",
  "6" = "Not at Risk"
)

# Brand colors for SARA statuses
ch_color_map <- c(
  "Extirpated"      = "#800080", # Purple
  "Endangered"      = "#FF0000", # Bright Red
  "Threatened"      = "#FF1493", # Bright Pink
  "Special Concern" = "#FF8C00", # Orange
  "No Status"       = "#808080", # Grey
  "Not at Risk"     = "#28A745", # Green
  "NULL"            = "#ADB5BD"  # Light Grey
)

# Modebar cleanup helper function for Plotly
clean_plotly_config <- function(p) {
  config(
    p,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c(
      "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", 
      "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian",
      "toggleSpikelines"
    )
  )
}

# 1. Gracefully load cache on initialization
if (file.exists(CACHE_PATH)) {
  data_payload <- readRDS(CACHE_PATH)
} else {
  data_payload <- list(
    kba_layer           = NULL, 
    cpcad_layer         = NULL, 
    ch_layer            = NULL,
    cpcad_overlaps      = NULL,
    ch_kba_overlaps     = NULL,
    national_cpcad_km2  = 0,
    national_ch_km2     = 0,
    cpcad_prov_summary  = NULL,
    ch_prov_summary     = NULL,
    timestamp           = "No Local Cache Binary Found"
  )
}

# 2. CREATE CUSTOM BRANDING THEME
kba_theme <- create_theme(
  adminlte_color(
    light_blue = "#0AA1F4"      # Primary Blue
  ),
  adminlte_sidebar(
    dark_bg = "#2f4858",        # Secondary Dark Blue/Teal background
    dark_hover_bg = "#3a3426",  # Secondary Dark Brown background on hover
    dark_color = "#ffffff"
  ),
  adminlte_global(
    content_bg = "#3a3426",     # Main background color
    box_bg = "#ffffff"
  )
)

# 3. --- USER INTERFACE DESIGN ---
ui <- dashboardPage(
  header = dashboardHeader(title = "Key Biodiversity Areas (KBA) in Canada Analytics Dashboard", titleWidth = 320),
  
  sidebar = dashboardSidebar(
    width = 320,
    
    tags$div(
      style = "padding: 15px;",
      
      h5("Spatial Filter Controls", class = "client-subhead", style = "margin-top: 0; margin-bottom: 10px; color: #92BF00;"),
      
      selectInput(
        "provinceFilter", "Province / Territory:", 
        choices = get_province_choices(data_payload$kba_layer)
      ),
      selectInput(
        "kbaFilter", "Specific KBA Site:", 
        choices = get_kba_choices(data_payload$kba_layer)
      ),
      
      hr(style = "border-color: #92BF00; border-width: 1px; margin: 15px 0;"),
      
      h5("Layer Visibility Toggles", class = "client-subhead", style = "color: #ffffff; margin-bottom: 8px;"),
      checkboxInput("showKBA", "Show KBAs (Green)", value = TRUE),
      checkboxInput("showCPCAD", "Show CPCAD Layers", value = FALSE),
      checkboxInput("showCH", "Show Critical Habitat Layers", value = FALSE),
      
      hr(style = "border-color: #92BF00; border-width: 1px; margin: 15px 0;"),
      
      tags$div(
        style = "text-align: center; margin-top: 15px;",
        tags$small(textOutput("cacheTimeText"), class = "client-body", style = "color: #b8c7ce; font-size: 11px;")
      )
    )
  ),
  
  body = dashboardBody(
    use_theme(kba_theme),
    
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@300;700&display=swap"),
      tags$style(HTML("
        h1, h2, .logo, .main-header .logo { 
          font-family: 'rift', 'Impact', sans-serif !important; 
          font-weight: 700 !important; 
          text-transform: uppercase;
        }
        
        h3 {
          font-family: 'Open Sans', sans-serif !important;
          font-weight: 700 !important;
          font-size: 20px !important;
          line-height: 1.3 !important;
          margin-top: 0 !important;
          margin-bottom: 8px !important;
        }
        
        h4 {
          font-family: 'Open Sans', sans-serif !important;
          font-weight: 700 !important;
          font-size: 16px !important;
          line-height: 1.3 !important;
          margin-top: 0 !important;
          margin-bottom: 10px !important;
        }
        
        .client-subhead, h5, label, .box-title { 
          font-family: 'Open Sans', sans-serif !important; 
          font-weight: 700 !important; 
        }
        
        .client-body, p, span, li, td, th, input, select, .control-label { 
          font-family: 'Open Sans', sans-serif !important; 
          font-weight: 300 !important; 
        }
        
        .main-header .navbar { background-color: #0AA1F4 !important; }
        .main-header .logo { background-color: #2f4858 !important; color: #92BF00 !important; }
        .content-wrapper, .right-side { background-color: #3a3426 !important; }
        #right-panel { background: #ffffff; border-left: 4px solid #92BF00; padding: 20px; height: calc(100vh - 80px); overflow-y: auto; }
        
        .metric-container { margin-bottom: 15px; }
        .metric-box { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 6px; padding: 12px; height: 100%; display: flex; flex-direction: column; justify-content: center; }
        .metric-title { font-size: 11px; text-transform: uppercase; color: #2f4858; font-weight: 700; line-height: 1.2; margin-bottom: 6px; }
        .metric-value { font-size: 18px; color: #0AA1F4; font-weight: bold; line-height: 1; }
        .metric-value-ch { font-size: 18px; color: #FF0000; font-weight: bold; line-height: 1; }
        
        .well-unprotected { background-color: #fff1f2; border-left: 5px solid #ef4444; color: #3a3426; padding: 12px; border-radius: 6px; }
        .well-protected { background-color: #f0fdf4; border-left: 5px solid #92BF00; color: #2f4858; padding: 12px; border-radius: 6px; }
        
        .header-footnote { font-size: 11px; color: #64748b; font-style: italic; line-height: 1.4; margin-top: 15px; margin-bottom: 5px; }
        .chart-box-container { margin-bottom: 15px; }
      "))
    ),
    
    fluidRow(
      column(
        width = 6,
        box(
          title = "National Conservation Baseline Map", width = NULL, solidHeader = TRUE, status = "primary",
          leafglOutput("mapElement", height = "780px")
        )
      ),
      
      column(
        width = 6, id = "right-panel",
        h3("Biodiversity & Conservation Attributes", style = "color: #2f4858;"),
        p("Detailed site telemetry, legal protection, and species risk attributes.", class = "client-body", style = "color: #475569; margin-bottom: 15px;"),
        hr(style = "border-color: #92BF00; margin: 15px 0;"),
        
        # Telemetry Header Metrics
        uiOutput("kbaSelectionHeader"),
        br(),
        
        # Dynamic Charts
        conditionalPanel(
          condition = "input.kbaFilter != 'All' && input.kbaFilter != ''",
          div(
            style = "width: 100%; overflow-x: hidden;",
            fluidRow(
              class = "chart-box-container",
              column(
                width = 12,
                h4("CPCAD Protection Breakdown", style = "color: #2f4858; font-size: 14px; margin-bottom: 5px;"),
                plotlyOutput("kbaProtectionPlot", height = "220px", width = "100%")
              )
            ),
            fluidRow(
              class = "chart-box-container",
              style = "margin-top: 10px;",
              column(
                width = 12,
                h4("Critical Habitat Coverage", style = "color: #2f4858; font-size: 14px; margin-bottom: 5px;"),
                plotlyOutput("kbaChStatusPlot", height = "220px", width = "100%")
              )
            )
          ),
          hr(style = "border-color: #e2e8f0; margin: 15px 0;")
        ),
        
        # Attribute Data Tables
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "KBA Sites Inventory",
            br(),
            p("Designated Key Biodiversity Areas within current spatial selection.", class = "client-body", style = "color: #3a3426; font-size: 12px; margin-bottom: 12px;"),
            DTOutput("kbaTable")
          ),
          tabPanel(
            "CPCAD Protected Areas",
            br(),
            p("Individual contributing protected and conserved area shapes.", class = "client-body", style = "color: #3a3426; font-size: 12px; margin-bottom: 12px;"),
            DTOutput("overlapTable")
          ),
          tabPanel(
            "Critical Habitat (CH)",
            br(),
            p("Species at Risk Critical Habitat overlapping this site.", class = "client-body", style = "color: #3a3426; font-size: 12px; margin-bottom: 12px;"),
            DTOutput("chTable")
          )
        )
      )
    )
  )
)

# 4. --- SERVER COMPUTATION LOGIC ---
server <- function(input, output, session) {
  
  current_data <- reactiveValues(
    kba                = data_payload$kba_layer,
    cpcad              = data_payload$cpcad_layer,
    ch                 = data_payload$ch_layer,
    cpcad_overlaps     = data_payload$cpcad_overlaps,
    ch_kba_overlaps    = data_payload$ch_kba_overlaps,
    national_cpcad_km2 = data_payload$national_cpcad_km2,
    national_ch_km2    = data_payload$national_ch_km2,
    cpcad_prov_summary = data_payload$cpcad_prov_summary,
    ch_prov_summary    = data_payload$ch_prov_summary,
    time               = data_payload$timestamp
  )
  
  selected_kba_id <- reactive({
    if (is.null(input$kbaFilter) || input$kbaFilter == "All" || input$kbaFilter == "") return("All")
    sub(" - .*", "", trimws(input$kbaFilter))
  })
  
  static_footnote_ui <- tags$p(
    class = "header-footnote",
    "CPCAD totals reflect terrestrial and inland protected areas per province/territory; offshore marine protected areas are categorized under Federal / Offshore jurisdiction. Critical Habitat and KBA totals include adjacent coastal and marine areas."
  )
  
  output$cacheTimeText <- renderText({ paste("Data Timestamp:", current_data$time) })
  
  filtered_kba <- reactive({
    df <- current_data$kba
    if (is.null(df)) return(NULL)
    if (input$provinceFilter != "All") { 
      kba_cols <- tolower(colnames(df))
      jur_idx <- match(TRUE, kba_cols %in% c("jurisdiction_en", "jurisdiction_fr", "jurisdiction_es", "jurisdiction"))
      if (!is.na(jur_idx)) {
        col_name <- colnames(df)[jur_idx]
        df <- df[df[[col_name]] == input$provinceFilter, ]
      }
    }
    df
  })
  
  observeEvent(input$provinceFilter, {
    req(current_data$kba)
    subset_kbas <- filtered_kba()
    req(subset_kbas)
    
    kba_choices <- get_kba_choices(subset_kbas)
    current_sel <- input$kbaFilter
    
    if (current_sel %in% kba_choices) {
      updateSelectInput(session, "kbaFilter", choices = kba_choices, selected = current_sel)
    } else {
      updateSelectInput(session, "kbaFilter", choices = kba_choices, selected = "All")
    }
  }, ignoreInit = TRUE)
  
  # --- ROBUST SAFE HELPER FUNCTIONS ---
  safe_extract <- function(df, col_names, default = 0) {
    if (is.null(df) || nrow(df) == 0) return(default)
    df_cols <- tolower(colnames(df))
    for (col in col_names) {
      match_idx <- match(tolower(col), df_cols)
      if (!is.na(match_idx) && !is.null(df[[match_idx]])) {
        val <- df[[match_idx]][1]
        if (!is.na(val)) return(val)
      }
    }
    return(default)
  }
  
  safe_col_sum <- function(df, candidates, default = 0) {
    if (is.null(df) || nrow(df) == 0) return(default)
    df_cols <- tolower(colnames(df))
    for (cand in candidates) {
      match_idx <- match(tolower(cand), df_cols)
      if (!is.na(match_idx)) {
        val <- suppressWarnings(as.numeric(df[[match_idx]]))
        return(sum(val, na.rm = TRUE))
      }
    }
    return(default)
  }
  
  safe_get_col <- function(df, candidates) {
    if (is.null(df) || nrow(df) == 0) return(rep(NA, nrow(df)))
    df_cols <- tolower(colnames(df))
    for (cand in candidates) {
      match_idx <- match(tolower(cand), df_cols)
      if (!is.na(match_idx)) return(df[[match_idx]])
    }
    return(rep(NA, nrow(df)))
  }
  
  normalize_jurisdiction <- function(x) {
    x_clean <- toupper(trimws(as.character(x)))
    case_when(
      x_clean %in% c("1", "AB", "ALBERTA")                               ~ "ALBERTA",
      x_clean %in% c("2", "BC", "BRITISH COLUMBIA")                      ~ "BRITISH COLUMBIA",
      x_clean %in% c("3", "MB", "MANITOBA")                              ~ "MANITOBA",
      x_clean %in% c("4", "NB", "NEW BRUNSWICK")                         ~ "NEW BRUNSWICK",
      x_clean %in% c("5", "NL", "NEWFOUNDLAND AND LABRADOR")             ~ "NEWFOUNDLAND AND LABRADOR",
      x_clean %in% c("6", "NT", "NORTHWEST TERRITORIES")                 ~ "NORTHWEST TERRITORIES",
      x_clean %in% c("7", "NS", "NOVA SCOTIA")                           ~ "NOVA SCOTIA",
      x_clean %in% c("8", "NU", "NUNAVUT")                               ~ "NUNAVUT",
      x_clean %in% c("9", "ON", "ONTARIO")                               ~ "ONTARIO",
      x_clean %in% c("10", "PE", "PRINCE EDWARD ISLAND")                 ~ "PRINCE EDWARD ISLAND",
      x_clean %in% c("11", "QC", "QUEBEC", "QUÉBEC")                     ~ "QUEBEC",
      x_clean %in% c("12", "SK", "SASKATCHEWAN")                         ~ "SASKATCHEWAN",
      x_clean %in% c("13", "YT", "YUKON")                                ~ "YUKON",
      x_clean %in% c("14", "15", "16", "17", "18", "19", "20", "21",
                     "FEDERAL OFFSHORE/MARINE", "FEDERAL / OFFSHORE")   ~ "FEDERAL OFFSHORE/MARINE",
      TRUE                                                               ~ x_clean
    )
  }
  
  safe_summary_sum <- function(summary_df, target_prov, km2_candidates) {
    if (is.null(summary_df) || nrow(summary_df) == 0) return(0)
    
    jur_col <- safe_get_col(summary_df, c("jur_clean", "jurisdiction", "province", "jur", "prov_terr", "loc", "provterr_e"))
    if (all(is.na(jur_col))) return(0)
    
    norm_jur    <- normalize_jurisdiction(jur_col)
    norm_target <- normalize_jurisdiction(target_prov)
    
    matched_df  <- summary_df[norm_jur == norm_target, , drop = FALSE]
    if (nrow(matched_df) == 0) return(0)
    
    return(safe_col_sum(matched_df, km2_candidates, 0))
  }
  
  get_provincial_cpcad_area <- function(prov, summary_df, cpcad_layer) {
    val <- safe_summary_sum(summary_df, prov, c("cpcad_km2", "area_km2", "total_km2", "pa_oecm_km2", "pa_km2", "oecm_km2"))
    if (val > 0) return(val)
    
    if (!is.null(cpcad_layer) && nrow(cpcad_layer) > 0) {
      loc_col <- safe_get_col(cpcad_layer, c("loc", "jur_id", "loc_e", "province_e", "jurisdiction"))
      if (!all(is.na(loc_col))) {
        norm_loc <- normalize_jurisdiction(loc_col)
        norm_target <- normalize_jurisdiction(prov)
        matched_layer <- cpcad_layer[norm_loc == norm_target, ]
        if (nrow(matched_layer) > 0) {
          return(tryCatch({
            sum(as.numeric(sf::st_area(sf::st_make_valid(matched_layer))) / 1e6, na.rm = TRUE)
          }, error = function(e) {
            suppressWarnings({
              sf::sf_use_s2(FALSE)
              area_val <- sum(as.numeric(sf::st_area(sf::st_make_valid(matched_layer))) / 1e6, na.rm = TRUE)
              sf::sf_use_s2(TRUE)
              return(area_val)
            })
          }))
        }
      }
    }
    return(0)
  }
  
  get_provincial_ch_area <- function(prov, summary_df, ch_layer) {
    val <- safe_summary_sum(summary_df, prov, c("ch_km2", "area_km2", "total_km2", "total_ch_km2", "ch_area_km2"))
    if (val > 0) return(val)
    
    if (!is.null(ch_layer) && nrow(ch_layer) > 0) {
      prov_col <- safe_get_col(ch_layer, c("provterr_e", "provterr", "province_e", "jurisdiction"))
      if (!all(is.na(prov_col))) {
        norm_prov <- normalize_jurisdiction(prov_col)
        norm_target <- normalize_jurisdiction(prov)
        matched_layer <- ch_layer[norm_prov == norm_target, ]
        if (nrow(matched_layer) > 0) {
          return(tryCatch({
            sum(as.numeric(sf::st_area(sf::st_make_valid(matched_layer))) / 1e6, na.rm = TRUE)
          }, error = function(e) {
            suppressWarnings({
              sf::sf_use_s2(FALSE)
              area_val <- sum(as.numeric(sf::st_area(sf::st_make_valid(matched_layer))) / 1e6, na.rm = TRUE)
              sf::sf_use_s2(TRUE)
              return(area_val)
            })
          }))
        }
      }
    }
    return(0)
  }
  
  output$kbaProtectionPlot <- renderPlotly({
    req(selected_kba_id() != "All")
    
    target_kba <- current_data$kba %>% 
      filter(as.character(kbasiteid) == selected_kba_id()) %>% 
      st_drop_geometry()
    
    req(nrow(target_kba) > 0)
    colnames(target_kba) <- tolower(colnames(target_kba))
    
    pct_1 <- min(100, round(safe_extract(target_kba, c("cpcad_1_prop", "pa_proportion")) * 100, 1))
    pct_2 <- min(100, round(safe_extract(target_kba, c("cpcad_2_prop", "oecm_proportion")) * 100, 1))
    pct_3 <- min(100, round(safe_extract(target_kba, c("cpcad_3_prop")) * 100, 1))
    pct_4 <- min(100, round(safe_extract(target_kba, c("cpcad_4_prop")) * 100, 1))
    pct_5 <- min(100, round(safe_extract(target_kba, c("cpcad_5_prop")) * 100, 1))
    
    total_cpcad_pct <- min(100, pct_1 + pct_2 + pct_3 + pct_4 + pct_5)
    unprotected_pct <- max(0, round(100 - total_cpcad_pct, 1))
    
    plot_df <- data.frame(
      Category = c(
        "1 - Protected area (PA)",
        "2 - Other effective area-based conservation measure (OECM)",
        "3 - Interim - protected area (PA)",
        "4 - Interim - other effective area-based conservation measure (OECM)",
        "5 - Not applicable",
        "Unprotected / Other"
      ),
      Percentage = c(pct_1, pct_2, pct_3, pct_4, pct_5, unprotected_pct),
      Color = c("#0AA1F4", "#FFCB00", "#7dd3fc", "#fde047", "#94a3b8", "#E2E8F0"),
      stringsAsFactors = FALSE
    ) %>% filter(Percentage > 0)
    
    plot_ly(
      plot_df, 
      labels = ~Category, 
      values = ~Percentage, 
      type = 'pie',
      hole = 0.60,
      domain = list(x = c(0.1, 0.9), y = c(0, 1)),
      marker = list(colors = plot_df$Color),
      textinfo = 'none',
      hoverinfo = 'text',
      text = ~paste0("<b>", Category, "</b><br>", Percentage, "%"),
      hoverlabel = list(font = list(family = "Open Sans, sans-serif", size = 12))
    ) %>%
      layout(
        showlegend = FALSE,
        autosize = TRUE,
        annotations = list(
          text = paste0("<span style='font-size:15px; font-weight:bold; color:#2f4858;'>", round(total_cpcad_pct, 1), "%</span><br><span style='font-size:10px; font-weight:700; color:#64748b;'>TOTAL CPCAD OVERLAP</span>"),
          x = 0.5, y = 0.5,
          showarrow = FALSE
        ),
        margin = list(l = 10, r = 10, t = 5, b = 5)
      ) %>%
      clean_plotly_config()
  })
  
  output$kbaChStatusPlot <- renderPlotly({
    req(selected_kba_id() != "All")
    req(current_data$ch_kba_overlaps)
    
    target_id <- selected_kba_id()
    
    kba_id_col <- safe_get_col(current_data$ch_kba_overlaps, c("kbasiteid", "siteid"))
    ch_overlaps <- current_data$ch_kba_overlaps[trimws(as.character(kba_id_col)) == target_id, ]
    
    target_kba <- current_data$kba %>% 
      filter(as.character(kbasiteid) == target_id) %>% 
      st_drop_geometry()
    
    colnames(target_kba) <- tolower(colnames(target_kba))
    kba_total_km2 <- safe_extract(target_kba, c("kba_total_area_km2", "kba_total_area_ha"))
    
    base_statuses <- data.frame(
      Code    = as.character(0:6),
      English = c("NULL", "Extirpated", "Endangered", "Threatened", "Special Concern", "No Status", "Not at Risk"),
      stringsAsFactors = FALSE
    )
    
    if (!is.null(ch_overlaps) && nrow(ch_overlaps) > 0 && !is.na(kba_total_km2) && kba_total_km2 > 0) {
      sara_col <- safe_get_col(ch_overlaps, c("sara_status", "status"))
      
      ch_overlaps <- ch_overlaps %>% 
        mutate(
          overlap_km2 = as.numeric(st_area(.)) / 1e6,
          status_code = trimws(as.character(sara_col))
        ) %>%
        st_drop_geometry() %>%
        group_by(status_code) %>%
        summarize(total_overlap_km2 = sum(overlap_km2, na.rm = TRUE), .groups = "drop")
      
      summary_df <- base_statuses %>%
        left_join(ch_overlaps, by = c("Code" = "status_code")) %>%
        mutate(
          total_overlap_km2 = coalesce(total_overlap_km2, 0),
          pct_kba = round(pmin(100, (total_overlap_km2 / kba_total_km2) * 100), 1),
          label_display = paste0(English, " (Code ", Code, ")"),
          data_label = paste0(pct_kba, "% (", round(total_overlap_km2, 1), " km²)")
        )
    } else {
      summary_df <- base_statuses %>%
        mutate(
          total_overlap_km2 = 0,
          pct_kba = 0,
          label_display = paste0(English, " (Code ", Code, ")"),
          data_label = "0% (0 km²)"
        )
    }
    
    fallback_colors <- c(
      "NULL"            = "#94a3b8", "Extirpated"      = "#475569",
      "Endangered"      = "#d32f2f", "Threatened"      = "#e91e63",
      "Special Concern" = "#f57f17", "No Status"       = "#cbd5e1",
      "Not at Risk"     = "#2e7d32"
    )
    
    summary_df <- summary_df %>%
      filter(pct_kba > 0) %>%
      mutate(
        mapped_col = unname(ch_color_map[English]),
        bar_color  = ifelse(is.na(mapped_col), unname(fallback_colors[English]), mapped_col),
        label_display = factor(label_display, levels = rev(label_display))
      )
    
    if (nrow(summary_df) == 0) {
      plot_ly() %>%
        layout(
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
          annotations = list(
            text = "No overlapping Critical Habitat identified.",
            x = 0.5, y = 0.5, showarrow = FALSE,
            font = list(size = 13, color = "#64748b", family = "Open Sans, sans-serif")
          ),
          margin = list(l = 10, r = 10, t = 10, b = 10)
        ) %>%
        clean_plotly_config()
    } else {
      plot_ly(
        summary_df, 
        x = ~pct_kba, y = ~label_display, type = 'bar', orientation = 'h',
        marker = list(color = ~bar_color), text = ~data_label, textposition = 'none',
        hoverinfo = 'text', hovertext = ~paste0("<b>", label_display, "</b><br>Coverage: ", data_label)
      ) %>%
        layout(
          autosize = TRUE,
          xaxis = list(title = "% Overlap of Total KBA Area", zeroline = TRUE),
          yaxis = list(title = "", automargin = TRUE),
          margin = list(l = 10, r = 20, t = 10, b = 35), 
          showlegend = FALSE
        ) %>%
        clean_plotly_config()
    }
  })
  
  output$mapElement <- renderLeaflet({
    req(current_data$kba)
    
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -96.8, lat = 62.4, zoom = 4) %>%
      addLegend(
        position = "bottomright",
        colors = c("#92BF00", "#0AA1F4", "#FFCB00", "#FF0000", "#FF1493"),
        labels = c("Key Biodiversity Area", "CPCAD - Protected", "CPCAD - OECM", "Critical Habitat - Endangered", "Critical Habitat - Threatened"),
        title = "Conservation Layers", opacity = 0.85
      )
  })
  
  observe({
    kba_raw <- filtered_kba()
    req(kba_raw)
    if (is.null(kba_raw) || nrow(kba_raw) == 0) return()
    
    proxy <- leafletProxy("mapElement") %>% 
      clearShapes() %>% clearGroup("CPCAD_PA") %>% clearGroup("CPCAD_OECM") %>%
      clearGroup("CH_Endangered") %>% clearGroup("CH_Threatened") %>%
      leafgl::clearGlLayers()
    
    kba_data <- kba_raw %>% filter(as.character(st_geometry_type(.)) %in% c("POLYGON", "MULTIPOLYGON"))
    selected_prov <- input$provinceFilter
    target_kba_id <- selected_kba_id()
    
    cpcad_data <- if (target_kba_id != "All" && !is.null(current_data$cpcad_overlaps)) {
      overlaps <- current_data$cpcad_overlaps
      site_col <- safe_get_col(overlaps, c("kbasiteid", "siteid"))
      overlaps[trimws(as.character(site_col)) == target_kba_id, ]
    } else {
      current_data$cpcad
    }
    
    if (!is.null(cpcad_data) && nrow(cpcad_data) > 0 && selected_prov != "All" && target_kba_id == "All") {
      cpcad_codes <- switch(selected_prov,
                            "Alberta"                   = c("1", "AB", "ALBERTA", "48"),
                            "British Columbia"          = c("2", "BC", "BRITISH COLUMBIA", "59"),
                            "Manitoba"                  = c("3", "MB", "MANITOBA", "46"),
                            "New Brunswick"             = c("4", "NB", "NEW BRUNSWICK", "13"),
                            "Newfoundland and Labrador" = c("5", "NL", "NEWFOUNDLAND AND LABRADOR", "10"),
                            "Northwest Territories"     = c("6", "NT", "NORTHWEST TERRITORIES", "61"),
                            "Nova Scotia"               = c("7", "NS", "NOVA SCOTIA", "12"),
                            "Nunavut"                   = c("8", "NU", "NUNAVUT", "62"),
                            "Ontario"                   = c("9", "ON", "ONTARIO", "35"),
                            "Prince Edward Island"      = c("10", "PE", "PRINCE EDWARD ISLAND", "11"),
                            "Quebec"                    = c("11", "QC", "QUEBEC", "QUÉBEC", "24"),
                            "Saskatchewan"              = c("12", "SK", "SASKATCHEWAN", "47"),
                            "Yukon"                     = c("13", "YT", "YUKON", "60"),
                            "Federal / Offshore"        = as.character(14:21),
                            "Federal Offshore/Marine"   = as.character(14:21),
                            NULL
      )
      
      if (!is.null(cpcad_codes)) {
        loc_col <- safe_get_col(cpcad_data, c("loc", "jur_id", "loc_e", "province_e", "jurisdiction"))
        if (!all(is.na(loc_col))) {
          cpcad_vals <- toupper(trimws(as.character(loc_col)))
          cpcad_data <- cpcad_data[cpcad_vals %in% cpcad_codes, ]
        }
      }
    }
    
    ch_data <- current_data$ch
    if (!is.null(ch_data) && nrow(ch_data) > 0 && selected_prov != "All" && target_kba_id == "All") {
      ch_codes <- switch(selected_prov,
                         "Ontario"                   = c("ON", "ONTARIO", "35"),
                         "British Columbia"          = c("BC", "BRITISH COLUMBIA", "59"),
                         "Alberta"                   = c("AB", "ALBERTA", "48"),
                         "Quebec"                    = c("QC", "QUEBEC", "QUÉBEC", "24"),
                         "Saskatchewan"              = c("SK", "SASKATCHEWAN", "47"),
                         "Manitoba"                  = c("MB", "MANITOBA", "46"),
                         "Nova Scotia"               = c("NS", "NOVA SCOTIA", "12"),
                         "New Brunswick"             = c("NB", "NEW BRUNSWICK", "13"),
                         "Newfoundland and Labrador" = c("NL", "NEWFOUNDLAND AND LABRADOR", "10"),
                         "Prince Edward Island"      = c("PE", "PRINCE EDWARD ISLAND", "11"),
                         "Yukon"                     = c("YT", "YUKON", "60"),
                         "Northwest Territories"     = c("NT", "NORTHWEST TERRITORIES", "61"),
                         "Nunavut"                   = c("NU", "NUNAVUT", "62"),
                         NULL
      )
      
      if (!is.null(ch_codes)) {
        prov_col <- safe_get_col(ch_data, c("provterr_e", "provterr", "province_e"))
        if (!all(is.na(prov_col))) {
          pattern <- paste0("\\b(", paste(ch_codes, collapse = "|"), ")\\b")
          ch_data <- ch_data[grepl(pattern, toupper(as.character(prov_col)), ignore.case = TRUE), ]
        }
      }
    }
    
    if (input$showCPCAD && !is.null(cpcad_data) && nrow(cpcad_data) > 0) {
      cpcad_poly <- suppressWarnings({
        cpcad_data %>% 
          sf::st_make_valid() %>%
          filter(!st_is_empty(.)) %>% 
          filter(as.character(st_geometry_type(.)) %in% c("POLYGON", "MULTIPOLYGON")) %>%
          sf::st_cast("POLYGON")
      })
      
      if (nrow(cpcad_poly) > 0) {
        paoecm_col <- safe_get_col(cpcad_poly, c("pa_oecm_df", "paoecm_df", "pa_oecm"))
        
        cpcad_pa   <- cpcad_poly[as.character(paoecm_col) %in% c("1", "3"), ]
        cpcad_oecm <- cpcad_poly[as.character(paoecm_col) %in% c("2", "4"), ]
        
        name_val <- safe_get_col(cpcad_poly, c("name_e", "name", "pa_name_e"))
        
        if (nrow(cpcad_pa) > 0) {
          popup_pa <- safe_get_col(cpcad_pa, c("name_e", "name", "pa_name_e"))
          popup_pa[is.na(popup_pa)] <- "Protected Area"
          
          proxy %>% leafgl::addGlPolygons(
            data = cpcad_pa, color = "#0AA1F4", fillColor = "#0AA1F4", fillOpacity = 0.40,
            group = "CPCAD_PA", popup = popup_pa
          )
        }
        
        if (nrow(cpcad_oecm) > 0) {
          popup_oecm <- safe_get_col(cpcad_oecm, c("name_e", "name", "pa_name_e"))
          popup_oecm[is.na(popup_oecm)] <- "Conserved Area (OECM)"
          
          proxy %>% leafgl::addGlPolygons(
            data = cpcad_oecm, color = "#FFCB00", fillColor = "#FFCB00", fillOpacity = 0.40,
            group = "CPCAD_OECM", popup = popup_oecm
          )
        }
      }
    }
    
    if (isTRUE(input$showCH)) {
      ch_poly <- if (target_kba_id != "All" && !is.null(current_data$ch_kba_overlaps)) {
        overlaps <- current_data$ch_kba_overlaps
        site_col <- safe_get_col(overlaps, c("kbasiteid", "siteid"))
        overlaps[trimws(as.character(site_col)) == target_kba_id, ]
      } else {
        ch_data
      }
      
      if (!is.null(ch_poly) && nrow(ch_poly) > 0) {
        ch_poly <- suppressWarnings({
          ch_poly %>% 
            sf::st_make_valid() %>% 
            filter(!st_is_empty(.)) %>% 
            filter(as.character(st_geometry_type(.)) %in% c("POLYGON", "MULTIPOLYGON")) %>%
            sf::st_cast("POLYGON")
        })
        
        if (nrow(ch_poly) > 0) {
          sara_col <- safe_get_col(ch_poly, c("sara_status", "status"))
          clean_status <- trimws(as.character(sara_col))
          
          ch_endangered <- ch_poly[clean_status %in% c("2", "2.0", "Endangered"), ]
          ch_threatened <- ch_poly[clean_status %in% c("3", "3.0", "Threatened"), ]
          
          if (nrow(ch_endangered) > 0) {
            pop_end <- safe_get_col(ch_endangered, c("commname_e", "commname", "sitename_e"))
            pop_end[is.na(pop_end)] <- "Endangered Critical Habitat"
            
            proxy %>% leafgl::addGlPolygons(
              data = ch_endangered, color = "#FF0000", fillColor = "#FF0000", fillOpacity = 0.50,
              group = "CH_Endangered", popup = pop_end
            )
          }
          
          if (nrow(ch_threatened) > 0) {
            pop_thr <- safe_get_col(ch_threatened, c("commname_e", "commname", "sitename_e"))
            pop_thr[is.na(pop_thr)] <- "Threatened Critical Habitat"
            
            proxy %>% leafgl::addGlPolygons(
              data = ch_threatened, color = "#FF1493", fillColor = "#FF1493", fillOpacity = 0.50,
              group = "CH_Threatened", popup = pop_thr
            )
          }
        }
      }
    }
    
    if (input$showKBA && nrow(kba_data) > 0) {
      proxy %>% addPolygons(
        data = kba_data, color = "#2f4858", weight = 1.5, fillOpacity = 0.50, fillColor = "#92BF00",
        layerId = ~kbasiteid, label = ~paste("KBA:", kbasiteid, "-", nationalname),
        highlightOptions = highlightOptions(weight = 2.5, color = "#ffffff", fillOpacity = 0.75)
      )
    }
    
    if (target_kba_id == "All") {
      if (input$provinceFilter %in% c("All", "Federal / Offshore", "Federal Offshore/Marine")) {
        proxy %>% setView(lng = -96.8, lat = 62.4, zoom = 4)
      } else if (nrow(kba_data) > 0) {
        bbox <- st_bbox(kba_data)
        proxy %>% fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]], lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
      }
    }
  })
  
  observe({
    req(current_data$kba)
    proxy <- leafletProxy("mapElement") %>% clearGroup("selection_highlight")
    
    if (selected_kba_id() != "All") {
      target_kba <- current_data$kba %>% 
        filter(as.character(kbasiteid) == selected_kba_id()) %>% 
        filter(as.character(st_geometry_type(.)) %in% c("POLYGON", "MULTIPOLYGON"))
      
      req(nrow(target_kba) > 0)
      
      proxy %>% addPolygons(
        data = target_kba, color = "#2f4858", weight = 3.0, fillOpacity = 0.0,
        group = "selection_highlight"
      )
      
      bbox <- st_bbox(target_kba)
      proxy %>% fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]], lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
    }
  })
  
  observeEvent(input$mapElement_shape_click, {
    click <- input$mapElement_shape_click
    req(click, click$id)
    
    kba_df <- current_data$kba
    req(kba_df)
    
    target_kba <- kba_df %>% filter(as.character(kbasiteid) == as.character(click$id)) %>% st_drop_geometry()
    
    if (nrow(target_kba) > 0) {
      composite_string <- paste0(target_kba$kbasiteid[1], " - ", target_kba$nationalname[1])
      updateSelectInput(session, "kbaFilter", selected = composite_string)
    }
  })
  
  output$kbaSelectionHeader <- renderUI({
    req(current_data$kba)
    
    COLOR_KBA        <- "#92BF00"
    COLOR_PROTECTED  <- "#0AA1f4"
    COLOR_HABITAT    <- "#d32f2f"
    COLOR_CONSERVED  <- "#FFCB00"
    COLOR_ENDANGERED <- "#FF0000"
    COLOR_THREATENED <- "#FF1493"
    
    if (selected_kba_id() == "All") {
      req(filtered_kba())
      summary_df <- filtered_kba() %>% st_drop_geometry()
      if (nrow(summary_df) == 0) return(tags$p("No data available for selected criteria."))
      
      header_title <- if (input$provinceFilter == "All") "Canada National Overview" else paste0(input$provinceFilter, " Overview")
      sub_title    <- if (input$provinceFilter == "All") "Aggregated nationwide conservation statistics." else paste0("Aggregated metrics for ", input$provinceFilter, ".")
      
      total_kba_km2  <- safe_col_sum(summary_df, c("kba_total_area_km2", "kba_total_area_ha"))
      pa_kba_ha       <- safe_col_sum(summary_df, c("pa_area_ha", "pa_area_km2"))
      oecm_kba_ha     <- safe_col_sum(summary_df, c("oecm_area_ha", "oecm_area_km2"))
      
      kba_pa_pct      <- if (total_kba_km2 > 0) min(100, (pa_kba_ha / total_kba_km2) * 100) else 0
      kba_oecm_pct    <- if (total_kba_km2 > 0) min(100, (oecm_kba_ha / total_kba_km2) * 100) else 0
      
      ch_endangered_ha <- safe_col_sum(summary_df, c("ch_endangered_ha", "ch_endangered_km2"))
      ch_threatened_ha <- safe_col_sum(summary_df, c("ch_threatened_ha", "ch_threatened_km2"))
      kba_ch_end_pct   <- if (total_kba_km2 > 0) min(100, (ch_endangered_ha / total_kba_km2) * 100) else 0
      kba_ch_thr_pct   <- if (total_kba_km2 > 0) min(100, (ch_threatened_ha / total_kba_km2) * 100) else 0
      
      total_cpcad_km2 <- if (is.null(input$provinceFilter) || input$provinceFilter == "All") {
        if (!is.null(current_data$national_cpcad_km2)) current_data$national_cpcad_km2 else 0
      } else {
        get_provincial_cpcad_area(input$provinceFilter, current_data$cpcad_prov_summary, current_data$cpcad)
      }
      
      total_ch_km2 <- if (is.null(input$provinceFilter) || input$provinceFilter == "All") {
        if (!is.null(current_data$national_ch_km2)) current_data$national_ch_km2 else 0
      } else {
        get_provincial_ch_area(input$provinceFilter, current_data$ch_prov_summary, current_data$ch)
      }
      
      tags$div(
        h3(header_title, style = "color: #2f4858;"),
        p(sub_title, class = "client-body", style = "color: #475569; margin-bottom: 15px;"),
        
        fluidRow(
          class = "metric-container",
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBAs Total Area"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_KBA, "; font-weight: bold;"), paste0(format(round(total_kba_km2, 1), big.mark=","), " km²")))),
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Protected & OECM Total Area"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_PROTECTED, "; font-weight: bold;"), paste0(format(round(total_cpcad_km2, 1), big.mark=","), " km²")))),
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Critical Habitat Total Area"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_HABITAT, "; font-weight: bold;"), paste0(format(round(total_ch_km2, 1), big.mark=","), " km²"))))
        ),
        
        fluidRow(
          class = "metric-container",
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - Protected Areas %"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_PROTECTED, "; font-weight: bold;"), paste0(round(kba_pa_pct, 1), " %")))),
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - Conserved Areas %"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_CONSERVED, "; font-weight: bold;"), paste0(round(kba_oecm_pct, 1), " %"))))
        ),
        
        fluidRow(
          class = "metric-container",
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - Endangered Critical Habitat %"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_ENDANGERED, "; font-weight: bold;"), paste0(round(kba_ch_end_pct, 1), " %")))),
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - Threatened Critical Habitat %"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_THREATENED, "; font-weight: bold;"), paste0(round(kba_ch_thr_pct, 1), " %"))))
        ),
        static_footnote_ui
      )
    } else {
      target_kba <- current_data$kba %>% 
        filter(as.character(kbasiteid) == selected_kba_id()) %>% 
        st_drop_geometry()
      
      req(nrow(target_kba) > 0)
      
      site_name <- safe_extract(target_kba, c("nationalname", "name_e"), "Selected Site")
      site_id   <- safe_extract(target_kba, c("kbasiteid", "siteid"), selected_kba_id())
      site_jur  <- safe_extract(target_kba, c("jurisdiction_en", "jurisdiction"), "")
      
      site_kba_km2 <- safe_col_sum(target_kba, c("kba_total_area_km2", "kba_total_area_ha"))
      
      site_pa_ha        <- safe_col_sum(target_kba, c("pa_area_ha", "pa_area_km2"))
      site_oecm_ha      <- safe_col_sum(target_kba, c("oecm_area_ha", "oecm_area_km2"))
      site_pa_pct       <- if (site_kba_km2 > 0) min(100, (site_pa_ha / site_kba_km2) * 100) else 0
      site_oecm_pct     <- if (site_kba_km2 > 0) min(100, (site_oecm_ha / site_kba_km2) * 100) else 0
      
      site_ch_end_ha    <- safe_col_sum(target_kba, c("ch_endangered_ha", "ch_endangered_km2"))
      site_ch_thr_ha    <- safe_col_sum(target_kba, c("ch_threatened_ha", "ch_threatened_km2"))
      site_ch_total_km2 <- site_ch_end_ha + site_ch_thr_ha + safe_col_sum(target_kba, c("ch_specialconcern_ha", "ch_specialconcern_km2"))
      
      site_ch_end_pct   <- if (site_kba_km2 > 0) min(100, (site_ch_end_ha / site_kba_km2) * 100) else 0
      site_ch_thr_pct   <- if (site_kba_km2 > 0) min(100, (site_ch_thr_ha / site_kba_km2) * 100) else 0
      
      tags$div(
        h3(paste0("KBA Site Overview: ", site_name, " (ID: ", site_id, ")"), style = "color: #2f4858;"),
        p(tags$strong("Jurisdiction: "), site_jur, class = "client-body", style = "color: #475569; margin-bottom: 15px;"),
        
        fluidRow(
          class = "metric-container",
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBAs Total Area"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_KBA, "; font-weight: bold;"), paste0(format(round(site_kba_km2, 1), big.mark=","), " km²")))),
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Protected & OECM Total Area"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_PROTECTED, "; font-weight: bold;"), paste0(format(round(site_pa_ha + site_oecm_ha, 1), big.mark=","), " km²")))),
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Critical Habitat Total Area"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_HABITAT, "; font-weight: bold;"), paste0(format(round(site_ch_total_km2, 1), big.mark=","), " km²"))))
        ),
        
        fluidRow(
          class = "metric-container",
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - Protected Areas %"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_PROTECTED, "; font-weight: bold;"), paste0(round(site_pa_pct, 1), " %")))),
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - Conserved Areas %"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_CONSERVED, "; font-weight: bold;"), paste0(round(site_oecm_pct, 1), " %"))))
        ),
        
        fluidRow(
          class = "metric-container",
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - Endangered Critical Habitat %"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_ENDANGERED, "; font-weight: bold;"), paste0(round(site_ch_end_pct, 1), " %")))),
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - Threatened Critical Habitat %"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_THREATENED, "; font-weight: bold;"), paste0(round(site_ch_thr_pct, 1), " %"))))
        ),
        static_footnote_ui
      )
    }
  })
  
  output$kbaTable <- renderDT({
    req(current_data$kba)
    target_id <- selected_kba_id()
    
    table_data <- current_data$kba %>% st_drop_geometry()
    
    if (target_id != "All") {
      table_data <- table_data %>% filter(as.character(kbasiteid) == target_id)
    } else if (input$provinceFilter != "All") {
      jur_col <- safe_get_col(table_data, c("jurisdiction_en", "jurisdiction"))
      table_data <- table_data[jur_col == input$provinceFilter, ]
    }
    
    if (nrow(table_data) == 0) {
      return(datatable(data.frame(Message = "No KBA sites found for this selection."), options = list(dom = 't'), rownames = FALSE))
    }
    
    kba_id_vec      <- safe_get_col(table_data, c("kbasiteid", "siteid"))
    name_vec        <- safe_get_col(table_data, c("nationalname", "name_e"))
    jur_vec         <- safe_get_col(table_data, c("jurisdiction_en", "jurisdiction"))
    level_vec       <- safe_get_col(table_data, c("kbalevel_en", "level"))
    
    kba_area_vec    <- suppressWarnings(as.numeric(safe_get_col(table_data, c("kba_total_area_km2", "kba_total_area_ha"))))
    pa_area_vec     <- suppressWarnings(as.numeric(safe_get_col(table_data, c("pa_area_ha", "pa_area_km2"))))
    pa_prop_vec     <- suppressWarnings(as.numeric(safe_get_col(table_data, c("pa_proportion", "cpcad_1_prop"))))
    
    oecm_area_vec   <- suppressWarnings(as.numeric(safe_get_col(table_data, c("oecm_area_ha", "oecm_area_km2"))))
    oecm_prop_vec   <- suppressWarnings(as.numeric(safe_get_col(table_data, c("oecm_proportion", "cpcad_2_prop"))))
    
    ch_end_area_vec <- suppressWarnings(as.numeric(safe_get_col(table_data, c("ch_endangered_ha", "ch_endangered_km2"))))
    ch_end_prop_vec <- suppressWarnings(as.numeric(safe_get_col(table_data, c("ch_endangered_proportion"))))
    
    ch_thr_area_vec <- suppressWarnings(as.numeric(safe_get_col(table_data, c("ch_threatened_ha", "ch_threatened_km2"))))
    ch_thr_prop_vec <- suppressWarnings(as.numeric(safe_get_col(table_data, c("ch_threatened_proportion"))))
    
    final_table <- data.frame(
      `Site ID`                              = as.character(kba_id_vec),
      `Site Name`                            = as.character(name_vec),
      `Jurisdiction`                         = as.character(jur_vec),
      `Accreditation`                        = case_when(
        grepl("Global|Mondial", level_vec, ignore.case = TRUE) ~ "Global",
        grepl("National", level_vec, ignore.case = TRUE)      ~ "National",
        is.na(level_vec) | level_vec == ""                     ~ "Not Specified",
        TRUE                                                   ~ as.character(level_vec)
      ),
      `Total Area (km2)`                      = round(coalesce(kba_area_vec, 0), 1),
      `Protected Area (km2)`                  = round(coalesce(pa_area_vec, 0), 1),
      `Protection %`                          = round(coalesce(pa_prop_vec, 0) * 100, 1),
      `Conserved Area (km2)`                  = round(coalesce(oecm_area_vec, 0), 1),
      `Conserved %`                          = round(coalesce(oecm_prop_vec, 0) * 100, 1),
      `Critical Habitat - Endangered (km2)`   = round(coalesce(ch_end_area_vec, 0), 1),
      `Critical Habitat - Endangered %`       = round(coalesce(ch_end_prop_vec, 0) * 100, 1),
      `Critical Habitat - Threatened (km2)`   = round(coalesce(ch_thr_area_vec, 0), 1),
      `Critical Habitat - Threatened %`       = round(coalesce(ch_thr_prop_vec, 0) * 100, 1),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    datatable(final_table, options = list(pageLength = 15, scrollX = TRUE, dom = 'tp'), rownames = FALSE)
  })
  
  output$overlapTable <- renderDT({
    req(current_data$cpcad_overlaps)
    target_id <- selected_kba_id()
    
    table_sf <- current_data$cpcad_overlaps
    
    if (target_id != "All") {
      table_sf <- table_sf %>% filter(as.character(kbasiteid) == target_id)
    } else if (input$provinceFilter != "All") {
      req(filtered_kba())
      prov_kba_ids <- unique(as.character(filtered_kba()$kbasiteid))
      table_sf <- table_sf %>% filter(as.character(kbasiteid) %in% prov_kba_ids)
    }
    
    if (nrow(table_sf) == 0) {
      return(datatable(data.frame(Message = "No overlapping CPCAD sites found."), options = list(dom = 't'), rownames = FALSE))
    }
    
    calc_overlap_km2 <- round(as.numeric(st_area(table_sf)) / 1e6, 2)
    table_data       <- st_drop_geometry(table_sf)
    
    kba_area_val   <- suppressWarnings(as.numeric(safe_get_col(table_data, c("kba_total_area_km2", "kba_total_area_ha"))))
    coverage_pct   <- ifelse(!is.na(kba_area_val) & kba_area_val > 0, round((calc_overlap_km2 / kba_area_val) * 100, 1), 0)
    coverage_pct   <- pmin(100.0, coverage_pct)
    
    raw_paoecm_val <- safe_get_col(table_data, c("pa_oecm_df", "paoecm_df", "pa_oecm"))
    level_of_prot  <- case_when(
      as.character(raw_paoecm_val) %in% c("1") ~ "Protected area (PA)",
      as.character(raw_paoecm_val) %in% c("2") ~ "Other effective area-based conservation measure (OECM)",
      as.character(raw_paoecm_val) %in% c("3") ~ "Interim - protected area (PA)",
      as.character(raw_paoecm_val) %in% c("4") ~ "Interim - other effective area-based conservation measure (OECM)",
      as.character(raw_paoecm_val) %in% c("5") ~ "Not applicable",
      is.na(raw_paoecm_val) | as.character(raw_paoecm_val) == "" ~ "Not Reported",
      TRUE                                      ~ as.character(raw_paoecm_val)
    )
    
    raw_iucn_val <- safe_get_col(table_data, c("iucn_cat", "iucn"))
    iucn_label   <- case_when(
      as.character(raw_iucn_val) %in% c("1", "Ia") ~ "Ia - Strict Nature Reserve",
      as.character(raw_iucn_val) %in% c("2", "Ib") ~ "Ib - Wilderness Area",
      as.character(raw_iucn_val) %in% c("3", "II") ~ "II - National Park",
      as.character(raw_iucn_val) %in% c("4", "III")~ "III - Natural Monument or Feature",
      as.character(raw_iucn_val) %in% c("5", "IV") ~ "IV - Habitat/Species Management Area",
      as.character(raw_iucn_val) %in% c("6", "V")  ~ "V - Protected Landscape/Seascape",
      as.character(raw_iucn_val) %in% c("7", "VI") ~ "VI - Sustainable Use Area",
      as.character(raw_iucn_val) %in% c("8", "NR") ~ "Not Reported",
      as.character(raw_iucn_val) %in% c("9", "NA") ~ "Not Applicable",
      is.na(raw_iucn_val) | as.character(raw_iucn_val) == "" ~ "Not Reported",
      TRUE                                         ~ as.character(raw_iucn_val)
    )
    
    final_table <- data.frame(
      `Protected Area Name` = safe_get_col(table_data, c("name_e", "name", "pa_name_e")),
      `Site Type`           = safe_get_col(table_data, c("type_e", "pa_type", "type")),
      `Overlap (km²)`       = calc_overlap_km2,
      `Coverage %`          = coverage_pct,
      `Level of Protection` = level_of_prot,
      `IUCN Category`       = iucn_label,
      `Managing Authority`  = safe_get_col(table_data, c("mgmt_e", "mgmt", "parent_blk")),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    datatable(final_table, options = list(pageLength = 15, scrollX = TRUE, dom = 'tp'), rownames = FALSE)
  })
  
  output$chTable <- renderDT({
    req(current_data$ch_kba_overlaps)
    
    raw_overlaps <- current_data$ch_kba_overlaps
    target_id    <- selected_kba_id()
    
    if (target_id != "All" && target_id != "") {
      table_data <- raw_overlaps %>% 
        filter(trimws(as.character(kbasiteid)) == target_id)
    } else if (!is.null(input$provinceFilter) && input$provinceFilter != "All") {
      req(filtered_kba())
      prov_kba_ids <- unique(trimws(as.character(filtered_kba()$kbasiteid)))
      table_data <- raw_overlaps %>% 
        filter(trimws(as.character(kbasiteid)) %in% prov_kba_ids)
    } else {
      table_data <- raw_overlaps
    }
    
    if (nrow(table_data) == 0) {
      return(datatable(
        data.frame(Message = paste0("No Critical Habitat boundaries found for selection: ", target_id)), 
        options = list(dom = 't'), 
        rownames = FALSE
      ))
    }
    
    has_geom <- inherits(table_data, "sf")
    
    if (has_geom) {
      calc_overlap_km2 <- round(as.numeric(st_area(table_data)) / 1e6, 2)
      table_data       <- st_drop_geometry(table_data)
    } else {
      calc_overlap_km2 <- rep(NA_real_, nrow(table_data))
    }
    
    site_area_km2 <- suppressWarnings(as.numeric(safe_get_col(table_data, c("areakm2", "kba_total_area_km2", "kba_total_area_ha"))))
    
    calc_coverage_pct <- ifelse(
      !is.na(site_area_km2) & site_area_km2 > 0 & !is.na(calc_overlap_km2),
      round((calc_overlap_km2 / site_area_km2) * 100, 1),
      NA_real_
    )
    calc_coverage_pct <- pmin(100.0, calc_coverage_pct)
    
    sara_status_raw <- safe_get_col(table_data, c("sara_status", "status"))
    sara_status_lbl <- case_when(
      as.character(sara_status_raw) %in% c("1", "Extirpated")      ~ "Extirpated",
      as.character(sara_status_raw) %in% c("2", "Endangered")      ~ "Endangered",
      as.character(sara_status_raw) %in% c("3", "Threatened")      ~ "Threatened",
      as.character(sara_status_raw) %in% c("4", "Special Concern")  ~ "Special Concern",
      TRUE                                                         ~ as.character(sara_status_raw)
    )
    
    agency_raw <- safe_get_col(table_data, c("sara_agency", "agency"))
    agency_lbl <- case_when(
      as.character(agency_raw) %in% c("1", "ECCC") ~ "Environment and Climate Change Canada",
      as.character(agency_raw) %in% c("2", "DFO")  ~ "Fisheries and Oceans Canada",
      as.character(agency_raw) %in% c("3", "PCA")  ~ "Parks Canada Agency",
      TRUE                                         ~ as.character(agency_raw)
    )
    
    rd_raw <- safe_get_col(table_data, c("rd_status", "rdstatus"))
    rd_lbl <- case_when(
      as.character(rd_raw) %in% c("1", "Final")    ~ "Final",
      as.character(rd_raw) %in% c("2", "Proposed") ~ "Proposed",
      as.character(rd_raw) %in% c("3", "Draft")    ~ "Draft",
      TRUE                                         ~ as.character(rd_raw)
    )
    
    taxon_raw <- safe_get_col(table_data, c("taxon"))
    taxon_lbl <- case_when(
      as.character(taxon_raw) %in% c("1", "Amphibians", "AM", "Amphibien")           ~ "Amphibians",
      as.character(taxon_raw) %in% c("2", "Birds", "BI", "AV", "Oiseau")             ~ "Birds",
      as.character(taxon_raw) %in% c("3", "Fishes", "FI", "Poisson")                 ~ "Fishes (freshwater)",
      as.character(taxon_raw) %in% c("4", "Invertebrates", "IN", "Invertébré")        ~ "Invertebrates",
      as.character(taxon_raw) %in% c("5", "Lichens", "LI")                            ~ "Lichens",
      as.character(taxon_raw) %in% c("6", "Mammals", "MA", "Mammifère")              ~ "Mammals",
      as.character(taxon_raw) %in% c("7", "Mosses", "MO", "Mousse")                   ~ "Mosses",
      as.character(taxon_raw) %in% c("8", "Reptiles", "RE", "Reptile")               ~ "Reptiles",
      as.character(taxon_raw) %in% c("9", "Vascular Plants", "VP", "PL", "Plante")   ~ "Vascular Plants",
      as.character(taxon_raw) %in% c("10", "Non-vascular Plants", "NV")              ~ "Non-vascular Plants",
      as.character(taxon_raw) %in% c("11", "Molluscs", "MOLL", "Arthropods")         ~ "Molluscs",
      as.character(taxon_raw) %in% c("12", "Fungi", "FU", "Champignons")             ~ "Fungi",
      as.character(taxon_raw) %in% c("13", "Corals", "Sponges", "CO")                ~ "Corals / Sponges",
      is.na(taxon_raw) | as.character(taxon_raw) %in% c("", "0", "99")                ~ "Not Specified",
      TRUE                                                                           ~ as.character(taxon_raw)
    )
    
    final_table <- data.frame(
      `Site Name`               = safe_get_col(table_data, c("sitename_e", "sitename")),
      `Overlap (km2)`           = calc_overlap_km2,
      `Coverage (%)`            = calc_coverage_pct,
      `Species Common Name`     = safe_get_col(table_data, c("commname_e", "commname")),
      `Species Scientific Name` = safe_get_col(table_data, c("sciname")),
      `Taxon`                   = taxon_lbl,
      `COSEWIC ID`              = safe_get_col(table_data, c("cosewic_id", "cosewicid")),
      `SARA Status`             = sara_status_lbl,
      `Province`                = safe_get_col(table_data, c("provterr_e", "provterr")),
      `Sensitive?`              = safe_get_col(table_data, c("sensitive_e", "sensitive")),
      `Recovery Agency`         = agency_lbl,
      `Recovery Doc`            = safe_get_col(table_data, c("rdoc_name_e", "rdoc_name")),
      `Doc Status`              = rd_lbl,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    datatable(final_table, options = list(pageLength = 10, scrollX = TRUE, dom = 'tp'), rownames = FALSE)
  })
}

# 5. --- LAUNCH APP ---
shinyApp(ui, server)