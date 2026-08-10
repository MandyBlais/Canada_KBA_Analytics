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
  header = dashboardHeader(title = "Key Biodiversity Areas (KBAs) in Canada Analytics Dashboard", titleWidth = 320),
  
  sidebar = dashboardSidebar(
    width = 320,
    sidebarMenu(
      tags$div(
        style = "padding: 15px; color: #fff;",
        h4("Spatial Filter Configurations", class = "client-subhead", style = "margin-top: 0; margin-bottom: 12px;"),
        p("Isolate regions using the dropdowns or click directly on a map boundary polygon.", class = "client-body", style = "color: #b8c7ce; font-size: 12px; margin-bottom: 15px;"),
        hr(style = "border-color: #92BF00; border-width: 2px; margin: 15px 0;"),
        
        # Geographic Hierarchy Filters
        selectInput(
          "provinceFilter", "Province / Territory:", 
          choices = get_province_choices(data_payload$kba_layer)
        ),
        
        selectInput(
          "kbaFilter", "Specific KBA Site:", 
          choices = get_kba_choices(data_payload$kba_layer)
        ),
        
        hr(style = "border-color: #92BF00; border-width: 2px; margin: 15px 0;"),
        
        # Layer Visibility Controls
        h5("Map Layer Toggles", class = "client-subhead", style = "color: #fff; margin-bottom: 10px;"),
        checkboxInput("showKBA", "Key Biodiversity Areas (KBAs) Layer", value = TRUE),
        checkboxInput("showCPCAD", "PA & OECM (CPCAD) Layer", value = FALSE),
        checkboxInput("showCH", "Critical Habitats (CH) Layer", value = FALSE),
        
        hr(style = "border-color: #92BF00; border-width: 2px; margin: 15px 0;"),
        
        # Operational Utilities
        actionButton(
          "runSync", "Force API Data Refresh", 
          style = "width: 100%; font-weight: bold; background-color: #FFCB00; color: #3a3426; border-color: #FFCB00;"
        ),
        br(), br(),
        tags$small(textOutput("cacheTimeText"), class = "client-body", style = "color: #9ca8b0;")
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
        .content-wrapper { background-color: #3a3426 !important; }
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
        
        uiOutput("kbaSelectionHeader"),
        br(),
        
        # PLOTLY CHARTS UI ROW (Rendered dynamically when a single site is selected)
        conditionalPanel(
          condition = "input.kbaFilter != 'All' && input.kbaFilter != ''",
          div(
            style = "width: 100%; overflow-x: hidden;",
            fluidRow(
              class = "chart-box-container",
              column(
                width = 12,
                h4("PA & OECM Proportions", style = "color: #2f4858; font-size: 14px; margin-bottom: 5px;"),
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
        
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "KBA Sites Inventory",
            br(),
            p("Designated Key Biodiversity Areas within current spatial selection.", class = "client-body", style = "color: #3a3426; font-size: 12px; margin-bottom: 12px;"),
            DTOutput("kbaTable")
          ),
          tabPanel(
            "Protected & OECM Areas",
            br(),
            p("Individual contributing protected and other effective area-based conservation areas.", class = "client-body", style = "color: #3a3426; font-size: 12px; margin-bottom: 12px;"),
            DTOutput("overlapTable")
          ),
          tabPanel(
            "Critical Habitat Areas",
            br(),
            p("Species at Risk Critical Habitat overlapping KBA sites.", class = "client-body", style = "color: #3a3426; font-size: 12px; margin-bottom: 12px;"),
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
  
  # Static footnote block definition
  static_footnote_ui <- tags$p(
    class = "header-footnote",
    "CPCAD Protected Areas & Other Effective Area-Based Conservation Measures (PA & OECM) totals reflect terrestrial and inland protected areas per province/territory; offshore marine protected areas are categorized under Federal / Offshore jurisdiction. Critical Habitat and KBA totals include adjacent coastal and marine areas."
  )
  
  observeEvent(input$runSync, {
    showModal(modalDialog("Querying APIs & Re-Calculating Spatial Intersections... Please wait.", footer = NULL))
    tryCatch({
      source("data_engine.R")
      refresh_spatial_cache()
      
      new_payload <- readRDS(CACHE_PATH)
      current_data$kba                <- new_payload$kba_layer
      current_data$cpcad              <- new_payload$cpcad_layer
      current_data$ch                 <- new_payload$ch_layer
      current_data$cpcad_overlaps     <- new_payload$cpcad_overlaps
      current_data$ch_kba_overlaps    <- new_payload$ch_kba_overlaps
      current_data$national_cpcad_km2 <- new_payload$national_cpcad_km2
      current_data$national_ch_km2    <- new_payload$national_ch_km2
      current_data$cpcad_prov_summary <- new_payload$cpcad_prov_summary
      current_data$ch_prov_summary    <- new_payload$ch_prov_summary
      current_data$time               <- new_payload$timestamp
      
      updateSelectInput(session, "provinceFilter", choices = get_province_choices(current_data$kba))
      updateSelectInput(session, "kbaFilter", choices = get_kba_choices(current_data$kba))
      
    }, error = function(e) {
      showNotification(paste("Sync Processing Fault:", e$message), type = "error", duration = 10)
    })
    removeModal()
  })
  
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
  
  # Helper to safely extract numeric values from dataframe columns (case-insensitive)
  safe_extract <- function(df, col_names, default = 0) {
    if (is.null(df) || nrow(df) == 0) return(default)
    df_cols_lower <- tolower(colnames(df))
    
    for (col in col_names) {
      match_idx <- match(tolower(col), df_cols_lower)
      if (!is.na(match_idx)) {
        val <- df[[match_idx]][1]
        if (!is.na(val) && is.numeric(val)) return(val)
      }
    }
    return(default)
  }
  
  # --- CHART 1: FIXED DONUT CHART FOR KBA PROTECTION & CONSERVATION PROPORTIONS ---
  output$kbaProtectionPlot <- renderPlotly({
    req(selected_kba_id() != "All")
    
    target_kba <- current_data$kba %>% 
      filter(trimws(as.character(kbasiteid)) == trimws(selected_kba_id())) %>% 
      st_drop_geometry()
    
    req(nrow(target_kba) > 0)
    
    colnames(target_kba) <- tolower(colnames(target_kba))
    
    # Safe Extraction across primary and secondary fallback column names
    raw_1 <- safe_extract(target_kba, c("cpcad_1_prop", "cpcad_1_pct", "cpcad_1_perc", "prop_cpcad_1"))
    raw_2 <- safe_extract(target_kba, c("cpcad_2_prop", "cpcad_2_pct", "cpcad_2_perc", "prop_cpcad_2"))
    raw_3 <- safe_extract(target_kba, c("cpcad_3_prop", "cpcad_3_pct", "cpcad_3_perc", "prop_cpcad_3"))
    raw_4 <- safe_extract(target_kba, c("cpcad_4_prop", "cpcad_4_pct", "cpcad_4_perc", "prop_cpcad_4"))
    raw_5 <- safe_extract(target_kba, c("cpcad_5_prop", "cpcad_5_pct", "cpcad_5_perc", "prop_cpcad_5"))
    
    # Fallback checks to pa_proportion / oecm_proportion if individual categories are zero/missing
    if (sum(c(raw_1, raw_2, raw_3, raw_4, raw_5), na.rm = TRUE) == 0) {
      raw_1 <- safe_extract(target_kba, c("pa_proportion", "pa_prop"))
      raw_2 <- safe_extract(target_kba, c("oecm_proportion", "oecm_prop"))
    }
    
    # Auto-detect scale (decimal 0-1 vs percentage 0-100)
    max_val <- max(c(raw_1, raw_2, raw_3, raw_4, raw_5), na.rm = TRUE)
    multiplier <- if (max_val <= 1.0 && max_val > 0) 100 else 1
    
    pct_1 <- min(100, round(raw_1 * multiplier, 1))
    pct_2 <- min(100, round(raw_2 * multiplier, 1))
    pct_3 <- min(100, round(raw_3 * multiplier, 1))
    pct_4 <- min(100, round(raw_4 * multiplier, 1))
    pct_5 <- min(100, round(raw_5 * multiplier, 1))
    
    total_pa_oecm_pct <- min(100, round(pct_1 + pct_2 + pct_3 + pct_4, 1))
    total_cpcad_all   <- pct_1 + pct_2 + pct_3 + pct_4 + pct_5
    unprotected_pct   <- max(0, round(100 - total_cpcad_all, 1))
    
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
    )
    
    plot_df_filtered <- plot_df %>% filter(Percentage > 0)
    
    # Render fallback slice if zero protection is recorded
    if (nrow(plot_df_filtered) == 0) {
      plot_df_filtered <- data.frame(
        Category = "Unprotected / Other",
        Percentage = 100,
        Color = "#E2E8F0",
        stringsAsFactors = FALSE
      )
    }
    
    plot_ly(
      plot_df_filtered, 
      labels = ~Category, 
      values = ~Percentage, 
      type = 'pie',
      hole = 0.60,
      domain = list(x = c(0.1, 0.9), y = c(0, 1)),
      marker = list(colors = plot_df_filtered$Color),
      textinfo = 'none',
      hoverinfo = 'text',
      text = ~paste0("<b>", Category, "</b><br>", Percentage, "%"),
      hoverlabel = list(
        font = list(family = "Open Sans, sans-serif", size = 12)
      )
    ) %>%
      layout(
        showlegend = FALSE,
        autosize = TRUE,
        annotations = list(
          text = paste0("<span style='font-size:15px; font-weight:bold; color:#2f4858;'>", total_pa_oecm_pct, "%</span><br><span style='font-size:10px; font-weight:700; color:#64748b;'>TOTAL PA & OECM</span>"),
          x = 0.5, y = 0.5,
          showarrow = FALSE
        ),
        margin = list(l = 10, r = 10, t = 5, b = 5)
      ) %>%
      clean_plotly_config()
  })
  
  # --- CHART 2: CRITICAL HABITAT SARA STATUS (CODES 0 - 6) OVERLAP % ---
  output$kbaChStatusPlot <- renderPlotly({
    req(selected_kba_id() != "All")
    req(current_data$ch_kba_overlaps)
    
    target_id <- selected_kba_id()
    
    ch_overlaps <- current_data$ch_kba_overlaps %>% 
      filter(trimws(as.character(kbasiteid)) == target_id)
    
    target_kba <- current_data$kba %>% 
      filter(as.character(kbasiteid) == target_id) %>% 
      st_drop_geometry()
    
    colnames(target_kba) <- tolower(colnames(target_kba))
    kba_total_ha <- target_kba$kba_total_area_ha[1]
    
    base_statuses <- data.frame(
      Code    = as.character(0:6),
      English = c(
        "NULL",
        "Extirpated",
        "Endangered",
        "Threatened",
        "Special Concern",
        "No Status",
        "Not at Risk"
      ),
      stringsAsFactors = FALSE
    )
    
    if (nrow(ch_overlaps) > 0 && !is.na(kba_total_ha) && kba_total_ha > 0) {
      ch_overlaps <- ch_overlaps %>% 
        mutate(
          overlap_ha = as.numeric(st_area(.)) / 10000,
          status_code = trimws(as.character(SARA_Status))
        ) %>%
        st_drop_geometry() %>%
        group_by(status_code) %>%
        summarize(total_overlap_ha = sum(overlap_ha, na.rm = TRUE), .groups = "drop")
      
      summary_df <- base_statuses %>%
        left_join(ch_overlaps, by = c("Code" = "status_code")) %>%
        mutate(
          total_overlap_ha = coalesce(total_overlap_ha, 0),
          pct_kba = round(pmin(100, (total_overlap_ha / kba_total_ha) * 100), 1),
          label_display = paste0(English, " (Code ", Code, ")"),
          data_label = paste0(pct_kba, "% (", round(total_overlap_ha * 0.01, 1), " km²)")
        )
    } else {
      summary_df <- base_statuses %>%
        mutate(
          total_overlap_ha = 0,
          pct_kba = 0,
          label_display = paste0(English, " (Code ", Code, ")"),
          data_label = "0% (0 km²)"
        )
    }
    
    fallback_colors <- c(
      "NULL"            = "#94a3b8",
      "Extirpated"      = "#475569",
      "Endangered"      = "#d32f2f",
      "Threatened"      = "#e91e63",
      "Special Concern" = "#f57f17",
      "No Status"       = "#cbd5e1",
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
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(
            text = "No overlapping Critical Habitat identified.",
            x = 0.5, y = 0.5,
            showarrow = FALSE,
            font = list(size = 13, color = "#64748b", family = "Open Sans, sans-serif")
          ),
          margin = list(l = 10, r = 10, t = 10, b = 10)
        ) %>%
        clean_plotly_config()
    } else {
      plot_ly(
        summary_df, 
        x = ~pct_kba, 
        y = ~label_display, 
        type = 'bar', 
        orientation = 'h',
        marker = list(color = ~bar_color),
        text = ~data_label,
        textposition = 'none',
        hoverinfo = 'text',
        hovertext = ~paste0("<b>", label_display, "</b><br>Coverage: ", data_label)
      ) %>%
        layout(
          autosize = TRUE,
          xaxis = list(
            title = "% Overlap of Total KBA Area",
            zeroline = TRUE,
            titlefont = list(size = 10, family = "Open Sans, sans-serif"),
            tickfont = list(size = 10, family = "Open Sans, sans-serif")
          ),
          yaxis = list(
            title = "",
            automargin = TRUE,
            tickfont = list(size = 10, family = "Open Sans, sans-serif")
          ),
          margin = list(l = 10, r = 20, t = 10, b = 35), 
          showlegend = FALSE
        ) %>%
        clean_plotly_config()
    }
  })
  
  # --- LEAFLET MAP INITIALIZATION ---
  output$mapElement <- renderLeaflet({
    req(current_data$kba)
    
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -96.8, lat = 62.4, zoom = 4) %>%
      addLegend(
        position = "bottomright",
        colors = c("#92BF00", "#0AA1F4", "#FFCB00", "#FF0000", "#FF1493"),
        labels = c(
          "KBAs", 
          "Protected Areas",
          "OECM Areas",
          "CH - Endangered",
          "CH - Threatened"
        ),
        title = "Conservation Layers",
        opacity = 0.85
      ) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var legend = document.querySelector('.leaflet-control-legend');
          if (legend) {
            legend.style.fontFamily = 'Open Sans, sans-serif';
            legend.style.fontWeight = '300';
            legend.style.backgroundColor = 'rgba(255, 255, 255, 0.95)';
            legend.style.border = '2px solid #2f4858';
            legend.style.borderRadius = '6px';
            var title = legend.querySelector('strong');
            if (title) {
              title.style.fontFamily = 'rift, Impact, sans-serif';
              title.style.fontWeight = '700';
              title.style.textTransform = 'uppercase';
              title.style.color = '#2f4858';
            }
          }
        }
      ")
  })
  
  # --- OBSERVER 1: MAP LAYERS (HARDWARE ACCELERATED VIA LEAFGL) ---
  observe({
    kba_raw <- filtered_kba()
    req(kba_raw)
    if (is.null(kba_raw) || nrow(kba_raw) == 0) return()
    
    proxy <- leafletProxy("mapElement") %>% 
      clearShapes() %>%
      clearGroup("CPCAD_PA") %>%
      clearGroup("CPCAD_OECM") %>%
      clearGroup("CH_Endangered") %>%
      clearGroup("CH_Threatened") %>%
      leafgl::clearGlLayers()
    
    kba_data <- kba_raw %>% filter(as.character(st_geometry_type(.)) %in% c("POLYGON", "MULTIPOLYGON"))
    selected_prov <- input$provinceFilter
    
    # --- CPCAD PROVINCIAL FILTERING ---
    cpcad_data <- current_data$cpcad
    if (!is.null(cpcad_data) && nrow(cpcad_data) > 0 && selected_prov != "All") {
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
                            "Quebec"                    = c("11", "QC", "QUEBEC", "24"),
                            "Saskatchewan"              = c("12", "SK", "SASKATCHEWAN", "47"),
                            "Yukon"                     = c("13", "YT", "YUKON", "60"),
                            "Federal / Offshore"        = as.character(14:21),
                            "Federal Offshore/Marine"   = as.character(14:21),
                            NULL
      )
      
      if (!is.null(cpcad_codes)) {
        cpcad_cols <- tolower(colnames(cpcad_data))
        loc_idx <- match(TRUE, cpcad_cols %in% c("loc", "jur_id", "loc_e", "province_e"))
        if (!is.na(loc_idx)) {
          col_name <- colnames(cpcad_data)[loc_idx]
          cpcad_vals <- toupper(trimws(as.character(cpcad_data[[col_name]])))
          cpcad_data <- cpcad_data[cpcad_vals %in% cpcad_codes, ]
        }
      }
    }
    
    # --- CRITICAL HABITAT PROVINCIAL FILTERING ---
    ch_data <- current_data$ch
    if (!is.null(ch_data) && nrow(ch_data) > 0 && selected_prov != "All") {
      ch_codes <- switch(selected_prov,
                         "Ontario"                   = c("ON", "ONTARIO", "35"),
                         "British Columbia"          = c("BC", "BRITISH COLUMBIA", "59"),
                         "Alberta"                   = c("AB", "ALBERTA", "48"),
                         "Quebec"                    = c("QC", "QUEBEC", "24"),
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
        ch_cols <- tolower(colnames(ch_data))
        prov_idx <- match(TRUE, ch_cols %in% c("provterr_e", "provterr", "province_e"))
        if (!is.na(prov_idx)) {
          col_name <- colnames(ch_data)[prov_idx]
          pattern <- paste0("\\b(", paste(ch_codes, collapse = "|"), ")\\b")
          ch_data <- ch_data[grepl(pattern, toupper(as.character(ch_data[[col_name]])), ignore.case = TRUE), ]
        }
      }
    }
    
    # --- RENDER CPCAD (SPLIT BY PA AND CONSERVED/OECM) ---
    if (input$showCPCAD && !is.null(cpcad_data) && nrow(cpcad_data) > 0) {
      cpcad_poly <- suppressWarnings({
        cpcad_data %>% 
          sf::st_make_valid() %>%
          filter(!st_is_empty(.)) %>% 
          filter(as.character(st_geometry_type(.)) %in% c("POLYGON", "MULTIPOLYGON")) %>%
          sf::st_cast("POLYGON")
      })
      
      if (nrow(cpcad_poly) > 0) {
        cpcad_names <- colnames(cpcad_poly)
        name_field  <- cpcad_names[tolower(cpcad_names) %in% c("name_e", "name", "pa_name_e")][1]
        
        cpcad_pa   <- cpcad_poly %>% filter(as.character(PA_OECM_DF) %in% c("1", "3"))
        cpcad_oecm <- cpcad_poly %>% filter(as.character(PA_OECM_DF) %in% c("2", "4"))
        
        if (nrow(cpcad_pa) > 0) {
          popup_pa <- if (!is.na(name_field) && name_field %in% names(cpcad_pa)) {
            pop <- as.character(cpcad_pa[[name_field]])
            ifelse(is.na(pop), "Protected Area", pop)
          } else NULL
          
          proxy %>% leafgl::addGlPolygons(
            data = cpcad_pa,
            color = "#0AA1F4",
            fillColor = "#0AA1F4",
            fillOpacity = 0.40,
            group = "CPCAD_PA",
            popup = popup_pa
          )
        }
        
        if (nrow(cpcad_oecm) > 0) {
          popup_oecm <- if (!is.na(name_field) && name_field %in% names(cpcad_oecm)) {
            pop <- as.character(cpcad_oecm[[name_field]])
            ifelse(is.na(pop), "OECM Area", pop)
          } else NULL
          
          proxy %>% leafgl::addGlPolygons(
            data = cpcad_oecm,
            color = "#FFCB00",
            fillColor = "#FFCB00",
            fillOpacity = 0.40,
            group = "CPCAD_OECM",
            popup = popup_oecm
          )
        }
      }
    }
    
    # --- RENDER CRITICAL HABITAT ---
    if (isTRUE(input$showCH)) {
      target_id <- selected_kba_id()
      
      ch_poly <- if (target_id != "All" && !is.null(current_data$ch_kba_overlaps)) {
        current_data$ch_kba_overlaps %>% 
          filter(trimws(as.character(kbasiteid)) == trimws(target_id))
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
          ch_names   <- colnames(ch_poly)
          comm_field <- ch_names[tolower(ch_names) %in% c("commname_e", "commname", "sitename_e")][1]
          
          ch_poly <- ch_poly %>% mutate(clean_status = trimws(as.character(SARA_Status)))
          
          ch_endangered <- ch_poly %>% filter(clean_status %in% c("2", "2.0", "Endangered"))
          ch_threatened <- ch_poly %>% filter(clean_status %in% c("3", "3.0", "Threatened"))
          
          if (nrow(ch_endangered) > 0) {
            popup_end <- if (!is.na(comm_field) && comm_field %in% names(ch_endangered)) {
              pop <- as.character(ch_endangered[[comm_field]])
              ifelse(is.na(pop), "Endangered CH", pop)
            } else NULL
            
            proxy %>% leafgl::addGlPolygons(
              data = ch_endangered,
              color = "#FF0000",
              fillColor = "#FF0000",
              fillOpacity = 0.50,
              group = "CH_Endangered",
              popup = popup_end
            )
          }
          
          if (nrow(ch_threatened) > 0) {
            popup_thr <- if (!is.na(comm_field) && comm_field %in% names(ch_threatened)) {
              pop <- as.character(ch_threatened[[comm_field]])
              ifelse(is.na(pop), "Threatened CH", pop)
            } else NULL
            
            proxy %>% leafgl::addGlPolygons(
              data = ch_threatened,
              color = "#FF1493",
              fillColor = "#FF1493",
              fillOpacity = 0.50,
              group = "CH_Threatened",
              popup = popup_thr
            )
          }
        }
      }
    }
    
    # --- RENDER KBAS (STANDARD LEAFLET) ---
    if (input$showKBA && nrow(kba_data) > 0) {
      proxy %>% addPolygons(
        data = kba_data,
        color = "#2f4858",
        weight = 1.5,
        fillOpacity = 0.50,
        fillColor = "#92BF00",
        layerId = ~kbasiteid,
        label = ~paste("KBA:", kbasiteid, "-", nationalname),
        highlightOptions = highlightOptions(weight = 2.5, color = "#ffffff", fillOpacity = 0.75)
      )
    }
    
    # --- RECENTER MAP ---
    if (selected_kba_id() == "All") {
      if (input$provinceFilter %in% c("All", "Federal / Offshore")) {
        proxy %>% setView(lng = -96.8, lat = 62.4, zoom = 4)
      } else if (nrow(kba_data) > 0) {
        bbox <- st_bbox(kba_data)
        proxy %>% fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]], lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
      }
    }
  })
  
  # --- OBSERVER 2: ACTIVE SELECTION OVERLAYS ---
  observe({
    req(current_data$kba)
    proxy <- leafletProxy("mapElement") %>% clearGroup("selection_highlight")
    
    if (selected_kba_id() != "All") {
      target_kba <- current_data$kba %>% 
        filter(as.character(kbasiteid) == selected_kba_id()) %>% 
        filter(as.character(st_geometry_type(.)) %in% c("POLYGON", "MULTIPOLYGON"))
      
      req(nrow(target_kba) > 0)
      
      proxy %>% addPolygons(
        data = target_kba,
        color = "#2f4858",
        weight = 3.0,
        fillOpacity = 0.0,
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
  
  # --- HEADER METRICS UI ---
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
      
      colnames(summary_df) <- tolower(colnames(summary_df))
      
      header_title <- if (input$provinceFilter == "All") "Canada National Overview" else paste0(input$provinceFilter, " Overview")
      sub_title    <- if (input$provinceFilter == "All") "Aggregated nationwide conservation statistics." else paste0("Aggregated metrics for ", input$provinceFilter, ".")
      
      total_kba_ha  <- sum(summary_df$kba_total_area_ha, na.rm = TRUE)
      total_kba_km2 <- total_kba_ha * 0.01
      
      pa_kba_ha       <- sum(summary_df$pa_area_ha, na.rm = TRUE)
      oecm_kba_ha     <- sum(summary_df$oecm_area_ha, na.rm = TRUE)
      kba_pa_pct      <- if (total_kba_ha > 0) min(100, (pa_kba_ha / total_kba_ha) * 100) else 0
      kba_oecm_pct    <- if (total_kba_ha > 0) min(100, (oecm_kba_ha / total_kba_ha) * 100) else 0
      
      ch_endangered_ha <- sum(summary_df$ch_endangered_ha, na.rm = TRUE)
      ch_threatened_ha <- sum(summary_df$ch_threatened_ha, na.rm = TRUE)
      kba_ch_end_pct   <- if (total_kba_ha > 0) min(100, (ch_endangered_ha / total_kba_ha) * 100) else 0
      kba_ch_thr_pct   <- if (total_kba_ha > 0) min(100, (ch_threatened_ha / total_kba_ha) * 100) else 0
      
      total_cpcad_km2 <- if (is.null(input$provinceFilter) || input$provinceFilter == "All") {
        if (!is.null(current_data$national_cpcad_km2)) current_data$national_cpcad_km2 else 0
      } else {
        prov_summary <- current_data$cpcad_prov_summary
        if (!is.null(prov_summary) && nrow(prov_summary) > 0) {
          matched <- prov_summary %>% 
            filter(tolower(trimws(JUR_CLEAN)) == tolower(trimws(input$provinceFilter)))
          if (nrow(matched) > 0) sum(matched$CPCAD_KM2, na.rm = TRUE) else 0
        } else {
          0
        }
      }
      
      total_ch_km2 <- if (is.null(input$provinceFilter) || input$provinceFilter == "All") {
        if (!is.null(current_data$national_ch_km2)) current_data$national_ch_km2 else 0
      } else {
        ch_summary <- current_data$ch_prov_summary
        if (!is.null(ch_summary) && nrow(ch_summary) > 0) {
          matched <- ch_summary %>% 
            filter(tolower(trimws(JUR_CLEAN)) == tolower(trimws(input$provinceFilter)))
          if (nrow(matched) > 0) sum(matched$CH_KM2, na.rm = TRUE) else 0
        } else {
          0
        }
      }
      
      tags$div(
        h3(header_title, style = "color: #2f4858;"),
        p(sub_title, class = "client-body", style = "color: #475569; margin-bottom: 15px;"),
        
        fluidRow(
          class = "metric-container",
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBAs Total Area (TA)"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_KBA, "; font-weight: bold;"), paste0(format(round(total_kba_km2, 1), big.mark=","), " km²")))),
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Protected & OECM TA"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_PROTECTED, "; font-weight: bold;"), paste0(format(round(total_cpcad_km2, 1), big.mark=","), " km²")))),
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Critical Habitat TA"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_HABITAT, "; font-weight: bold;"), paste0(format(round(total_ch_km2, 1), big.mark=","), " km²"))))
        ),
        
        fluidRow(
          class = "metric-container",
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - Protected Areas %"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_PROTECTED, "; font-weight: bold;"), paste0(round(kba_pa_pct, 1), " %")))),
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - OECM Areas %"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_CONSERVED, "; font-weight: bold;"), paste0(round(kba_oecm_pct, 1), " %"))))
        ),
        
        fluidRow(
          class = "metric-container",
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - Endangered CH %"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_ENDANGERED, "; font-weight: bold;"), paste0(round(kba_ch_end_pct, 1), " %")))),
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - Threatened CH %"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_THREATENED, "; font-weight: bold;"), paste0(round(kba_ch_thr_pct, 1), " %"))))
        ),
        static_footnote_ui
      )
    } else {
      target_kba <- current_data$kba %>% 
        filter(as.character(kbasiteid) == selected_kba_id()) %>% 
        st_drop_geometry()
      
      req(nrow(target_kba) > 0)
      colnames(target_kba) <- tolower(colnames(target_kba))
      
      site_name <- target_kba$nationalname[1]
      site_id   <- target_kba$kbasiteid[1]
      site_jur  <- target_kba$jurisdiction_en[1]
      
      site_kba_ha  <- sum(target_kba$kba_total_area_ha, na.rm = TRUE)
      site_kba_km2 <- site_kba_ha * 0.01
      
      site_pa_ha        <- sum(target_kba$pa_area_ha, na.rm = TRUE)
      site_oecm_ha      <- sum(target_kba$oecm_area_ha, na.rm = TRUE)
      site_pa_pct       <- if (site_kba_ha > 0) min(100, (site_pa_ha / site_kba_ha) * 100) else 0
      site_oecm_pct     <- if (site_kba_ha > 0) min(100, (site_oecm_ha / site_kba_ha) * 100) else 0
      
      site_ch_end_ha    <- sum(target_kba$ch_endangered_ha, na.rm = TRUE)
      site_ch_thr_ha    <- sum(target_kba$ch_threatened_ha, na.rm = TRUE)
      site_ch_total_km2 <- (site_ch_end_ha + site_ch_thr_ha + sum(target_kba$ch_specialconcern_ha, na.rm = TRUE)) * 0.01
      
      site_ch_end_pct   <- if (site_kba_ha > 0) min(100, (site_ch_end_ha / site_kba_ha) * 100) else 0
      site_ch_thr_pct   <- if (site_kba_ha > 0) min(100, (site_ch_thr_ha / site_kba_ha) * 100) else 0
      
      tags$div(
        h3(paste0("KBA Site Overview: ", site_name, " (ID: ", site_id, ")"), style = "color: #2f4858;"),
        p(tags$strong("Jurisdiction: "), site_jur, class = "client-body", style = "color: #475569; margin-bottom: 15px;"),
        
        fluidRow(
          class = "metric-container",
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBAs Total Area (TA)"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_KBA, "; font-weight: bold;"), paste0(format(round(site_kba_km2, 1), big.mark=","), " km²")))),
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Protected & OECM TA"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_PROTECTED, "; font-weight: bold;"), paste0(format(round((site_pa_ha + site_oecm_ha) * 0.01, 1), big.mark=","), " km²")))),
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Critical Habitat TA"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_HABITAT, "; font-weight: bold;"), paste0(format(round(site_ch_total_km2, 1), big.mark=","), " km²"))))
        ),
        
        fluidRow(
          class = "metric-container",
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - Protected Areas %"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_PROTECTED, "; font-weight: bold;"), paste0(round(site_pa_pct, 1), " %")))),
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - OECM Areas %"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_CONSERVED, "; font-weight: bold;"), paste0(round(site_oecm_pct, 1), " %"))))
        ),
        
        fluidRow(
          class = "metric-container",
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - Endangered CH %"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_ENDANGERED, "; font-weight: bold;"), paste0(round(site_ch_end_pct, 1), " %")))),
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - Threatened CH %"),
                             tags$div(class = "metric-value", style = paste0("color: ", COLOR_THREATENED, "; font-weight: bold;"), paste0(round(site_ch_thr_pct, 1), " %"))))
        ),
        static_footnote_ui
      )
    }
  })
  
  # --- TABLE 0: KBA SITES INVENTORY ---
  output$kbaTable <- renderDT({
    req(current_data$kba)
    target_id <- selected_kba_id()
    
    table_data <- current_data$kba %>% st_drop_geometry()
    
    if (target_id != "All") {
      table_data <- table_data %>% filter(as.character(kbasiteid) == target_id)
    } else if (input$provinceFilter != "All") {
      table_data <- table_data %>% filter(jurisdiction_en == input$provinceFilter)
    }
    
    if (nrow(table_data) == 0) {
      return(datatable(data.frame(Message = "No KBA sites found for this selection."), options = list(dom = 't'), rownames = FALSE))
    }
    
    colnames(table_data) <- tolower(colnames(table_data))
    
    final_table <- table_data %>%
      mutate(
        `Accreditation` = case_when(
          grepl("Global|Mondial", kbalevel_en, ignore.case = TRUE)   ~ "Global",
          grepl("National", kbalevel_en, ignore.case = TRUE)        ~ "National",
          is.na(kbalevel_en) | kbalevel_en == ""                    ~ "Not Specified",
          TRUE                                                      ~ as.character(kbalevel_en)
        ),
        `Total Area (km2)`                      = round(kba_total_area_ha * 0.01, 1),
        `Protected Area (km2)`                  = round(pa_area_ha * 0.01, 1),
        `Protection %`                          = round(pa_proportion * 100, 1),
        `Conserved Area (km2)`                  = round(oecm_area_ha * 0.01, 1),
        `Conserved %`                          = round(oecm_proportion * 100, 1),
        `Critical Habitat - Endangered (km2)`   = round(ch_endangered_ha * 0.01, 1),
        `Critical Habitat - Endangered %`       = round(ch_endangered_proportion * 100, 1),
        `Critical Habitat - Threatened (km2)`   = round(ch_threatened_ha * 0.01, 1),
        `Critical Habitat - Threatened %`       = round(ch_threatened_proportion * 100, 1)
      ) %>%
      select(
        `Site ID`                              = kbasiteid,
        `Site Name`                            = nationalname,
        `Jurisdiction`                         = jurisdiction_en,
        `Accreditation`,
        `Total Area (km2)`,
        `Protected Area (km2)`,
        `Protection %`,
        `Conserved Area (km2)`,
        `Conserved %`,
        `Critical Habitat - Endangered (km2)`,
        `Critical Habitat - Endangered %`,
        `Critical Habitat - Threatened (km2)`,
        `Critical Habitat - Threatened %`
      )
    
    datatable(final_table, options = list(pageLength = 15, scrollX = TRUE, dom = 'tp'), rownames = FALSE)
  })
  
  # --- TABLE 1: CPCAD OVERLAPS INVENTORY ---
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
      return(datatable(data.frame(Message = "No overlapping PA or OECM sites found."), options = list(dom = 't'), rownames = FALSE))
    }
    
    table_data <- table_sf %>%
      mutate(
        OVERLAP_HA  = as.numeric(st_area(.)) / 10000,
        OVERLAP_KM2 = round(OVERLAP_HA * 0.01, 2)
      ) %>%
      st_drop_geometry()
    
    colnames(table_data) <- tolower(colnames(table_data))
    
    get_col <- function(df, candidates) {
      found <- candidates[candidates %in% colnames(df)]
      if (length(found) > 0) df[[found[1]]] else rep(NA, nrow(df))
    }
    
    table_data <- table_data %>%
      mutate(
        kba_ha         = suppressWarnings(as.numeric(get_col(., c("kba_total_area_ha")))),
        overlap_ha_val = suppressWarnings(as.numeric(get_col(., c("overlap_ha")))),
        overlap_ha_val = ifelse(is.na(overlap_ha_val), 0, overlap_ha_val),
        raw_pct        = ifelse(!is.na(kba_ha) & kba_ha > 0, round((overlap_ha_val / kba_ha) * 100, 1), 0),
        coverage_pct   = pmin(100.0, raw_pct),
        
        raw_paoecm_val = get_col(., c("pa_oecm_df", "paoecm_df", "pa_oecm")),
        level_of_protection = case_when(
          as.character(raw_paoecm_val) %in% c("1") ~ "Protected Area",
          as.character(raw_paoecm_val) %in% c("2") ~ "Other Effective Area-Based Conservation Measure (OECM)",
          as.character(raw_paoecm_val) %in% c("3") ~ "Interim - Protected Area",
          as.character(raw_paoecm_val) %in% c("4") ~ "Interim - OECM",
          as.character(raw_paoecm_val) %in% c("5") ~ "Not applicable",
          is.na(raw_paoecm_val) | as.character(raw_paoecm_val) == "" ~ "Not Reported",
          TRUE                                      ~ as.character(raw_paoecm_val)
        ),
        
        raw_iucn_val  = get_col(., c("iucn_cat", "iucn")),
        iucn_label = case_when(
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
      )
    
    final_table <- table_data %>%
      select(
        `Protected Area Name` = any_of(c("name_e", "name", "pa_name_e")),
        `Site Type`           = any_of(c("type_e", "pa_type", "type")),
        `Overlap (km²)`       = overlap_km2,
        `Coverage %`          = coverage_pct,
        `Level of Protection` = level_of_protection,
        `IUCN Category`       = iucn_label,
        `Managing Authority`  = any_of(c("mgmt_e", "mgmt", "parent_blk"))
      )
    
    datatable(final_table, options = list(pageLength = 15, scrollX = TRUE, dom = 'tp'), rownames = FALSE)
  })
  
  # --- TABLE 2: CRITICAL HABITAT INVENTORY ---
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
      table_data <- st_drop_geometry(table_data)
    } else {
      calc_overlap_km2 <- rep(NA_real_, nrow(table_data))
    }
    
    site_area_km2 <- suppressWarnings(as.numeric(table_data$areakm2))
    
    if (all(is.na(site_area_km2)) && "KBA_TOTAL_AREA_HA" %in% names(table_data)) {
      site_area_km2 <- suppressWarnings(as.numeric(table_data$KBA_TOTAL_AREA_HA)) / 100
    }
    
    calc_coverage_pct <- ifelse(
      !is.na(site_area_km2) & site_area_km2 > 0 & !is.na(calc_overlap_km2),
      round((calc_overlap_km2 / site_area_km2) * 100, 1),
      NA_real_
    )
    calc_coverage_pct <- pmin(100.0, calc_coverage_pct)
    
    table_data <- table_data %>%
      mutate(
        sara_status_label = case_when(
          as.character(SARA_Status) %in% c("1", "Extirpated")      ~ "Extirpated",
          as.character(SARA_Status) %in% c("2", "Endangered")      ~ "Endangered",
          as.character(SARA_Status) %in% c("3", "Threatened")      ~ "Threatened",
          as.character(SARA_Status) %in% c("4", "Special Concern")  ~ "Special Concern",
          TRUE                                                     ~ as.character(SARA_Status)
        ),
        
        sara_agency_label = case_when(
          as.character(SARA_Agency) %in% c("1", "ECCC") ~ "Environment and Climate Change Canada",
          as.character(SARA_Agency) %in% c("2", "DFO")  ~ "Fisheries and Oceans Canada",
          as.character(SARA_Agency) %in% c("3", "PCA")  ~ "Parks Canada Agency",
          TRUE                                          ~ as.character(SARA_Agency)
        ),
        
        rd_status_label = case_when(
          as.character(RD_Status) %in% c("1", "Final")    ~ "Final",
          as.character(RD_Status) %in% c("2", "Proposed") ~ "Proposed",
          as.character(RD_Status) %in% c("3", "Draft")    ~ "Draft",
          TRUE                                            ~ as.character(RD_Status)
        ),
        
        taxon_label = case_when(
          as.character(Taxon) %in% c("1", "Amphibians", "AM", "Amphibien")           ~ "Amphibians",
          as.character(Taxon) %in% c("2", "Birds", "BI", "AV", "Oiseau")             ~ "Birds",
          as.character(Taxon) %in% c("3", "Fishes", "FI", "Poisson")                 ~ "Fishes (freshwater)",
          as.character(Taxon) %in% c("4", "Invertebrates", "IN", "Invertébré")        ~ "Invertebrates",
          as.character(Taxon) %in% c("5", "Lichens", "LI")                            ~ "Lichens",
          as.character(Taxon) %in% c("6", "Mammals", "MA", "Mammifère")              ~ "Mammals",
          as.character(Taxon) %in% c("7", "Mosses", "MO", "Mousse")                   ~ "Mosses",
          as.character(Taxon) %in% c("8", "Reptiles", "RE", "Reptile")               ~ "Reptiles",
          as.character(Taxon) %in% c("9", "Vascular Plants", "VP", "PL", "Plante")   ~ "Vascular Plants",
          as.character(Taxon) %in% c("10", "Non-vascular Plants", "NV")              ~ "Non-vascular Plants",
          as.character(Taxon) %in% c("11", "Molluscs", "MOLL", "Arthropods")         ~ "Molluscs",
          as.character(Taxon) %in% c("12", "Fungi", "FU", "Champignons")             ~ "Fungi",
          as.character(Taxon) %in% c("13", "Corals", "Sponges", "CO")                ~ "Corals / Sponges",
          is.na(Taxon) | as.character(Taxon) %in% c("", "0", "99")                    ~ "Not Specified",
          TRUE                                                                       ~ as.character(Taxon)
        )
      )
    
    final_table <- table_data %>%
      mutate(
        `Overlap (km2)` = calc_overlap_km2,
        `Coverage (%)`  = calc_coverage_pct
      ) %>%
      select(
        `Site Name`               = SiteName_E,
        `Overlap (km2)`,
        `Coverage (%)`,
        `Species Common Name`     = CommName_E,
        `Species Scientific Name` = SciName,
        `Taxon`                   = taxon_label,
        `COSEWIC ID`              = COSEWIC_ID,
        `SARA Status`             = sara_status_label,
        `Province`                = ProvTerr_E,
        `Sensitive?`              = Sensitive_E,
        `Recovery Agency`         = sara_agency_label,
        `Recovery Doc`            = RDoc_Name_E,
        `Doc Status`              = rd_status_label
      )
    
    datatable(final_table, options = list(pageLength = 10, scrollX = TRUE, dom = 'tp'), rownames = FALSE)
  })
}

# 5. --- LAUNCH APP ---
shinyApp(ui, server)