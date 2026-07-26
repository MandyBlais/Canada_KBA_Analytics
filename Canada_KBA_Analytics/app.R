# PART 2 - SHINY APP DASHBOARD INTERFACE

library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(gfonts)
library(fresh)

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
  header = dashboardHeader(title = "Canada KBA Protection Dashboard", titleWidth = 320),
  
  sidebar = dashboardSidebar(
    width = 320,
    sidebarMenu(
      tags$div(
        style = "padding: 15px; color: #fff;",
        h4("Spatial Filter Configurations", class = "client-subhead", style = "margin-top: 0;"),
        p("Isolate regions using the dropdowns or click directly on a map boundary polygon.", class = "client-body", style = "color: #b8c7ce; font-size: 12px;"),
        hr(style = "border-color: #92bf00; border-width: 2px;"),
        
        # Geographic Hierarchy Filters
        selectInput(
          "provinceFilter", "Province / Territory:", 
          choices = get_province_choices(data_payload$kba_layer)
        ),
        
        selectInput(
          "kbaFilter", "Select Specific KBA Site:", 
          choices = c("All", if (!is.null(data_payload$kba_layer)) sort(unique(paste0(data_payload$kba_layer$kbasiteid, " - ", data_payload$kba_layer$nationalname))) else "All")
        ),
        
        hr(style = "border-color: #92bf00; border-width: 2px;"),
        
        # Layer Visibility Controls
        h5("Map Layer Toggles", class = "client-subhead", style = "color: #fff;"),
        checkboxInput("showKBA", "Show KBAs (Green)", value = TRUE),
        checkboxInput("showCPCAD", "Show CPCAD Overlaps (Blue)", value = FALSE),
        checkboxInput("showCH", "Show Critical Habitat (Yellow)", value = FALSE),
        
        hr(style = "border-color: #92bf00; border-width: 2px;"),
        # Operational Utilities
        actionButton(
          "runSync", "Force API Data Refresh", 
          style = "width: 100%; font-weight: bold; background-color: #ffcb00; color: #3a3426; border-color: #ffcb00;"
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
        h1, h2, h3, .logo, .main-header .logo { 
          font-family: 'rift', 'Impact', sans-serif !important; 
          font-weight: 700 !important; 
          text-transform: uppercase;
        }
        
        .client-subhead, h4, h5, label, .box-title { 
          font-family: 'Open Sans', sans-serif !important; 
          font-weight: 700 !important; 
        }
        
        .client-body, p, span, li, td, th, input, select, .control-label { 
          font-family: 'Open Sans', sans-serif !important; 
          font-weight: 300 !important; 
        }
        
        .main-header .navbar { background-color: #0AA1F4 !important; }
        .main-header .logo { background-color: #2f4858 !important; color: #92bf00 !important; }
        .content-wrapper { background-color: #3a3426 !important; }
        #right-panel { background: #ffffff; border-left: 4px solid #92bf00; padding: 20px; height: calc(100vh - 80px); overflow-y: auto; }
        
        .metric-box { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 6px; padding: 10px; margin-bottom: 10px; }
        .metric-title { font-size: 10px; text-transform: uppercase; color: #2f4858; font-weight: 700; }
        .metric-value { font-size: 16px; color: #0AA1F4; font-weight: bold; }
        .metric-value-ch { font-size: 16px; color: #d97706; font-weight: bold; }
        
        .well-unprotected { background-color: #fff1f2; border-left: 5px solid #ef4444; color: #3a3426; padding: 12px; border-radius: 6px; }
        .well-protected { background-color: #f0fdf4; border-left: 5px solid #92bf00; color: #2f4858; padding: 12px; border-radius: 6px; }
        
        .header-footnote { font-size: 11px; color: #64748b; font-style: italic; line-height: 1.4; margin-top: 8px; margin-bottom: 5px; }
      "))
    ),
    
    fluidRow(
      # Map Viewport Layout Column
      column(
        width = 6,
        box(
          title = "National Conservation Baseline Map", width = NULL, solidHeader = TRUE, status = "primary",
          leafletOutput("mapElement", height = "780px")
        )
      ),
      
      # Dynamic Side Attributes Column
      column(
        width = 6, id = "right-panel",
        h3("Biodiversity & Conservation Attributes", style = "margin-top: 0; color: #2f4858;"),
        p("Detailed site telemetry, legal protection, and species risk attributes.", class = "client-body", style = "color: #3a3426; margin-bottom: 15px;"),
        hr(style = "border-color: #92bf00;"),
        
        uiOutput("kbaSelectionHeader"),
        br(),
        
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "KBA Sites Inventory",
            br(),
            p("Designated Key Biodiversity Areas within current spatial selection.", class = "client-body", style = "color: #3a3426; font-size: 12px;"),
            DTOutput("kbaTable")
          ),
          tabPanel(
            "CPCAD Protected Areas",
            br(),
            p("Individual contributing protected and conserved area shapes.", class = "client-body", style = "color: #3a3426; font-size: 12px;"),
            DTOutput("overlapTable")
          ),
          tabPanel(
            "Critical Habitat (CH)",
            br(),
            p("Species at Risk Critical Habitat overlapping this site.", class = "client-body", style = "color: #3a3426; font-size: 12px;"),
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
    if (is.null(input$kbaFilter) || input$kbaFilter == "All") return("All")
    sub(" - .*", "", input$kbaFilter)
  })
  
  # Static footnote block definition
  static_footnote_ui <- tags$p(
    class = "header-footnote",
    "CPCAD totals reflect terrestrial and inland protected areas per province/territory; offshore marine protected areas are categorized under Federal / Offshore jurisdiction. Critical Habitat and KBA totals include adjacent coastal and marine areas."
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
      kba_choices <- c("All", sort(unique(paste0(current_data$kba$kbasiteid, " - ", current_data$kba$nationalname))))
      updateSelectInput(session, "kbaFilter", choices = kba_choices)
      
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
    
    kba_choices <- c("All", sort(unique(paste0(subset_kbas$kbasiteid, " - ", subset_kbas$nationalname))))
    
    current_sel <- input$kbaFilter
    if (current_sel %in% kba_choices) {
      updateSelectInput(session, "kbaFilter", choices = kba_choices, selected = current_sel)
    } else {
      updateSelectInput(session, "kbaFilter", choices = kba_choices, selected = "All")
    }
  }, ignoreInit = TRUE)
  
  # --- LEAFLET MAP INITIALIZATION ---
  output$mapElement <- renderLeaflet({
    req(current_data$kba)
    
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -96.8, lat = 62.4, zoom = 4) %>%
      addLegend(
        position = "bottomright",
        colors = c("#92bf00", "#0AA1F4", "#FFCB00"),
        labels = c(
          "Key Biodiversity Area (Green)", 
          "CPCAD Protected Area (Blue)",
          "Critical Habitat (Yellow)"
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
  
  # --- OBSERVER 1: MAP LAYERS (FULL CPCAD + CH DISPLAY) ---
  observe({
    kba_raw <- filtered_kba()
    
    # 1. Guard against NULL or empty layer data
    req(kba_raw)
    if (is.null(kba_raw) || nrow(kba_raw) == 0) return()
    
    proxy <- leafletProxy("mapElement") %>% clearShapes()
    
    # 2. Filter geometries safely
    kba_data <- kba_raw %>% filter(as.character(st_geometry_type(.)) %in% c("POLYGON", "MULTIPOLYGON"))
    
    # Filter CPCAD layer by province selection
    cpcad_data <- current_data$cpcad
    if (!is.null(cpcad_data) && nrow(cpcad_data) > 0 && input$provinceFilter != "All") {
      cpcad_cols <- tolower(colnames(cpcad_data))
      loc_idx <- match(TRUE, cpcad_cols %in% c("loc", "loc_e", "jur_id", "province_e", "jurisdiction_en", "jurisdiction_es"))
      if (!is.na(loc_idx)) {
        col_name <- colnames(cpcad_data)[loc_idx]
        cpcad_data <- cpcad_data[grepl(input$provinceFilter, cpcad_data[[col_name]], ignore.case = TRUE), ]
      }
    }
    
    # Filter CH layer by province selection
    ch_data <- current_data$ch
    if (!is.null(ch_data) && nrow(ch_data) > 0 && input$provinceFilter != "All") {
      ch_cols <- tolower(colnames(ch_data))
      prov_idx <- match(TRUE, ch_cols %in% c("provterr_e", "provterr", "jurisdiction_en", "jurisdiction_es", "province_e"))
      if (!is.na(prov_idx)) {
        col_name <- colnames(ch_data)[prov_idx]
        ch_data <- ch_data[grepl(input$provinceFilter, ch_data[[col_name]], ignore.case = TRUE), ]
      }
    }
    
    # Render CPCAD
    if (input$showCPCAD && !is.null(cpcad_data) && nrow(cpcad_data) > 0) {
      cpcad_poly <- cpcad_data %>% filter(as.character(st_geometry_type(.)) %in% c("POLYGON", "MULTIPOLYGON"))
      if (nrow(cpcad_poly) > 0) {
        cpcad_names  <- colnames(cpcad_poly)
        name_field   <- cpcad_names[tolower(cpcad_names) %in% c("name_e", "name", "pa_name_e")][1]
        cpcad_labels <- if (!is.na(name_field)) cpcad_poly[[name_field]] else "CPCAD Site"
        
        proxy %>% addPolygons(
          data = cpcad_poly,
          color = "#0AA1F4",
          weight = 1.0,
          fillColor = "#0AA1F4",
          fillOpacity = 0.40,
          label = paste("CPCAD:", cpcad_labels),
          highlightOptions = highlightOptions(weight = 2, color = "#ffffff", fillOpacity = 0.65)
        )
      }
    }
    
    # Render Critical Habitat
    if (input$showCH && !is.null(ch_data) && nrow(ch_data) > 0) {
      ch_poly <- ch_data %>% filter(as.character(st_geometry_type(.)) %in% c("POLYGON", "MULTIPOLYGON"))
      if (nrow(ch_poly) > 0) {
        ch_names   <- colnames(ch_poly)
        comm_field <- ch_names[tolower(ch_names) %in% c("commname_e", "commname", "sitename_e")][1]
        ch_labels  <- if (!is.na(comm_field)) ch_poly[[comm_field]] else "Critical Habitat"
        
        proxy %>% addPolygons(
          data = ch_poly,
          color = "#d97706",
          weight = 1.0,
          fillColor = "#FFCB00",
          fillOpacity = 0.45,
          label = paste("CH:", ch_labels),
          highlightOptions = highlightOptions(weight = 2, color = "#ffffff", fillOpacity = 0.70)
        )
      }
    }
    
    # Render KBAs
    if (input$showKBA && nrow(kba_data) > 0) {
      proxy %>% addPolygons(
        data = kba_data,
        color = "#2f4858",
        weight = 1.5,
        fillOpacity = 0.50,
        fillColor = "#92bf00",
        layerId = ~kbasiteid,
        label = ~paste("KBA:", kbasiteid, "-", nationalname),
        highlightOptions = highlightOptions(weight = 2.5, color = "#ffffff", fillOpacity = 0.75)
      )
    }
    
    # RECENTER LOGIC: Checks if 'All' or 'Federal / Offshore' is selected
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
        filter(kbasiteid == selected_kba_id()) %>% 
        filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
      
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
  
  # --- HEADER METRICS UI (OPTIMIZED WITH PRE-COMPUTED DISSOLVED METRICS) ---
  output$kbaSelectionHeader <- renderUI({
    # Ensure base spatial layer exists before rendering header
    req(current_data$kba)
    
    if (selected_kba_id() == "All") {
      req(filtered_kba())
      summary_df <- filtered_kba() %>% st_drop_geometry()
      if (nrow(summary_df) == 0) return(tags$p("No data available for selected criteria."))
      
      colnames(summary_df) <- tolower(colnames(summary_df))
      
      header_title <- if (input$provinceFilter == "All") "Canada National Overview" else paste0(input$provinceFilter, " Overview")
      sub_title    <- if (input$provinceFilter == "All") "Aggregated nationwide conservation statistics." else paste0("Aggregated metrics for ", input$provinceFilter, ".")
      
      total_kba_ha  <- sum(summary_df$kba_total_area_ha, na.rm = TRUE)
      total_kba_km2 <- total_kba_ha * 0.01
      
      prot_kba_ha   <- sum(summary_df$protected_area_ha, na.rm = TRUE)
      kba_cpcad_pct <- if (total_kba_ha > 0) min(100, (prot_kba_ha / total_kba_ha) * 100) else 0
      
      ch_kba_ha     <- sum(summary_df$critical_habitat_ha, na.rm = TRUE)
      kba_ch_pct    <- if (total_kba_ha > 0) min(100, (ch_kba_ha / total_kba_ha) * 100) else 0
      
      # Fast Lookup for Pre-Calculated CPCAD Union Area
      total_cpcad_km2 <- if (is.null(input$provinceFilter) || input$provinceFilter == "All") {
        current_data$national_cpcad_km2 %||% 0
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
      
      # Fast Lookup for Pre-Calculated Critical Habitat Union Area
      total_ch_km2 <- if (is.null(input$provinceFilter) || input$provinceFilter == "All") {
        current_data$national_ch_km2 %||% 0
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
        h4(header_title, style = "font-weight: bold; color: #0f172a; margin-top: 5px;"),
        p(sub_title),
        br(),
        fluidRow(
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Total KBA Area"),
                             tags$div(class = "metric-value", paste0(format(round(total_kba_km2, 1), big.mark=","), " km²")))),
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - CPCAD Protection"),
                             tags$div(class = "metric-value", paste0(round(kba_cpcad_pct, 1), " %")))),
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Total CPCAD Area"),
                             tags$div(class = "metric-value", paste0(format(total_cpcad_km2, big.mark=","), " km²"))))
        ),
        br(),
        fluidRow(
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - Critical Habitat"),
                             tags$div(class = "metric-value-ch", paste0(round(kba_ch_pct, 1), " %")))),
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Total Critical Habitat Area"),
                             tags$div(class = "metric-value-ch", paste0(format(total_ch_km2, big.mark=","), " km²"))))
        ),
        static_footnote_ui
      )
    } else {
      # Safe check before filtering
      target_kba <- current_data$kba %>% 
        filter(as.character(kbasiteid) == as.character(selected_kba_id())) %>% 
        st_drop_geometry()
      
      req(nrow(target_kba) > 0)
      colnames(target_kba) <- tolower(colnames(target_kba))
      
      kba_km2  <- round(target_kba$kba_total_area_ha * 0.01, 2)
      prot_pct <- min(100, round(target_kba$cumulative_proportion * 100, 1))
      ch_pct   <- min(100, round(target_kba$critical_habitat_proportion * 100, 1))
      
      tags$div(
        h4(target_kba$nationalname, style = "font-weight: bold; color: #0f172a; margin-top: 5px;"),
        p(tags$strong("Jurisdiction: "), target_kba$jurisdiction_en, " | ", tags$strong("Site ID: "), target_kba$kbasiteid),
        br(),
        fluidRow(
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Total KBA Area"),
                             tags$div(class = "metric-value", paste0(format(kba_km2, big.mark=","), " km²")))),
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - CPCAD Protection"),
                             tags$div(class = "metric-value", paste0(prot_pct, " %")))),
          column(4, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "KBA - Critical Habitat"),
                             tags$div(class = "metric-value-ch", paste0(ch_pct, " %"))))
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
      table_data <- table_data %>% filter(as.character(kbasiteid) == as.character(target_id))
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
        `Total Area (km²)`     = round(kba_total_area_ha * 0.01, 1),
        `Protected Area (km²)` = round(protected_area_ha * 0.01, 1),
        `Protection %`         = round(cumulative_proportion * 100, 1),
        `Critical Habitat %`   = round(critical_habitat_proportion * 100, 1)
      ) %>%
      select(
        `Site ID`              = kbasiteid,
        `Site Name`            = nationalname,
        `Jurisdiction`         = jurisdiction_en,
        `Accreditation`,
        `Total Area (km²)`,
        `Protected Area (km²)`,
        `Protection %`,
        `Critical Habitat %`
      )
    
    datatable(final_table, options = list(pageLength = 15, scrollX = TRUE, dom = 'tp'), rownames = FALSE)
  })
  
  # --- TABLE 1: CPCAD OVERLAPS INVENTORY ---
  output$overlapTable <- renderDT({
    req(current_data$cpcad_overlaps)
    target_id <- selected_kba_id()
    
    table_sf <- current_data$cpcad_overlaps
    
    if (target_id != "All") {
      table_sf <- table_sf %>% filter(as.character(kbasiteid) == as.character(target_id))
    } else if (input$provinceFilter != "All") {
      req(filtered_kba())
      prov_kba_ids <- unique(as.character(filtered_kba()$kbasiteid))
      table_sf <- table_sf %>% filter(as.character(kbasiteid) %in% prov_kba_ids)
    }
    
    if (nrow(table_sf) == 0) {
      return(datatable(data.frame(Message = "No overlapping CPCAD sites found."), options = list(dom = 't'), rownames = FALSE))
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
        kba_ha        = if ("kba_total_area_ha" %in% names(.)) as.numeric(kba_total_area_ha) else NA,
        raw_pct       = if_else(!is.na(kba_ha) & kba_ha > 0, round((overlap_ha / kba_ha) * 100, 1), 0),
        coverage_pct  = pmin(100.0, raw_pct),
        
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
        `IUCN Category`       = iucn_label,
        `Managing Authority`  = any_of(c("mgmt_e", "mgmt", "parent_blk"))
      )
    
    datatable(final_table, options = list(pageLength = 15, scrollX = TRUE, dom = 'tp'), rownames = FALSE)
  })
  
  # --- TABLE 2: CRITICAL HABITAT INVENTORY ---
  output$chTable <- renderDT({
    req(current_data$ch_kba_overlaps)
    target_id <- selected_kba_id()
    
    table_data <- current_data$ch_kba_overlaps %>% st_drop_geometry()
    
    if (target_id != "All") {
      table_data <- table_data %>% filter(as.character(kbasiteid) == as.character(target_id))
    } else if (input$provinceFilter != "All") {
      req(filtered_kba())
      prov_kba_ids <- unique(as.character(filtered_kba()$kbasiteid))
      table_data <- table_data %>% filter(as.character(kbasiteid) %in% prov_kba_ids)
    }
    
    if (nrow(table_data) == 0) {
      return(datatable(data.frame(Message = "No Critical Habitat boundaries found for this selection."), options = list(dom = 't'), rownames = FALSE))
    }
    
    colnames(table_data) <- make.unique(tolower(colnames(table_data)), sep = "_dup")
    
    get_col <- function(df, candidates) {
      found <- candidates[candidates %in% colnames(df)]
      if (length(found) > 0) df[[found[1]]] else rep(NA, nrow(df))
    }
    
    table_data <- table_data %>%
      mutate(
        raw_sara_status = as.character(get_col(., c("sara_status", "sara_status_1"))),
        raw_sara_agency = as.character(get_col(., c("sara_agency", "sara_agency_1"))),
        raw_rd_status   = as.character(get_col(., c("rd_status", "rd_status_1"))),
        raw_taxon_val   = get_col(., c("taxon", "taxon_1", "taxonomic_group_e")),
        
        sara_status_label = case_when(
          raw_sara_status %in% c("1", "Endangered")      ~ "Endangered",
          raw_sara_status %in% c("2", "Threatened")      ~ "Threatened",
          raw_sara_status %in% c("3", "Special Concern")  ~ "Special Concern",
          raw_sara_status %in% c("4", "Extirpated")       ~ "Extirpated",
          TRUE                                           ~ raw_sara_status
        ),
        
        sara_agency_label = case_when(
          raw_sara_agency %in% c("1", "ECCC") ~ "Environment and Climate Change Canada",
          raw_sara_agency %in% c("2", "DFO")  ~ "Fisheries and Oceans Canada",
          raw_sara_agency %in% c("3", "PCA")  ~ "Parks Canada Agency",
          TRUE                                ~ raw_sara_agency
        ),
        
        rd_status_label = case_when(
          raw_rd_status %in% c("1", "Final")    ~ "Final",
          raw_rd_status %in% c("2", "Proposed") ~ "Proposed",
          raw_rd_status %in% c("3", "Draft")    ~ "Draft",
          TRUE                                  ~ raw_rd_status
        ),
        
        taxon_label = case_when(
          as.character(raw_taxon_val) %in% c("1", "Amphibians", "AM", "Amphibien")           ~ "Amphibians",
          as.character(raw_taxon_val) %in% c("2", "Birds", "BI", "AV", "Oiseau")             ~ "Birds",
          as.character(raw_taxon_val) %in% c("3", "Fishes", "FI", "Poisson")                 ~ "Fishes (freshwater)",
          as.character(raw_taxon_val) %in% c("4", "Invertebrates", "IN", "Invertébré")        ~ "Invertebrates",
          as.character(raw_taxon_val) %in% c("5", "Lichens", "LI")                            ~ "Lichens",
          as.character(raw_taxon_val) %in% c("6", "Mammals", "MA", "Mammifère")              ~ "Mammals",
          as.character(raw_taxon_val) %in% c("7", "Mosses", "MO", "Mousse")                   ~ "Mosses",
          as.character(raw_taxon_val) %in% c("8", "Reptiles", "RE", "Reptile")               ~ "Reptiles",
          as.character(raw_taxon_val) %in% c("9", "Vascular Plants", "VP", "PL", "Plante")   ~ "Vascular Plants",
          as.character(raw_taxon_val) %in% c("10", "Non-vascular Plants", "NV")              ~ "Non-vascular Plants",
          as.character(raw_taxon_val) %in% c("11", "Molluscs", "MOLL", "Arthropods")         ~ "Molluscs",
          as.character(raw_taxon_val) %in% c("12", "Fungi", "FU", "Champignons")             ~ "Fungi",
          as.character(raw_taxon_val) %in% c("13", "Corals", "Sponges", "CO")                ~ "Corals / Sponges",
          is.na(raw_taxon_val) | as.character(raw_taxon_val) %in% c("", "0", "99")            ~ "Not Specified",
          TRUE                                                                                ~ as.character(raw_taxon_val)
        )
      )
    
    final_table <- table_data %>%
      mutate(
        site_name_col  = get_col(., c("sitename_e", "sitename", "site_name_e")),
        comm_name_col  = get_col(., c("commname_e", "commname")),
        sci_name_col   = get_col(., c("sciname", "scientific_name")),
        cosewic_col    = get_col(., c("cosewic_id", "cosewicid")),
        prov_col       = get_col(., c("provterr_e", "provterr")),
        sensitive_col  = get_col(., c("sensitive_e", "sensitive")),
        rdoc_name_col  = get_col(., c("rdoc_name_e", "rdoc_name"))
      ) %>%
      select(
        `Site Name`       = site_name_col,
        `Common Name`     = comm_name_col,
        `Scientific Name` = sci_name_col,
        `Taxon`           = taxon_label,
        `COSEWIC ID`      = cosewic_col,
        `SARA Status`     = sara_status_label,
        `Province`        = prov_col,
        `Sensitive?`      = sensitive_col,
        `Recovery Agency` = sara_agency_label,
        `Recovery Doc`    = rdoc_name_col,
        `Doc Status`      = rd_status_label
      )
    
    datatable(final_table, options = list(pageLength = 10, scrollX = TRUE, dom = 'tp'), rownames = FALSE)
  })
}

# 5. --- LAUNCH APP ---
shinyApp(ui, server)