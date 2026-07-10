# PART 2 - SHINY APP DASHBOARD INTERFACE

library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(DT)

CACHE_FILE <- "data/cached_compiled_data.rds"

# 1. Gracefully load cache on initialization
if (file.exists(CACHE_FILE)) {
  data_payload <- readRDS(CACHE_FILE)
} else {
  # Safe fallback structure if the app is booted without a cache file
  data_payload <- list(
    kba_layer = NULL, 
    cpcad_layer = NULL, 
    overlaps = NULL, 
    timestamp = "No Local Cache Binary Found"
  )
}

# --- USER INTERFACE DESIGN ---
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Canada KBA Protection Dashboard", titleWidth = 320),
  
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      tags$div(style = "padding: 15px; color: #fff;",
               h4("Spatial Filter Configurations", style = "margin-top: 0; font-weight: bold;"),
               p("Isolate regions using the dropdowns or click directly on a map boundary polygon.", style = "color: #b8c7ce; font-size: 12px;"),
               hr(style = "border-color: #4f595f;"),
               
               # Geographic Hierarchy Filters
               selectInput("provinceFilter", "Province / Territory:", 
                           choices = c("All", sort(unique(data_payload$kba_layer$jurisdiction_en)))),
               
               selectInput("kbaFilter", "Select Specific KBA Site:", 
                           choices = c("All", sort(unique(paste0(data_payload$kba_layer$kbasiteid, " - ", data_payload$kba_layer$nationalname))))),
               
               hr(style = "border-color: #4f595f;"),
               # Operational Utilities
               actionButton("runSync", "Force API Data Refresh", class = "btn-warning", style = "width: 100%; font-weight: bold;"),
               br(), br(),
               tags$small(textOutput("cacheTimeText"), style = "color: #9ca8b0;") # Fixed here
      )
    )
  ),
  
  dashboardBody(
    # Custom CSS injectors to ensure readable layouts and custom scroll behaviors
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #ffffff !important; }
      #right-panel { background: #fdfdfd; border-left: 1px solid #e2e8f0; padding: 20px; height: calc(100vh - 80px); overflow-y: auto; }
      .metric-box { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 6px; padding: 12px; margin-bottom: 10px; }
      .metric-title { font-size: 11px; text-transform: uppercase; color: #64748b; font-weight: bold; }
      .metric-value { font-size: 18px; color: #0f172a; font-weight: bold; }
      .well-unprotected { background-color: #fff1f2; border: 1px solid #fecdd3; color: #9f1239; padding: 15px; border-radius: 6px; }
      .well-protected { background-color: #f0fdf4; border: 1px solid #bbf7d0; color: #166534; padding: 15px; border-radius: 6px; }
    "))),
    
    fluidRow(
      # Map Viewport Layout Column
      column(width = 7,
             box(title = "National Conservation Baseline Map", width = NULL, solidHeader = TRUE, status = "success",
                 leafletOutput("mapElement", height = "760px")
             )
      ),
      
      # Dynamic Side Attributes Column
      column(width = 5, id = "right-panel",
             h3("Biodiversity & Conservation Attributes", style = "margin-top: 0; font-weight: bold; color: #1e293b;"),
             p("Detailed site telemetry and overlapping legal designations.", style = "color: #64748b; margin-bottom: 20px;"),
             hr(),
             
             # Selected KBA Core Metadata Block
             uiOutput("kbaSelectionHeader"),
             br(),
             
             # Tabular breakdown of intersecting CPCAD elements
             h4("Overlapping CPCAD Sites Inventory", style = "font-weight: bold; color: #334155;"),
             p("Individual contributing federal, provincial, and territorial conservation shapes.", style = "color: #64748b; font-size: 12px;"),
             DTOutput("overlapTable")
      )
    )
  )
)

# --- SERVER COMPUTATION LOGIC ---
server <- function(input, output, session) {
  
  # Reactive memory container tracking spatial assets
  current_data <- reactiveValues(
    kba           = data_payload$kba_layer,
    cpcad         = data_payload$cpcad_layer,
    intersections = data_payload$overlaps,
    time          = data_payload$timestamp
  )
  
  # Extract true alpha ID string out of the composite dropdown text selection
  selected_kba_id <- reactive({
    if (is.null(input$kbaFilter) || input$kbaFilter == "All") return("All")
    sub(" - .*", "", input$kbaFilter)
  })
  
  # Reactive Trigger: In-app execution button to pipeline data engine source
  observeEvent(input$runSync, {
    showModal(modalDialog("Querying APIs & Re-Calculating Spatial Intersections... Please wait.", footer = NULL))
    tryCatch({
      source("data_engine.R")
      refresh_spatial_cache()
      
      new_payload <- readRDS(CACHE_FILE)
      current_data$kba           <- new_payload$kba_layer
      current_data$cpcad         <- new_payload$cpcad_layer
      current_data$intersections <- new_payload$overlaps
      current_data$time          <- new_payload$timestamp
      
      # Refresh UI selection indices based on newly retrieved cache schema
      updateSelectInput(session, "provinceFilter", choices = c("All", sort(unique(current_data$kba$jurisdiction_en))))
      kba_choices <- c("All", sort(unique(paste0(current_data$kba$kbasiteid, " - ", current_data$kba$nationalname))))
      updateSelectInput(session, "kbaFilter", choices = kba_choices)
      
    }, error = function(e) {
      showNotification(paste("Sync Processing Fault:", e$message), type = "error", duration = 10)
    })
    removeModal()
  })
  
  output$cacheTimeText <- renderText({ paste("Data Timestamp:", current_data$time) })
  
  # Master Filter Logic: Modifies baseline visibility parameters without dropping empty shapes
  filtered_kba <- reactive({
    df <- current_data$kba
    if (is.null(df)) return(NULL)
    if (input$provinceFilter != "All") { 
      df <- df %>% filter(jurisdiction_en == input$provinceFilter) 
    }
    df
  })
  
  # Sync dynamic dropdown cascade filters: Changes available sites when changing provinces
  observeEvent(input$provinceFilter, {
    req(current_data$kba)
    if (input$provinceFilter == "All") {
      subset_kbas <- current_data$kba
    } else {
      subset_kbas <- current_data$kba %>% filter(jurisdiction_en == input$provinceFilter)
    }
    kba_choices <- c("All", sort(unique(paste0(subset_kbas$kbasiteid, " - ", subset_kbas$nationalname))))
    
    # Preserve active selection if it remains valid under the new scope filter
    current_sel <- input$kbaFilter
    if (current_sel %in% kba_choices) {
      updateSelectInput(session, "kbaFilter", choices = kba_choices, selected = current_sel)
    } else {
      updateSelectInput(session, "kbaFilter", choices = kba_choices, selected = "All")
    }
  }, ignoreInit = TRUE)
  
  # --- LEAFLET MAP RENDERING PIPELINE ---
  output$mapElement <- renderLeaflet({
    req(current_data$kba)
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -96.8, lat = 62.4, zoom = 4)
  })
  
  # Observer 1: Updates base polygons based on broad provincial scope selections
  observe({
    req(filtered_kba())
    proxy <- leafletProxy("mapElement") %>% clearShapes()
    
    # CRITICAL FIX: Ensure only valid 2D polygon features pass into leaflet layout
    polygon_only_kba <- filtered_kba() %>% 
      filter(st_is(., c("POLYGON", "MULTIPOLYGON")))
    
    proxy %>% addPolygons(
      data = polygon_only_kba,
      color = "#475569", weight = 1.5, fillOpacity = 0.2, fillColor = "#64748b",
      layerId = ~kbasiteid,
      label = ~paste(kbasiteid, "-", nationalname),
      highlightOptions = highlightOptions(weight = 3, color = "#16a34a", fillOpacity = 0.4, bringToFront = FALSE)
    )
  })
  
  # Observer 2: Draws specific high-visibility highlighted overlays when an isolated KBA is picked
  observe({
    req(current_data$kba)
    proxy <- leafletProxy("mapElement") %>% clearGroup("selection_highlight")
    
    if (selected_kba_id() != "All") {
      # 1. Isolate target KBA baseline perimeter shape and strip geometry contaminants
      target_shape <- current_data$kba %>% 
        filter(kbasiteid == selected_kba_id()) %>% 
        filter(st_is(., c("POLYGON", "MULTIPOLYGON")))
      
      req(nrow(target_shape) > 0)
      
      # 2. Isolate corresponding geometric intersection overlap segments and secure type parameters
      target_overlaps <- current_data$intersections %>% 
        filter(kbasiteid == selected_kba_id()) %>% 
        filter(st_is(., c("POLYGON", "MULTIPOLYGON")))
      
      # Draw distinct dark outline around the primary selected site boundary
      proxy %>% addPolygons(
        data = target_shape,
        color = "#1e3a8a", weight = 3, fillOpacity = 0.05,
        group = "selection_highlight"
      )
      
      # If overlaps exist, layer the cookie-cut pieces inside using a bright fill color
      if (!is.null(target_overlaps) && nrow(target_overlaps) > 0) {
        proxy %>% addPolygons(
          data = target_overlaps,
          color = "#dc2626", weight = 1, fillColor = "#ef4444", fillOpacity = 0.45,
          group = "selection_highlight",
          label = ~paste("Overlap with:", NAME_E)
        )
      }
      
      # Dynamically re-center map view to bounding box coordinates of selection asset
      bbox <- st_bbox(target_shape)
      proxy %>% fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]], lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
    }
  })
  
  # Connect Map click boundaries to update sidebar dropdown selectors seamlessly
  observeEvent(input$mapElement_shape_click, {
    click_id = input$mapElement_shape_click$id
    target_kba <- current_data$kba %>% filter(kbasiteid == click_id) %>% st_drop_geometry()
    if (nrow(target_kba) > 0) {
      composite_string <- paste0(target_kba$kbasiteid, " - ", target_kba$nationalname)
      updateSelectInput(session, "kbaFilter", selected = composite_string)
    }
  })
  
  # --- SIDEBAR UI GENERATION ---
  output$kbaSelectionHeader <- renderUI({
    if (selected_kba_id() == "All") {
      tags$div(class = "well-protected", style = "background-color: #f8fafc; border-color: #cbd5e1; color: #475569;",
               p(tags$strong("No Site Selected")),
               p("Click any boundary polygon on the national map or select an ID using the sidebar input controls to query metrics.")
      )
    } else {
      target_kba <- current_data$kba %>% filter(kbasiteid == selected_kba_id()) %>% st_drop_geometry()
      req(nrow(target_kba) > 0)
      
      # Determine descriptive status text layout based on actual calculation metrics
      status_block <- if (target_kba$CUMULATIVE_PROPORTION == 0) {
        tags$div(class = "well-unprotected",
                 p(tags$strong("Status: Unprotected")),
                 p("This designated Key Biodiversity Area does not geographically intersect any recognized CPCAD preservation nodes.")
        )
      } else {
        tags$div(class = "well-protected",
                 p(tags$strong("Status: Partially/Fully Protected")),
                 p(paste0("This site features active spatial convergence with intersecting protected layers. Score: ", 
                          round(target_kba$CUMULATIVE_PROPORTION * 100, 2), "%"))
        )
      }
      
      # Construct the metrics display dashboard metrics panel cards
      tags$div(
        h4(target_kba$nationalname, style = "font-weight: bold; color: #0f172a; margin-top: 5px;"),
        p(tags$strong("Jurisdiction: "), target_kba$jurisdiction_en, " | ", tags$strong("Site ID: "), target_kba$kbasiteid),
        br(),
        status_block,
        br(),
        fluidRow(
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Total Area"),
                             tags$div(class = "metric-value", paste0(format(round(target_kba$KBA_TOTAL_AREA_HA, 1), big.mark=","), " Ha")))),
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Total Score"),
                             tags$div(class = "metric-value", paste0(round(target_kba$CUMULATIVE_PROPORTION * 100, 1), " %"))))
        )
      )
    }
  })
  
  # --- CPCAD BREAKDOWN INVENTORY COMPILING ---
  output$overlapTable <- renderDT({
    if (selected_kba_id() == "All" || is.null(current_data$intersections)) {
      # Return clean empty data frame context if no active query window is specified
      blank_df <- data.frame(NAME_E = character(), IUCN_CAT = character(), TYPE_E = character(), OVERLAP_AREA_HA = numeric(), PROPORTION_PROTECTED = character())
      return(datatable(blank_df, options = list(dom = 't'), rownames = FALSE))
    }
    
    table_data <- current_data$intersections %>%
      filter(kbasiteid == selected_kba_id()) %>%
      st_drop_geometry()
    
    if (nrow(table_data) == 0) {
      # Graceful empty format container handling sites with 0 matching intersections
      empty_df <- data.frame(Message = "No overlapping designations found for this site.")
      return(datatable(empty_df, options = list(dom = 't'), rownames = FALSE))
    }
    
    table_data <- table_data %>%
      select(NAME_E, IUCN_CAT, TYPE_E, OVERLAP_AREA_HA, PROPORTION_PROTECTED) %>%
      mutate(
        OVERLAP_AREA_HA = round(OVERLAP_AREA_HA, 1),
        PROPORTION_PROTECTED = paste0(round(PROPORTION_PROTECTED * 100, 2), " %")
      ) %>%
      rename(
        `Protected Area Name` = NAME_E,
        `IUCN Cat`            = IUCN_CAT,
        `Type`                = TYPE_E,
        `Overlap (Ha)`        = OVERLAP_AREA_HA,
        `Coverage %`          = PROPORTION_PROTECTED
      )
    
    datatable(table_data, 
              options = list(pageLength = 5, scrollX = TRUE, dom = 'tp'), 
              rownames = FALSE,
              selection = "none")
  })
}

# --- LAUNCH CORE KERNEL ---
shinyApp(ui, server)
