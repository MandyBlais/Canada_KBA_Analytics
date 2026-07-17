# PART 2 - SHINY APP DASHBOARD INTERFACE

library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(gfonts)
library(fresh)

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


# 2. CREATE CUSTOM BRANDING THEME
kba_theme <- create_theme(
  adminlte_color(
    light_blue = "#0AA1F4"      # Primary Blue (accent)
  ),
  adminlte_sidebar(
    dark_bg = "#2f4858",        # Secondary Dark Blue/Teal background
    dark_hover_bg = "#3a3426",  # Secondary Dark Brown background on hover
    dark_color = "#ffffff"
  ),
  adminlte_global(
    content_bg = "#3a3426",     # Main background color (Dark Brown)
    box_bg = "#ffffff"
  )
)

# 3. --- USER INTERFACE DESIGN ---
ui <- dashboardPage(
  header = dashboardHeader(title = "Canada KBA Protection Dashboard", titleWidth = 320),
  
  sidebar = dashboardSidebar(
    width = 320,
    sidebarMenu(
      tags$div(style = "padding: 15px; color: #fff;",
               h4("Spatial Filter Configurations", class = "client-subhead", style = "margin-top: 0;"),
               p("Isolate regions using the dropdowns or click directly on a map boundary polygon.", class = "client-body", style = "color: #b8c7ce; font-size: 12px;"),
               hr(style = "border-color: #92bf00; border-width: 2px;"), # Green primary accent line
               
               # Geographic Hierarchy Filters
               selectInput("provinceFilter", "Province / Territory:", 
                           choices = c("All", sort(unique(data_payload$kba_layer$jurisdiction_en)))),
               
               selectInput("kbaFilter", "Select Specific KBA Site:", 
                           choices = c("All", sort(unique(paste0(data_payload$kba_layer$kbasiteid, " - ", data_payload$kba_layer$nationalname))))),
               
               hr(style = "border-color: #92bf00; border-width: 2px;"), # Green primary accent line
               # Operational Utilities
               actionButton("runSync", "Force API Data Refresh", 
                            style = "width: 100%; font-weight: bold; background-color: #ffcb00; color: #3a3426; border-color: #ffcb00;"), # Yellow primary accent button
               br(), br(),
               tags$small(textOutput("cacheTimeText"), class = "client-body", style = "color: #9ca8b0;")
      )
    )
  ),
  
  body = dashboardBody(
    # Use the fresh custom theme
    use_theme(kba_theme),
    
    # Custom Head Injectors for Typography and Specific Branded UI elements
    tags$head(
      # Import Open Sans from Google Fonts
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@300;700&display=swap"),
      # Import Rift from Adobe Fonts (assuming Adobe Web Project ID is active, or using local/web-font fallback)
      # If using Adobe Fonts, swap the URL below with your client's Adobe Project CSS URL:
      tags$link(rel = "stylesheet", href = "https://use.typekit.net/xxxxxxx.css"), 
      
      tags$style(HTML("
        /* --- BRANDED TYPOGRAPHY --- */
        /* Fallback to Impact/sans-serif if Rift (Adobe) isn't loaded locally or via Typekit */
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
          font-weight: 300 !important; /* Open Sans Light */
        }
        
        /* --- BRANDED INTERFACE COLORS --- */
        /* Header styling */
        .main-header .navbar { background-color: #0AA1F4 !important; } /* Blue Accent */
        .main-header .logo { background-color: #2f4858 !important; color: #92bf00 !important; } /* Dark Blue background, Green Logo text */
        
        /* Layout structures */
        .content-wrapper { background-color: #3a3426 !important; } /* Secondary Brown Background */
        #right-panel { background: #ffffff; border-left: 4px solid #92bf00; padding: 20px; height: calc(100vh - 80px); overflow-y: auto; } /* Green accent left border */
        
        /* Metric Styling */
        .metric-box { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 6px; padding: 12px; margin-bottom: 10px; }
        .metric-title { font-size: 11px; text-transform: uppercase; color: #2f4858; font-weight: 700; }
        .metric-value { font-size: 18px; color: #0AA1F4; font-weight: bold; }
        
        /* Callout Wells */
        .well-unprotected { background-color: #fff1f2; border-left: 5px solid #ffcb00; color: #3a3426; padding: 15px; border-radius: 6px; } /* Yellow Accent border */
        .well-protected { background-color: #f0fdf4; border-left: 5px solid #92bf00; color: #2f4858; padding: 15px; border-radius: 6px; } /* Green Accent border */
      "))
    ),
    
    fluidRow(
      # Map Viewport Layout Column
      column(width = 7,
             box(title = "National Conservation Baseline Map", width = NULL, solidHeader = TRUE, status = "primary",
                 leafletOutput("mapElement", height = "760px")
             )
      ),
      
      # Dynamic Side Attributes Column
      column(width = 5, id = "right-panel",
             h3("Biodiversity & Conservation Attributes", style = "margin-top: 0; color: #2f4858;"),
             p("Detailed site telemetry and overlapping legal designations.", class = "client-body", style = "color: #3a3426; margin-bottom: 20px;"),
             hr(style = "border-color: #92bf00;"),
             
             # Selected KBA Core Metadata Block
             uiOutput("kbaSelectionHeader"),
             br(),
             
             # Tabular breakdown of intersecting CPCAD elements
             h4("Overlapping CPCAD Sites Inventory", class = "client-subhead", style = "color: #2f4858;"),
             p("Individual contributing federal, provincial, and territorial conservation shapes.", class = "client-body", style = "color: #3a3426; font-size: 12px;"),
             DTOutput("overlapTable")
      )
    )
  )
)

# 4. --- SERVER COMPUTATION LOGIC ---
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
      setView(lng = -96.8, lat = 62.4, zoom = 4) %>%
      
      # ADD MATCHED CONSERVATION LEGEND WITH BRANDED CSS INJECTION
      addLegend(
        position = "bottomright",
        colors = c("#92bf00", "#0AA1F4", "#FFCB00"), # Added Branded Yellow Hex
        labels = c(
          "Key Biodiversity Area (KBA)", 
          "Protected Area Overlap (CPCAD)",
          "Active CPCAD Outline & Overlap" # Explains the yellow hover
        ),
        title = "Conservation Boundaries",
        opacity = 0.85
      ) %>%
      # Inline CSS injection to force Leaflet layers to respect client font guidelines
      htmlwidgets::onRender("
        function(el, x) {
          var legend = document.querySelector('.leaflet-control-legend');
          if (legend) {
            legend.style.fontFamily = 'Open Sans, sans-serif';
            legend.style.fontWeight = '300';
            legend.style.backgroundColor = 'rgba(255, 255, 255, 0.95)';
            legend.style.border = '2px solid #2f4858';
            legend.style.borderRadius = '6px';
            
            // Force Title to use Rift Bold / Headline font
            var title = legend.querySelector('strong');
            if (title) {
              title.style.fontFamily = 'rift, Impact, sans-serif';
              title.style.fontWeight = '700';
              title.style.textTransform = 'uppercase';
              title.style.letterSpacing = '1px';
              title.style.color = '#2f4858';
            }
          }
        }
      ")
  })
  
  # Observer 1: Updates base polygons based on broad provincial scope selections
  observe({
    req(filtered_kba())
    proxy <- leafletProxy("mapElement") %>% clearShapes()
    
    # SAFE GEOMETRY RESOLUTION: Explicitly find and bind the geometry column regardless of casing
    kba_data <- filtered_kba()
    geom_col <- attr(kba_data, "sf_column")
    if (!(geom_col %in% colnames(kba_data))) {
      # Fallback: if columns were capitalized to GEOMETRY, re-bind it
      true_geom <- colnames(kba_data)[typeof(st_geometry(kba_data)) == "list" | grepl("geometry", colnames(kba_data), ignore.case = TRUE)][1]
      st_geometry(kba_data) <- true_geom
    }
    
    polygon_only_kba <- kba_data %>% 
      filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
    
    proxy %>% addPolygons(
      data = polygon_only_kba,
      color = "#2f4858",       # Client Secondary Dark Blue for crisp outlines
      weight = 1.0,            # Thin border line weight
      fillOpacity = 0.65,      
      fillColor = "#92bf00",   # Client Primary Brand Green
      layerId = ~kbasiteid,
      label = ~paste(kbasiteid, "-", nationalname),
      # Branded highlight interaction
      highlightOptions = highlightOptions(
        weight = 2.5, 
        color = "#ffcb00",     # Client Primary Yellow on hover
        fillOpacity = 0.8, 
        bringToFront = FALSE
      )
    )
  })
  
  # Observer 2: Highlights selection AND extracts local spatial intersections
  observe({
    req(current_data$kba)
    proxy <- leafletProxy("mapElement") %>% clearGroup("selection_highlight")
    
    if (selected_kba_id() != "All") {
      # Re-verify geometry tracking for master KBA layer
      kba_master <- current_data$kba
      geom_col_kba <- attr(kba_master, "sf_column")
      if (!(geom_col_kba %in% colnames(kba_master))) {
        true_geom <- colnames(kba_master)[grepl("geometry", colnames(kba_master), ignore.case = TRUE)][1]
        st_geometry(kba_master) <- true_geom
      }
      
      target_shape <- kba_master %>% 
        filter(kbasiteid == selected_kba_id()) %>% 
        filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
      
      req(nrow(target_shape) > 0)
      
      # Re-verify geometry tracking for intersections layer
      intersections_master <- current_data$intersections
      geom_col_int <- attr(intersections_master, "sf_column")
      if (!(geom_col_int %in% colnames(intersections_master))) {
        true_geom_int <- colnames(intersections_master)[grepl("geometry", colnames(intersections_master), ignore.case = TRUE)][1]
        st_geometry(intersections_master) <- true_geom_int
      }
      
      target_overlaps <- intersections_master %>% 
        filter(as.character(kbasiteid) == as.character(selected_kba_id())) %>% 
        filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
      
      # Draw distinct dark outline around the primary selected site boundary
      proxy %>% addPolygons(
        data = target_shape,
        color = "#3a3426",     # Client Secondary Dark Brown frame boundary
        weight = 3.0, 
        fillOpacity = 0.0,      # Clear fill to showcase overlaps clearly inside
        group = "selection_highlight"
      )
      
      # Layer the cookie-cut pieces inside using the crisp Blue Brand accent
      if (!is.null(target_overlaps) && nrow(target_overlaps) > 0) {
        proxy %>% addPolygons(
          data = target_overlaps,
          color = "#0AA1F4", 
          weight = 1.5, 
          fillColor = "#0AA1F4", # Client Primary Brand Blue
          fillOpacity = 0.50,
          group = "selection_highlight",
          label = ~paste("Overlap with:", NAME_E)
        )
      }
      
      # Dynamically re-center map view to bounding box coordinates of selection asset
      bbox <- st_bbox(target_shape)
      proxy %>% fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]], lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
    }
  })
  
  # Observer 3: Highlights the entire uncut CPCAD boundary AND fills the KBA overlap area on table row hover
  observe({
    proxy <- leafletProxy("mapElement") %>% clearGroup("cpcad_hover_highlight")
    
    # Ensure a row is actively hovered and a KBA selection exists
    if (is.null(input$hovered_cpcad_row) || selected_kba_id() == "All") return()
    
    req(current_data$intersections, current_data$cpcad)
    target_id <- selected_kba_id()
    
    # 1. Isolate the cookie-cut intersection slice representing the active table row
    hover_table_subset <- current_data$intersections %>% 
      filter(as.character(kbasiteid) == as.character(target_id)) %>% 
      filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
    
    req(nrow(hover_table_subset) > 0)
    hovered_row_data <- hover_table_subset[input$hovered_cpcad_row + 1, ]
    
    # 2. Extract Protected Area Name safely using a non-spatial attribute look-up
    row_attributes <- hovered_row_data %>% st_drop_geometry()
    colnames(row_attributes) <- toupper(colnames(row_attributes))
    pa_name_field <- if("NAME_E" %in% colnames(row_attributes)) "NAME_E" else "name_e"
    hovered_pa_name <- row_attributes[[pa_name_field]]
    
    # 3. Pull uncut geometry out of master storage without mutating master column schemas
    # We use a character match on the original layer attributes
    full_uncut_shape <- current_data$cpcad %>% 
      filter(st_drop_geometry(.) %>% select(matches("^NAME_E$|^name_e$")) %>% pull(1) == hovered_pa_name) %>% 
      filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
    
    # 4. Layer Both Highlights onto the Map Proxy
    # Outline: Draw complete outer boundaries extending everywhere (Hollow Yellow Frame)
    if (nrow(full_uncut_shape) > 0) {
      proxy %>% addPolygons(
        data = full_uncut_shape,
        color = "#FFCB00",       # Branded Yellow Outline
        weight = 3.5, 
        fillColor = "transparent", 
        group = "cpcad_hover_highlight"
      )
    }
    
    # Fill: Shade the intersection piece exactly where it hits inside the KBA polygon boundary
    proxy %>% addPolygons(
      data = hovered_row_data,
      stroke = FALSE,            
      fillColor = "#FFCB00",   # Branded Yellow Fill
      fillOpacity = 0.55,        
      group = "cpcad_hover_highlight"
    )
  })
  
  # Connect Map click boundaries to update sidebar dropdown selectors seamlessly
  observeEvent(input$mapElement_shape_click, {
    click <- input$mapElement_shape_click
    req(click)
    
    click_id <- click$id
    # Guard: Ensure the ID is not NULL, not NA, and has exactly 1 element
    req(click_id, length(click_id) == 1, !is.na(click_id))
    
    # Grab the KBA data frame
    kba_df <- current_data$kba
    req(kba_df)
    
    # Match data types safely
    if (is.numeric(kba_df$kbasiteid)) {
      click_id <- as.numeric(click_id)
    } else {
      click_id <- as.character(click_id)
    }
    
    # Filter safely using %in% instead of == to prevent size-0 evaluation crashes
    target_kba <- kba_df %>% 
      filter(kbasiteid %in% !!click_id) %>% 
      st_drop_geometry()
    
    if (nrow(target_kba) > 0) {
      composite_string <- paste0(target_kba$kbasiteid[1], " - ", target_kba$nationalname[1])
      updateSelectInput(session, "kbaFilter", selected = composite_string)
    }
  })
  
  # --- SIDEBAR UI GENERATION ---
  output$kbaSelectionHeader <- renderUI({
    if (selected_kba_id() == "All") {
      req(current_data$kba)
      
      # Clean spatial definitions and drop geometry for blazing fast summarization
      national_summary <- current_data$kba %>% st_drop_geometry()
      
      # Calculate national baseline metrics
      # 1. Total KBA Area across Canada (Summing hectares and converting to km2)
      total_national_area_ha <- sum(national_summary$KBA_TOTAL_AREA_HA, na.rm = TRUE)
      total_national_area_km2 <- total_national_area_ha * 0.01
      
      # 2. Total National Proportion of Protection
      # Weighted average calculation: Sum of individual overlapping areas divided by total area
      if ("CUMULATIVE_PROPORTION" %in% colnames(national_summary)) {
        total_protected_area_ha <- sum(national_summary$KBA_TOTAL_AREA_HA * national_summary$CUMULATIVE_PROPORTION, na.rm = TRUE)
        national_proportion_protected <- (total_protected_area_ha / total_national_area_ha) * 100
      } else {
        national_proportion_protected <- 0
      }
      
      tags$div(
        h4("Canada National Overview", style = "font-weight: bold; color: #0f172a; margin-top: 5px;"),
        p("Aggregated nationwide metrics for all designated Key Biodiversity Areas."),
        br(),
        tags$div(class = "well-protected", style = "background-color: #f8fafc; border-color: #cbd5e1; color: #475569;",
                 p(tags$strong("Interactive Map Baseline")),
                 p("Click any boundary polygon on the national map or select an ID using the sidebar input controls to query localized site-level telemetry.")
        ),
        br(),
        fluidRow(
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Total KBA Area (Canada)"),
                             tags$div(class = "metric-value", paste0(format(round(total_national_area_km2, 2), big.mark=","), " km²")))),
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "National Protection Coverage"),
                             tags$div(class = "metric-value", paste0(round(national_proportion_protected, 1), " %"))))
        )
      )
    } else {
      target_kba <- current_data$kba %>% filter(kbasiteid == selected_kba_id()) %>% st_drop_geometry()
      req(nrow(target_kba) > 0)
      
      status_block <- if (target_kba$CUMULATIVE_PROPORTION == 0) {
        tags$div(class = "well-unprotected",
                 p(tags$strong("Status: Unprotected")),
                 p("This designated Key Biodiversity Area does not geographically intersect any recognized CPCAD preservation nodes.")
        )
      } else {
        tags$div(class = "well-protected",
                 p(tags$strong("Status: Partially/Fully Protected")),
                 p(paste0("This site features active spatial convergence with intersecting protected layers. ", 
                          round(target_kba$CUMULATIVE_PROPORTION * 100, 1), "% Protected."))
        )
      }
      
      tags$div(
        h4(target_kba$nationalname, style = "font-weight: bold; color: #0f172a; margin-top: 5px;"),
        p(tags$strong("Jurisdiction: "), target_kba$jurisdiction_en, " | ", tags$strong("Site ID: "), target_kba$kbasiteid),
        br(),
        status_block,
        br(),
        fluidRow(
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Total Area"),
                             # Convert Ha to km2 (Ha * 0.01) and change label
                             tags$div(class = "metric-value", paste0(format(round(target_kba$KBA_TOTAL_AREA_HA * 0.01, 2), big.mark=","), " km²")))),
          column(6, tags$div(class = "metric-box",
                             tags$div(class = "metric-title", "Proportion of Protection"),
                             tags$div(class = "metric-value", paste0(round(target_kba$CUMULATIVE_PROPORTION * 100, 1), " %"))))
        )
      )
    }
  })
  
  # --- CPCAD BREAKDOWN INVENTORY COMPILING ---
  output$overlapTable <- renderDT({
    req(current_data$intersections)
    
    target_id <- selected_kba_id()
    
    # 1. Drop spatial definitions early and filter based on selection
    table_data <- current_data$intersections %>% st_drop_geometry()
    
    # --- CASE SAFETY SHIELD ---
    # Convert all incoming column names to uppercase to defeat ESRI casing mismatches
    colnames(table_data) <- toupper(colnames(table_data))
    
    if (target_id != "All") {
      # Make sure we check for casing-standardized KBASITEID
      kba_col <- if ("KBASITEID" %in% colnames(table_data)) "KBASITEID" else "kbasiteid"
      
      if (is.numeric(table_data[[kba_col]])) {
        table_data <- table_data %>% filter(.data[[kba_col]] == as.numeric(target_id))
      } else {
        table_data <- table_data %>% filter(as.character(.data[[kba_col]]) == as.character(target_id))
      }
    }
    
    # 2. Return an empty styled table if no data matches
    if (nrow(table_data) == 0) {
      empty_df <- data.frame(Message = "No overlapping designations found for this site.")
      return(
        datatable(
          empty_df, 
          options = list(
            dom = 't',
            initComplete = JS(
              "function(settings, json) {",
              "  $(this.api().table().header()).css({'font-family': 'Open Sans, sans-serif', 'background-color': '#2f4858', 'color': '#ffffff'});",
              "}"
            )
          ), 
          rownames = FALSE
        )
      )
    }
    
    # 3. Pipeline Safety: Handled safely with guaranteed UPPERCASE column matching
    if (!"NAME_E" %in% colnames(table_data)) table_data$NAME_E <- "Unknown Protected Area"
    if (!"IUCN_CAT" %in% colnames(table_data)) table_data$IUCN_CAT <- "8"
    if (!"TYPE_E" %in% colnames(table_data)) table_data$TYPE_E <- "Unknown"
    if (!"MGMT_E" %in% colnames(table_data)) table_data$MGMT_E <- "Not Reported"
    if (!"GOV_TYPE" %in% colnames(table_data)) table_data$GOV_TYPE <- 12 # Fallback to 12 (Not reported)
    if (!"MPLAN" %in% colnames(table_data)) table_data$MPLAN <- "Not Reported"
    if (!"MPLAN_REF" %in% colnames(table_data)) table_data$MPLAN_REF <- "No Link"
    if (!"OVERLAP_AREA_HA" %in% colnames(table_data)) table_data$OVERLAP_AREA_HA <- 0
    
    # 3b. Pipeline patch: Dynamically generate PROPORTION_PROTECTED if missing
    if (!"PROPORTION_PROTECTED" %in% colnames(table_data)) {
      if ("KBA_TOTAL_AREA_HA" %in% colnames(table_data)) {
        table_data <- table_data %>% 
          mutate(PROPORTION_PROTECTED = OVERLAP_AREA_HA / KBA_TOTAL_AREA_HA)
      } else {
        table_data$PROPORTION_PROTECTED <- 0
      }
    }
    
    # 4. Clean transformations & format management plan URLs dynamically
    table_data <- table_data %>%
      mutate(
        OVERLAP_KM2 = round(as.numeric(OVERLAP_AREA_HA) * 0.01, 2),
        
        # Keep raw_prop as a 0.0 - 1.0 decimal so DT can handle it perfectly
        raw_prop = as.numeric(PROPORTION_PROTECTED),
        raw_prop = ifelse(is.na(raw_prop) | is.nan(raw_prop), 0, raw_prop),
        
        # Map the IUCN codes to their full string labels safely using case_when
        IUCN_LABEL = case_when(
          as.character(IUCN_CAT) == "1" ~ "1 - Strict Nature Preserve",
          as.character(IUCN_CAT) == "2" ~ "2 - Wilderness Area",
          as.character(IUCN_CAT) == "3" ~ "3 - National Park",
          as.character(IUCN_CAT) == "4" ~ "4 - National Monument or Feature",
          as.character(IUCN_CAT) == "5" ~ "5 - Habitat/Species Management Area",
          as.character(IUCN_CAT) == "6" ~ "6 - Protected Landscape/Seascape",
          as.character(IUCN_CAT) == "7" ~ "7 - Protected Area with Sustainable Use of Natural Resources",
          as.character(IUCN_CAT) == "8" ~ "8 - Not Reported",
          as.character(IUCN_CAT) == "9" ~ "9 - Not Applicable",
          TRUE                          ~ as.character(IUCN_CAT)
        ),
        
        # Map GOV_TYPE domain codes to their respective English displayed values
        GOV_LABEL = case_match(
          as.integer(GOV_TYPE),
          1  ~ "National government",
          2  ~ "Sub-national government",
          3  ~ "Government-delegated management",
          4  ~ "Transboundary governance",
          5  ~ "Collaborative governance",
          6  ~ "Joint governance",
          7  ~ "Individual landowners",
          8  ~ "Non-profit organizations",
          9  ~ "For-profit organizations",
          10 ~ "Indigenous peoples",
          11 ~ "Local communities",
          12 ~ "Not reported",
          .default = "Not reported"
        ),
        
        # Map MPLAN short integers (0 and 1) to Yes and No strings
        MPLAN_LABEL = case_match(
          as.integer(MPLAN),
          0 ~ "No",
          1 ~ "Yes",
          .default = "Not Reported" # Safe fallback for NA or blank values
        ),
        
        # Turn valid web URLs in MPLAN_REF into clickable hyperlink anchors safely
        MPLAN_LINK = case_when(
          grepl("^https?://", MPLAN_REF, ignore.case = TRUE) ~ 
            paste0("<a href='", MPLAN_REF, "' target='_blank' style='color: #0AA1F4; font-weight: bold;'>View Plan ⧉</a>"),
          
          grepl("^www\\.", MPLAN_REF, ignore.case = TRUE) ~ 
            paste0("<a href='https://", MPLAN_REF, "' target='_blank' style='color: #0AA1F4; font-weight: bold;'>View Plan ⧉</a>"),
          
          TRUE ~ MPLAN_REF
        )
      ) %>%
      select(
        `Protected Area Name` = NAME_E,
        `Site Type`           = TYPE_E,
        `Overlap (km²)`       = OVERLAP_KM2,
        `Coverage %`          = raw_prop,
        `IUCN Category`       = IUCN_LABEL,
        `Managing Authority`  = MGMT_E,
        `Gov Type`            = GOV_LABEL,
        `Mgmt Plan Status`    = MPLAN_LABEL,   
        `Mgmt Plan Link`      = MPLAN_LINK
      )
    
    # 5. Render DataTable with client branding and hover event triggers
    dt_widget <- datatable(
      table_data, 
      escape = FALSE, # Allows HTML anchor tags in MPLAN_LINK to render as clickable links!
      options = list(
        pageLength = 5, 
        scrollX = TRUE, 
        autoWidth = FALSE, 
        dom = 'tp',
        columnDefs = list(
          list(width = '200px', targets = 0), 
          list(width = '120px', targets = 1), 
          list(width = '100px', targets = 2), 
          list(width = '100px', targets = 3), 
          list(width = '240px', targets = 4), 
          list(width = '180px', targets = 5), 
          list(width = '150px', targets = 6), 
          list(width = '150px', targets = 7), 
          list(width = '120px', targets = 8)  
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().header()).css({",
          "    'font-family': 'Open Sans, sans-serif',",
          "    'font-weight': '700',",
          "    'background-color': '#2f4858',", 
          "    'color': '#ffffff'",
          "  });",
          "}"
        )
      ), 
      rownames = FALSE,
      selection = "none"
    ) %>% 
      formatPercentage(columns = "Coverage %", digits = 1)
    
    # Attach client-side hover listener passing row indices to input$hovered_cpcad_row
    htmlwidgets::onRender(dt_widget, "
      function(el, x) {
        var table = $(el).find('table').DataTable();
        
        // Broadcast R-compatible row index when mouse hovers a row
        $(el).on('mouseenter', 'tbody tr', function() {
          var rowIndex = table.row(this).index();
          if (rowIndex !== undefined) {
            Shiny.setInputValue('hovered_cpcad_row', rowIndex, {priority: 'event'});
          }
        });
        
        // Wipe state when mouse clears out of table content area
        $(el).on('mouseleave', 'tbody', function() {
          Shiny.setInputValue('hovered_cpcad_row', null);
        });
      }
    ")
  })
}

# 5. --- LAUNCH CORE KERNEL ---
shinyApp(ui, server)
