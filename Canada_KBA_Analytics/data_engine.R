# ==========================================
# PART 1 - DATA ENGINE (WITH CATEGORIZED DISSOLVED METRICS & ATTRIBUTE PATCHING)
# ==========================================

library(arcgislayers)
library(sf)
library(dplyr)
library(tidyr)

KBA_API_URL   <- "https://gis.natureserve.ca/arcgis/rest/services/EBAR-KBA/KBA_Accepted_Sites/FeatureServer/0"
CPCAD_API_URL <- "https://maps-cartes.ec.gc.ca/arcgis/rest/services/CWS_SCF/CPCAD/MapServer/0"
CH_API_URL    <- "https://maps-cartes.ec.gc.ca/arcgis/rest/services/CWS_SCF/CriticalHabitat/MapServer/3"
CACHE_PATH    <- "data/cached_compiled_data.rds"

# PROJ representation of ESRI:102008 (North America Albers Equal Area Conic)
crs_esri_102008 <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

refresh_spatial_cache <- function(output_path = CACHE_PATH) {
  
  message("--- Starting Data Update Sequence (KBAs, CPCAD, Critical Habitat) ---")
  
  message("Connecting to endpoints...")
  kba_client   <- arc_open(KBA_API_URL)
  cpcad_client <- arc_open(CPCAD_API_URL)
  ch_client    <- arc_open(CH_API_URL)
  
  # 1. Download accepted KBAs directly in EPSG:3978
  message("Downloading Accepted KBA features (Server-side EPSG:3978)...")
  kba_sf <- arc_select(kba_client, crs = 3978) %>%
    st_transform(crs_esri_102008) %>%
    rename_with(tolower) %>%
    st_zm(drop = TRUE) %>%
    st_make_valid() %>%
    mutate(KBA_TOTAL_AREA_HA = as.numeric(st_area(.)) / 10000)
  
  # 2. Download ALL CPCAD features across Canada
  message("Streaming ALL CPCAD features nationwide...")
  cpcad_sf <- arc_select(
    cpcad_client,
    crs = 3978,
    fields = c(
      "PARENT_ID", "ZONE_ID", "NAME_E", "IUCN_CAT", "PA_OECM_DF", 
      "TYPE_E", "OWNER_E", "MGMT_E", "GOV_TYPE", "LOC", "JUR_ID", 
      "MPLAN", "MPLAN_REF"
    )
  ) %>%
    st_transform(crs_esri_102008) %>%
    st_zm(drop = TRUE) %>%
    st_make_valid() %>%
    mutate(CPCAD_TOTAL_AREA_HA = as.numeric(st_area(.)) / 10000)
  
  # 3. Download ALL Critical Habitat features across Canada
  message("Streaming ALL Critical Habitat (CH) features nationwide...")
  ch_sf <- arc_select(
    ch_client,
    crs = 3978,
    fields = c(
      "OBJECTID", "SiteID", "SiteName_E", "COSEWIC_ID", "CommName_E", 
      "SciName", "Population_E", "SARA_Status", "SARA_Agency", 
      "ProvTerr_E", "Sensitive_E", "Taxon", "RDoc_Name_E", "RD_Status"
    )
  ) %>%
    st_transform(crs_esri_102008) %>%
    st_zm(drop = TRUE) %>%
    st_make_valid() %>%
    mutate(CH_TOTAL_AREA_HA = as.numeric(st_area(.)) / 10000)

  
  # ------------------------------------------------------------------
  # PRE-CALCULATE DISSOLVED METRICS (EXACT UN-SIMPLIFIED GEOMETRIES)
  # ------------------------------------------------------------------
  message("Pre-calculating dissolved national and regional areas for CPCAD and CH...")
  
  # A. CPCAD Dissolved Area Calculations
  # Optimized Provincial CPCAD Dissolved Area
  cpcad_prov_summary <- cpcad_sf %>%
    mutate(
      RAW_JUR = toupper(trimws(coalesce(as.character(LOC), as.character(JUR_ID)))),
      JUR_CLEAN = case_when(
        RAW_JUR %in% c("1", "AB", "ALBERTA", "48")                    ~ "Alberta",
        RAW_JUR %in% c("2", "BC", "BRITISH COLUMBIA", "59")           ~ "British Columbia",
        RAW_JUR %in% c("3", "MB", "MANITOBA", "46")                   ~ "Manitoba",
        RAW_JUR %in% c("4", "NB", "NEW BRUNSWICK", "13")              ~ "New Brunswick",
        RAW_JUR %in% c("5", "NL", "NEWFOUNDLAND AND LABRADOR", "10") ~ "Newfoundland and Labrador",
        RAW_JUR %in% c("6", "NT", "NORTHWEST TERRITORIES", "61")      ~ "Northwest Territories",
        RAW_JUR %in% c("7", "NS", "NOVA SCOTIA", "12")                ~ "Nova Scotia",
        RAW_JUR %in% c("8", "NU", "NUNAVUT", "62")                    ~ "Nunavut",
        RAW_JUR %in% c("9", "ON", "ONTARIO", "35")                    ~ "Ontario",
        RAW_JUR %in% c("10", "PE", "PRINCE EDWARD ISLAND", "11")      ~ "Prince Edward Island",
        RAW_JUR %in% c("11", "QC", "QUEBEC", "24")                    ~ "Quebec",
        RAW_JUR %in% c("12", "SK", "SASKATCHEWAN", "47")              ~ "Saskatchewan",
        RAW_JUR %in% c("13", "YT", "YUKON", "60")                     ~ "Yukon",
        RAW_JUR %in% c("14", "15", "16", "17", "18", "19", "20", "21") ~ "Federal Offshore/Marine",
        TRUE                                                           ~ RAW_JUR
      )
    ) %>%
    group_by(JUR_CLEAN) %>%
    summarize(
      # st_combine aggregates geometries into a single MULTIPOLYGON before unioning
      geometry = st_union(st_combine(geometry)),
      .groups = "drop"
    ) %>%
    mutate(CPCAD_KM2 = round(as.numeric(st_area(geometry)) / 1e6, 1)) %>%
    st_drop_geometry()
  
  # Compute National CPCAD Total directly from dissolved provincial areas
  national_cpcad_km2 <- sum(cpcad_prov_summary$CPCAD_KM2, na.rm = TRUE)
  
  # B. Critical Habitat Dissolved Area Calculations
  # Optimized Provincial Critical Habitat Dissolved Area
  ch_prov_summary <- ch_sf %>%
    tidyr::separate_rows(ProvTerr_E, sep = ";") %>%
    mutate(
      RAW_JUR = toupper(trimws(as.character(ProvTerr_E))),
      JUR_CLEAN = case_when(
        RAW_JUR %in% c("ON", "ONTARIO", "35")                    ~ "Ontario",
        RAW_JUR %in% c("BC", "BRITISH COLUMBIA", "59")           ~ "British Columbia",
        RAW_JUR %in% c("AB", "ALBERTA", "48")                    ~ "Alberta",
        RAW_JUR %in% c("QC", "QUEBEC", "24")                     ~ "Quebec",
        RAW_JUR %in% c("SK", "SASKATCHEWAN", "47")               ~ "Saskatchewan",
        RAW_JUR %in% c("MB", "MANITOBA", "46")                   ~ "Manitoba",
        RAW_JUR %in% c("NS", "NOVA SCOTIA", "12")                ~ "Nova Scotia",
        RAW_JUR %in% c("NB", "NEW BRUNSWICK", "13")              ~ "New Brunswick",
        RAW_JUR %in% c("NL", "NEWFOUNDLAND AND LABRADOR", "10")  ~ "Newfoundland and Labrador",
        RAW_JUR %in% c("PE", "PRINCE EDWARD ISLAND", "11")       ~ "Prince Edward Island",
        RAW_JUR %in% c("YT", "YUKON", "60")                      ~ "Yukon",
        RAW_JUR %in% c("NT", "NORTHWEST TERRITORIES", "61")      ~ "Northwest Territories",
        RAW_JUR %in% c("NU", "NUNAVUT", "62")                    ~ "Nunavut",
        TRUE                                                     ~ RAW_JUR
      )
    ) %>%
    group_by(JUR_CLEAN) %>%
    summarize(
      geometry = st_union(st_combine(geometry)),
      .groups = "drop"
    ) %>%
    mutate(CH_KM2 = round(as.numeric(st_area(geometry)) / 1e6, 1)) %>%
    st_drop_geometry()
  
  # Compute National Critical Habitat Total by dissolving nationwide once
  national_ch_km2 <- round(as.numeric(st_area(st_union(st_combine(ch_sf)))) / 1e6, 1)
  
  # 5. Spatial Intersections (KBA x CPCAD)
  message("Calculating KBA x CPCAD spatial intersections...")
  intersection_sf <- st_intersection(kba_sf, cpcad_sf)
  if (nrow(intersection_sf) > 0) {
    intersection_sf <- intersection_sf[!st_is_empty(intersection_sf), ]
    intersection_sf <- st_collection_extract(intersection_sf, "POLYGON")
    intersection_sf <- intersection_sf[st_dimension(intersection_sf) == 2, ]
  }
  
  # 6. Spatial Intersections (KBA x CH)
  message("Calculating KBA x Critical Habitat spatial intersections...")
  ch_kba_intersection_sf <- st_intersection(kba_sf, ch_sf)
  if (nrow(ch_kba_intersection_sf) > 0) {
    ch_kba_intersection_sf <- ch_kba_intersection_sf[!st_is_empty(ch_kba_intersection_sf), ]
    ch_kba_intersection_sf <- st_collection_extract(ch_kba_intersection_sf, "POLYGON")
    ch_kba_intersection_sf <- ch_kba_intersection_sf[st_dimension(ch_kba_intersection_sf) == 2, ]
  }
  
  # 7. Protection Metrics (CPCAD inside KBAs by Domain 1 & 3 vs 2 & 4)
  if (nrow(intersection_sf) > 0) {
    # Combined Overall Protection
    kba_protection_metrics <- intersection_sf %>%
      group_by(kbasiteid) %>%
      summarize(.groups = "drop") %>%
      mutate(PROTECTED_AREA_HA = as.numeric(st_area(.) / 10000)) %>%
      st_drop_geometry() %>%
      left_join(kba_sf %>% st_drop_geometry() %>% select(kbasiteid, KBA_TOTAL_AREA_HA), by = "kbasiteid") %>%
      mutate(CUMULATIVE_PROPORTION = pmin(1.0, PROTECTED_AREA_HA / KBA_TOTAL_AREA_HA)) %>%
      select(kbasiteid, PROTECTED_AREA_HA, CUMULATIVE_PROPORTION)
    
    # Protected Areas: PA_OECM_DF Domain 1 (PA) & 3 (Interim PA)
    kba_pa_metrics <- intersection_sf %>%
      filter(as.character(PA_OECM_DF) %in% c("1", "3")) %>%
      group_by(kbasiteid) %>%
      summarize(.groups = "drop") %>%
      mutate(PA_AREA_HA = as.numeric(st_area(.) / 10000)) %>%
      st_drop_geometry()
    
    # Conserved Areas: PA_OECM_DF Domain 2 (OECM) & 4 (Interim OECM)
    kba_oecm_metrics <- intersection_sf %>%
      filter(as.character(PA_OECM_DF) %in% c("2", "4")) %>%
      group_by(kbasiteid) %>%
      summarize(.groups = "drop") %>%
      mutate(OECM_AREA_HA = as.numeric(st_area(.) / 10000)) %>%
      st_drop_geometry()
    
    kba_protection_metrics <- kba_protection_metrics %>%
      left_join(kba_pa_metrics, by = "kbasiteid") %>%
      left_join(kba_oecm_metrics, by = "kbasiteid") %>%
      mutate(
        PA_AREA_HA = coalesce(PA_AREA_HA, 0.0),
        OECM_AREA_HA = coalesce(OECM_AREA_HA, 0.0)
      )
  } else {
    kba_protection_metrics <- tibble(
      kbasiteid = character(),
      PROTECTED_AREA_HA = numeric(),
      CUMULATIVE_PROPORTION = numeric(),
      PA_AREA_HA = numeric(),
      OECM_AREA_HA = numeric()
    )
  }
  
  # 8. Critical Habitat Metrics (CH inside KBAs for Endangered domain 2 vs Threatened domain 3)
  if (nrow(ch_kba_intersection_sf) > 0) {
    # Combined Critical Habitat
    kba_ch_metrics <- ch_kba_intersection_sf %>%
      group_by(kbasiteid) %>%
      summarize(.groups = "drop") %>%
      mutate(CRITICAL_HABITAT_HA = as.numeric(st_area(.) / 10000)) %>%
      st_drop_geometry() %>%
      left_join(kba_sf %>% st_drop_geometry() %>% select(kbasiteid, KBA_TOTAL_AREA_HA), by = "kbasiteid") %>%
      mutate(CRITICAL_HABITAT_PROPORTION = pmin(1.0, CRITICAL_HABITAT_HA / KBA_TOTAL_AREA_HA)) %>%
      select(kbasiteid, CRITICAL_HABITAT_HA, CRITICAL_HABITAT_PROPORTION)
    
    # Endangered CH: SARA_Status domain 2
    kba_ch_endangered <- ch_kba_intersection_sf %>%
      filter(as.character(SARA_Status) == "2") %>%
      group_by(kbasiteid) %>%
      summarize(.groups = "drop") %>%
      mutate(CH_ENDANGERED_HA = as.numeric(st_area(.) / 10000)) %>%
      st_drop_geometry()
    
    # Threatened CH: SARA_Status domain 3
    kba_ch_threatened <- ch_kba_intersection_sf %>%
      filter(as.character(SARA_Status) == "3") %>%
      group_by(kbasiteid) %>%
      summarize(.groups = "drop") %>%
      mutate(CH_THREATENED_HA = as.numeric(st_area(.) / 10000)) %>%
      st_drop_geometry()
    
    kba_ch_metrics <- kba_ch_metrics %>%
      left_join(kba_ch_endangered, by = "kbasiteid") %>%
      left_join(kba_ch_threatened, by = "kbasiteid") %>%
      mutate(
        CH_ENDANGERED_HA = coalesce(CH_ENDANGERED_HA, 0.0),
        CH_THREATENED_HA = coalesce(CH_THREATENED_HA, 0.0)
      )
  } else {
    kba_ch_metrics <- tibble(
      kbasiteid = character(),
      CRITICAL_HABITAT_HA = numeric(),
      CRITICAL_HABITAT_PROPORTION = numeric(),
      CH_ENDANGERED_HA = numeric(),
      CH_THREATENED_HA = numeric()
    )
  }
  
  # 9. COMPILE & PATCH KBA ATTRIBUTE TABLE
  message("Cleaning and patching KBA attribute table...")
  
  kba_compiled <- kba_sf %>%
    left_join(kba_protection_metrics, by = "kbasiteid") %>%
    left_join(kba_ch_metrics, by = "kbasiteid") %>%
    mutate(
      calc_prop_protected = PROTECTED_AREA_HA / KBA_TOTAL_AREA_HA,
      
      CUMULATIVE_PROPORTION = case_when(
        is.na(CUMULATIVE_PROPORTION)  ~ pmax(0.0, pmin(1.0, calc_prop_protected)),
        CUMULATIVE_PROPORTION < 0     ~ pmax(0.0, pmin(1.0, calc_prop_protected)),
        TRUE                          ~ pmax(0.0, pmin(1.0, CUMULATIVE_PROPORTION))
      ),
      
      PROTECTED_AREA_HA           = coalesce(PROTECTED_AREA_HA, 0.0),
      PA_AREA_HA                  = coalesce(PA_AREA_HA, 0.0),
      OECM_AREA_HA                = coalesce(OECM_AREA_HA, 0.0),
      CRITICAL_HABITAT_HA         = coalesce(CRITICAL_HABITAT_HA, 0.0),
      CH_ENDANGERED_HA            = coalesce(CH_ENDANGERED_HA, 0.0),
      CH_THREATENED_HA            = coalesce(CH_THREATENED_HA, 0.0),
      CRITICAL_HABITAT_PROPORTION = coalesce(CRITICAL_HABITAT_PROPORTION, 0.0),
      
      PA_PROPORTION               = pmin(1.0, PA_AREA_HA / KBA_TOTAL_AREA_HA),
      OECM_PROPORTION             = pmin(1.0, OECM_AREA_HA / KBA_TOTAL_AREA_HA),
      CH_ENDANGERED_PROPORTION    = pmin(1.0, CH_ENDANGERED_HA / KBA_TOTAL_AREA_HA),
      CH_THREATENED_PROPORTION    = pmin(1.0, CH_THREATENED_HA / KBA_TOTAL_AREA_HA)
    ) %>%
    select(-calc_prop_protected) %>%
    st_transform(4326)
  
  # GEOMETRY SIMPLIFICATION FOR MAP RENDERING
  message("Simplifying background map layers (CPCAD and CH) for UI responsiveness...")
  cpcad_map_sf <- sf::st_simplify(cpcad_sf, dTolerance = 100, preserveTopology = TRUE)
  ch_map_sf    <- sf::st_simplify(ch_sf, dTolerance = 100, preserveTopology = TRUE)
  
  # Transform background and overlap layers to WGS84 for Leaflet
  cpcad_optimized      <- cpcad_map_sf %>% st_transform(4326)
  ch_optimized         <- ch_map_sf %>% st_transform(4326)
  overlap_layer_4326   <- intersection_sf %>% st_transform(4326)
  ch_kba_overlaps_4326 <- ch_kba_intersection_sf %>% st_transform(4326)
  
  # Assemble Cache Payload
  output_payload <- list(
    kba_layer           = kba_compiled,
    cpcad_layer         = cpcad_optimized,
    ch_layer            = ch_optimized,
    cpcad_overlaps      = overlap_layer_4326,
    ch_kba_overlaps     = ch_kba_overlaps_4326,
    national_cpcad_km2  = national_cpcad_km2,
    national_ch_km2     = national_ch_km2,
    cpcad_prov_summary  = cpcad_prov_summary,
    ch_prov_summary     = ch_prov_summary,
    timestamp           = Sys.time()
  )
  
  # Ensure destination directory exists before writing file
  dir_name <- dirname(output_path)
  if (!dir.exists(dir_name)) {
    dir.create(dir_name, recursive = TRUE)
  }
  
  # Write compressed .rds file to working directory path
  saveRDS(output_payload, file = output_path, compress = "xz")
  
  message("--- Sync Complete. Patched cache saved directly to ", output_path, " ---")
  
  return(output_payload)
}
