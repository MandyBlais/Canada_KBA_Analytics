# ==========================================
# PART 1 - DATA ENGINE (FIXED CASE MATCHING & CATEGORY PARSING)
# ==========================================

library(arcgislayers)
library(sf)
library(dplyr)
library(tidyr)

KBA_API_URL   <- "https://gis.natureserve.ca/arcgis/rest/services/EBAR-KBA/KBA_Accepted_Sites/FeatureServer/0"
CPCAD_API_URL <- "https://maps-cartes.ec.gc.ca/arcgis/rest/services/CWS_SCF/CPCAD/MapServer/0"
CH_API_URL    <- "https://maps-cartes.ec.gc.ca/arcgis/rest/services/CWS_SCF/CriticalHabitat/MapServer/3"
CACHE_PATH    <- "data/cached_compiled_data.rds"

crs_esri_102008 <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

refresh_spatial_cache <- function(output_path = CACHE_PATH) {
  
  message("--- Starting Data Update Sequence (KBAs, CPCAD, Critical Habitat) ---")
  
  message("Connecting to endpoints...")
  kba_client   <- arc_open(KBA_API_URL)
  cpcad_client <- arc_open(CPCAD_API_URL)
  ch_client    <- arc_open(CH_API_URL)
  
  # 1. Download accepted KBAs
  message("Downloading Accepted KBA features...")
  kba_sf <- arc_select(kba_client, crs = 3978) %>%
    rename_with(tolower) %>%
    st_transform(crs_esri_102008) %>%
    st_zm(drop = TRUE) %>%
    st_make_valid()
  
  # Ensure kbasiteid exists regardless of API source casing
  if (!"kbasiteid" %in% colnames(kba_sf)) {
    possible_id_cols <- c("siteid", "kba_site_id", "objectid")
    match_col <- possible_id_cols[possible_id_cols %in% colnames(kba_sf)][1]
    if (!is.na(match_col)) {
      kba_sf <- kba_sf %>% rename(kbasiteid = !!sym(match_col))
    }
  }
  
  kba_sf <- kba_sf %>%
    mutate(
      kbasiteid = trimws(as.character(kbasiteid)),
      kba_total_area_ha = as.numeric(st_area(.)) / 10000
    )
  
  # 2. Download ALL CPCAD features
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
    mutate(cpcad_total_area_ha = as.numeric(st_area(.)) / 10000)
  
  # 3. Download ALL Critical Habitat features
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
    mutate(ch_total_area_ha = as.numeric(st_area(.)) / 10000)
  
  # Pre-calculate dissolved national and regional summaries
  message("Pre-calculating dissolved summaries...")
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
    summarize(geometry = st_union(st_combine(geometry)), .groups = "drop") %>%
    mutate(cpcad_km2 = round(as.numeric(st_area(geometry)) / 1e6, 1)) %>%
    st_drop_geometry()
  
  national_cpcad_km2 <- sum(cpcad_prov_summary$cpcad_km2, na.rm = TRUE)
  
  ch_prov_summary <- ch_sf %>%
    tidyr::separate_rows(ProvTerr_E, sep = ";") %>%
    mutate(
      RAW_JUR = toupper(trimws(as.character(ProvTerr_E))),
      JUR_CLEAN = case_when(
        RAW_JUR %in% c("ON", "ONTARIO", "35")                    ~ "Ontario",
        RAW_JUR %in% c("BC", "BRITISH COLUMBIA", "59")           ~ "British Columbia",
        RAW_JUR %in% c("AB", "ALBERTA", "48")                    ~ "Alberta",
        RAW_JUR %in% c("QC", "QUEBEC", "24")                     ~ "Quebec",
        RAW_JUR %in% c("SK", "SASKATCHEWAN", "47")              ~ "Saskatchewan",
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
    summarize(geometry = st_union(st_combine(geometry)), .groups = "drop") %>%
    mutate(ch_km2 = round(as.numeric(st_area(geometry)) / 1e6, 1)) %>%
    st_drop_geometry()
  
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
  
  # 7. Protection Metrics Calculation (WITH DISSOLVE TO REMOVE OVERLAPS)
  if (nrow(intersection_sf) > 0) {
    
    intersection_sf <- intersection_sf %>%
      mutate(clean_pa_oecm_df = trimws(as.character(PA_OECM_DF)))
    
    # Combined Overall Protection Across All Categories (Dissolved per KBA)
    kba_protection_metrics <- intersection_sf %>%
      group_by(kbasiteid) %>%
      summarize(geometry = st_union(st_combine(geometry)), .groups = "drop") %>%
      mutate(protected_area_ha = as.numeric(st_area(geometry) / 10000)) %>%
      st_drop_geometry() %>%
      left_join(kba_sf %>% st_drop_geometry() %>% select(kbasiteid, kba_total_area_ha), by = "kbasiteid") %>%
      mutate(cumulative_proportion = pmin(1.0, protected_area_ha / kba_total_area_ha)) %>%
      select(kbasiteid, protected_area_ha, cumulative_proportion)
    
    # Helper: Dissolves overlapping polygons per category prior to computing area
    calc_cpcad_cat_area <- function(df, cat_code) {
      df %>%
        filter(clean_pa_oecm_df == as.character(cat_code)) %>%
        group_by(kbasiteid) %>%
        summarize(geometry = st_union(st_combine(geometry)), .groups = "drop") %>%
        mutate(area_ha = as.numeric(st_area(geometry) / 10000)) %>%
        st_drop_geometry()
    }
    
    cat1_ha <- calc_cpcad_cat_area(intersection_sf, "1") %>% rename(cpcad_1_area_ha = area_ha)
    cat2_ha <- calc_cpcad_cat_area(intersection_sf, "2") %>% rename(cpcad_2_area_ha = area_ha)
    cat3_ha <- calc_cpcad_cat_area(intersection_sf, "3") %>% rename(cpcad_3_area_ha = area_ha)
    cat4_ha <- calc_cpcad_cat_area(intersection_sf, "4") %>% rename(cpcad_4_area_ha = area_ha)
    cat5_ha <- calc_cpcad_cat_area(intersection_sf, "5") %>% rename(cpcad_5_area_ha = area_ha)
    
    kba_pa_metrics <- intersection_sf %>%
      filter(clean_pa_oecm_df %in% c("1", "3")) %>%
      group_by(kbasiteid) %>%
      summarize(geometry = st_union(st_combine(geometry)), .groups = "drop") %>%
      mutate(pa_area_ha = as.numeric(st_area(geometry) / 10000)) %>%
      st_drop_geometry()
    
    kba_oecm_metrics <- intersection_sf %>%
      filter(clean_pa_oecm_df %in% c("2", "4")) %>%
      group_by(kbasiteid) %>%
      summarize(geometry = st_union(st_combine(geometry)), .groups = "drop") %>%
      mutate(oecm_area_ha = as.numeric(st_area(geometry) / 10000)) %>%
      st_drop_geometry()
    
    kba_protection_metrics <- kba_protection_metrics %>%
      left_join(kba_pa_metrics, by = "kbasiteid") %>%
      left_join(kba_oecm_metrics, by = "kbasiteid") %>%
      left_join(cat1_ha, by = "kbasiteid") %>%
      left_join(cat2_ha, by = "kbasiteid") %>%
      left_join(cat3_ha, by = "kbasiteid") %>%
      left_join(cat4_ha, by = "kbasiteid") %>%
      left_join(cat5_ha, by = "kbasiteid")
  } else {
    kba_protection_metrics <- tibble(
      kbasiteid = character(),
      protected_area_ha = numeric(),
      cumulative_proportion = numeric(),
      pa_area_ha = numeric(),
      oecm_area_ha = numeric(),
      cpcad_1_area_ha = numeric(),
      cpcad_2_area_ha = numeric(),
      cpcad_3_area_ha = numeric(),
      cpcad_4_area_ha = numeric(),
      cpcad_5_area_ha = numeric()
    )
  }
  
  # 8. Critical Habitat Metrics (WITH DISSOLVE TO REMOVE OVERLAPS)
  if (nrow(ch_kba_intersection_sf) > 0) {
    kba_ch_metrics <- ch_kba_intersection_sf %>%
      group_by(kbasiteid) %>%
      summarize(geometry = st_union(st_combine(geometry)), .groups = "drop") %>%
      mutate(critical_habitat_ha = as.numeric(st_area(geometry) / 10000)) %>%
      st_drop_geometry() %>%
      left_join(kba_sf %>% st_drop_geometry() %>% select(kbasiteid, kba_total_area_ha), by = "kbasiteid") %>%
      mutate(critical_habitat_proportion = pmin(1.0, critical_habitat_ha / kba_total_area_ha)) %>%
      select(kbasiteid, critical_habitat_ha, critical_habitat_proportion)
    
    kba_ch_endangered <- ch_kba_intersection_sf %>%
      filter(as.character(SARA_Status) == "2") %>%
      group_by(kbasiteid) %>%
      summarize(geometry = st_union(st_combine(geometry)), .groups = "drop") %>%
      mutate(ch_endangered_ha = as.numeric(st_area(geometry) / 10000)) %>%
      st_drop_geometry()
    
    kba_ch_threatened <- ch_kba_intersection_sf %>%
      filter(as.character(SARA_Status) == "3") %>%
      group_by(kbasiteid) %>%
      summarize(geometry = st_union(st_combine(geometry)), .groups = "drop") %>%
      mutate(ch_threatened_ha = as.numeric(st_area(geometry) / 10000)) %>%
      st_drop_geometry()
    
    kba_ch_metrics <- kba_ch_metrics %>%
      left_join(kba_ch_endangered, by = "kbasiteid") %>%
      left_join(kba_ch_threatened, by = "kbasiteid")
  } else {
    kba_ch_metrics <- tibble(
      kbasiteid = character(),
      critical_habitat_ha = numeric(),
      critical_habitat_proportion = numeric(),
      ch_endangered_ha = numeric(),
      ch_threatened_ha = numeric()
    )
  }
  
  # 9. COMPILE & PATCH KBA ATTRIBUTE TABLE
  message("Cleaning and patching KBA attribute table...")
  
  kba_compiled <- kba_sf %>%
    left_join(kba_protection_metrics, by = "kbasiteid") %>%
    left_join(kba_ch_metrics, by = "kbasiteid") %>%
    mutate(
      protected_area_ha           = coalesce(protected_area_ha, 0.0),
      pa_area_ha                  = coalesce(pa_area_ha, 0.0),
      oecm_area_ha                = coalesce(oecm_area_ha, 0.0),
      cpcad_1_area_ha             = coalesce(cpcad_1_area_ha, 0.0),
      cpcad_2_area_ha             = coalesce(cpcad_2_area_ha, 0.0),
      cpcad_3_area_ha             = coalesce(cpcad_3_area_ha, 0.0),
      cpcad_4_area_ha             = coalesce(cpcad_4_area_ha, 0.0),
      cpcad_5_area_ha             = coalesce(cpcad_5_area_ha, 0.0),
      
      critical_habitat_ha         = coalesce(critical_habitat_ha, 0.0),
      ch_endangered_ha            = coalesce(ch_endangered_ha, 0.0),
      ch_threatened_ha            = coalesce(ch_threatened_ha, 0.0),
      
      cumulative_proportion       = pmin(1.0, protected_area_ha / kba_total_area_ha),
      critical_habitat_proportion = pmin(1.0, critical_habitat_ha / kba_total_area_ha),
      
      pa_proportion               = pmin(1.0, pa_area_ha / kba_total_area_ha),
      oecm_proportion             = pmin(1.0, oecm_area_ha / kba_total_area_ha),
      
      cpcad_1_prop                = pmin(1.0, cpcad_1_area_ha / kba_total_area_ha),
      cpcad_2_prop                = pmin(1.0, cpcad_2_area_ha / kba_total_area_ha),
      cpcad_3_prop                = pmin(1.0, cpcad_3_area_ha / kba_total_area_ha),
      cpcad_4_prop                = pmin(1.0, cpcad_4_area_ha / kba_total_area_ha),
      cpcad_5_prop                = pmin(1.0, cpcad_5_area_ha / kba_total_area_ha),
      
      ch_endangered_proportion    = pmin(1.0, ch_endangered_ha / kba_total_area_ha),
      ch_threatened_proportion    = pmin(1.0, ch_threatened_ha / kba_total_area_ha)
    ) %>%
    st_transform(4326)
  
  # Simplification & Leaflet reprojection (with valid topology checks)
  cpcad_map_sf <- sf::st_simplify(cpcad_sf, dTolerance = 100, preserveTopology = TRUE) %>% st_make_valid()
  ch_map_sf    <- sf::st_simplify(ch_sf, dTolerance = 100, preserveTopology = TRUE) %>% st_make_valid()
  
  cpcad_optimized      <- cpcad_map_sf %>% st_transform(4326)
  ch_optimized         <- ch_map_sf %>% st_transform(4326)
  overlap_layer_4326   <- intersection_sf %>% st_transform(4326)
  ch_kba_overlaps_4326 <- ch_kba_intersection_sf %>% st_transform(4326)
  
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
  
  dir_name <- dirname(output_path)
  if (!dir.exists(dir_name)) dir.create(dir_name, recursive = TRUE)
  
  saveRDS(output_payload, file = output_path, compress = "xz")
  message("--- Sync Complete. Cache updated cleanly. ---")
  return(output_payload)
}