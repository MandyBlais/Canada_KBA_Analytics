# ==========================================
# PART 1 - DATA ENGINE (Sanitized ASCII)
# ==========================================

library(arcgislayers) # Official Esri REST API connector
library(sf)
library(dplyr)

# Querying ONLY the official, published registry
KBA_API_URL   <- "https://gis.natureserve.ca/arcgis/rest/services/EBAR-KBA/KBA_Accepted_Sites/FeatureServer/0"
CPCAD_API_URL <- "https://maps-cartes.ec.gc.ca/arcgis/rest/services/CWS_SCF/CPCAD/MapServer/0"
CACHE_PATH    <- "data/cached_compiled_data.rds"

refresh_spatial_cache <- function() {
  
  message("--- Starting Data Update Sequence (Accepted KBAs Only) ---")
  
  
  
  message("Connecting to endpoints...")
  
  kba_client   <- arc_open(KBA_API_URL)
  
  cpcad_client <- arc_open(CPCAD_API_URL)
  
  
  
  # 1. Download accepted KBAs directly in EPSG:3978 from the server
  
  message("Downloading Accepted KBA features (Server-side EPSG:3978)...")
  
  kba_sf <- arc_select(kba_client, crs = 3978)
  
  message(paste("-> Successfully retrieved", nrow(kba_sf), "Accepted KBA site boundaries."))
  
  
  
  # 2. Create a formal spatial polygon box matching the server's projection
  
  message("Calculating spatial query envelopes in EPSG:3978...")
  
  kba_bbox_sf <- st_as_sfc(st_bbox(kba_sf))
  
  
  
  # 3. Pull CPCAD polygons directly in EPSG:3978 matching the envelope
  
  message("Streaming spatially filtered CPCAD features...")
  
  cpcad_sf <- arc_select(
    
    cpcad_client,
    
    filter_geom = kba_bbox_sf, 
    
    crs = 3978,
    
    fields = c("PARENT_ID", "ZONE_ID", "NAME_E", "IUCN_CAT", "PA_OECM_DF", "TYPE_E", "OWNER_E") 
    
  )
  
  message(paste("-> Successfully retrieved", nrow(cpcad_sf), "intersecting CPCAD records."))
  
  
  
  # 4. Clean geometry dimensions and repair topologies
  
  message("Validating, repairing, and flattening geometry dimensions...")
  
  kba_sf   <- kba_sf %>% st_make_valid() %>% st_zm(drop = TRUE)
  
  cpcad_sf <- cpcad_sf %>% st_make_valid() %>% st_zm(drop = TRUE)
  
  
  
  # Calculate baseline total area for every KBA (in Hectares) before intersection
  
  kba_sf$KBA_TOTAL_AREA_HA <- as.numeric(st_area(kba_sf) / 10000)
  
  
  
  # 5. Perform Geometric Intersection to capture spatial overlap shapes
  
  message("Calculating spatial intersections...")
  
  intersection_sf <- st_intersection(kba_sf, cpcad_sf)
  
  intersection_sf <- intersection_sf[!st_is_empty(intersection_sf), ]
  
  
  
  # Strictly extract ONLY polygon structures, discarding stray points/lines from edge-grazing
  
  if (nrow(intersection_sf) > 0) {
    
    message("Extracting pure polygon geometries and discarding stray intersection points...")
    
    intersection_sf <- st_collection_extract(intersection_sf, "POLYGON")
    
    # Clear out any empty geometries left behind by the extraction process
    
    intersection_sf <- intersection_sf[!st_is_empty(intersection_sf), ] 
    
  }
  
  
  
  # Maintain strict 2D polygon features (Double-safety check)
  
  if (nrow(intersection_sf) > 0) {
    
    intersection_sf <- intersection_sf[which(st_dimension(intersection_sf) == 2), ]
    
  }
  
  
  
  # 6. Aggregate cumulative protection statistics per individual KBA
  message("Compiling protection metrics...")
  if (nrow(intersection_sf) > 0) {
    
    # Calculate the area of the overlapping pieces in Hectares
    intersection_sf$OVERLAP_AREA_HA <- as.numeric(st_area(intersection_sf) / 10000)
    
    cumulative_protection <- intersection_sf %>%
      st_drop_geometry() %>%
      # Calculate the proportion of the parent KBA that this specific piece represents
      mutate(PROPORTION_PROTECTED = OVERLAP_AREA_HA / KBA_TOTAL_AREA_HA) %>% 
      group_by(kbasiteid) %>%  
      summarize(
        # Sum up all protected pieces inside this KBA, capping at 100% (1.0)
        CUMULATIVE_PROPORTION = min(1.0, sum(PROPORTION_PROTECTED, na.rm = TRUE)),
        .groups = "drop"
      )
  } else {
    # Baseline empty table structure fallback if absolutely zero intersections happen across Canada
    cumulative_protection <- tibble(kbasiteid = character(), CUMULATIVE_PROPORTION = numeric())
  }


# 7. Finalize Master KBA Layer (Ensures ALL accepted sites are retained via left_join)

kba_compiled <- kba_sf %>%
  
  left_join(cumulative_protection, by = "kbasiteid") %>% 
  
  mutate(CUMULATIVE_PROPORTION = coalesce(CUMULATIVE_PROPORTION, 0.0)) %>%
  
  st_transform(4326) # Project to WGS84 for Leaflet Map



# 8. Optimize CPCAD layer payload size for fast map rendering

intersected_cpcad_ids <- unique(intersection_sf$ZONE_ID)

cpcad_optimized <- cpcad_sf %>%
  
  filter(ZONE_ID %in% intersected_cpcad_ids) %>%
  
  st_transform(4326)



# Preserve the actual spatial intersection shapes for mapping overlays

overlap_layer_4326 <- intersection_sf %>% st_transform(4326)



# 9. Save App-Ready Payload

output_payload <- list(
  
  kba_layer   = kba_compiled,       # Every single official KBA site (Unprotected sites = 0.0)
  
  cpcad_layer = cpcad_optimized,     # Lightweight CPCAD base outlines intersecting our target set
  
  overlaps    = overlap_layer_4326,  # Precise geometric overlap shapes + attribute tables
  
  timestamp   = Sys.time()
  
)



if(!dir.exists("data")) dir.create("data")

saveRDS(output_payload, CACHE_PATH)

message("--- Sync Complete. Accepted KBA master cache refreshed. ---")

}
