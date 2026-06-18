# Fetch MassGIS context layers, clipped to a buffered Hampshire-College bbox,
# and write compact GeoJSON to data/ for the Land context tab.
#
# Run from the project root:
#   Rscript data-prep/fetch_massgis_layers.R
#
# Re-run any time you want to refresh the local snapshots. Each layer is
# fetched fresh from MassGIS ArcGIS Online or arcgisserver.digital.mass.gov.

suppressPackageStartupMessages({
  library(sf)
})

PARCELS_PATH <- "data/hampshire_college_parcels_combined.geojson"
OUT_DIR      <- "data"
BUFFER_DEG   <- 0.05   # ~3.5 mi at this latitude; covers campus + near-context

if (!file.exists(PARCELS_PATH)) {
  stop("Parcels file not found at ", PARCELS_PATH,
       " — run this script from the project root.")
}

parcels <- sf::st_read(PARCELS_PATH, quiet = TRUE) |> sf::st_transform(4326)
bb <- sf::st_bbox(parcels)
bb_buf <- c(
  xmin = unname(bb["xmin"]) - BUFFER_DEG,
  ymin = unname(bb["ymin"]) - BUFFER_DEG,
  xmax = unname(bb["xmax"]) + BUFFER_DEG,
  ymax = unname(bb["ymax"]) + BUFFER_DEG
)
message(sprintf("Querying bbox: %.4f, %.4f, %.4f, %.4f",
                bb_buf["xmin"], bb_buf["ymin"], bb_buf["xmax"], bb_buf["ymax"]))

# Two MassGIS hosts:
#   AGOL_HOSTED — services1.arcgis.com (MassGIS-owned AGOL feature services)
#   STATE_HOST  — arcgisserver.digital.mass.gov (state ArcGIS Server)
AGOL_HOSTED <- "https://services1.arcgis.com/hGdibHYSPO59RG1h/arcgis/rest/services"
STATE_HOST  <- "https://arcgisserver.digital.mass.gov/arcgisserver/rest/services/AGOL"

fetch_layer <- function(host, service, layer_id, out_basename, label) {
  out_path <- file.path(OUT_DIR, out_basename)
  url <- sprintf(
    paste0("%s/%s/FeatureServer/%d/query?",
           "where=1%%3D1&outFields=*",
           "&geometryType=esriGeometryEnvelope",
           "&geometry=%f,%f,%f,%f",
           "&inSR=4326&outSR=4326&f=geojson"),
    host, service, layer_id,
    bb_buf["xmin"], bb_buf["ymin"], bb_buf["xmax"], bb_buf["ymax"]
  )

  message(sprintf("Fetching %s ...", label))
  x <- tryCatch(sf::st_read(url, quiet = TRUE),
                error = function(e) {
                  message("  FAILED: ", conditionMessage(e))
                  NULL
                })
  if (is.null(x)) return(invisible(NULL))

  n <- nrow(x)
  message(sprintf("  %d features → %s", n, out_path))
  if (n >= 2000) {
    message("  NOTE: hit the 2000-record ceiling — may need pagination.")
  }

  x <- sf::st_make_valid(x)
  sf::st_write(x, out_path, driver = "GeoJSON",
               delete_dsn = TRUE, quiet = TRUE)
  invisible(x)
}

# MassDOT Roads (statewide road centerlines with class + name).
fetch_layer(AGOL_HOSTED, "MassDOTRoads_gdb", 0,
            "massdot_roads.geojson",
            "MassDOT Roads (centerlines)")

# Building structures (2-D footprints from MassGIS).
fetch_layer(STATE_HOST, "BuildingStructures2D", 0,
            "buildings.geojson",
            "Building Structures (2-D)")

# DEP Wetland Areas (regulatory polygons; not the linear features layer).
fetch_layer(AGOL_HOSTED, "DEP_Wetlands", 1,
            "dep_wetlands.geojson",
            "DEP Wetlands (regulatory polygons)")

# Protected & Recreational Open Space — combines public + private + CR with
# rich attributes (SITE_NAME, OWNER_TYPE, LEV_PROT, FEESYM, etc.).
fetch_layer(STATE_HOST, "openspace", 0,
            "protected_openspace.geojson",
            "Protected & Recreational Open Space")

# NHESP Priority Habitats of Rare Species (MESA-regulated areas).
fetch_layer(STATE_HOST, "NHESP_Priority_Habitats", 0,
            "nhesp_priority_habitats.geojson",
            "NHESP Priority Habitat")

# Areas of Critical Environmental Concern.
fetch_layer(STATE_HOST, "ACECs", 0,
            "acecs.geojson",
            "Areas of Critical Environmental Concern")

# NHESP Certified Vernal Pools (points) — protected under Amherst's local
# Wetlands Protection Bylaw beyond what the state WPA covers.
fetch_layer(STATE_HOST, "NHESP_Certified_Vernal_Pools", 0,
            "vernal_pools_certified.geojson",
            "NHESP Certified Vernal Pools")

# NHESP Potential Vernal Pools (points) — photo-interpreted, not yet certified.
fetch_layer(STATE_HOST, "NHESP_Potential_Vernal_Pools", 0,
            "vernal_pools_potential.geojson",
            "NHESP Potential Vernal Pools")

# Wellhead Protection — Zone II (approved) + IWPA (interim).
# Layer 0 = Zone II polygons, Layer 1 = IWPA polygons. Fetch both, tag with
# the source layer, then concatenate so the app can render them together.
zone2 <- fetch_layer(STATE_HOST, "IWPA_Zone2", 0,
                     "wellhead_zone2_only.geojson",
                     "Wellhead Protection — Zone II")
iwpa  <- fetch_layer(STATE_HOST, "IWPA_Zone2", 1,
                     "wellhead_iwpa_only.geojson",
                     "Wellhead Protection — IWPA (interim)")

if (!is.null(zone2) || !is.null(iwpa)) {
  combine_safely <- function(a, b) {
    if (is.null(a)) return(b)
    if (is.null(b)) return(a)
    common <- intersect(names(a), names(b))
    rbind(a[, common, drop = FALSE], b[, common, drop = FALSE])
  }
  if (!is.null(zone2)) zone2$ZONE_TYPE <- "Zone II (approved)"
  if (!is.null(iwpa))  iwpa$ZONE_TYPE  <- "IWPA (interim)"
  combined <- combine_safely(zone2, iwpa)
  combined <- sf::st_make_valid(combined)
  out <- file.path(OUT_DIR, "wellhead_protection.geojson")
  sf::st_write(combined, out, driver = "GeoJSON",
               delete_dsn = TRUE, quiet = TRUE)
  message(sprintf("Combined wellhead protection: %d features → %s",
                  nrow(combined), out))
  # Clean up the intermediate per-layer files
  file.remove(file.path(OUT_DIR, "wellhead_zone2_only.geojson"))
  file.remove(file.path(OUT_DIR, "wellhead_iwpa_only.geojson"))
}

# Remove old per-layer files that the new openspace layer supersedes.
for (f in c("data/public_conservation_lands.geojson",
            "data/conservation_restrictions.geojson")) {
  if (file.exists(f)) {
    file.remove(f)
    message("Removed superseded file: ", f)
  }
}

message("Done.")
