# Compute per-parcel developable land for Hampshire College parcels.
#
# Output:
#   data/parcels_developable.geojson   — parcels with added columns:
#       gross_acres
#       net_unconstrained_acres   (gross minus wetlands minus permanent open space)
#       net_easy_acres            (net unconstrained minus wetland buffer minus priority habitat)
#       pct_developable           (net_easy / gross * 100)
#       frontage_ft               (length of parcel boundary within ~30 ft of a MassDOT road)
#       frontage_roads            (semi-colon list of street names contributing frontage)
#
# Run from project root:
#   Rscript data-prep/compute_developable.R

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

PARCELS_PATH   <- "data/hampshire_college_parcels_combined.geojson"
WETLANDS_PATH  <- "data/dep_wetlands.geojson"
OPENSPACE_PATH <- "data/protected_openspace.geojson"
HABITAT_PATH   <- "data/nhesp_priority_habitats.geojson"
ROADS_PATH     <- "data/massdot_roads.geojson"

# Conversion / parameter constants
FT_PER_M     <- 3.28084
M_PER_FT     <- 1 / FT_PER_M
SQM_PER_ACRE <- 4046.8564224

HALF_ROW_M          <- 30 * M_PER_FT   # half-ROW proxy for frontage detection (~9.14 m)
WET_BUFFER_M        <- 100 * M_PER_FT  # WPA Buffer Zone (~30.48 m) — standard scenario
WET_BUFFER_STRICT_M <- 200 * M_PER_FT  # conservative scenario (~Riverfront Area + local no-disturb)

# Massachusetts State Plane Mainland (m), good for area + length math
CRS_MA <- 26986

# --- Load ---
message("Loading inputs...")
parcels   <- st_read(PARCELS_PATH,   quiet = TRUE) |> st_transform(CRS_MA) |> st_make_valid()
wetlands  <- st_read(WETLANDS_PATH,  quiet = TRUE) |> st_transform(CRS_MA) |> st_make_valid()
openspace <- st_read(OPENSPACE_PATH, quiet = TRUE) |> st_transform(CRS_MA) |> st_make_valid()
habitat   <- st_read(HABITAT_PATH,   quiet = TRUE) |> st_transform(CRS_MA) |> st_make_valid()
roads     <- st_read(ROADS_PATH,     quiet = TRUE) |> st_transform(CRS_MA)

# Only treat permanently protected open space as an absolute exclusion. Land
# with limited / no formal protection is a softer constraint we don't subtract.
os_perm <- openspace[!is.na(openspace$LEV_PROT) & openspace$LEV_PROT == "P", ]
message(sprintf("  %d parcels, %d wetlands, %d permanent open-space polys, %d habitat polys, %d roads",
                nrow(parcels), nrow(wetlands), nrow(os_perm), nrow(habitat), nrow(roads)))

# --- Build constraint geometries (unioned MULTIPOLYGONs for fast diff) ---
message("Building constraint unions...")
abs_excl          <- st_union(c(st_geometry(wetlands), st_geometry(os_perm)))
wet_buffer        <- st_union(st_buffer(st_geometry(wetlands), WET_BUFFER_M))
wet_buffer_strict <- st_union(st_buffer(st_geometry(wetlands), WET_BUFFER_STRICT_M))
reg_burden        <- st_union(c(wet_buffer, st_geometry(habitat)))
reg_burden_strict <- st_union(c(wet_buffer_strict, st_geometry(habitat)))

# --- Per-parcel: areas after differences ---
message("Computing per-parcel developable areas...")
parcels$gross_acres <- as.numeric(st_area(parcels)) / SQM_PER_ACRE

# st_difference of a length-n sf against a length-1 geometry returns length-n
g_net_unconstrained <- st_difference(st_geometry(parcels), abs_excl)
parcels$net_unconstrained_acres <- as.numeric(st_area(g_net_unconstrained)) / SQM_PER_ACRE

g_net_easy <- st_difference(g_net_unconstrained, reg_burden)
parcels$net_easy_acres <- as.numeric(st_area(g_net_easy)) / SQM_PER_ACRE

g_net_easy_strict <- st_difference(g_net_unconstrained, reg_burden_strict)
parcels$net_easy_strict_acres <- as.numeric(st_area(g_net_easy_strict)) / SQM_PER_ACRE

parcels$pct_developable <- 100 * parcels$net_easy_acres / pmax(parcels$gross_acres, 1e-9)
parcels$pct_developable <- pmin(100, pmax(0, parcels$pct_developable))

parcels$pct_developable_strict <- 100 * parcels$net_easy_strict_acres / pmax(parcels$gross_acres, 1e-9)
parcels$pct_developable_strict <- pmin(100, pmax(0, parcels$pct_developable_strict))

# --- Per-parcel: road frontage ---
message("Computing road frontage (this part is slower)...")

# Ramps (CLASS 6) don't confer frontage
roads_usable <- roads[!is.na(roads$CLASS) & roads$CLASS != 6, ]
road_corridor <- st_union(st_buffer(st_geometry(roads_usable), HALF_ROW_M))

# Per-parcel: walk boundary geometries explicitly via st_geometry() — sf's [i]
# is column-indexed, not row-indexed, so direct `boundaries[i]` would return
# the same value every iteration.
boundary_geoms <- st_geometry(st_boundary(parcels))
parcel_geoms   <- st_geometry(parcels)
parcels$frontage_ft    <- 0
parcels$frontage_roads <- ""

for (i in seq_len(nrow(parcels))) {
  bg <- boundary_geoms[i]
  pg <- parcel_geoms[i]

  hit <- tryCatch(st_intersection(bg, road_corridor), error = function(e) NULL)
  if (is.null(hit) || length(hit) == 0) next

  # st_intersection of a line vs a polygon can yield a GEOMETRYCOLLECTION
  # containing points where the line touches the polygon edge. Keep only the
  # line components so st_length() reflects real frontage.
  hit_lines <- tryCatch(st_collection_extract(hit, "LINESTRING"),
                        error = function(e) hit)
  parcels$frontage_ft[i] <- as.numeric(sum(st_length(hit_lines))) * FT_PER_M

  # Identify which road centerlines run within HALF_ROW_M of the parcel
  nearby_idx <- which(lengths(st_is_within_distance(
    roads_usable, pg, dist = HALF_ROW_M)) > 0)
  if (length(nearby_idx) > 0) {
    street_names <- unique(na.omit(roads_usable$STREETNAME[nearby_idx]))
    street_names <- street_names[nzchar(street_names)]
    parcels$frontage_roads[i] <- paste(sort(street_names), collapse = "; ")
  }
}
parcels$frontage_ft <- round(parcels$frontage_ft)

# --- Round numeric columns for tidy output ---
parcels$gross_acres             <- round(parcels$gross_acres, 2)
parcels$net_unconstrained_acres <- round(parcels$net_unconstrained_acres, 2)
parcels$net_easy_acres          <- round(parcels$net_easy_acres, 2)
parcels$net_easy_strict_acres   <- round(parcels$net_easy_strict_acres, 2)
parcels$pct_developable         <- round(parcels$pct_developable, 1)
parcels$pct_developable_strict  <- round(parcels$pct_developable_strict, 1)

# --- Summary ---
totals <- parcels |>
  st_drop_geometry() |>
  summarize(
    parcels             = n(),
    gross               = sum(gross_acres),
    net_unconstrained   = sum(net_unconstrained_acres),
    net_easy            = sum(net_easy_acres),
    net_easy_strict     = sum(net_easy_strict_acres),
    frontage_total_ft   = sum(frontage_ft)
  )
message(sprintf(
  "Totals: %d parcels | gross %.1f ac | net unconstrained %.1f ac | net easy %.1f ac (strict %.1f ac) | frontage %s ft",
  totals$parcels, totals$gross, totals$net_unconstrained, totals$net_easy,
  totals$net_easy_strict, format(totals$frontage_total_ft, big.mark = ",")
))

# --- Write out: per-parcel summary ---
out_path <- "data/parcels_developable.geojson"
parcels_wgs <- st_transform(parcels, 4326)
st_write(parcels_wgs, out_path, driver = "GeoJSON",
         delete_dsn = TRUE, quiet = TRUE)
message(sprintf("Wrote %s", out_path))

# --- Write out: developable-area polygons (the easy-to-build footprints) ---
# g_net_easy is an sfc with one geometry per parcel; some are empty when the
# parcel is fully constrained. Keep only non-empty ones, tag them with the
# parcel ID + acreage, and write a single GeoJSON the app can overlay.
write_dev_polys <- function(g, acres_col, out_basename, label) {
  keep <- !sf::st_is_empty(g)
  if (!any(keep)) return(invisible(NULL))
  dev_polys <- st_sf(
    MAP_PAR_ID     = parcels$MAP_PAR_ID[keep],
    TOWN           = parcels$TOWN[keep],
    net_easy_acres = parcels[[acres_col]][keep],
    geometry       = st_geometry(g)[keep],
    crs            = st_crs(parcels)
  ) |> st_make_valid()
  out_path <- file.path("data", out_basename)
  st_write(st_transform(dev_polys, 4326), out_path, driver = "GeoJSON",
           delete_dsn = TRUE, quiet = TRUE)
  message(sprintf("Wrote %s (%s; %d parcels with developable area)",
                  out_path, label, nrow(dev_polys)))
}

write_dev_polys(g_net_easy, "net_easy_acres",
                "parcels_developable_polys.geojson", "standard 100-ft buffer")
write_dev_polys(g_net_easy_strict, "net_easy_strict_acres",
                "parcels_developable_polys_strict.geojson", "strict 200-ft buffer")

message("Done.")
