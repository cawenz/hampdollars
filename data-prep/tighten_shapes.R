# Tighten the hand-drawn "Hampshire Next" vision shapes so neighbouring areas
# meet cleanly and conservation hugs the Hampshire College parcel lines:
#   1. vertex-snap every area onto the parcel lines (+ each other) within TOL m
#   2. priority pass (identity-preserving): Core Campus > Development > Conservation
#        - removes overlaps (e.g. conservation bleeding into the development parcel)
#        - closes slivers/gaps narrower than ~2*TOL between adjacent areas
#        - leaves large intentional blank spaces open (morphological-closing mask)
# Each original KML feature stays a separate feature (names/ids preserved).
# Writes cleaned per-category GeoJSON + a before/after preview PNG + metrics.
#
#   Rscript data-prep/tighten_shapes.R [tolerance_m]
suppressPackageStartupMessages({ library(sf); library(ggplot2) })

args   <- commandArgs(trailingOnly = TRUE)
TOL    <- if (length(args) >= 1) as.numeric(args[1]) else 20  # metres (snap + max gap to close)
PFILL  <- if (length(args) >= 2) as.numeric(args[2]) else 45  # metres: reach for extending Development to parcel lines
CRS_MA <- 26986
SQM_AC <- 4046.8564224
KML    <- "data/concepts/hampshire_next.kml"
OUT    <- "data/concepts/clean"; dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
ac     <- function(g) sum(as.numeric(st_area(g))) / SQM_AC
poly_only <- function(p) {
  p <- st_make_valid(p)
  p <- tryCatch(st_collection_extract(p, "POLYGON"), error = function(e) p)
  st_union(p)
}

rd <- function(lyr, category, prio) {
  g <- st_make_valid(st_zm(st_transform(st_read(KML, layer = lyr, quiet = TRUE), CRS_MA)))
  g <- g[st_geometry_type(g) %in% c("POLYGON", "MULTIPOLYGON"), ]
  nm <- if ("Name" %in% names(g)) g$Name else paste0(category, " ", seq_len(nrow(g)))
  st_sf(Name = trimws(ifelse(is.na(nm), category, nm)), category = category, prio = prio,
        geometry = st_geometry(g))
}
# priority: who keeps its ground where areas overlap / who absorbs a closed gap
dev  <- rd("Development",                          "Development",  2)
cons <- rd("Conservation - trails + habitat",      "Conservation", 1)
core <- rd("Hampshire Next - Retained facilities", "Core Campus",  3)
all  <- rbind(dev, cons, core)
orig_geo <- st_geometry(all)        # untouched hand-drawn shapes (for "leave original" regions)

parc   <- st_make_valid(st_transform(st_read("data/hampshire_college_parcels_combined.geojson", quiet = TRUE), CRS_MA))
parc_b <- st_union(st_boundary(st_union(st_geometry(parc))))

# ---- 1) vertex-snap onto parcel lines + the other areas' boundaries ---------
snap_tgt <- st_union(c(parc_b, st_union(st_boundary(st_geometry(all)))))
st_geometry(all) <- st_make_valid(st_snap(st_geometry(all), snap_tgt, tolerance = TOL))

area0 <- as.numeric(st_area(st_geometry(all)))             # snapped area, for bloat report
ovl <- function(a, b) {
  i <- suppressWarnings(st_intersection(st_union(st_geometry(a)), st_union(st_geometry(b))))
  if (length(i) == 0) 0 else ac(i)
}
cd_before <- ovl(all[all$category == "Conservation", ], all[all$category == "Development", ])

# ---- 1b) extend Development to nearby parcel lines (parcel = the divider) ----
# Pull each Development area out to a parcel line within PFILL, but ONLY within
# that area's own parcels and own town -- so it never jumps Bay Road / the town
# line into a foreign parcel, and never floods open conservation.
PU      <- st_geometry(parc)
twn     <- st_make_valid(st_transform(st_read("data/amherst_hadley.geojson", quiet = TRUE), CRS_MA))
amh     <- st_union(st_geometry(twn[grepl("Amh", twn$town, ignore.case = TRUE), ]))
had     <- st_union(st_geometry(twn[grepl("Had", twn$town, ignore.case = TRUE), ]))
town_of <- function(g) {
  a <- suppressWarnings(st_intersection(g, amh)); h <- suppressWarnings(st_intersection(g, had))
  if ((if (length(a)) ac(a) else 0) >= (if (length(h)) ac(h) else 0)) amh else had
}
isd <- all$category == "Development"
geo <- st_geometry(all); lst <- lapply(geo, identity)
flip_dev_before <- ac(st_union(geo[isd]))
for (i in which(isd)) {
  di    <- geo[i]
  homep <- st_union(PU[lengths(st_intersects(PU, di)) > 0])         # parcels this area sits in
  add   <- poly_only(suppressWarnings(
             st_intersection(st_intersection(st_intersection(
               st_buffer(di, PFILL), st_buffer(parc_b, PFILL)), homep), town_of(di))))
  lst[[i]] <- st_geometry(poly_only(st_union(c(di, add))))[[1]]
}
st_geometry(all) <- st_sfc(lst, crs = CRS_MA)

# ---- 2) overlap resolution (UNBUFFERED -> no growth into neighbours) ---------
# Higher priority keeps its ground; lower yields exactly along the true border.
geo     <- st_geometry(all); prio <- all$prio
ord     <- order(-prio, -as.numeric(st_area(geo)))
claimed <- NULL; newgeo <- lapply(geo, identity)
for (k in ord) {
  piece <- geo[k]
  if (!is.null(claimed)) piece <- poly_only(suppressWarnings(st_difference(piece, claimed)))
  if (length(piece) == 0 || all(st_is_empty(piece))) piece <- geo[k]
  newgeo[[k]] <- st_geometry(piece)[[1]]
  claimed <- if (is.null(claimed)) geo[k] else st_union(claimed, geo[k])
}
st_geometry(all) <- st_sfc(newgeo, crs = CRS_MA)

# ---- 3) close only the thin slivers; assign each to its top-priority neighbour
geo      <- st_geometry(all)
assembly <- st_union(geo)
gapmask  <- st_buffer(st_buffer(assembly, TOL), -TOL)      # morphological closing
gaps     <- poly_only(suppressWarnings(st_difference(gapmask, assembly)))
if (length(gaps) && !all(st_is_empty(gaps))) {
  parts  <- suppressWarnings(st_cast(gaps, "POLYGON"))
  newgeo <- lapply(geo, identity)
  gb     <- st_buffer(geo, 0.5)
  for (p in seq_along(parts)) {
    touch <- which(lengths(st_intersects(gb, parts[p])) > 0)
    if (!length(touch)) next
    best  <- touch[order(-prio[touch], -as.numeric(st_area(geo[touch])))][1]
    merged <- st_union(c(st_sfc(newgeo[[best]], crs = CRS_MA), parts[p]))
    newgeo[[best]] <- st_geometry(poly_only(merged))[[1]]
  }
  st_geometry(all) <- st_sfc(newgeo, crs = CRS_MA)
}
flip_ac <- ac(st_union(st_geometry(all)[isd])) - flip_dev_before

# ---- metrics AFTER ----------------------------------------------------------
cd_after  <- ovl(all[all$category == "Conservation", ], all[all$category == "Development", ])
gap_after <- { d <- suppressWarnings(st_difference(gapmask, st_union(st_geometry(all)))); if (length(d)) ac(d) else 0 }
bloat <- (sum(as.numeric(st_area(st_geometry(all)))) - sum(area0)) / 4046.8564224
cat(sprintf("TOL = %g m\n", TOL))
for (cat_i in c("Development", "Conservation", "Core Campus"))
  cat(sprintf("  %-13s %.0f ac across %d areas\n", cat_i,
              ac(all[all$category == cat_i, ]), sum(all$category == cat_i)))
cat(sprintf("  conservation-into-development overlap: %.2f ac -> %.2f ac\n", cd_before, cd_after))
cat(sprintf("  development grown by parcel-line fill (PFILL=%g m): %.2f ac\n", PFILL, flip_ac))
cat(sprintf("  net area change vs snapped originals (bloat check): %+.1f ac\n", bloat))
cat(sprintf("  remaining gap area inside the assembly: %.2f ac\n", gap_after))

# ---- leave-original regions: restore untouched hand-drawn shapes ------------
# Areas whose centre falls inside a KEEP box keep their original geometry (the
# tightening looked wrong there). Bay Road / Lower Hadley corner:
KEEP <- list(st_bbox(c(xmin = -72.552, ymin = 42.313, xmax = -72.534, ymax = 42.323), crs = 4326))
keepg <- st_union(do.call(c, lapply(KEEP, function(b) st_transform(st_as_sfc(b), CRS_MA))))
pts   <- suppressWarnings(st_point_on_surface(st_geometry(all)))
inkeep <- lengths(st_intersects(pts, keepg)) > 0
if (any(inkeep)) {
  cur <- st_geometry(all)
  lst <- lapply(seq_along(cur), function(i) if (inkeep[i]) orig_geo[[i]] else cur[[i]])
  st_geometry(all) <- st_sfc(lst, crs = CRS_MA)
  cat(sprintf("  reverted %d area(s) to original (Bay Road corner): %s\n",
              sum(inkeep), paste(all$Name[inkeep], collapse = "; ")))
}

# ---- write cleaned GeoJSON per category (feature order preserved) -----------
write_clean <- function(category, file) {
  g84 <- st_transform(all[all$category == category, c("Name", "category")], 4326)
  st_write(g84, file.path(OUT, file), delete_dsn = TRUE, quiet = TRUE)
}
write_clean("Development",  "development.geojson")
write_clean("Conservation", "conservation.geojson")
write_clean("Core Campus",  "core_campus.geojson")
cat("Wrote cleaned GeoJSON to", OUT, "\n")

# ---- before/after preview ---------------------------------------------------
to84 <- function(g) st_transform(st_geometry(g), 4326)
COL  <- c(Development = "#C0392B", Conservation = "#2E7D32", `Core Campus` = "#607D8B")
preview <- ggplot() +
  geom_sf(data = to84(parc), fill = NA, color = "#111", linewidth = 0.3, linetype = "22") +
  geom_sf(data = to84(cons), fill = NA, color = "#000", linewidth = 0.25) +   # original conservation outline
  geom_sf(data = st_transform(all[, "category"], 4326), aes(fill = category, color = category),
          alpha = 0.45, linewidth = 0.4) +
  scale_fill_manual(values = COL) + scale_color_manual(values = COL) +
  labs(title = sprintf("Cleaned areas (fill) vs original conservation (black) & parcels (dashed) - TOL %g m", TOL)) +
  theme_minimal(base_size = 9)
ggsave("data-prep/tighten_preview.png", preview, width = 11, height = 9, dpi = 110)
cat("Wrote data-prep/tighten_preview.png\n")
