# Hampshire Next vision map — themed concept layers (from the Google Earth KML)
# plus DEP wetlands with toggleable 100 ft (state WPA) and 200 ft (Amherst rule)
# buffers. Writes a self-contained vision_map.html.
#
#   Rscript vision_map.R
suppressPackageStartupMessages({ library(sf); library(leaflet); library(dplyr); library(htmlwidgets) })

KML        <- "data/concepts/hampshire_next.kml"
WETLANDS   <- "data/dep_wetlands.geojson"
PARCELS    <- "data/hampshire_college_parcels_combined.geojson"
CRS_MA     <- 26986
FT_TO_M    <- 0.3048
SQM_ACRE   <- 4046.8564224
acres <- function(g) as.numeric(st_area(st_transform(g, CRS_MA))) / SQM_ACRE

# ---- vision themes (one leaflet group per KML folder) ----------------------
# folder -> display label, fill color, fill opacity (Ring envelope = outline only)
THEMES <- tibble::tribble(
  ~layer,                                  ~label,                         ~col,       ~op,
  "Development",                           "Development (housing + commercial)", "#C0392B", 0.45,
  "Conservation - trails + habitat",       "Conservation, trails & habitat",     "#2E7D32", 0.35,
  "Hampshire Next - Retained facilities",  "Retained campus facilities",         "#607D8B", 0.40,
  "Town of Amherst Recreation",            "Town of Amherst recreation",         "#0277BD", 0.40,
  "Recreation/Public Facilities",          "Recreation / public facilities",     "#7B1FA2", 0.40,
  "Hampshire Next",                        "Vision envelope & misc.",            "#9C7A4A", 0.06)

read_theme <- function(lyr) {
  g <- tryCatch(st_read(KML, layer = lyr, quiet = TRUE), error = function(e) NULL)
  if (is.null(g) || !nrow(g)) return(NULL)
  g <- st_make_valid(st_zm(st_transform(g, 4326)))
  nm <- if ("Name" %in% names(g)) g$Name else paste0("feature ", seq_len(nrow(g)))
  g$.nm <- trimws(ifelse(is.na(nm), "(unnamed)", nm))
  g$.ac <- round(acres(g), 1)
  g[st_geometry_type(g) %in% c("POLYGON","MULTIPOLYGON"), ]
}

# ---- wetlands + buffers, clipped to a campus envelope ----------------------
campus_geoms <- lapply(THEMES$layer, function(l){ g <- read_theme(l); if (is.null(g)) NULL else st_union(st_geometry(g)) })
campus_geoms <- campus_geoms[!vapply(campus_geoms, is.null, logical(1))]
campus   <- st_transform(st_sf(geometry = do.call(c, campus_geoms)), CRS_MA)
envelope <- st_buffer(st_union(st_geometry(campus)), 300)   # 300 m around the vision

wet <- st_read(WETLANDS, quiet = TRUE) |> st_transform(CRS_MA) |> st_make_valid()
wet <- wet[lengths(st_intersects(wet, envelope)) > 0, ]
wet_u   <- st_union(st_geometry(wet))
buf100  <- st_buffer(wet_u, 100*FT_TO_M, nQuadSegs = 30)
buf200  <- st_buffer(wet_u, 200*FT_TO_M, nQuadSegs = 30)
ring100 <- st_make_valid(st_difference(buf100, wet_u))          # 0–100 ft (state WPA)
ring200 <- st_make_valid(st_difference(buf200, buf100))         # 100–200 ft (Amherst rule)
to_wgs  <- function(g) st_transform(st_intersection(g, envelope), 4326)
wet_w   <- st_transform(st_intersection(wet, envelope), 4326)
r100_w  <- to_wgs(ring100); r200_w <- to_wgs(ring200)

parcels <- st_read(PARCELS, quiet = TRUE) |> st_transform(4326) |> st_make_valid()

# ---- build map -------------------------------------------------------------
m <- leaflet(height = "97vh", width = "100%", options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles(providers$CartoDB.Positron, group = "Map") |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
  addPolygons(data = parcels, group = "Hampshire College parcels",
              fill = FALSE, color = "#111", weight = 1.5, opacity = 0.7, dashArray = "4")

for (i in seq_len(nrow(THEMES))) {
  g <- read_theme(THEMES$layer[i]); if (is.null(g) || !nrow(g)) next
  m <- m |> addPolygons(data = g, group = THEMES$label[i],
        color = THEMES$col[i], weight = 2, opacity = 0.9,
        fillColor = THEMES$col[i], fillOpacity = THEMES$op[i],
        label = ~lapply(sprintf("<b>%s</b><br>%s ac", .nm, formatC(.ac, format="f", digits=1)), htmltools::HTML),
        highlightOptions = highlightOptions(weight = 3, fillOpacity = min(THEMES$op[i] + 0.2, 0.8), bringToFront = TRUE))
}

m <- m |>
  addPolygons(data = wet_w, group = "Wetlands (DEP)",
              color = "#01579B", weight = 1, fillColor = "#4FC3F7", fillOpacity = 0.5) |>
  addPolygons(data = st_sf(geometry = r100_w), group = "Wetland buffer · 100 ft (state WPA)",
              color = "#0277BD", weight = 1, fillColor = "#4FC3F7", fillOpacity = 0.30) |>
  addPolygons(data = st_sf(geometry = r200_w), group = "Wetland buffer · +200 ft (Amherst rule)",
              color = "#039BE5", weight = 1, fillColor = "#B3E5FC", fillOpacity = 0.30) |>
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    overlayGroups = c("Hampshire College parcels", THEMES$label,
                      "Wetlands (DEP)", "Wetland buffer · 100 ft (state WPA)",
                      "Wetland buffer · +200 ft (Amherst rule)"),
    options = layersControlOptions(collapsed = FALSE)) |>
  hideGroup(c("Wetland buffer · 100 ft (state WPA)", "Wetland buffer · +200 ft (Amherst rule)")) |>
  fitBounds(lng1 = as.numeric(st_bbox(envelope |> st_transform(4326))[1]),
            lat1 = as.numeric(st_bbox(envelope |> st_transform(4326))[2]),
            lng2 = as.numeric(st_bbox(envelope |> st_transform(4326))[3]),
            lat2 = as.numeric(st_bbox(envelope |> st_transform(4326))[4]))

m$sizingPolicy <- htmlwidgets::sizingPolicy(browser.fill = TRUE, defaultWidth = "100%", defaultHeight = "100%")

# pandoc (for a single self-contained file) ships with Quarto / RStudio.
pandoc_dirs <- c(
  "/Applications/quarto/bin/tools",
  "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64",
  "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64",
  Sys.getenv("RSTUDIO_PANDOC"))
hit <- pandoc_dirs[file.exists(file.path(pandoc_dirs, "pandoc"))]
sc <- FALSE
if (length(hit)) { Sys.setenv(RSTUDIO_PANDOC = hit[1]); sc <- TRUE }
saveWidget(m, "vision_map.html", selfcontained = sc, title = "Hampshire Next — land vision")
if (sc) unlink("vision_map_files", recursive = TRUE)   # tidy the sidecar from earlier runs
dev_ac <- { g <- read_theme("Development"); sum(g$.ac) }
cat(sprintf("Wrote vision_map.html | Development folder: %.0f ac across %d areas\n",
            dev_ac, nrow(read_theme("Development"))))
