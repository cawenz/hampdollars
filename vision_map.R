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

# Net buildable density presets (units/acre, after ~33% for streets/infrastructure).
# Row-home figure (6.4) is from the project's CLT modeling: 98 ac -> ~626 homes.
DENSITY <- c("Row homes" = 6.4, "Townhouses" = 12, "Low-rise multifamily" = 20)
fmt <- function(x) formatC(round(x), format = "d", big.mark = ",")
units_html <- function(nm, ac) sprintf(
  "<b>%s</b> · %.1f ac<br>Row homes: ~%s · Townhouses: ~%s · Multifamily: ~%s homes",
  nm, ac, fmt(ac*DENSITY[1]), fmt(ac*DENSITY[2]), fmt(ac*DENSITY[3]))

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

# Development areas -> unit yield + centroid label points
dev      <- read_theme("Development")
dev$rowhomes <- round(dev$.ac * DENSITY[["Row homes"]])
dev_pts  <- st_transform(suppressWarnings(st_point_on_surface(st_transform(dev, CRS_MA))), 4326)
dev_xy   <- st_coordinates(dev_pts)
dev_tot  <- sum(dev$.ac)
cap_html <- sprintf(paste0(
  "<div style='background:rgba(255,255,255,0.93); padding:8px 11px; border-radius:8px; ",
  "font:13px/1.45 Arial, sans-serif; box-shadow:0 1px 4px rgba(0,0,0,.25); max-width:240px;'>",
  "<b>Housing capacity — %.0f development acres</b><br>",
  "Row homes (6.4/ac): <b>~%s</b><br>Townhouses (12/ac): <b>~%s</b><br>",
  "Low-rise multifamily (20/ac): <b>~%s</b>",
  "<div style='font-size:11px; color:#666; margin-top:4px;'>Net of ~33%% for streets ",
  "(CLT model). Map labels show row-home counts; hover an area for all densities.</div></div>"),
  dev_tot, fmt(dev_tot*DENSITY[1]), fmt(dev_tot*DENSITY[2]), fmt(dev_tot*DENSITY[3]))

# ---- build map -------------------------------------------------------------
m <- leaflet(height = "97vh", width = "100%", options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles(providers$CartoDB.Positron, group = "Map") |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
  addPolygons(data = parcels, group = "Hampshire College parcels",
              fill = FALSE, color = "#111", weight = 1.5, opacity = 0.7, dashArray = "4")

for (i in seq_len(nrow(THEMES))) {
  g <- read_theme(THEMES$layer[i]); if (is.null(g) || !nrow(g)) next
  lab <- if (THEMES$layer[i] == "Development")
           lapply(seq_len(nrow(g)), function(j) htmltools::HTML(units_html(g$.nm[j], g$.ac[j])))
         else lapply(sprintf("<b>%s</b><br>%s ac", g$.nm, formatC(g$.ac, format="f", digits=1)), htmltools::HTML)
  m <- m |> addPolygons(data = g, group = THEMES$label[i],
        color = THEMES$col[i], weight = 2, opacity = 0.9,
        fillColor = THEMES$col[i], fillOpacity = THEMES$op[i], label = lab,
        highlightOptions = highlightOptions(weight = 3, fillOpacity = min(THEMES$op[i] + 0.2, 0.8), bringToFront = TRUE))
}
# Permanent row-home unit labels on each development area (toggle with the group)
m <- m |> addLabelOnlyMarkers(
  lng = dev_xy[, 1], lat = dev_xy[, 2], group = "Development (housing + commercial)",
  label = lapply(sprintf("~%s homes", fmt(dev$rowhomes)), htmltools::HTML),
  labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
    style = list("color" = "#7a1f12", "font-weight" = "700", "font-size" = "12px",
                 "text-shadow" = "0 0 3px #fff, 0 0 3px #fff, 0 0 3px #fff")))

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
  addControl(html = cap_html, position = "bottomleft") |>
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
