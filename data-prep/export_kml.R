# Export selected layers as KML for use in Google Earth, with explicit
# styling (so polygons render with a visible outline + translucent fill).
#
# Writes three files under data/exports/kml/:
#   hampshire_parcels.kml          — all Hampshire College parcels
#   wetlands_200ft_envelope.kml    — DEP wetlands + 200 ft buffer, clipped
#                                    to a 500 m envelope around parcels
#   protected_open_space.kml       — Protected & Recreational Open Space
#
# Run from project root:
#   Rscript data-prep/export_kml.R

suppressPackageStartupMessages({
  library(sf)
})

OUT_DIR <- "data/exports/kml"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

FT_TO_M <- 0.3048

# ---- KML helpers ----
# KML color format is AABBGGRR (alpha then BGR). Convert from web #RRGGBB.
hex_to_kml <- function(rrggbb, alpha = 255) {
  s <- sub("^#", "", rrggbb)
  rr <- substr(s, 1, 2); gg <- substr(s, 3, 4); bb <- substr(s, 5, 6)
  sprintf("%02x%s%s%s", alpha, bb, gg, rr)
}

xml_escape <- function(x) {
  x <- gsub("&", "&amp;",  x, fixed = TRUE)
  x <- gsub("<", "&lt;",   x, fixed = TRUE)
  x <- gsub(">", "&gt;",   x, fixed = TRUE)
  x
}

cdata <- function(x) {
  # Description payloads need CDATA so HTML survives Google Earth's parser
  x[is.na(x)] <- ""
  sprintf("<![CDATA[%s]]>", x)
}

# Convert one sf geometry to a KML <Geometry> string. Handles POLYGON,
# MULTIPOLYGON, LINESTRING, POINT.
geom_to_kml <- function(g) {
  if (sf::st_is_empty(g)) return("")
  type <- as.character(sf::st_geometry_type(g))
  fmt_ring <- function(coords) {
    paste(apply(coords, 1, function(p) sprintf("%.7f,%.7f,0", p[1], p[2])),
          collapse = " ")
  }
  poly_to_kml <- function(rings) {
    outer <- sprintf("<outerBoundaryIs><LinearRing><coordinates>%s</coordinates></LinearRing></outerBoundaryIs>",
                     fmt_ring(rings[[1]]))
    inners <- ""
    if (length(rings) > 1) {
      inners <- paste(vapply(rings[-1], function(r) {
        sprintf("<innerBoundaryIs><LinearRing><coordinates>%s</coordinates></LinearRing></innerBoundaryIs>",
                fmt_ring(r))
      }, character(1)), collapse = "")
    }
    sprintf("<Polygon>%s%s</Polygon>", outer, inners)
  }
  if (type == "POLYGON") {
    poly_to_kml(g)
  } else if (type == "MULTIPOLYGON") {
    sprintf("<MultiGeometry>%s</MultiGeometry>",
            paste(vapply(g, poly_to_kml, character(1)), collapse = ""))
  } else if (type == "LINESTRING") {
    sprintf("<LineString><coordinates>%s</coordinates></LineString>",
            fmt_ring(g))
  } else if (type == "MULTILINESTRING") {
    sprintf("<MultiGeometry>%s</MultiGeometry>",
            paste(vapply(g, function(l) {
              sprintf("<LineString><coordinates>%s</coordinates></LineString>",
                      fmt_ring(l))
            }, character(1)), collapse = ""))
  } else if (type == "POINT") {
    sprintf("<Point><coordinates>%.7f,%.7f,0</coordinates></Point>",
            g[1], g[2])
  } else {
    ""
  }
}

# Build a complete KML document for a polygon/line layer with shared styling.
build_kml <- function(sf_obj, doc_name, names_vec, descriptions_vec,
                      style_id = "layerStyle",
                      line_color_hex = "#1a1a1a", line_alpha = 255,
                      line_width = 3,
                      fill_color_hex = NULL, fill_alpha = 80) {
  geoms <- sf::st_geometry(sf_obj)
  placemarks <- vapply(seq_along(geoms), function(i) {
    sprintf(
      "<Placemark><name>%s</name><description>%s</description><styleUrl>#%s</styleUrl>%s</Placemark>",
      xml_escape(as.character(names_vec[i])),
      cdata(descriptions_vec[i]),
      style_id,
      geom_to_kml(geoms[[i]])
    )
  }, character(1))

  fill_block <- if (is.null(fill_color_hex)) {
    "<PolyStyle><fill>0</fill><outline>1</outline></PolyStyle>"
  } else {
    sprintf("<PolyStyle><color>%s</color><fill>1</fill><outline>1</outline></PolyStyle>",
            hex_to_kml(fill_color_hex, fill_alpha))
  }

  style_block <- sprintf(
    "<Style id=\"%s\"><LineStyle><color>%s</color><width>%d</width></LineStyle>%s<BalloonStyle><text>$[description]</text></BalloonStyle></Style>",
    style_id,
    hex_to_kml(line_color_hex, line_alpha),
    line_width,
    fill_block
  )

  paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<kml xmlns="http://www.opengis.net/kml/2.2">\n',
    '<Document>\n',
    sprintf("<name>%s</name>\n", xml_escape(doc_name)),
    style_block, "\n",
    paste(placemarks, collapse = "\n"),
    "\n</Document>\n</kml>\n"
  )
}

# ---- Hampshire parcels ----
message("Exporting Hampshire parcels...")
parcels <- sf::st_read("data/hampshire_college_parcels_combined.geojson",
                       quiet = TRUE) |>
  sf::st_transform(4326) |> sf::st_make_valid()

field <- function(name) {
  if (name %in% names(parcels)) parcels[[name]] else rep(NA, nrow(parcels))
}
opt_row <- function(label, value, format_fn = identity) {
  ifelse(!is.na(value) & nzchar(as.character(value)),
         paste0("<tr><td style='color:#666; padding-right:10px;'>", label,
                "</td><td><b>", format_fn(value), "</b></td></tr>"),
         "")
}
fmt_money <- function(x) paste0("$", formatC(x, big.mark = ",", format = "d"))

owner     <- field("OWNER1")
zoning    <- field("ZONING")
use_desc  <- field("USE_DESC")
addr      <- field("SITE_ADDR")
acres     <- field("ACRES_CALC")
totval    <- field("TOTAL_VAL")
fy_val    <- field("FY")
enc_lbl   <- field("ENCUMBRANCE")
creditor  <- field("creditor")
silo_year <- field("silo_year")
instr     <- field("instrument")
book_page <- field("book_page")
enc_notes <- field("enc_notes")

is_enc <- !is.na(enc_lbl) & enc_lbl != "Unencumbered"
enc_detail <- ifelse(
  is_enc,
  paste0(
    enc_lbl,
    ifelse(!is.na(creditor) & nzchar(creditor),
           paste0(" (", creditor, ")"), ""),
    ifelse(!is.na(silo_year) & nzchar(as.character(silo_year)),
           paste0(", ", silo_year), ""),
    ifelse(!is.na(instr) & nzchar(instr),
           paste0(" — ", instr), ""),
    ifelse(!is.na(book_page) & nzchar(book_page),
           paste0(", ", book_page), "")
  ),
  ifelse(!is.na(enc_lbl), enc_lbl, "")
)

parcel_desc <- paste0(
  "<div style='font-family:Arial,sans-serif; font-size:13px;'>",
  "<div style='font-size:14px; margin-bottom:6px;'><b>", parcels$MAP_PAR_ID,
  "</b> <span style='color:#777;'>(", parcels$TOWN, ")</span></div>",
  ifelse(!is.na(addr) & nzchar(addr), paste0(addr, "<br/>"), ""),
  "<table style='border-collapse:collapse; margin-top:6px;'>",
  opt_row("Owner",       owner),
  opt_row("Zoning",      zoning),
  opt_row("Land use",    use_desc),
  opt_row("Acres",       acres),
  opt_row("Assessed",    totval, fmt_money),
  opt_row("FY",          fy_val),
  opt_row("Encumbrance", enc_detail),
  "</table>",
  ifelse(!is.na(enc_notes) & nzchar(enc_notes),
         paste0("<div style='margin-top:6px; color:#444; font-size:12px;'>",
                enc_notes, "</div>"), ""),
  "</div>"
)

writeLines(
  build_kml(parcels,
            doc_name        = "Hampshire College parcels",
            names_vec       = parcels$MAP_PAR_ID,
            descriptions_vec = parcel_desc,
            style_id        = "parcelStyle",
            line_color_hex  = "#009b9e", line_alpha = 255, line_width = 4,
            fill_color_hex  = "#009b9e", fill_alpha = 50),
  file.path(OUT_DIR, "hampshire_parcels.kml")
)

# ---- Wetlands + buffer rings, clipped tight to parcels ----
# Three separately-toggleable layers:
#   1. Actual wetlands (the DEP polygons themselves, per-feature with type info)
#   2. 0–100 ft buffer ring (the state WPA Buffer Zone reach)
#   3. 100–200 ft buffer ring (additional conservative reach)
# Rings are computed as st_difference so they don't overlap each other or
# the wetland itself — toggling all three on shows three concentric bands.
message("Computing wetland + 100/200 ft buffer rings...")
wet <- sf::st_read("data/dep_wetlands.geojson", quiet = TRUE) |>
  sf::st_transform(26986) |> sf::st_make_valid()

# Clip envelope: 500 m around parcels (~1640 ft).
par_mp <- sf::st_transform(parcels, 26986) |> sf::st_make_valid()
parcel_envelope <- sf::st_buffer(sf::st_union(sf::st_geometry(par_mp)), 500)

wet_union <- sf::st_union(sf::st_geometry(wet))
buf_100   <- sf::st_buffer(wet_union, 100 * FT_TO_M, nQuadSegs = 30)
buf_200   <- sf::st_buffer(wet_union, 200 * FT_TO_M, nQuadSegs = 30)

ring_100 <- sf::st_difference(buf_100, wet_union) |> sf::st_make_valid()
ring_200 <- sf::st_difference(buf_200, buf_100)   |> sf::st_make_valid()

# Clip each to the parcel envelope, transform to WGS84
clip_wgs <- function(g) {
  sf::st_intersection(g, parcel_envelope) |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}

# (1) Actual wetlands as per-feature placemarks (keep cover type + acreage)
wet_clipped <- sf::st_intersection(wet, parcel_envelope) |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

wet_desc <- paste0(
  "<div style='font-family:Arial,sans-serif; font-size:13px;'>",
  "<b>", ifelse(is.na(wet_clipped$IT_VALDESC),
                "Wetland resource area", wet_clipped$IT_VALDESC), "</b>",
  ifelse(!is.na(wet_clipped$AREAACRES),
         paste0("<br/>", round(wet_clipped$AREAACRES, 2), " ac mapped"), ""),
  "<br/><span style='color:#666; font-size:11px;'>",
  "Source: MassDEP wetlands. Planning grade; not a jurisdictional boundary.",
  "</span></div>"
)
wet_names <- ifelse(is.na(wet_clipped$IT_VALDESC),
                    "Wetland resource area", wet_clipped$IT_VALDESC)

writeLines(
  build_kml(wet_clipped,
            doc_name         = "DEP wetlands — actual (near Hampshire parcels)",
            names_vec        = wet_names,
            descriptions_vec = wet_desc,
            style_id         = "wetActualStyle",
            line_color_hex   = "#01579B", line_alpha = 255, line_width = 2,
            fill_color_hex   = "#4FC3F7", fill_alpha = 180),
  file.path(OUT_DIR, "wetlands_actual.kml")
)

# (2) 0–100 ft buffer ring
ring_100_wgs <- clip_wgs(ring_100)
writeLines(
  build_kml(sf::st_sf(geometry = ring_100_wgs),
            doc_name         = "Wetland buffer: 0–100 ft (WPA Buffer Zone)",
            names_vec        = "Wetland buffer: 0–100 ft",
            descriptions_vec = paste0(
              "<div style='font-family:Arial,sans-serif; font-size:13px;'>",
              "The 100-foot Buffer Zone under the Massachusetts Wetlands ",
              "Protection Act. Work in this ring requires a Notice of Intent ",
              "and Order of Conditions from the local Conservation Commission.",
              "</div>"),
            style_id         = "wetBuf100Style",
            line_color_hex   = "#0277BD", line_alpha = 255, line_width = 2,
            fill_color_hex   = "#4FC3F7", fill_alpha = 95),
  file.path(OUT_DIR, "wetlands_buffer_0_to_100ft.kml")
)

# (3) 100–200 ft buffer ring
ring_200_wgs <- clip_wgs(ring_200)
writeLines(
  build_kml(sf::st_sf(geometry = ring_200_wgs),
            doc_name         = "Wetland buffer: 100–200 ft (conservative reach)",
            names_vec        = "Wetland buffer: 100–200 ft",
            descriptions_vec = paste0(
              "<div style='font-family:Arial,sans-serif; font-size:13px;'>",
              "The 100–200 ft band beyond the WPA Buffer Zone. Approximates ",
              "the state Riverfront Area and a conservative reading of ",
              "Amherst's local Wetlands Protection Bylaw.",
              "</div>"),
            style_id         = "wetBuf200Style",
            line_color_hex   = "#039BE5", line_alpha = 200, line_width = 1,
            fill_color_hex   = "#B3E5FC", fill_alpha = 70),
  file.path(OUT_DIR, "wetlands_buffer_100_to_200ft.kml")
)

# (4) Combined envelope: actual wetlands UNION 200 ft buffer, as a single
# polygon. Useful when you want one toggle that shows the total regulatory
# reach without managing three layers.
combined_env <- sf::st_union(buf_200) |>
  sf::st_intersection(parcel_envelope) |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

writeLines(
  build_kml(sf::st_sf(geometry = combined_env),
            doc_name         = "DEP wetlands + 200 ft buffer (combined envelope)",
            names_vec        = "DEP wetlands plus 200 ft buffer",
            descriptions_vec = paste0(
              "<div style='font-family:Arial,sans-serif; font-size:13px;'>",
              "MassDEP regulatory wetlands unioned with a 200-foot outward ",
              "buffer, clipped to a 500-m envelope around Hampshire parcels. ",
              "Treat the polygon edge as the outer extent of the regulated ",
              "wetland envelope under a conservative reading of state and ",
              "local rules.",
              "</div>"),
            style_id         = "wetEnvelopeStyle",
            line_color_hex   = "#01579B", line_alpha = 255, line_width = 2,
            fill_color_hex   = "#4FC3F7", fill_alpha = 90),
  file.path(OUT_DIR, "wetlands_actual_plus_200ft_envelope.kml")
)

# Remove the old combined envelope file from before the rename
old_file <- file.path(OUT_DIR, "wetlands_200ft_envelope.kml")
if (file.exists(old_file)) file.remove(old_file)

# ---- Protected and recreational open space ----
message("Exporting protected open space...")
osp <- sf::st_read("data/protected_openspace.geojson", quiet = TRUE) |>
  sf::st_transform(4326) |> sf::st_make_valid()

# Clip open space to the same parcel envelope so the KML is local and small
osp_mp <- sf::st_transform(osp, 26986) |> sf::st_make_valid()
keep_idx <- lengths(sf::st_intersects(osp_mp, parcel_envelope)) > 0
osp_local <- osp[keep_idx, ]

owner_label <- c(M = "Municipal", S = "State", F = "Federal",
                 P = "Private (nonprofit)", L = "Land trust",
                 N = "Nonprofit", O = "Other")
prot_label  <- c(P = "Permanent", L = "Limited", N = "None")

name_col <- ifelse(is.na(osp_local$SITE_NAME) | !nzchar(osp_local$SITE_NAME),
                   "Protected open space", osp_local$SITE_NAME)
desc_col <- paste0(
  "<div style='font-family:Arial,sans-serif; font-size:13px;'>",
  "<b>", name_col, "</b><br/>",
  "Owner: ", unname(owner_label[osp_local$OWNER_TYPE]), "<br/>",
  "Protection: ", unname(prot_label[osp_local$LEV_PROT]),
  ifelse(!is.na(osp_local$GIS_ACRES),
         paste0("<br/>Mapped acres: ",
                formatC(round(osp_local$GIS_ACRES, 1),
                        big.mark = ",", format = "f", digits = 1)),
         ""),
  "</div>"
)

writeLines(
  build_kml(osp_local,
            doc_name        = "Protected & Recreational Open Space (near Hampshire parcels)",
            names_vec       = name_col,
            descriptions_vec = desc_col,
            style_id        = "openSpaceStyle",
            line_color_hex  = "#FF6F00", line_alpha = 255, line_width = 2,
            fill_color_hex  = "#FFC107", fill_alpha = 90),
  file.path(OUT_DIR, "protected_open_space.kml")
)

# ---- Write a layer manifest CSV alongside the KMLs ----
manifest <- data.frame(
  file = c(
    "hampshire_parcels.kml",
    "protected_open_space.kml",
    "wetlands_actual.kml",
    "wetlands_buffer_0_to_100ft.kml",
    "wetlands_buffer_100_to_200ft.kml",
    "wetlands_actual_plus_200ft_envelope.kml"
  ),
  category = c(
    "Parcels",
    "Protected land",
    "Wetlands",
    "Wetlands",
    "Wetlands",
    "Wetlands"
  ),
  what_it_is = c(
    "All Hampshire College parcels in Amherst and Hadley, each placemark labeled with the parcel ID. Clicking a parcel opens a balloon showing owner, zoning, land use, acres, FY assessed value, and encumbrance (creditor, silo year, instrument, book and page).",
    "MassGIS Protected and Recreational Open Space polygons clipped to a 500-meter envelope around the Hampshire parcels. Each placemark balloon shows site name, owner type (municipal, state, federal, land trust, private nonprofit, other), protection level (permanent, limited, none), and mapped acres.",
    "MassDEP regulatory wetland polygons themselves, one placemark per wetland, clipped to a 500-meter envelope around the Hampshire parcels. Each placemark balloon shows the wetland cover type (wooded swamp, shrub swamp, shallow marsh, deep marsh, open water, bog, etc.) and mapped acres.",
    "The 0 to 100 ft ring around every wetland: the Massachusetts Wetlands Protection Act Buffer Zone. Drawn as the difference between the 100 ft buffer and the wetland itself, so it occupies its own ring with no overlap.",
    "The 100 to 200 ft ring beyond the WPA Buffer Zone: an additional band that approximates the Riverfront Area and a conservative reading of Amherst's local Wetlands Protection Bylaw. Drawn as the difference between the 200 ft and 100 ft buffers, so it occupies its own ring with no overlap.",
    "A single unioned polygon: every wetland plus its 200 ft outward buffer, merged into one shape. Combines the three-layer split (actual + 0 to 100 ft ring + 100 to 200 ft ring) into one toggle for the total regulated reach."
  ),
  when_to_use = c(
    "When you need any parcel-level information (ownership, zoning, encumbrance, value). Drop this on top of any basemap for context.",
    "When you want to see what's permanently conserved around the Hampshire holdings. Useful for understanding which neighbors are off the market or which areas already form a regional green network.",
    "When you want to see only the actual wetland edges, without any buffer. Useful for orientation and for identifying cover types per wetland.",
    "When the conversation is about the standard state-WPA buffer reach. This is the regulatory boundary triggered by a Notice of Intent.",
    "When you want to flag the additional area covered under a conservative or strict-buffer scenario, such as Amherst's local bylaw read generously. Use alongside the 0 to 100 ft ring to show the total regulated reach.",
    "When you want one toggle that shows the full regulated extent (wetland plus 200 ft) without managing three layers. Visually equivalent to turning on all three split layers together."
  ),
  source = c(
    "MassGIS Level 3 Standardized Assessors' Parcels (Amherst FY2024, Hadley FY2025) plus encumbrance lookup from the Hampshire County Registry of Deeds and Hampshire College FY2025 audit.",
    "MassGIS Protected and Recreational Open Space.",
    "MassGIS DEP Wetlands (regulatory polygons, photo-interpreted at ~1:12,000).",
    "Derived locally by buffering DEP wetlands 100 ft outward in MA State Plane Mainland (EPSG:26986), then differencing the wetland itself.",
    "Derived locally by buffering DEP wetlands 200 ft outward and differencing the 100 ft buffer.",
    "Derived locally by buffering DEP wetlands 200 ft outward and unioning."
  ),
  stringsAsFactors = FALSE
)
# Add live size + present-in-folder columns so the manifest matches what's on disk
manifest$size_kb <- NA_real_
manifest$present <- FALSE
for (i in seq_len(nrow(manifest))) {
  fp <- file.path(OUT_DIR, manifest$file[i])
  if (file.exists(fp)) {
    manifest$size_kb[i] <- round(file.info(fp)$size / 1024, 1)
    manifest$present[i] <- TRUE
  }
}

manifest_path <- file.path(OUT_DIR, "kml_layers_manifest.csv")
utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")

message("Done. Files in ", OUT_DIR, ":")
for (f in list.files(OUT_DIR, pattern = "\\.kml$", full.names = TRUE)) {
  message(sprintf("  %s  (%s KB)", f,
                  formatC(round(file.info(f)$size / 1024, 1),
                          format = "f", digits = 1)))
}
message(sprintf("Manifest: %s", manifest_path))
