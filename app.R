# Hampshire College Property Explorer
# Shiny app: explore parcels on a map, filter/select in a table,
# and see running totals for acreage and assessed value.

library(shiny)
library(bslib)
library(leaflet)
library(markdown)
library(sf)
library(DT)
library(dplyr)
library(htmltools)
library(jsonlite)
library(leafpm)   # Geoman drawing toolbar wrapper

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- Land use labels ----
use_labels <- c(
  '000'  = 'Unclassified',
  '1010' = 'Single family residential',
  '1060' = 'Accessory land with improvement',
  '3900' = 'Developable commercial land',
  '7130' = 'Agricultural — field crops (Ch. 61A)',
  '7180' = 'Agricultural — pasture (Ch. 61A)',
  '9040' = 'Private school — general',
  '9041' = 'Private school — residential',
  '9042' = 'Private school — commercial',
  '9420' = 'Private college or university',
  '942V' = 'Private college — vacant land'
)

# ---- Data load ----
parcels <- st_read("data/hampshire_college_parcels_combined.geojson", quiet = TRUE)

# ---- Optional: MassGIS context layers (clipped snapshots) ----
# Produced by data-prep/fetch_massgis_layers.R. Each is loaded lazily and only
# wired into the Land context tab if the file exists, so the app still runs
# on a clean checkout where these have not been fetched.
load_optional_layer <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    sf::st_read(path, quiet = TRUE) |> sf::st_transform(4326) |> sf::st_make_valid(),
    error = function(e) {
      message("Failed to load ", path, ": ", conditionMessage(e))
      NULL
    }
  )
}
dep_wetlands       <- load_optional_layer("data/dep_wetlands.geojson")
protected_open     <- load_optional_layer("data/protected_openspace.geojson")
priority_habitat   <- load_optional_layer("data/nhesp_priority_habitats.geojson")
acecs              <- load_optional_layer("data/acecs.geojson")
wellhead_protect   <- load_optional_layer("data/wellhead_protection.geojson")
vernal_certified   <- load_optional_layer("data/vernal_pools_certified.geojson")
vernal_potential   <- load_optional_layer("data/vernal_pools_potential.geojson")
roads_centerline   <- load_optional_layer("data/massdot_roads.geojson")
buildings          <- load_optional_layer("data/buildings.geojson")
if (!is.null(acecs) && nrow(acecs) == 0) acecs <- NULL  # treat empty as missing

# Per-parcel developable-land analysis (computed by data-prep/compute_developable.R)
parcels_dev             <- load_optional_layer("data/parcels_developable.geojson")
parcels_dev_poly        <- load_optional_layer("data/parcels_developable_polys.geojson")
parcels_dev_poly_strict <- load_optional_layer("data/parcels_developable_polys_strict.geojson")

# ---- Concept sketches: load any *.geojson under data/concepts/ ----
# Each file becomes part of a single combined sf with `concept_source` set to
# the file's basename (no extension). Renders as a toggleable overlay on the
# Land context and Developable land tabs.
load_concept_sketches <- function(dir = "data/concepts") {
  if (!dir.exists(dir)) return(NULL)
  files <- list.files(dir, pattern = "\\.geojson$",
                      full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) return(NULL)
  parts <- lapply(files, function(f) {
    g <- tryCatch(sf::st_read(f, quiet = TRUE), error = function(e) NULL)
    if (is.null(g) || nrow(g) == 0) return(NULL)
    g <- sf::st_transform(g, 4326)
    keep <- sf::st_sf(
      concept_source = tools::file_path_sans_ext(basename(f)),
      geometry       = sf::st_geometry(g)
    )
    keep
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) return(NULL)
  out <- do.call(rbind, parts)
  sf::st_make_valid(out)
}
concept_sketches <- load_concept_sketches()

# ---- Optional: BankUnited 2016 collateral polygon (computed from Plan Book 235/100) ----
# If data/bankunited_2016_collateral.geojson exists, load it for the collateral overlay.
# This polygon is reconstructed by metes-and-bounds from the 2016 mortgage Exhibit A
# and the Feb 2026 UCC continuation. The 17.6-acre carve-out sits inside parcel 22D-15.
# We build `parcels_trimmed` (parcels with the overlay's footprint cut out) further
# below, AFTER parcels has been enriched with the ENCUMBRANCE column and friends —
# otherwise the trimmed copy misses those derived columns.
bu_collateral_path <- "data/bankunited_2016_collateral.geojson"
bu_collateral <- if (file.exists(bu_collateral_path)) {
  sf::st_read(bu_collateral_path, quiet = TRUE) |>
    sf::st_transform(sf::st_crs(parcels)) |>
    sf::st_make_valid()
} else {
  NULL
}

if (!"USE_DESC" %in% names(parcels)) parcels$USE_DESC <- NA_character_
missing <- is.na(parcels$USE_DESC) | parcels$USE_DESC == ""
parcels$USE_DESC[missing] <- unname(use_labels[parcels$USE_CODE[missing]])
parcels$USE_DESC[is.na(parcels$USE_DESC)] <- parcels$USE_CODE[is.na(parcels$USE_DESC)]

# ---- Encumbrance data ----
# Optional lookup CSV: data/encumbrances.csv
# Columns: MAP_PAR_ID, TOWN, creditor, silo_year, instrument, book_page, enc_notes
# A parcel is considered "Unencumbered" if it has no matching row.
enc_path <- "data/encumbrances.csv"
enc_cols <- c("MAP_PAR_ID", "TOWN", "creditor", "silo_year",
              "instrument", "book_page", "enc_notes")

if (file.exists(enc_path)) {
  encumbrances <- utils::read.csv(enc_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  miss <- setdiff(enc_cols, names(encumbrances))
  for (m in miss) encumbrances[[m]] <- NA_character_
  encumbrances <- encumbrances[, enc_cols]
  parcels <- parcels |> dplyr::left_join(encumbrances, by = c("MAP_PAR_ID", "TOWN"))
} else {
  message("Note: data/encumbrances.csv not found — all parcels will display as Unencumbered.")
  for (nm in setdiff(enc_cols, c("MAP_PAR_ID", "TOWN"))) parcels[[nm]] <- NA
}

# Short display label for encumbrance
parcels$ENCUMBRANCE <- dplyr::case_when(
  grepl("M&T|Peoples?.?United", parcels$creditor, ignore.case = TRUE) ~ "M&T Bank",
  grepl("BankUnited",           parcels$creditor, ignore.case = TRUE) ~ "BankUnited",
  grepl("Kendall",              parcels$creditor, ignore.case = TRUE) ~ "Kendall Foundation",
  TRUE ~ "Unencumbered"
)

enc_levels <- c("M&T Bank",
                "BankUnited",
                "Kendall Foundation",
                "Unencumbered")

parcels <- parcels |>
  dplyr::mutate(
    row_id = dplyr::row_number(),
    enc_badge_bg = dplyr::case_when(
      ENCUMBRANCE == "M&T Bank"           ~ "#FDECC9",
      ENCUMBRANCE == "BankUnited"         ~ "#D7E6F6",
      ENCUMBRANCE == "Kendall Foundation" ~ "#D6ECD0",
      TRUE ~ "#EDECE6"
    ),
    enc_badge_fg = dplyr::case_when(
      ENCUMBRANCE == "M&T Bank"           ~ "#8A5A00",
      ENCUMBRANCE == "BankUnited"         ~ "#1F3B5A",
      ENCUMBRANCE == "Kendall Foundation" ~ "#1F5B1F",
      TRUE ~ "#6c757d"
    ),
    popup_html = paste0(
      "<div style='font-family:\"Open Sans\",sans-serif; font-size:13px; line-height:1.5;'>",
      "<strong>", MAP_PAR_ID, "</strong> ",
      "<span style='color:#777;'>(", TOWN, ")</span><br/>",
      ifelse(nchar(SITE_ADDR) > 0, paste0(SITE_ADDR, "<br/>"), ""),
      "<span style='color:#555;'>", USE_DESC, "</span>",
      " <span style='color:#999;'>(", USE_CODE, ")</span><br/>",
      "Acres: ", ACRES_CALC, "<br/>",
      "Assessed: <strong>$", formatC(TOTAL_VAL, format = "d", big.mark = ","), "</strong>",
      " <span style='color:#777;'>(FY", FY, ")</span>",
      ifelse(!is.na(YEAR_BUILT) & YEAR_BUILT > 0, paste0("<br/>Built: ", YEAR_BUILT), ""),
      "<div style='margin-top:8px; padding:6px 10px; border-radius:6px; display:inline-block;",
      " background:", enc_badge_bg, "; color:", enc_badge_fg, "; font-weight:600;'>",
      ENCUMBRANCE, "</div>",
      ifelse(!is.na(book_page) & nchar(book_page) > 0,
             paste0("<div style='color:#777; font-size:12px; margin-top:4px;'>",
                    book_page, "</div>"),
             ""),
      "</div>"
    )
  )

use_choices <- parcels |>
  sf::st_drop_geometry() |>
  dplyr::distinct(USE_CODE, USE_DESC) |>
  dplyr::arrange(USE_CODE)
town_levels <- sort(unique(parcels$TOWN))

total_parcels    <- nrow(parcels)
total_acres      <- sum(parcels$ACRES_CALC, na.rm = TRUE)
total_value      <- sum(parcels$TOTAL_VAL,  na.rm = TRUE)
encumbered_mask  <- parcels$ENCUMBRANCE != "Unencumbered"
encumbered_n     <- sum(encumbered_mask)
encumbered_acres <- sum(parcels$ACRES_CALC[encumbered_mask], na.rm = TRUE)

# ---- Palettes ----
use_colors <- c(
  '000'  = '#E6C26A',   # distinct warm yellow (was dim gray #888780)
  '1010' = '#D85A30',
  '1060' = '#D4537E',
  '3900' = '#EF9F27',
  '7130' = '#97C459',
  '7180' = '#639922',
  '9040' = '#7F77DD',
  '9041' = '#AFA9EC',
  '9042' = '#534AB7',
  '9420' = '#6D63CC',
  '942V' = '#CECBF6'
)
use_pal  <- colorFactor(palette = use_colors, domain = names(use_colors))
town_pal <- colorFactor(palette = c("#378ADD", "#D85A30"), domain = town_levels)

enc_colors <- c(
  "M&T Bank"           = "#E8A33D",
  "BankUnited"         = "#1F77B4",
  "Kendall Foundation" = "#2CA02C",
  "Unencumbered"       = "#F2F2F2"
)
enc_pal <- function(x) unname(enc_colors[as.character(x)])


# ---- Theme ----
app_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = bslib::font_google("Open Sans", wght = c(400, 600, 700, 800)),
  heading_font = bslib::font_google("Open Sans", wght = c(400, 600, 700, 800)),
  "navbar-brand-padding-y" = "0.25rem"
)

# ---- Build the Encumbrance-view geometry ----
# parcels_trimmed is a copy of the fully-decorated `parcels` (with all derived
# columns like ENCUMBRANCE, enc_badge_bg, popup_html, row_id), but with the
# BankUnited collateral footprint cut out of any overlapping base parcel. Used
# ONLY when "Color parcels by" is set to Encumbrance, so the overlay polygon
# can sit on the satellite basemap without a base parcel stealing the hover.
parcels_trimmed <- parcels
if (!is.null(bu_collateral) && nrow(bu_collateral) > 0) {
  parcels_trimmed <- sf::st_make_valid(parcels_trimmed)
  overlaps_bu <- suppressMessages(
    lengths(sf::st_intersects(parcels_trimmed, bu_collateral)) > 0
  )
  if (any(overlaps_bu)) {
    bu_union <- sf::st_union(bu_collateral)
    parcels_trimmed$geometry[overlaps_bu] <- suppressMessages(
      sf::st_difference(parcels_trimmed$geometry[overlaps_bu], bu_union)
    )
    parcels_trimmed <- sf::st_make_valid(parcels_trimmed)
    message(sprintf(
      "Trimmed BankUnited collateral (%.2f ac) out of %d base parcel(s) for Encumbrance view.",
      as.numeric(sum(sf::st_area(bu_collateral))) / 4046.8564224,
      sum(overlaps_bu)
    ))
  }
}

export_cols <- c("TOWN", "MAP_PAR_ID", "LOC_ID", "SITE_ADDR", "USE_CODE", "USE_DESC",
                 "ACRES_CALC", "LOT_SIZE", "LOT_UNITS",
                 "LAND_VAL", "BLDG_VAL", "OTHER_VAL", "TOTAL_VAL",
                 "YEAR_BUILT", "STYLE", "BLD_AREA", "ZONING",
                 "LS_DATE", "LS_PRICE", "FY", "PROP_ID", "OWNER1",
                 "ENCUMBRANCE", "creditor", "silo_year", "instrument",
                 "book_page", "enc_notes")

stat_tile <- function(label, value, sublabel = NULL) {
  tags$div(
    class = "stat-tile",
    tags$div(class = "stat-label", label),
    tags$div(class = "stat-value", value),
    if (!is.null(sublabel)) tags$div(class = "stat-sublabel", sublabel)
  )
}

# Height used for both the map and the selection panel next to it
MAP_HEIGHT <- "620px"

# ---- UI ----
ui <- bslib::page_navbar(
  title = tags$div(
    class = "me-auto",
    style = "display: flex; align-items: center; gap: 14px;",
    # tags$img(
    #   src = "hampshire-logo.png",
    #   alt = "Hampshire College",
    #   style = "height: 22px; width: auto; display: block;"
    # ),
    tags$span(
      "Financial Analysis",
      style = "color: #ffffff; font-weight: 400; font-size: 15px; letter-spacing: 0.2px;"
    )
  ),
  bg = "#000000",
  inverse = TRUE,
  theme = app_theme,
  header = tags$head(
    tags$style(HTML(paste0("
      body, .leaflet-container, .leaflet-popup-content, table.dataTable,
      .dataTables_wrapper { font-family: 'Open Sans', sans-serif; }
      /* Flatly + Open Sans makes strong tags barely distinguishable from regular
         weight. Force a true bold. */
      strong, b { font-weight: 700; }
      .navbar-brand { padding-top: 0.4rem; padding-bottom: 0.4rem; }
      /* Nav-bar tab links: legible on black background */
      .navbar .nav-link {
        color: rgba(255,255,255,0.75) !important;
        font-weight: 500;
        padding: 0.55rem 1rem !important;
        transition: color 0.12s;
      }
      .navbar .nav-link:hover,
      .navbar .nav-link:focus {
        color: #ffffff !important;
      }
      .navbar .nav-link.active,
      .navbar .nav-item.active > .nav-link {
        color: #ffffff !important;
        border-bottom: 2px solid #ffffff;
      }
      .download-row .btn { margin-right: 8px; }
      .navbar .container-fluid, .navbar > .container { align-items: center; }
      .navbar-nav { align-items: center; margin-left: 2rem; }
      .navbar-nav .nav-link { padding-top: 0.5rem; padding-bottom: 0.5rem; }
      .navbar-brand { margin-right: 0; }
      .stats-row {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 16px;
        margin: 0 0 20px 0;
      }
      .stat-tile {
        background: #ffffff;
        border: 1px solid #e5e5e5;
        border-radius: 8px;
        padding: 16px 20px;
      }
      .stat-label {
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.6px;
        color: #6c757d;
      }
      .stat-value {
        font-size: 26px;
        font-weight: 600;
        margin-top: 4px;
        color: #1a1a1a;
      }
      .stat-sublabel { font-size: 12px; color: #6c757d; margin-top: 2px; }

      #selection_panel { height: ", MAP_HEIGHT, "; }
      .selection-panel {
        background: #ffffff;
        border: 1px solid #e5e5e5;
        border-radius: 8px;
        padding: 18px 20px;
        height: 100%;
        display: flex;
        flex-direction: column;
        gap: 14px;
      }
      .selection-panel .panel-heading {
        font-size: 14px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.6px;
        color: #1a1a1a;
        margin: 0;
      }
      .selection-panel .helper-text {
        font-size: 13px;
        color: #6c757d;
        line-height: 1.5;
      }
      .selection-panel .metric-label {
        font-size: 12px;
        color: #6c757d;
        text-transform: uppercase;
        letter-spacing: 0.4px;
      }
      .selection-panel .metric-value {
        font-size: 20px;
        font-weight: 600;
        color: #1a1a1a;
      }
      .selection-panel .metric-row {
        display: flex;
        justify-content: space-between;
        align-items: baseline;
      }
      .enc-breakdown {
        font-size: 12px;
        border-top: 1px solid #eee;
        padding-top: 10px;
      }
      .enc-breakdown .enc-row {
        display: flex;
        justify-content: space-between;
        align-items: baseline;
        margin: 3px 0;
      }
      .enc-breakdown .enc-swatch {
        display: inline-block;
        width: 10px;
        height: 10px;
        border-radius: 2px;
        margin-right: 6px;
        vertical-align: middle;
      }
      .enc-breakdown .enc-name { color: #333; }
      .enc-breakdown .enc-fig  { color: #333; font-variant-numeric: tabular-nums; }

      .enc-pill {
        display: inline-block;
        padding: 2px 8px;
        border-radius: 10px;
        font-size: 11px;
        font-weight: 600;
        vertical-align: middle;
      }

      .section-gap { height: 28px; }

      /* Map display controls — prominent strip above the map */
      .map-controls {
        display: flex;
        flex-wrap: wrap;
        align-items: flex-end;
        gap: 28px 40px;
        padding: 14px 18px;
        margin: 18px 0 14px 0;
        background: #F8F9FA;
        border: 1px solid #E5E5E5;
        border-radius: 8px;
      }
      .map-control-group {
        display: flex;
        flex-direction: column;
      }
      .map-control-label {
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 0.8px;
        text-transform: uppercase;
        color: #495057;
        margin-bottom: 6px;
      }
      /* Make radio buttons look like segmented controls */
      .map-control-group .shiny-options-group {
        display: inline-flex;
        gap: 0;
      }
      .map-control-group .radio-inline {
        margin: 0 !important;
        padding: 7px 14px;
        border: 1px solid #CED4DA;
        background: #FFFFFF;
        cursor: pointer;
        font-size: 14px;
        transition: background 0.12s, color 0.12s;
      }
      .map-control-group .radio-inline:first-child { border-radius: 6px 0 0 6px; }
      .map-control-group .radio-inline:last-child  { border-radius: 0 6px 6px 0; }
      .map-control-group .radio-inline + .radio-inline { margin-left: -1px !important; }
      .map-control-group .radio-inline:has(input:checked) {
        background: #1F3864;
        color: #FFFFFF;
        border-color: #1F3864;
        z-index: 1;
      }
      .map-control-group .radio-inline input[type='radio'] {
        display: none;
      }
      /* Developable-land map: custom HTML legend in the map's bottom-right */
      .hd-dev-legend {
        background: rgba(255,255,255,0.92);
        padding: 8px 12px;
        border-radius: 6px;
        border: 1px solid rgba(0,0,0,0.15);
        font-family: 'Open Sans', sans-serif;
        font-size: 12px;
        line-height: 1.5;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        max-width: 220px;
      }
      .hd-leg-title {
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.6px;
        font-size: 10.5px;
        color: #495057;
        margin-bottom: 4px;
      }
      .hd-leg-row {
        display: flex;
        align-items: center;
        gap: 8px;
        margin: 2px 0;
      }
      .hd-leg-swatch {
        display: inline-block;
        width: 20px;
        height: 12px;
        border: 1.5px solid #555;
        border-radius: 2px;
        flex-shrink: 0;
      }
      .hd-leg-label { color: #1a1a1a; }

      /* Layer guide: tighten markdown rendering inside the bslib container */
      .layer-guide h1 { font-size: 28px; margin-top: 0; margin-bottom: 1rem; font-weight: 700; }
      .layer-guide h2 { font-size: 22px; margin-top: 2rem; margin-bottom: 0.6rem; font-weight: 700; }
      .layer-guide h3 { font-size: 17px; margin-top: 1.4rem; margin-bottom: 0.4rem; font-weight: 700; }
      .layer-guide p, .layer-guide li { color: #1a1a1a; }
      .layer-guide ul { padding-left: 1.4rem; }
      .layer-guide code {
        background: #F2F2F0; padding: 1px 5px; border-radius: 3px;
        font-size: 90%; color: #1a1a1a;
      }
      .layer-guide strong { font-weight: 700; }

      /* Opacity slider — compact */
      .map-control-group .irs { height: 36px; }
      .map-control-group .irs-bar { background: #1F3864; border-top-color: #1F3864; border-bottom-color: #1F3864; }
      .map-control-group .irs-handle > i:first-child { background: #1F3864; }
      .map-control-group .form-group { margin-bottom: 0; }
    ")))
  ),
  
  # ---- Explorer tab ----
  bslib::nav_panel(
    title = "Explorer",
    bslib::layout_sidebar(
      fillable = FALSE,
      sidebar = bslib::sidebar(
        width = 300,
        open=F,
        tags$div(class = "text-muted small text-uppercase mb-2",
                 style = "letter-spacing: 0.6px;", "Filters"),
        selectInput("town", "Town",
                    choices = c("All towns" = "All", setNames(town_levels, town_levels))),
        checkboxGroupInput("use_code", "Land use",
                           choiceNames  = use_choices$USE_DESC,
                           choiceValues = use_choices$USE_CODE,
                           selected     = use_choices$USE_CODE),
        checkboxGroupInput("encumbrance", "Encumbrance",
                           choiceNames  = enc_levels,
                           choiceValues = enc_levels,
                           selected     = enc_levels)
      ),
      
      tags$div(
        # Top stats row
        tags$div(
          class = "stats-row",
          stat_tile("Parcels", formatC(total_parcels, big.mark = ","),
                    "across Amherst & Hadley"),
          stat_tile("Total acres",
                    formatC(round(total_acres, 1), big.mark = ",",
                            format = "f", digits = 1),
                    "all Hampshire College holdings"),
          stat_tile("Assessed value",
                    paste0("$", formatC(total_value, format = "d", big.mark = ",")),
                    "combined land + buildings"),
          stat_tile("Encumbered",
                    paste0(formatC(encumbered_n, big.mark = ","), " parcels"),
                    paste0(formatC(round(encumbered_acres, 1), big.mark = ",",
                                   format = "f", digits = 1), " acres pledged to creditors"))
        ),
        
        # Map display controls — placed prominently above the map
        tags$div(
          class = "map-controls",
          tags$div(
            class = "map-control-group",
            tags$label("Color parcels by", class = "map-control-label"),
            radioButtons("color_by", label = NULL,
                         choices = c("Land use"    = "use",
                                     "Town"        = "town",
                                     "Encumbrance" = "encumbrance"),
                         selected = "encumbrance", inline = TRUE)
          ),
          tags$div(
            class = "map-control-group",
            tags$label("Fill opacity", class = "map-control-label"),
            sliderInput("fill_opacity", label = NULL,
                        min = 0.10, max = 1.00, value = 0.75, step = 0.05,
                        ticks = FALSE, width = "220px")
          )
        ),
        
        # Encumbrance-mode caveat banner (rendered server-side; visible only
        # when color_by == "encumbrance")
        uiOutput("encumbrance_banner"),
        
        # Map row
        fluidRow(
          column(
            width = 3,
            uiOutput("selection_panel")
          ),
          column(
            width = 9,
            bslib::card(
              bslib::card_body(padding = 0, leafletOutput("map", height = MAP_HEIGHT))
            )
          )
        ),
        
        tags$div(class = "section-gap"),
        
        # Table
        bslib::card(
          bslib::card_header(
            tags$div(
              style = "display:flex; justify-content:space-between; align-items:center; gap:12px; flex-wrap:wrap;",
              tags$span("Parcels (click to select; shift-click for range)"),
              tags$div(
                class = "download-row",
                downloadButton("dl_csv_filtered", "CSV (filtered)",
                               class = "btn-sm btn-outline-secondary"),
                downloadButton("dl_csv_selected", "CSV (selected)",
                               class = "btn-sm btn-outline-secondary"),
                downloadButton("dl_geojson_filtered", "GeoJSON (filtered)",
                               class = "btn-sm btn-outline-secondary"),
                downloadButton("dl_geojson_selected", "GeoJSON (selected)",
                               class = "btn-sm btn-outline-secondary")
              )
            )
          ),
          bslib::card_body(DTOutput("table"))
        )
      )
    )
  ),
  
  # ---- Land context tab ----
  bslib::nav_panel(
    title = "Land context",
    bslib::layout_sidebar(
      fillable = FALSE,
      sidebar = bslib::sidebar(
        width = 300,
        open = TRUE,
        tags$div(class = "text-muted small text-uppercase mb-2",
                 style = "letter-spacing: 0.6px;", "Overlays"),
        tags$div(class = "helper-text", style = "font-size: 12px; margin-bottom: 8px;",
                 "Switch basemap (Satellite / Light / Topographic) in the top-right corner of the map."),
        checkboxGroupInput(
          "land_overlays", label = NULL,
          choiceNames  = c("Hampshire parcels",
                           "Hillshade (terrain relief)",
                           "DEP wetlands",
                           "Vernal pools (certified + potential)",
                           "Protected & recreational open space",
                           "NHESP Priority Habitat (rare species)",
                           "Wellhead protection (Zone II + IWPA)",
                           "Areas of Critical Environmental Concern (ACECs)",
                           "Concept sketches"),
          choiceValues = c("parcels", "hillshade",
                           "wetlands", "vernalpools", "openspace",
                           "habitat", "wellhead", "acec",
                           "concepts"),
          selected     = c("parcels", "hillshade",
                           if (!is.null(concept_sketches)) "concepts" else NULL)
        ),
        sliderInput("land_overlay_opacity", "Overlay opacity",
                    min = 0.20, max = 1.00, value = 0.65, step = 0.05,
                    ticks = FALSE, width = "100%"),
        tags$hr(),
        tags$div(class = "text-muted small text-uppercase mb-2",
                 style = "letter-spacing: 0.6px;", "Drawn shapes"),
        uiOutput("drawn_summary"),
        tags$div(style = "margin-top: 12px; display: flex; flex-direction: column; gap: 6px;",
                 downloadButton("download_drawn_geojson", "Download drawn shapes (GeoJSON)",
                                class = "btn-sm btn-outline-primary",
                                style = "width: 100%;"),
                 actionButton("clear_drawn", "Clear all drawn shapes",
                              class = "btn-sm btn-outline-secondary",
                              style = "width: 100%;")),
        tags$hr(),
        tags$div(class = "text-muted small text-uppercase mb-2",
                 style = "letter-spacing: 0.6px;", "Drawing template"),
        tags$div(class = "helper-text", style = "font-size: 12px; margin-bottom: 8px;",
                 "Print-ready PDF with parcels and constraints. Clean version ",
                 "uses a white background and shows buildings + roads as ",
                 "drawn references; satellite version uses Esri imagery."),
        tags$div(style = "display: flex; flex-direction: column; gap: 6px;",
          downloadButton("download_pdf_template",
                         "Map template (clean, PDF)",
                         class = "btn-sm btn-outline-primary",
                         style = "width: 100%;"),
          downloadButton("download_pdf_template_satellite",
                         "Map template (satellite, PDF)",
                         class = "btn-sm btn-outline-primary",
                         style = "width: 100%;")
        )
      ),
      tags$div(
        tags$div(
          class = "helper-text",
          style = "margin-bottom: 14px;",
          "Use the toolbar at top-left of the map to draw polygons, rectangles, ",
          "or lines for study areas. ",
          tags$strong("Click anywhere on the map "),
          "(when no draw tool is active) to look up ground elevation from the ",
          "USGS 3DEP 1-meter elevation dataset. For terrain context, switch to the ",
          tags$strong("Topographic basemap"), " (top-right): it shows USGS contour ",
          "lines, hydrography, and labels...or enable the Hillshade overlay for ",
          "relief shading on any basemap. "
        ),
        bslib::card(
          bslib::card_body(padding = 0,
                           leafletOutput("land_map", height = MAP_HEIGHT))
        )
      )
    )
  ),

  # ---- Developable land tab ----
  bslib::nav_panel(
    title = "Developable land",
    tags$div(
      tags$div(
        class = "helper-text",
        style = "max-width: 1100px; margin: 20px auto 6px auto; padding: 0 1.25rem;",
        "Each parcel's gross acreage minus regulatory wetlands and permanently ",
        "protected open space gives ", tags$strong("net unconstrained"),
        " acres. Subtracting the 100-ft wetland Buffer Zone and any NHESP ",
        "Priority Habitat overlap gives ", tags$strong("net easy-to-build"),
        " acres (the part where development would face the least regulatory ",
        "friction). Estimated road frontage is the length of each parcel's ",
        "boundary that runs within ~30 ft of a MassDOT road centerline. ",
        "See the Layer guide tab for caveats on each input."
      ),
      tags$div(class = "stats-row", style = "max-width: 1100px; margin: 12px auto;",
               uiOutput("dev_stat_tiles")),
      tags$div(
        style = "max-width: 1100px; margin: 0 auto 14px auto; padding: 0 1.25rem; display:flex; gap:28px; align-items:center; flex-wrap: wrap;",
        tags$div(style = "display:flex; gap:10px; align-items:center;",
          tags$label("Show overlays:", style = "font-weight: 600; font-size: 13px; margin: 0;"),
          checkboxGroupInput("dev_overlays", label = NULL, inline = TRUE,
                             choiceNames  = c("Hillshade", "Wetlands",
                                              "Vernal pools",
                                              "Permanent open space",
                                              "Priority habitat",
                                              "Developable area (green)",
                                              "Concept sketches"),
                             choiceValues = c("hillshade", "wetlands",
                                              "vernalpools",
                                              "openspace", "habitat",
                                              "devarea", "concepts"),
                             selected = c("devarea", "wetlands",
                                          if (!is.null(concept_sketches)) "concepts" else NULL))
        ),
        tags$div(style = "display:flex; gap:10px; align-items:center;",
          tags$label("Wetland buffer:", style = "font-weight: 600; font-size: 13px; margin: 0; white-space: nowrap;"),
          radioButtons("dev_buffer_scenario", label = NULL, inline = TRUE,
                       choices = c("Standard (100 ft)" = "standard",
                                   "Strict (200 ft)"   = "strict"),
                       selected = "standard")
        ),
        tags$div(style = "display:flex; gap:10px; align-items:center; min-width: 220px;",
          tags$label("Overlay opacity:", style = "font-weight: 600; font-size: 13px; margin: 0; white-space: nowrap;"),
          sliderInput("dev_overlay_opacity", label = NULL,
                      min = 0.0, max = 1.0, value = 1.0, step = 0.05,
                      ticks = FALSE, width = "140px")
        )
      ),
      tags$div(style = "max-width: 1100px; margin: 0 auto; padding: 0 1.25rem;",
        bslib::card(
          bslib::card_body(padding = 0,
                           leafletOutput("dev_map", height = MAP_HEIGHT))
        ),
        tags$div(class = "section-gap"),
        bslib::card(
          bslib::card_header(tags$div(
            style = "display:flex; justify-content:space-between; align-items:center; gap:12px; flex-wrap:wrap;",
            tags$span("Selected parcels"),
            actionButton("clear_dev_selection", "Clear selection",
                         class = "btn-sm btn-outline-secondary")
          )),
          bslib::card_body(uiOutput("dev_selection_summary"))
        ),
        tags$div(class = "section-gap"),
        bslib::card(
          bslib::card_header(tags$span("Per-parcel summary")),
          bslib::card_body(DTOutput("dev_table"))
        )
      )
    )
  ),

  # ---- Layer guide tab ----
  bslib::nav_panel(
    title = "Layer guide",
    tags$div(
      class = "layer-guide",
      style = "max-width: 900px; margin: 2rem auto; padding: 0 1.25rem; line-height: 1.65;",
      if (file.exists("docs/layer_guide.md")) {
        shiny::includeMarkdown("docs/layer_guide.md")
      } else {
        tags$p(tags$em("Layer guide not found. Expected at docs/layer_guide.md."))
      }
    )
  ),

  # ---- Debt tab ----
  bslib::nav_panel(
    title = "Debt",
    tags$div(
      style = "max-width: 1100px; margin: 2rem auto; padding: 0 1.25rem; line-height: 1.6;",
      
      tags$h2("Debt summary", style = "margin-top: 0;"),
      tags$p(
        "Hampshire's outstanding debt at June 30, 2025, consists of two bond ",
        "series and one charitable-trust note. Balances and terms below are ",
        "taken from the College's FY2025 audited financial statements ",
        "(CliftonLarsonAllen LLP, dated November 25, 2025), Note 10. ",
        "Collateral descriptions come from the Hampshire County Registry of ",
        "Deeds mortgage instruments."
      ),
      
      # --- Summary cards row ---
      tags$div(
        style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 16px; margin: 24px 0;",
        # M&T card
        tags$div(
          style = "background: #FFFFFF; border: 1px solid #e5e5e5; border-left: 4px solid #E8A33D; border-radius: 8px; padding: 18px 22px;",
          tags$div(style = "font-size: 11px; font-weight: 700; letter-spacing: 0.6px; text-transform: uppercase; color: #8A5A00;",
                   "Series 2012 Bonds"),
          tags$div(style = "font-size: 24px; font-weight: 600; margin-top: 4px;", "$8,989,392"),
          tags$div(style = "font-size: 13px; color: #555; margin-top: 2px;", "M&T Bank · 4.4% · short-term")
        ),
        # BankUnited card
        tags$div(
          style = "background: #FFFFFF; border: 1px solid #e5e5e5; border-left: 4px solid #1F77B4; border-radius: 8px; padding: 18px 22px;",
          tags$div(style = "font-size: 11px; font-weight: 700; letter-spacing: 0.6px; text-transform: uppercase; color: #1F3B5A;",
                   "Series 2016 Bonds"),
          tags$div(style = "font-size: 24px; font-weight: 600; margin-top: 4px;", "$12,564,929"),
          tags$div(style = "font-size: 13px; color: #555; margin-top: 2px;", "BankUnited, N.A. · 2.8% · short-term")
        ),
        # Kendall card
        tags$div(
          style = "background: #FFFFFF; border: 1px solid #e5e5e5; border-left: 4px solid #2CA02C; border-radius: 8px; padding: 18px 22px;",
          tags$div(style = "font-size: 11px; font-weight: 700; letter-spacing: 0.6px; text-transform: uppercase; color: #1F5B1F;",
                   "2024 Note"),
          tags$div(style = "font-size: 24px; font-weight: 600; margin-top: 4px;", "$4,500,000"),
          tags$div(style = "font-size: 13px; color: #555; margin-top: 2px;", "Henry P. Kendall Foundation · 5.0% · long-term")
        )
      ),
      
      # --- Total line ---
      tags$div(
        style = "background: #F8F9FA; border: 1px solid #E5E5E5; border-radius: 8px; padding: 14px 22px; margin-bottom: 24px; display: flex; justify-content: space-between; align-items: baseline;",
        tags$span(style = "font-size: 13px; font-weight: 600; text-transform: uppercase; letter-spacing: 0.6px; color: #495057;",
                  "Total outstanding principal"),
        tags$span(style = "font-size: 22px; font-weight: 700; color: #1a1a1a; font-variant-numeric: tabular-nums;",
                  "$26,054,321")
      ),
      
      # --- Detailed table ---
      tags$h3("Details", style = "margin-top: 1.5rem;"),
      tags$div(
        style = "overflow-x: auto;",
        tags$table(
          class = "table table-sm",
          style = "font-size: 13px; vertical-align: top;",
          tags$thead(
            tags$tr(
              tags$th("Silo"),
              tags$th("Creditor"),
              tags$th("Instrument"),
              tags$th(style = "text-align: right;", "Outstanding (6/30/25)"),
              tags$th(style = "text-align: right;", "Rate"),
              tags$th("Maturity"),
              tags$th("Classification"),
              tags$th("Collateral")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(tags$strong("2012")),
              tags$td("M&T Bank"),
              tags$td("Mass. Dev. Finance Agency Revenue Bonds, Hampshire College Issue, Series 2012"),
              tags$td(style = "text-align: right; font-variant-numeric: tabular-nums;", "$8,989,392"),
              tags$td(style = "text-align: right; font-variant-numeric: tabular-nums;", "4.4%"),
              tags$td("2026"),
              tags$td("Short-term"),
              tags$td("~16.1 acres of 25A-23 (Amherst) + ~1.66 ac of 8_4_1 (Hadley). Named facilities: Multi-Sports, Day Care.")
            ),
            tags$tr(
              tags$td(tags$strong("2016")),
              tags$td("BankUnited, N.A."),
              tags$td("Mass. Dev. Finance Agency Revenue Bonds, Hampshire College Issue, Series 2016"),
              tags$td(style = "text-align: right; font-variant-numeric: tabular-nums;", "$12,564,929"),
              tags$td(style = "text-align: right; font-variant-numeric: tabular-nums;", "2.8%"),
              tags$td("2026"),
              tags$td("Short-term"),
              tags$td("17.6-acre carve-out of 22D-15, per Plan Book 235 Plan 100. Contains Library, Kern Center, Cole Science, Robert Crown Center, Red Barn.")
            ),
            tags$tr(
              tags$td(tags$strong("2024")),
              tags$td("Henry P. Kendall Foundation"),
              tags$td("Promissory note + mortgage, October 29, 2024"),
              tags$td(style = "text-align: right; font-variant-numeric: tabular-nums;", "$4,500,000"),
              tags$td(style = "text-align: right; font-variant-numeric: tabular-nums;", "5.0%"),
              tags$td("October 2049"),
              tags$td("Long-term"),
              tags$td("~340 acres across 8 Amherst + 2 Hadley parcels; assigned ROFR; restrictive covenants")
            )
          )
        )
      ),
      
      # --- Structural notes ---
      tags$h3("Notes on the debt structure", style = "margin-top: 2rem;"),
      tags$p(
        tags$strong("Covenant defaults. "),
        "Per the FY2025 audit (Note 10), Hampshire failed covenants on both the ",
        "Series 2012 and Series 2016 bonds in FY2024 and FY2025. The audit does ",
        "not specify which covenants. Both bond series have therefore been ",
        "reclassified from long-term to short-term debt on the balance sheet."
      ),
      tags$p(
        tags$strong("Series 2012 put option. "),
        "The 2012 bondholder exercised its put option on April 29, 2022, issuing ",
        "an irrevocable notice of mandatory tender originally due December 28, ",
        "2022. Tender has been extended successively; as of the audit date, the ",
        "extension ran to March 31, 2026 (Note 10) or September 2026 (Note 16)."
      ),
      tags$p(
        tags$strong("Atkins property and the Kendall note. "),
        "The Kendall note structure defers principal payments until the \"Atkins ",
        "property land sale\" closes. At that point a $3,000,000 balloon principal ",
        "payment is due, with monthly P+I payments beginning thereafter through ",
        "maturity in October 2049. Interest-only payments apply in the interim."
      ),
      tags$p(
        tags$strong("Extension mortgage. "),
        "Both 2012 and 2016 bondholders agreed to extend their mandatory tender ",
        "dates to September 2026 \"in exchange for a mortgage on certain ",
        "unencumbered properties\" (audit Note 16). This additional mortgage has ",
        "not been located in the Hampshire County Registry of Deeds. Additional ",
        "parcels may therefore be pledged beyond what is reflected on the ",
        "Explorer map."
      ),
      tags$p(
        tags$strong("Source. "),
        "Outstanding principal balances are from the Hampshire College FY2025 ",
        "audited financial statements (CliftonLarsonAllen LLP, November 25, 2025), ",
        "Note 10. Collateral descriptions are from the Registry of Deeds ",
        "instruments. The Kendall Foundation is identified by Registry ",
        "signatures at Bk 15269 Pg 247; the FY25 audit refers to this lender ",
        "as \"a charitable trust\"."
      )
    )
  ),
  
  # ---- About tab ----
  bslib::nav_panel(
    title = "About",
    tags$div(
      style = "max-width: 760px; margin: 2rem auto; padding: 0 1.25rem; line-height: 1.65;",
      
      tags$h2("About this tool", style = "margin-top: 0;"),
      tags$p(
        "This is an interactive explorer for the parcels owned by the Trustees ",
        "of Hampshire College in the Massachusetts towns of Amherst and Hadley. ",
        "Click any parcel on the map or any row in the table to select it; the ",
        "selection panel updates to show the combined acreage and assessed ",
        "value of whatever you've selected, broken down by encumbrance."
      ),
      
      tags$h3("Data sources"),
      tags$p(
        "Parcel boundaries and assessor attributes come from the ",
        tags$a(href = "https://www.mass.gov/info-details/massgis-data-property-tax-parcels",
               target = "_blank", rel = "noopener",
               "MassGIS Level 3 Standardized Assessors' Parcels"),
        " dataset, maintained by the Commonwealth of Massachusetts Bureau of ",
        "Geographic Information. Individual town vintages:"
      ),
      tags$ul(
        tags$li(tags$strong("Amherst:"), " fiscal year 2024 (municipal ID M008)"),
        tags$li(tags$strong("Hadley:"),  " fiscal year 2025 (municipal ID M117)")
      ),
      tags$p(
        "Land-use descriptions come from each town's Use Code Look-Up Table ",
        "(", tags$code("UC_LUT.dbf"), "), which publishes the Massachusetts ",
        "Department of Revenue's standard classification codes. Assessor's ",
        "parcel mapping is a representation of property boundaries, not an ",
        "authoritative source — the authoritative record is held at the ",
        "Hampshire County Registry of Deeds."
      ),
      
      tags$h3("About"),
      tags$p(
        "Built by Chris Wenz, 02F (current part-time Director of Institutional ",
        "Research at Hampshire), to support efforts to understand the college's ",
        "finances."
      ),
      
      tags$h3("Contact"),
      tags$p(
        "Questions, corrections, or suggestions are welcome. Reach me at ",
        tags$a(href = "mailto:cawenz@gmail.com", "cawenz@gmail.com"),
        "."
      ),
      
      tags$hr(style = "margin: 2rem 0;"),
      tags$p(
        style = "color: #6c757d; font-size: 13px;",
        "Built with ", tags$a(href = "https://shiny.posit.co/", target = "_blank",
                              rel = "noopener", "Shiny"),
        ", ", tags$a(href = "https://rstudio.github.io/leaflet/", target = "_blank",
                     rel = "noopener", "Leaflet for R"),
        ", and ", tags$a(href = "https://r-spatial.github.io/sf/", target = "_blank",
                         rel = "noopener", "sf"), "."
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  filtered <- reactive({
    # Use the BankUnited-trimmed geometry only when coloring by Encumbrance, so
    # the overlay can sit against the satellite basemap. In Land use / Town modes,
    # use the original contiguous parcel geometry (no hole in 22D-15).
    d <- if (isTRUE(input$color_by == "encumbrance")) parcels_trimmed else parcels
    if (input$town != "All") d <- d[d$TOWN == input$town, ]
    if (!is.null(input$use_code) && length(input$use_code) > 0)
      d <- d[d$USE_CODE %in% input$use_code, ]
    if (!is.null(input$encumbrance) && length(input$encumbrance) > 0)
      d <- d[d$ENCUMBRANCE %in% input$encumbrance, ]
    d
  })
  
  selected <- reactive({
    d <- filtered()
    sel <- input$table_rows_selected
    if (length(sel) == 0 || nrow(d) < max(sel)) return(NULL)
    d[sel, ]
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
      addProviderTiles("CartoDB.Positron", group = "Light") |>
      addLayersControl(
        baseGroups = c("Satellite", "Light"),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      setView(lng = -72.55, lat = 42.34, zoom = 12)
  })
  
  observe({
    d <- filtered()
    pal <- switch(input$color_by,
                  "town"        = town_pal,
                  "encumbrance" = enc_pal,
                  use_pal)
    field <- switch(input$color_by,
                    "town"        = d$TOWN,
                    "encumbrance" = d$ENCUMBRANCE,
                    d$USE_CODE)
    
    proxy <- leafletProxy("map") |>
      clearGroup("parcels") |>
      clearGroup("selected") |>
      clearGroup("collateral_overlay")
    
    if (nrow(d) == 0) return()
    
    proxy |>
      addPolygons(
        data = d,
        layerId = ~as.character(row_id),
        group = "parcels",
        fillColor = pal(field),
        fillOpacity = input$fill_opacity,
        color = "#1a1a1a",
        opacity = min(1, input$fill_opacity + 0.15),
        weight = 1.2,
        popup = ~popup_html,
        label = ~paste0(MAP_PAR_ID, " — ", SITE_ADDR),
        highlightOptions = highlightOptions(
          weight = 3, color = "#ffffff",
          fillOpacity = min(1, input$fill_opacity + 0.10), bringToFront = TRUE
        )
      )
    
    # ---- Collateral overlay: show the exact 17.6-ac BankUnited carve-out ----
    # Only draw when user is coloring by encumbrance, so it doesn't clutter the
    # land-use or town views. The overlap has been cut out of the base parcel at
    # data-load time, so this overlay sits directly on the satellite basemap and
    # is fully hoverable.
    if (!is.null(bu_collateral) && input$color_by == "encumbrance") {
      proxy |>
        addPolygons(
          data = bu_collateral,
          group = "collateral_overlay",
          fillColor = "#1F77B4",
          fillOpacity = input$fill_opacity,
          color = "#0D3D66",
          opacity = 1,
          weight = 2,
          popup = paste0(
            "<div style='font-family: Open Sans, sans-serif; font-size: 13px;'>",
            "<strong>BankUnited 2016 collateral</strong><br>",
            "17.6 acres (carve-out of parcel 22D-15)<br>",
            "Per Plan Book 235, Plan 100<br>",
            "Secures $12.56M Series 2016 bonds (per FY25 audit).<br>",
            "<em>Polygon reconstructed from metes-and-bounds; closure 0.01 ft.</em>",
            "</div>"
          ),
          label = "BankUnited 2016 collateral (17.6 ac)",
          highlightOptions = highlightOptions(
            weight = 3, color = "#ffffff",
            fillOpacity = min(1, input$fill_opacity + 0.10),
            bringToFront = TRUE
          )
        )
    }
    
    bb <- sf::st_bbox(d)
    proxy |> fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  })
  
  output$table <- renderDT({
    d <- sf::st_drop_geometry(filtered())
    DT::datatable(
      d |> dplyr::select(TOWN, MAP_PAR_ID, SITE_ADDR, USE_DESC, ACRES_CALC,
                         LAND_VAL, BLDG_VAL, TOTAL_VAL, ENCUMBRANCE, YEAR_BUILT, FY),
      colnames = c("Town", "Parcel", "Address", "Land use", "Acres",
                   "Land $", "Bldg $", "Total $", "Encumbrance", "Built", "FY"),
      selection = "multiple",
      rownames  = FALSE,
      options   = list(pageLength = 10, dom = "tip", scrollX = TRUE,
                       columnDefs = list(list(className = "dt-right",
                                              targets = c(4, 5, 6, 7, 9, 10))))
    ) |>
      DT::formatCurrency(c("LAND_VAL", "BLDG_VAL", "TOTAL_VAL"), digits = 0) |>
      DT::formatRound("ACRES_CALC", digits = 2)
  }, server = FALSE)
  
  observeEvent(input$map_shape_click, {
    req(input$map_shape_click$id)
    d <- filtered()
    hit <- which(as.character(d$row_id) == input$map_shape_click$id)
    if (length(hit) == 0) return()
    sel <- input$table_rows_selected
    sel <- if (hit %in% sel) setdiff(sel, hit) else union(sel, hit)
    DT::dataTableProxy("table") |> DT::selectRows(sel)
  })
  
  observeEvent(input$table_rows_selected, ignoreNULL = FALSE, {
    d <- filtered()
    leafletProxy("map") |> clearGroup("selected")
    sel <- input$table_rows_selected
    if (length(sel) > 0 && nrow(d) >= max(sel)) {
      leafletProxy("map") |>
        addPolygons(data = d[sel, ], group = "selected",
                    fill = FALSE, color = "#FFEB3B", weight = 4,
                    opacity = 1,
                    options = pathOptions(interactive = FALSE))
    }
  })
  
  observeEvent(list(input$town, input$use_code, input$encumbrance), ignoreInit = TRUE, {
    DT::dataTableProxy("table") |> DT::selectRows(NULL)
  })
  
  observeEvent(input$clear_sel, {
    DT::dataTableProxy("table") |> DT::selectRows(NULL)
  })
  
  # Encumbrance-mode caveat banner. Shown only when user has selected the
  # Encumbrance coloring — otherwise returns NULL so no space is taken up.
  output$encumbrance_banner <- renderUI({
    if (!isTRUE(input$color_by == "encumbrance")) return(NULL)
    tags$div(
      class = "alert alert-warning",
      style = "margin: 0 0 14px 0; font-size: 13px; line-height: 1.5;",
      tags$strong("Incomplete picture. "),
      "Additional properties may have been pledged to the 2012 and 2016 ",
      "bondholders in exchange for their September 2026 extension, but the ",
      "instrument has not been found in the Registry. Parcels shown here as ",
      "unencumbered may actually be encumbered."
    )
  })
  
  output$selection_panel <- renderUI({
    sel_df <- selected()
    has_sel <- !is.null(sel_df) && nrow(sel_df) > 0
    
    clear_btn <- actionButton(
      "clear_sel",
      "Clear selection",
      class = paste("btn btn-sm",
                    if (has_sel) "btn-outline-secondary" else "btn-outline-secondary disabled"),
      style = "width: 100%;",
      disabled = if (!has_sel) NA else NULL
    )
    
    if (has_sel) {
      n   <- nrow(sel_df)
      ac  <- sum(sel_df$ACRES_CALC, na.rm = TRUE)
      val <- sum(sel_df$TOTAL_VAL,  na.rm = TRUE)
      
      # Encumbrance breakdown over the selection
      enc_df <- sf::st_drop_geometry(sel_df) |>
        dplyr::group_by(ENCUMBRANCE) |>
        dplyr::summarize(
          n_sel   = dplyr::n(),
          ac_sel  = sum(ACRES_CALC, na.rm = TRUE),
          val_sel = sum(TOTAL_VAL,  na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::arrange(
          factor(ENCUMBRANCE, levels = enc_levels)
        )
      
      enc_rows <- lapply(seq_len(nrow(enc_df)), function(i) {
        e <- enc_df[i, ]
        swatch_color <- unname(enc_colors[e$ENCUMBRANCE])
        tags$div(
          class = "enc-row",
          tags$span(
            tags$span(class = "enc-swatch",
                      style = paste0("background:", swatch_color, ";")),
            tags$span(class = "enc-name", e$ENCUMBRANCE)
          ),
          tags$span(
            class = "enc-fig",
            sprintf("%s · %s ac · $%s",
                    formatC(e$n_sel, big.mark = ","),
                    formatC(round(e$ac_sel, 1), big.mark = ",",
                            format = "f", digits = 1),
                    formatC(e$val_sel, format = "d", big.mark = ","))
          )
        )
      })
      
      body <- tagList(
        tags$div(class = "helper-text",
                 "Currently selected. Click parcels on the map or rows in the ",
                 "table to add or remove them."),
        tags$div(
          class = "metric-row",
          tags$div(
            tags$div(class = "metric-label", "Parcels"),
            tags$div(class = "metric-value", formatC(n, big.mark = ","))
          ),
          tags$div(
            style = "text-align: right;",
            tags$div(class = "metric-label", "Acres"),
            tags$div(class = "metric-value",
                     formatC(round(ac, 1), big.mark = ",",
                             format = "f", digits = 1))
          )
        ),
        tags$div(
          tags$div(class = "metric-label", "Assessed value"),
          tags$div(class = "metric-value",
                   paste0("$", formatC(val, format = "d", big.mark = ",")))
        ),
        tags$div(
          class = "enc-breakdown",
          tags$div(class = "metric-label", style = "margin-bottom: 6px;",
                   "By encumbrance"),
          enc_rows
        )
      )
    } else {
      body <- tagList(
        tags$div(class = "helper-text",
                 "Click parcels on the map to summarize them here. ",
                 "Shift-click in the table to select a range. ",
                 "Your running totals, including a breakdown by encumbrance, ",
                 "will appear in this panel."),
        tags$div(
          class = "metric-row",
          tags$div(
            tags$div(class = "metric-label", "Parcels"),
            tags$div(class = "metric-value", style = "color: #adb5bd;", "—")
          ),
          tags$div(
            style = "text-align: right;",
            tags$div(class = "metric-label", "Acres"),
            tags$div(class = "metric-value", style = "color: #adb5bd;", "—")
          )
        ),
        tags$div(
          tags$div(class = "metric-label", "Assessed value"),
          tags$div(class = "metric-value", style = "color: #adb5bd;", "—")
        )
      )
    }
    
    tags$div(
      class = "selection-panel",
      tags$div(class = "panel-heading", "Selection summary"),
      clear_btn,
      body
    )
  })
  
  # ===== Land context tab =====

  # Tile service URLs (all free, no API keys required)
  USGS_TOPO_URL <- paste0(
    "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/",
    "tile/{z}/{y}/{x}"
  )
  ESRI_HILLSHADE_URL <- paste0(
    "https://server.arcgisonline.com/ArcGIS/rest/services/Elevation/",
    "World_Hillshade/MapServer/tile/{z}/{y}/{x}"
  )

  # Reactive store for user-drawn shapes (as an sf collection in WGS84)
  drawn_sf <- reactiveVal(NULL)

  # Helper: parse a single leafpm GeoJSON Feature into a one-row sf data frame.
  # Stores the leaflet feature id so we can match against edits/deletes later.
  feature_to_sf <- function(feature) {
    if (is.null(feature)) return(NULL)
    fid <- feature[["_leaflet_id"]] %||% NA_integer_
    geo <- jsonlite::toJSON(feature, auto_unbox = TRUE, null = "null")
    out <- tryCatch(sf::st_read(geo, quiet = TRUE), error = function(e) NULL)
    if (is.null(out) || nrow(out) == 0) return(NULL)
    out$leaflet_id <- as.integer(fid)
    sf::st_sf(out, crs = 4326)
  }

  # ----- Vector overlay helpers (DRY across initial render + toggle) -----
  add_wetlands_layer <- function(map, data, op) {
    map |> addPolygons(
      data        = data,
      group       = "wetlands",
      fillColor   = "#3FA9F5",
      fillOpacity = pmin(1, op * 0.55),
      color       = "#1F77B4",
      opacity     = 1,
      weight      = 0.8,
      label       = ~paste0(IT_VALDESC, " — ", round(AREAACRES, 2), " ac"),
      options     = pathOptions(interactive = TRUE)
    )
  }
  add_openspace_layer <- function(map, data, op) {
    owner_label <- c(M = "Municipal", S = "State", F = "Federal",
                     P = "Private (nonprofit)", L = "Land trust",
                     N = "Nonprofit", O = "Other")
    prot_label  <- c(P = "Permanent", L = "Limited", N = "None")
    map |> addPolygons(
      data        = data,
      group       = "openspace",
      fillColor   = "#2CA02C",
      fillOpacity = pmin(1, op * 0.35),
      color       = "#1B5E20",
      opacity     = 1,
      weight      = 1.2,
      label       = ~paste0(
        ifelse(is.na(SITE_NAME) | SITE_NAME == "", "Protected open space", SITE_NAME),
        " — ", unname(owner_label[OWNER_TYPE]),
        ", ", unname(prot_label[LEV_PROT]), " protection"
      ),
      options     = pathOptions(interactive = TRUE)
    )
  }
  add_habitat_layer <- function(map, data, op) {
    map |> addPolygons(
      data        = data,
      group       = "habitat",
      fillColor   = "#9B59B6",
      fillOpacity = pmin(1, op * 0.40),
      color       = "#5E3370",
      opacity     = 1,
      weight      = 1.5,
      dashArray   = "6,3",
      label       = ~paste0("NHESP Priority Habitat #", PRIHAB_ID),
      options     = pathOptions(interactive = TRUE)
    )
  }
  add_wellhead_layer <- function(map, data, op) {
    color_for <- function(zt) {
      ifelse(grepl("^Zone II", zt %||% ""), "#0288D1", "#80DEEA")
    }
    map |> addPolygons(
      data        = data,
      group       = "wellhead",
      fillColor   = ~color_for(ZONE_TYPE),
      fillOpacity = pmin(1, op * 0.30),
      color       = ~color_for(ZONE_TYPE),
      opacity     = 1,
      weight      = 1.5,
      label       = ~paste0(SUPPLIER, " — ", ZONE_TYPE,
                            " (", TOWN, ")"),
      options     = pathOptions(interactive = TRUE)
    )
  }
  add_acec_layer <- function(map, data, op) {
    map |> addPolygons(
      data        = data,
      group       = "acec",
      fillColor   = "#FF7043",
      fillOpacity = pmin(1, op * 0.35),
      color       = "#BF360C",
      opacity     = 1,
      weight      = 2,
      label       = ~paste0(NAME, " ACEC"),
      options     = pathOptions(interactive = TRUE)
    )
  }
  # User-drawn concept sketches loaded from data/concepts/*.geojson.
  # Distinct magenta tone so they stand out from the regulatory palette.
  add_concepts_layer <- function(map, data, op) {
    geom_types <- as.character(sf::st_geometry_type(data))
    is_poly <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
    is_line <- geom_types %in% c("LINESTRING", "MULTILINESTRING")
    if (any(is_poly)) {
      map <- map |> addPolygons(
        data = data[is_poly, ], group = "concepts",
        fillColor = "#E91E63", fillOpacity = pmin(1, op * 0.30),
        color = "#880E4F", opacity = pmin(1, op),
        weight = 2, dashArray = "8,3",
        label = ~paste0("Concept: ", concept_source),
        options = pathOptions(interactive = TRUE)
      )
    }
    if (any(is_line)) {
      map <- map |> addPolylines(
        data = data[is_line, ], group = "concepts",
        color = "#880E4F", opacity = pmin(1, op),
        weight = 3, dashArray = "8,3",
        label = ~paste0("Concept: ", concept_source),
        options = pathOptions(interactive = TRUE)
      )
    }
    map
  }

  # Vernal pools are points. Certified = solid teal-green, Potential = hollow.
  add_vernalpools_layer <- function(map, certified, potential, op) {
    if (!is.null(potential) && nrow(potential) > 0) {
      map <- map |> addCircleMarkers(
        data = potential, group = "vernalpools",
        radius = 4, stroke = TRUE, weight = 1.5,
        color = "#00897B", fillColor = "#FFFFFF",
        opacity = pmin(1, op), fillOpacity = pmin(1, op * 0.5),
        label = "Potential vernal pool (NHESP)",
        options = pathOptions(interactive = TRUE)
      )
    }
    if (!is.null(certified) && nrow(certified) > 0) {
      map <- map |> addCircleMarkers(
        data = certified, group = "vernalpools",
        radius = 5, stroke = TRUE, weight = 1.5,
        color = "#004D40", fillColor = "#1DE9B6",
        opacity = pmin(1, op), fillOpacity = pmin(1, op * 0.9),
        label = ~paste0("Certified vernal pool #", CVP_NUM),
        options = pathOptions(interactive = TRUE)
      )
    }
    map
  }

  # Render the base land-context map. We add leafpm at render time so the
  # drawing toolbar is wired up exactly once.
  output$land_map <- renderLeaflet({
    bb <- sf::st_bbox(parcels)
    initial_overlays <- isolate(input$land_overlays %||% c("parcels", "hillshade"))
    initial_op       <- isolate(input$land_overlay_opacity %||% 0.65)

    m <- leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
      addProviderTiles("CartoDB.Positron", group = "Light") |>
      addTiles(
        urlTemplate = USGS_TOPO_URL,
        attribution = "USGS The National Map · USGS Topographic basemap",
        options     = tileOptions(maxZoom = 16),
        group       = "Topographic"
      ) |>
      addPolygons(
        data        = parcels,
        group       = "parcels_overlay",
        fillColor   = "#FFEB3B",
        fillOpacity = 0.12,
        color       = "#FF6B00",
        opacity     = 1,
        weight      = 2.5,
        label       = ~paste0(MAP_PAR_ID, " — ", SITE_ADDR),
        popup       = ~popup_html,
        highlightOptions = highlightOptions(
          weight = 4, color = "#ffffff",
          fillOpacity = 0.25, bringToFront = TRUE
        ),
        options     = pathOptions(interactive = TRUE)
      )

    if (!("parcels" %in% initial_overlays)) {
      m <- m |> hideGroup("parcels_overlay")
    }

    if ("hillshade" %in% initial_overlays) {
      m <- m |> addTiles(
        urlTemplate = ESRI_HILLSHADE_URL,
        attribution = "Hillshade &copy; Esri",
        options     = tileOptions(opacity = initial_op, maxZoom = 19),
        group       = "hillshade"
      )
    }

    if ("wetlands" %in% initial_overlays && !is.null(dep_wetlands)) {
      m <- add_wetlands_layer(m, dep_wetlands, initial_op)
    }
    if ("vernalpools" %in% initial_overlays &&
        (!is.null(vernal_certified) || !is.null(vernal_potential))) {
      m <- add_vernalpools_layer(m, vernal_certified, vernal_potential, initial_op)
    }
    if ("openspace" %in% initial_overlays && !is.null(protected_open)) {
      m <- add_openspace_layer(m, protected_open, initial_op)
    }
    if ("habitat" %in% initial_overlays && !is.null(priority_habitat)) {
      m <- add_habitat_layer(m, priority_habitat, initial_op)
    }
    if ("wellhead" %in% initial_overlays && !is.null(wellhead_protect)) {
      m <- add_wellhead_layer(m, wellhead_protect, initial_op)
    }
    if ("acec" %in% initial_overlays && !is.null(acecs)) {
      m <- add_acec_layer(m, acecs, initial_op)
    }
    if ("concepts" %in% initial_overlays && !is.null(concept_sketches)) {
      m <- add_concepts_layer(m, concept_sketches, initial_op)
    }

    m |> addLayersControl(
        baseGroups = c("Satellite", "Light", "Topographic"),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      leafpm::addPmToolbar(
        targetGroup = "drawn",
        toolbarOptions = leafpm::pmToolbarOptions(
          position         = "topleft",
          drawMarker       = FALSE,
          drawCircle       = FALSE,
          drawPolyline     = TRUE,
          drawRectangle    = TRUE,
          drawPolygon      = TRUE,
          editMode         = TRUE,
          cutPolygon       = FALSE,
          removalMode      = TRUE
        ),
        drawOptions = leafpm::pmDrawOptions(
          snappable    = TRUE,
          allowSelfIntersection = FALSE
        ),
        editOptions = leafpm::pmEditOptions(
          preventMarkerRemoval = FALSE,
          allowSelfIntersection = FALSE
        )
      ) |>
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]) |>
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          window._hd_land_map = map;

          // Track basemap for any future basemap-aware styling
          function setBaseClass(name) {
            if (name === 'Light') {
              el.classList.add('hd-light-base');
              el.classList.remove('hd-dark-base');
            } else {
              el.classList.add('hd-dark-base');
              el.classList.remove('hd-light-base');
            }
          }
          setBaseClass('Satellite');
          map.on('baselayerchange', function(e) { setBaseClass(e.name); });

          // Track ONLY user-drawn layers (not addPolygons overlays). We listen
          // for Geoman's create event and stamp each new layer so we can
          // remove exactly those on 'Clear all drawn shapes'.
          window._hd_drawn_ids = new Set();
          map.on('pm:create', function(e) {
            if (e && e.layer) window._hd_drawn_ids.add(L.Util.stamp(e.layer));
          });
          map.on('pm:remove', function(e) {
            if (e && e.layer) window._hd_drawn_ids.delete(L.Util.stamp(e.layer));
          });

          if (window.Shiny) {
            Shiny.addCustomMessageHandler('hd_clear_geoman', function(message) {
              var m = window._hd_land_map;
              if (!m) return;
              var toRemove = [];
              m.eachLayer(function(layer) {
                if (window._hd_drawn_ids.has(L.Util.stamp(layer))) {
                  toRemove.push(layer);
                }
              });
              toRemove.forEach(function(layer) { m.removeLayer(layer); });
              window._hd_drawn_ids.clear();
            });
          }
        }
      ")
  })

  # Toggle the overlays on/off based on the checkbox group.
  # Initial state is rendered inside output$land_map; this only handles changes.
  observeEvent(
    list(input$land_overlays, input$land_overlay_opacity),
    ignoreInit = TRUE,
    {
    proxy <- leafletProxy("land_map") |>
      clearGroup("hillshade") |>
      clearGroup("wetlands") |>
      clearGroup("vernalpools") |>
      clearGroup("openspace") |>
      clearGroup("habitat") |>
      clearGroup("wellhead") |>
      clearGroup("acec") |>
      clearGroup("concepts")

    overlays <- input$land_overlays %||% character(0)
    op <- input$land_overlay_opacity %||% 0.65

    if ("hillshade" %in% overlays) {
      proxy <- proxy |> addTiles(
        urlTemplate = ESRI_HILLSHADE_URL,
        attribution = "Hillshade &copy; Esri",
        options     = tileOptions(opacity = op, maxZoom = 19),
        group       = "hillshade"
      )
    }

    if ("wetlands" %in% overlays && !is.null(dep_wetlands)) {
      proxy <- add_wetlands_layer(proxy, dep_wetlands, op)
    }
    if ("vernalpools" %in% overlays &&
        (!is.null(vernal_certified) || !is.null(vernal_potential))) {
      proxy <- add_vernalpools_layer(proxy, vernal_certified, vernal_potential, op)
    }
    if ("openspace" %in% overlays && !is.null(protected_open)) {
      proxy <- add_openspace_layer(proxy, protected_open, op)
    }
    if ("habitat" %in% overlays && !is.null(priority_habitat)) {
      proxy <- add_habitat_layer(proxy, priority_habitat, op)
    }
    if ("wellhead" %in% overlays && !is.null(wellhead_protect)) {
      proxy <- add_wellhead_layer(proxy, wellhead_protect, op)
    }
    if ("acec" %in% overlays && !is.null(acecs)) {
      proxy <- add_acec_layer(proxy, acecs, op)
    }
    if ("concepts" %in% overlays && !is.null(concept_sketches)) {
      proxy <- add_concepts_layer(proxy, concept_sketches, op)
    }

    if ("parcels" %in% overlays) {
      proxy |> showGroup("parcels_overlay")
    } else {
      proxy |> hideGroup("parcels_overlay")
    }
  })

  # ----- Elevation lookup (USGS National Map point query service) -----
  # Free, no key, returns elevation in requested units. Synchronous; ~0.5–1 s per click.
  get_elevation_ft <- function(lng, lat) {
    url <- sprintf(
      "https://epqs.nationalmap.gov/v1/json?x=%f&y=%f&units=Feet&wkid=4326&includeDate=False",
      lng, lat
    )
    body <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
    if (is.null(body)) return(NA_real_)
    val <- suppressWarnings(as.numeric(body$value))
    if (is.na(val) || val < -1000) return(NA_real_)
    val
  }

  # Map click (when not in a drawing/edit mode) -> drop a pin with elevation
  observeEvent(input$land_map_click, {
    click <- input$land_map_click
    if (is.null(click)) return()
    ft <- get_elevation_ft(click$lng, click$lat)
    label <- if (is.na(ft)) {
      "Elevation unavailable"
    } else {
      sprintf("Elevation: %s ft (%.1f m)",
              formatC(round(ft, 1), big.mark = ",", format = "f", digits = 1),
              ft * 0.3048)
    }
    popup_html <- sprintf(
      "<div style='font-family:\"Open Sans\",sans-serif; font-size:13px;'>%s<br/><span style='color:#777; font-size:11px;'>USGS 3DEP · %.5f, %.5f</span></div>",
      label, click$lat, click$lng
    )
    leafletProxy("land_map") |>
      clearGroup("elev_pin") |>
      addCircleMarkers(
        lng = click$lng, lat = click$lat,
        group = "elev_pin",
        radius = 6, color = "#FFFFFF", weight = 2,
        fillColor = "#FF3D00", fillOpacity = 1,
        label = label,
        popup = popup_html
      )
  })

  # ----- Drawing event handlers -----

  # New shape drawn -> append to drawn_sf
  observeEvent(input$land_map_draw_new_feature, {
    new_feat <- feature_to_sf(input$land_map_draw_new_feature)
    if (is.null(new_feat)) return()

    current <- drawn_sf()
    if (is.null(current) || nrow(current) == 0) {
      drawn_sf(new_feat)
    } else {
      # Align columns before rbind (sf is picky about column order)
      common_cols <- intersect(names(current), names(new_feat))
      if (length(common_cols) == 0) common_cols <- character(0)
      drawn_sf(rbind(
        current[, c(common_cols, "leaflet_id"), drop = FALSE] |> unique(),
        new_feat[, c(common_cols, "leaflet_id"), drop = FALSE] |> unique()
      ))
    }
  })

  # Features edited -> replace matching rows by leaflet_id
  observeEvent(input$land_map_draw_edited_features, {
    edited <- input$land_map_draw_edited_features
    if (is.null(edited) || length(edited$features) == 0) return()

    current <- drawn_sf()
    if (is.null(current)) return()

    # Convert edited FeatureCollection to sf, then upsert by leaflet_id
    for (f in edited$features) {
      new_row <- feature_to_sf(f)
      if (is.null(new_row)) next
      fid <- new_row$leaflet_id
      keep <- is.na(current$leaflet_id) | current$leaflet_id != fid
      current <- rbind(current[keep, ], new_row)
    }
    drawn_sf(current)
  })

  # Features deleted -> drop matching rows by leaflet_id
  observeEvent(input$land_map_draw_deleted_features, {
    deleted <- input$land_map_draw_deleted_features
    if (is.null(deleted) || length(deleted$features) == 0) return()

    current <- drawn_sf()
    if (is.null(current) || nrow(current) == 0) return()

    deleted_ids <- vapply(deleted$features, function(f) {
      as.integer(f[["_leaflet_id"]] %||% NA_integer_)
    }, integer(1))

    keep <- !current$leaflet_id %in% deleted_ids
    drawn_sf(current[keep, ])
  })

  # Clear-all button: wipe the reactive AND ask the client to remove all
  # Geoman-managed layers (clearGroup doesn't reach into Geoman's internals).
  observeEvent(input$clear_drawn, {
    drawn_sf(NULL)
    session$sendCustomMessage("hd_clear_geoman", list())
  })

  # Export drawn shapes as a single GeoJSON. Pour back into data/concepts/ to
  # have them reappear as a "Concept sketches" overlay on next app start.
  output$download_drawn_geojson <- downloadHandler(
    filename = function() {
      sprintf("drawn_shapes_%s.geojson", format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      g <- drawn_sf()
      if (is.null(g) || nrow(g) == 0) {
        writeLines('{"type":"FeatureCollection","features":[]}', file)
        return()
      }
      # Drop internal columns we don't want polluting the saved file
      g$leaflet_id <- NULL
      sf::st_write(g, file, driver = "GeoJSON",
                   delete_dsn = TRUE, quiet = TRUE)
    }
  )

  # Export a printable PDF map template with parcels and constraint context on
  # a clean white background. Uses ggplot2 + ggspatial for a real cartographic
  # render (north arrow, scale bar, graticule).
  output$download_pdf_template <- downloadHandler(
    filename = function() {
      sprintf("hampshire_drawing_template_%s.pdf", format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      have_gg  <- requireNamespace("ggplot2",   quietly = TRUE)
      have_ggs <- requireNamespace("ggspatial", quietly = TRUE)
      if (!have_gg || !have_ggs) {
        stop("This export needs ggplot2 and ggspatial: ",
             "install.packages(c('ggplot2','ggspatial')).")
      }

      # Reproject everything to MA State Plane (meters) for accurate distances
      # and a clean printed grid.
      to_mp <- function(x) if (is.null(x)) NULL else sf::st_transform(x, 26986)
      par_mp <- to_mp(parcels)
      wet_mp <- to_mp(dep_wetlands)
      osp_mp <- to_mp(protected_open_perm)
      hab_mp <- to_mp(priority_habitat)
      rds_mp <- to_mp(roads_centerline)
      bld_mp <- to_mp(buildings)

      # Clip the view to the parcels' bbox (with ~10% padding) so the wider
      # context layers don't blow the print extent out.
      bb_p <- sf::st_bbox(par_mp)
      span <- max(bb_p["xmax"] - bb_p["xmin"], bb_p["ymax"] - bb_p["ymin"])
      pad  <- 0.08 * span
      view_xlim <- c(bb_p["xmin"] - pad, bb_p["xmax"] + pad)
      view_ylim <- c(bb_p["ymin"] - pad, bb_p["ymax"] + pad)

      # Clip dense layers (roads, buildings) to the view bbox so we render only
      # what's visible. 12k buildings statewide-clipped would slow ggsave.
      view_box <- sf::st_as_sfc(sf::st_bbox(c(
        xmin = unname(view_xlim[1]), ymin = unname(view_ylim[1]),
        xmax = unname(view_xlim[2]), ymax = unname(view_ylim[2])),
        crs = 26986))
      clip_to_view <- function(x) {
        if (is.null(x)) return(NULL)
        idx <- lengths(sf::st_intersects(x, view_box)) > 0
        if (!any(idx)) return(NULL)
        x[idx, ]
      }
      rds_mp <- clip_to_view(rds_mp)
      bld_mp <- clip_to_view(bld_mp)

      p <- ggplot2::ggplot()

      if (!is.null(wet_mp)) {
        p <- p + ggplot2::geom_sf(
          data = wet_mp, fill = "#4FC3F7", alpha = 0.22,
          color = "#01579B", linewidth = 0.12)
      }
      if (!is.null(osp_mp)) {
        p <- p + ggplot2::geom_sf(
          data = osp_mp, fill = "#FFC107", alpha = 0.18,
          color = "#FF6F00", linewidth = 0.15)
      }
      if (!is.null(hab_mp)) {
        p <- p + ggplot2::geom_sf(
          data = hab_mp, fill = NA,
          color = "#6A1B9A", linewidth = 0.35, linetype = "dashed")
      }
      if (!is.null(rds_mp)) {
        # Class 1-3 = arterials/highways (thicker); 4 = collector; 5 = local
        rds_major <- rds_mp[!is.na(rds_mp$CLASS) & rds_mp$CLASS %in% 1:3, ]
        rds_minor <- rds_mp[!is.na(rds_mp$CLASS) & rds_mp$CLASS %in% 4:5, ]
        if (nrow(rds_minor) > 0) {
          p <- p + ggplot2::geom_sf(data = rds_minor,
                                    color = "#9E9E9E", linewidth = 0.25)
        }
        if (nrow(rds_major) > 0) {
          p <- p + ggplot2::geom_sf(data = rds_major,
                                    color = "#616161", linewidth = 0.5)
        }
      }
      if (!is.null(bld_mp)) {
        p <- p + ggplot2::geom_sf(
          data = bld_mp, fill = "#757575", color = "#424242",
          linewidth = 0.08, alpha = 0.85)
      }

      p <- p +
        ggplot2::geom_sf(data = par_mp, fill = NA,
                         color = "#009b9e", linewidth = 0.7) +
        ggplot2::geom_sf_text(
          data = par_mp,
          ggplot2::aes(label = MAP_PAR_ID),
          size = 2.2, color = "#1a1a1a", fontface = "bold") +
        ggspatial::annotation_scale(location = "bl", style = "bar",
                                    line_width = 0.5) +
        ggspatial::annotation_north_arrow(
          location = "tr",
          style = ggspatial::north_arrow_minimal(
            text_size = 8, line_width = 0.6)) +
        ggplot2::coord_sf(crs = 26986,
                          xlim = view_xlim, ylim = view_ylim,
                          expand = FALSE) +
        ggplot2::theme_bw(base_size = 9) +
        ggplot2::labs(
          title    = "Hampshire College: Land Context Drawing Template",
          subtitle = sprintf(
            "Generated %s. Hand-sketch on this template, then retrace in the app to bring concepts back as a layer.",
            format(Sys.Date(), "%B %d, %Y")),
          caption  = paste(
            "Projection: NAD83 / Massachusetts State Plane Mainland (m).",
            "Teal = parcel boundaries. Gray polygons = buildings.",
            "Gray lines = roads (darker = arterials).",
            "Blue = DEP wetlands. Amber = permanent protected open space.",
            "Dashed purple = NHESP Priority Habitat."),
          x = NULL, y = NULL
        ) +
        ggplot2::theme(
          panel.grid       = ggplot2::element_line(color = "#eeeeee",
                                                   linewidth = 0.25),
          axis.text        = ggplot2::element_text(size = 6,
                                                   color = "#666"),
          plot.title       = ggplot2::element_text(size = 13,
                                                   face = "bold"),
          plot.subtitle    = ggplot2::element_text(size = 9,
                                                   color = "#444",
                                                   margin = ggplot2::margin(b = 6)),
          plot.caption     = ggplot2::element_text(size = 7,
                                                   color = "#666",
                                                   hjust = 0),
          panel.background = ggplot2::element_rect(fill = "#FFFFFF",
                                                   color = NA),
          plot.background  = ggplot2::element_rect(fill = "#FFFFFF",
                                                   color = NA),
          plot.margin      = ggplot2::margin(10, 12, 8, 12)
        )

      ggplot2::ggsave(file, plot = p, width = 11, height = 8.5,
                      units = "in", device = "pdf", dpi = 200)
    }
  )

  # Satellite-imagery version of the template. Fetches Esri World Imagery tiles
  # for the view extent at print time. Slower than the clean version because of
  # the network fetch; resulting PDF is also larger.
  output$download_pdf_template_satellite <- downloadHandler(
    filename = function() {
      sprintf("hampshire_drawing_template_satellite_%s.pdf",
              format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      have_gg  <- requireNamespace("ggplot2",   quietly = TRUE)
      have_ggs <- requireNamespace("ggspatial", quietly = TRUE)
      have_mt  <- requireNamespace("maptiles",  quietly = TRUE)
      have_tt  <- requireNamespace("tidyterra", quietly = TRUE)
      if (!have_gg || !have_ggs || !have_mt || !have_tt) {
        stop("This export needs ggplot2, ggspatial, maptiles, and tidyterra: ",
             "install.packages(c('ggplot2','ggspatial','maptiles','tidyterra')).")
      }

      to_mp <- function(x) if (is.null(x)) NULL else sf::st_transform(x, 26986)
      par_mp <- to_mp(parcels)
      wet_mp <- to_mp(dep_wetlands)
      osp_mp <- to_mp(protected_open_perm)
      hab_mp <- to_mp(priority_habitat)
      rds_mp <- to_mp(roads_centerline)

      bb_p <- sf::st_bbox(par_mp)
      span <- max(bb_p["xmax"] - bb_p["xmin"], bb_p["ymax"] - bb_p["ymin"])
      pad  <- 0.08 * span
      view_xlim <- c(unname(bb_p["xmin"]) - pad, unname(bb_p["xmax"]) + pad)
      view_ylim <- c(unname(bb_p["ymin"]) - pad, unname(bb_p["ymax"]) + pad)
      view_box <- sf::st_as_sfc(sf::st_bbox(c(
        xmin = view_xlim[1], ymin = view_ylim[1],
        xmax = view_xlim[2], ymax = view_ylim[2]),
        crs = 26986))

      # Clip roads to view (skip buildings; satellite imagery already shows them)
      clip_to_view <- function(x) {
        if (is.null(x)) return(NULL)
        idx <- lengths(sf::st_intersects(x, view_box)) > 0
        if (!any(idx)) return(NULL)
        x[idx, ]
      }
      rds_mp <- clip_to_view(rds_mp)

      # Fetch satellite tiles. zoom 15 gives ~1 m/px around this latitude;
      # that's enough resolution to print clearly at 11x8.5 in.
      tiles <- maptiles::get_tiles(
        view_box,
        provider = "Esri.WorldImagery",
        zoom     = 15,
        crop     = TRUE,
        cachedir = tempdir()
      )

      p <- ggplot2::ggplot() +
        tidyterra::geom_spatraster_rgb(data = tiles, maxcell = 5e6)

      # Constraint outlines: bright, no/low fill so imagery shows through
      if (!is.null(wet_mp)) {
        p <- p + ggplot2::geom_sf(
          data = wet_mp, fill = NA,
          color = "#4FC3F7", linewidth = 0.35)
      }
      if (!is.null(osp_mp)) {
        p <- p + ggplot2::geom_sf(
          data = osp_mp, fill = "#FFC107", alpha = 0.18,
          color = "#FFD54F", linewidth = 0.3)
      }
      if (!is.null(hab_mp)) {
        p <- p + ggplot2::geom_sf(
          data = hab_mp, fill = NA,
          color = "#F48FB1", linewidth = 0.45, linetype = "dashed")
      }
      if (!is.null(rds_mp)) {
        rds_major <- rds_mp[!is.na(rds_mp$CLASS) & rds_mp$CLASS %in% 1:3, ]
        rds_minor <- rds_mp[!is.na(rds_mp$CLASS) & rds_mp$CLASS %in% 4:5, ]
        if (nrow(rds_minor) > 0) {
          p <- p + ggplot2::geom_sf(data = rds_minor,
                                    color = "#FFEB3B", linewidth = 0.25,
                                    alpha = 0.75)
        }
        if (nrow(rds_major) > 0) {
          p <- p + ggplot2::geom_sf(data = rds_major,
                                    color = "#FFC107", linewidth = 0.5,
                                    alpha = 0.85)
        }
      }

      p <- p +
        ggplot2::geom_sf(data = par_mp, fill = NA,
                         color = "#1DE9B6", linewidth = 1.1) +
        # White-fill labels so parcel IDs read against any background
        ggplot2::geom_sf_label(
          data = par_mp,
          ggplot2::aes(label = MAP_PAR_ID),
          fill = "white", color = "black",
          size = 2.1, fontface = "bold", alpha = 0.85,
          label.padding = ggplot2::unit(0.10, "lines"),
          label.size = 0.1) +
        ggspatial::annotation_scale(
          location = "bl", style = "bar",
          bar_cols = c("white", "black"),
          text_col = "white", line_col = "white") +
        ggspatial::annotation_north_arrow(
          location = "tr",
          style = ggspatial::north_arrow_minimal(
            line_col = "white", fill = "white", text_col = "white",
            text_size = 8)) +
        ggplot2::coord_sf(crs = 26986,
                          xlim = view_xlim, ylim = view_ylim,
                          expand = FALSE) +
        ggplot2::labs(
          title    = "Hampshire College: Land Context Drawing Template (Satellite)",
          subtitle = sprintf(
            "Generated %s. Hand-sketch on this template, then retrace in the app to bring concepts back as a layer.",
            format(Sys.Date(), "%B %d, %Y")),
          caption  = paste(
            "Imagery: Esri World Imagery.",
            "Projection: NAD83 / Massachusetts State Plane Mainland (m).",
            "Teal = parcel boundaries. Yellow lines = roads.",
            "Cyan = DEP wetlands outline. Amber = permanent protected open space.",
            "Dashed pink = NHESP Priority Habitat."),
          x = NULL, y = NULL
        ) +
        ggplot2::theme_bw(base_size = 9) +
        ggplot2::theme(
          panel.grid       = ggplot2::element_blank(),
          axis.text        = ggplot2::element_text(size = 6, color = "#666"),
          plot.title       = ggplot2::element_text(size = 13, face = "bold"),
          plot.subtitle    = ggplot2::element_text(size = 9, color = "#444",
                              margin = ggplot2::margin(b = 6)),
          plot.caption     = ggplot2::element_text(size = 7, color = "#666",
                              hjust = 0),
          plot.margin      = ggplot2::margin(10, 12, 8, 12)
        )

      ggplot2::ggsave(file, plot = p, width = 11, height = 8.5,
                      units = "in", device = "pdf", dpi = 200)
    }
  )

  # Live summary of drawn shapes (count, area, length)
  output$drawn_summary <- renderUI({
    shapes <- drawn_sf()

    if (is.null(shapes) || nrow(shapes) == 0) {
      return(tags$div(
        class = "helper-text",
        style = "font-size: 13px;",
        "Draw shapes using the toolbar at the top-left of the map. ",
        "Live area and length will appear here."
      ))
    }

    geom_types <- as.character(sf::st_geometry_type(shapes))
    is_poly <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
    is_line <- geom_types %in% c("LINESTRING", "MULTILINESTRING")

    polys <- shapes[is_poly, ]
    lines <- shapes[is_line, ]

    polys_m <- if (nrow(polys) > 0) sf::st_transform(polys, 26986) else NULL
    lines_m <- if (nrow(lines) > 0) sf::st_transform(lines, 26986) else NULL

    poly_sqm   <- if (!is.null(polys_m)) sum(as.numeric(sf::st_area(polys_m))) else 0
    poly_acres <- poly_sqm / 4046.8564224
    poly_sqft  <- poly_sqm * 10.7639104
    # Perimeter = length of polygon boundary (in meters), summed across all rings
    poly_perim_ft <- if (!is.null(polys_m)) {
      sum(as.numeric(sf::st_length(sf::st_cast(polys_m, "MULTILINESTRING")))) * 3.28084
    } else 0

    line_feet <- if (!is.null(lines_m)) {
      sum(as.numeric(sf::st_length(lines_m))) * 3.28084
    } else 0

    metric_pair <- function(label, value) {
      tags$div(
        class = "metric-row",
        tags$div(class = "metric-label", style = "font-size: 11px;", label),
        tags$div(class = "enc-fig", style = "font-size: 13px; font-weight: 600;", value)
      )
    }

    tagList(
      if (nrow(polys) > 0) {
        tags$div(
          style = "margin-bottom: 10px;",
          tags$div(class = "metric-label",
                   paste0(nrow(polys), " ", if (nrow(polys) == 1) "polygon" else "polygons")),
          tags$div(class = "metric-value",
                   paste0(formatC(round(poly_acres, 2), big.mark = ",",
                                  format = "f", digits = 2), " acres")),
          tags$div(style = "margin-top: 6px;",
            metric_pair("Square feet",
                        formatC(round(poly_sqft), big.mark = ",", format = "d")),
            metric_pair("Perimeter",
                        paste0(formatC(round(poly_perim_ft), big.mark = ",",
                                       format = "d"), " ft"))
          )
        )
      },
      if (nrow(lines) > 0) {
        tags$div(
          style = "margin-top: 10px; padding-top: 10px; border-top: 1px solid #eee;",
          tags$div(class = "metric-label",
                   paste0(nrow(lines), " ", if (nrow(lines) == 1) "line" else "lines")),
          tags$div(class = "metric-value",
                   paste0(formatC(round(line_feet), big.mark = ","), " ft"))
        )
      }
    )
  })

  # ===== End Land context tab =====

  # ===== Developable land tab =====

  # Active buffer scenario: which net-easy column and polygon set to use.
  dev_is_strict   <- reactive(isTRUE(input$dev_buffer_scenario == "strict"))
  dev_acres_col   <- reactive(if (dev_is_strict()) "net_easy_strict_acres" else "net_easy_acres")
  dev_active_polys <- reactive(if (dev_is_strict()) parcels_dev_poly_strict else parcels_dev_poly)
  dev_scenario_lbl <- reactive(if (dev_is_strict()) "strict 200-ft buffer" else "standard 100-ft buffer")

  output$dev_stat_tiles <- renderUI({
    if (is.null(parcels_dev)) {
      return(tags$div(class = "helper-text",
                      "Developable analysis not found. Run ",
                      tags$code("Rscript data-prep/compute_developable.R"),
                      " to generate it."))
    }
    df <- sf::st_drop_geometry(parcels_dev)
    gross   <- sum(df$gross_acres, na.rm = TRUE)
    net_eas <- sum(df[[dev_acres_col()]], na.rm = TRUE)
    frnt    <- sum(df$frontage_ft, na.rm = TRUE)
    tagList(
      stat_tile("Parcels analyzed", formatC(nrow(df), big.mark = ",")),
      stat_tile("Gross acres",
                formatC(round(gross, 1), big.mark = ",", format = "f", digits = 1),
                "Hampshire College holdings"),
      stat_tile("Net easy-to-build",
                paste0(formatC(round(net_eas, 1), big.mark = ",",
                               format = "f", digits = 1), " ac"),
                paste0(round(100 * net_eas / max(gross, 1), 1), "% of gross · ",
                       dev_scenario_lbl())),
      stat_tile("Estimated frontage",
                paste0(formatC(round(frnt), big.mark = ",", format = "d"), " ft"),
                paste0("~", round(frnt / 5280, 2), " miles of road frontage"))
    )
  })

  # Pre-compute the permanent open space subset once (used as an overlay
  # AND mirrors the input to the developability calc).
  protected_open_perm <- if (!is.null(protected_open)) {
    protected_open[!is.na(protected_open$LEV_PROT) &
                   protected_open$LEV_PROT == "P", ]
  } else NULL

  # Build the parcel popup HTML once per parcel — it doesn't depend on inputs.
  dev_popup_html <- if (!is.null(parcels_dev)) {
    df <- sf::st_drop_geometry(parcels_dev)
    paste0(
      "<div style='font-family:\"Open Sans\",sans-serif; font-size:13px; min-width: 260px;'>",
      "<strong>", df$MAP_PAR_ID, "</strong> ",
      "<span style='color:#777;'>(", df$TOWN, ")</span><br/>",
      ifelse(nchar(df$SITE_ADDR) > 0, paste0(df$SITE_ADDR, "<br/>"), ""),
      "<table style='margin-top:8px; font-size:12px; border-collapse:collapse;'>",
      "<tr><td>Gross</td><td style='text-align:right; padding-left:14px;'>",
      df$gross_acres, " ac</td></tr>",
      "<tr><td>Net unconstrained</td><td style='text-align:right; padding-left:14px;'>",
      df$net_unconstrained_acres, " ac</td></tr>",
      "<tr><td><strong>Net easy (100 ft)</strong></td><td style='text-align:right; padding-left:14px;'><strong>",
      df$net_easy_acres, " ac</strong></td></tr>",
      "<tr><td>Net easy (200 ft)</td><td style='text-align:right; padding-left:14px;'>",
      df$net_easy_strict_acres, " ac</td></tr>",
      "<tr><td>% developable</td><td style='text-align:right; padding-left:14px;'>",
      df$pct_developable, "%</td></tr>",
      "<tr><td>Frontage</td><td style='text-align:right; padding-left:14px;'>",
      formatC(df$frontage_ft, big.mark = ",", format = "d"), " ft</td></tr>",
      "</table>",
      ifelse(!is.na(df$frontage_roads) & nchar(df$frontage_roads) > 0,
             paste0("<div style='color:#666; font-size:11px; margin-top:6px;'>On: ",
                    df$frontage_roads, "</div>"),
             ""),
      "</div>"
    )
  } else character(0)

  # Single function that paints the whole map state. Called once from
  # renderLeaflet for the initial frame, and again from an observer for every
  # control change. Works on both leaflet() and leafletProxy() objects.
  # HTML for the legend, reflecting which overlays are visible. Selected-parcel
  # row is included only when there's an active selection.
  build_dev_legend_html <- function(overlays, has_selection) {
    row <- function(swatch_css, label) {
      sprintf(
        '<div class="hd-leg-row"><span class="hd-leg-swatch" style="%s"></span><span class="hd-leg-label">%s</span></div>',
        swatch_css, label
      )
    }
    rows <- c(row("background:#009b9e; border:none; height:3px; margin-top:7px;",
                  "Parcel boundary"))
    if ("devarea"   %in% overlays) rows <- c(rows, row(
      "background:rgba(156,204,101,0.85); border-color:#558B2F;",
      "Developable area"))
    if ("wetlands"  %in% overlays) rows <- c(rows, row(
      "background:rgba(79,195,247,0.7); border-color:#01579B;",
      "DEP wetlands"))
    if ("vernalpools" %in% overlays) rows <- c(rows, row(
      "background:#1DE9B6; border-color:#004D40; border-radius:50%;",
      "Vernal pool (certified)"))
    if ("openspace" %in% overlays) rows <- c(rows, row(
      "background:rgba(255,193,7,0.55); border-color:#FF6F00;",
      "Permanent open space"))
    if ("habitat"   %in% overlays) rows <- c(rows, row(
      "background:rgba(186,104,200,0.55); border:1.5px dashed #6A1B9A;",
      "Priority habitat"))
    if ("hillshade" %in% overlays) rows <- c(rows, row(
      "background:linear-gradient(135deg,#ffffff 0%,#777 100%); border-color:#555;",
      "Hillshade"))
    if ("concepts"  %in% overlays) rows <- c(rows, row(
      "background:rgba(233,30,99,0.3); border:1.5px dashed #880E4F;",
      "Concept sketch"))
    if (has_selection) rows <- c(rows, row(
      "background:transparent; border-color:#FFEB3B; border-width:3px;",
      "Selected parcel"))

    paste0(
      '<div class="hd-dev-legend">',
      '<div class="hd-leg-title">Legend</div>',
      paste(rows, collapse = ""),
      '</div>'
    )
  }

  apply_dev_layers <- function(map, overlays, overlay_opacity = 1.0,
                               has_selection = FALSE, dev_polys = parcels_dev_poly,
                               is_proxy = FALSE) {
    if (is.null(parcels_dev)) return(map)

    label_vec <- paste0(parcels_dev$MAP_PAR_ID, " — ",
                        parcels_dev$pct_developable, "% developable")

    if (is_proxy) {
      map <- map |>
        clearGroup("parcels_dev") |>
        clearGroup("dev_hillshade") |>
        clearGroup("dev_wetlands") |>
        clearGroup("dev_vernalpools") |>
        clearGroup("dev_openspace") |>
        clearGroup("dev_habitat") |>
        clearGroup("dev_devarea") |>
        clearGroup("dev_concepts") |>
        removeControl("dev_legend")
    }

    # Multiplier applied to each overlay's base fill opacity
    om <- overlay_opacity

    # Hillshade tile (under constraint polys)
    if ("hillshade" %in% overlays) {
      map <- map |> addTiles(
        urlTemplate = ESRI_HILLSHADE_URL,
        attribution = "Hillshade &copy; Esri",
        options     = tileOptions(opacity = 0.5 * om, maxZoom = 19),
        group       = "dev_hillshade"
      )
    }
    if ("wetlands" %in% overlays && !is.null(dep_wetlands)) {
      map <- map |> addPolygons(
        data = dep_wetlands, group = "dev_wetlands",
        fillColor = "#4FC3F7", fillOpacity = 0.40 * om,
        color = "#01579B", opacity = 0.9 * om, weight = 0.6,
        options = pathOptions(interactive = FALSE)
      )
    }
    if ("vernalpools" %in% overlays &&
        (!is.null(vernal_certified) || !is.null(vernal_potential))) {
      if (!is.null(vernal_potential) && nrow(vernal_potential) > 0) {
        map <- map |> addCircleMarkers(
          data = vernal_potential, group = "dev_vernalpools",
          radius = 4, stroke = TRUE, weight = 1.5,
          color = "#00897B", fillColor = "#FFFFFF",
          opacity = pmin(1, om), fillOpacity = pmin(1, 0.5 * om),
          label = "Potential vernal pool",
          options = pathOptions(interactive = FALSE)
        )
      }
      if (!is.null(vernal_certified) && nrow(vernal_certified) > 0) {
        map <- map |> addCircleMarkers(
          data = vernal_certified, group = "dev_vernalpools",
          radius = 5, stroke = TRUE, weight = 1.5,
          color = "#004D40", fillColor = "#1DE9B6",
          opacity = pmin(1, om), fillOpacity = pmin(1, 0.9 * om),
          label = ~paste0("Certified vernal pool #", CVP_NUM),
          options = pathOptions(interactive = FALSE)
        )
      }
    }
    if ("openspace" %in% overlays && !is.null(protected_open_perm)) {
      map <- map |> addPolygons(
        data = protected_open_perm, group = "dev_openspace",
        fillColor = "#FFC107", fillOpacity = 0.35 * om,
        color = "#FF6F00", opacity = 0.9 * om, weight = 1,
        options = pathOptions(interactive = FALSE)
      )
    }
    if ("habitat" %in% overlays && !is.null(priority_habitat)) {
      map <- map |> addPolygons(
        data = priority_habitat, group = "dev_habitat",
        fillColor = "#BA68C8", fillOpacity = 0.30 * om,
        color = "#6A1B9A", opacity = 0.9 * om, weight = 1.3,
        dashArray = "6,3",
        options = pathOptions(interactive = FALSE)
      )
    }
    if ("devarea" %in% overlays && !is.null(dev_polys)) {
      map <- map |> addPolygons(
        data = dev_polys, group = "dev_devarea",
        fillColor = "#9CCC65", fillOpacity = 0.60 * om,
        color = "#558B2F", opacity = 0.95 * om, weight = 1,
        label = ~paste0(MAP_PAR_ID, " — ", round(net_easy_acres, 1),
                        " ac easy-to-build"),
        options = pathOptions(interactive = FALSE)
      )
    }
    if ("concepts" %in% overlays && !is.null(concept_sketches)) {
      geom_types <- as.character(sf::st_geometry_type(concept_sketches))
      is_poly <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
      is_line <- geom_types %in% c("LINESTRING", "MULTILINESTRING")
      if (any(is_poly)) {
        map <- map |> addPolygons(
          data = concept_sketches[is_poly, ], group = "dev_concepts",
          fillColor = "#E91E63", fillOpacity = 0.30 * om,
          color = "#880E4F", opacity = pmin(1, om),
          weight = 2, dashArray = "8,3",
          label = ~paste0("Concept: ", concept_source),
          options = pathOptions(interactive = FALSE)
        )
      }
      if (any(is_line)) {
        map <- map |> addPolylines(
          data = concept_sketches[is_line, ], group = "dev_concepts",
          color = "#880E4F", opacity = pmin(1, om),
          weight = 3, dashArray = "8,3",
          label = ~paste0("Concept: ", concept_source),
          options = pathOptions(interactive = FALSE)
        )
      }
    }

    # Parcel polygons — outlines only.
    map <- map |> addPolygons(
      data        = parcels_dev,
      group       = "parcels_dev",
      layerId     = ~as.character(MAP_PAR_ID),
      fill        = FALSE,
      color       = "#009b9e",
      weight      = 2.8,
      opacity     = 1,
      label       = label_vec,
      popup       = dev_popup_html,
      highlightOptions = highlightOptions(
        weight = 5, color = "#ffffff", opacity = 1,
        bringToFront = TRUE)
    )

    map <- map |> addControl(
      html     = HTML(build_dev_legend_html(overlays, has_selection)),
      position = "bottomright",
      layerId  = "dev_legend"
    )

    map
  }

  output$dev_map <- renderLeaflet({
    overlays        <- isolate(input$dev_overlays %||% c("devarea", "wetlands"))
    overlay_opacity <- isolate(input$dev_overlay_opacity %||% 1.0)
    init_polys      <- isolate(dev_active_polys())

    m <- leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
      addProviderTiles("CartoDB.Positron", group = "Light") |>
      addLayersControl(
        baseGroups = c("Satellite", "Light"),
        options = layersControlOptions(collapsed = FALSE)
      )
    m <- apply_dev_layers(m, overlays, overlay_opacity = overlay_opacity,
                          has_selection = FALSE, dev_polys = init_polys,
                          is_proxy = FALSE)
    if (!is.null(parcels_dev)) {
      bb <- sf::st_bbox(parcels_dev)
      m <- m |> fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    } else {
      m <- m |> setView(lng = -72.55, lat = 42.34, zoom = 14)
    }
    m
  })

  # Update on control changes only (initial paint happens inside renderLeaflet)
  observeEvent(
    list(input$dev_overlays, input$dev_overlay_opacity,
         input$dev_buffer_scenario, dev_selected_parcels()),
    ignoreInit = TRUE,
    {
      apply_dev_layers(
        leafletProxy("dev_map"),
        input$dev_overlays %||% character(0),
        overlay_opacity = input$dev_overlay_opacity %||% 1.0,
        has_selection   = length(dev_selected_parcels()) > 0,
        dev_polys       = dev_active_polys(),
        is_proxy = TRUE
      )
    }
  )

  # ----- Parcel selection on the dev map -----
  dev_selected_parcels <- reactiveVal(character(0))

  observeEvent(input$dev_map_shape_click, {
    click <- input$dev_map_shape_click
    if (is.null(click$id)) return()
    id <- as.character(click$id)
    current <- dev_selected_parcels()
    if (id %in% current) {
      dev_selected_parcels(setdiff(current, id))
    } else {
      dev_selected_parcels(union(current, id))
    }
  })

  observeEvent(input$clear_dev_selection, {
    dev_selected_parcels(character(0))
  })

  # Draw a yellow highlight outline on top of any selected parcels. This group
  # ("dev_selection") is NOT cleared by apply_dev_layers, so it persists across
  # control changes (color, opacity, overlays).
  observe({
    sel <- dev_selected_parcels()
    proxy <- leafletProxy("dev_map") |> clearGroup("dev_selection")
    if (length(sel) == 0 || is.null(parcels_dev)) return()
    sel_polys <- parcels_dev[parcels_dev$MAP_PAR_ID %in% sel, ]
    if (nrow(sel_polys) == 0) return()
    proxy |> addPolygons(
      data    = sel_polys,
      group   = "dev_selection",
      fill    = FALSE,
      color   = "#FFEB3B",
      weight  = 5,
      opacity = 1,
      options = pathOptions(interactive = FALSE)
    )
  })

  output$dev_selection_summary <- renderUI({
    sel <- dev_selected_parcels()
    if (length(sel) == 0 || is.null(parcels_dev)) {
      return(tags$div(
        class = "helper-text",
        "Click parcels on the map to add them to a selection. ",
        "Click again to remove. Selected parcels get a yellow highlight, and ",
        "their combined developable area appears here."
      ))
    }
    df <- sf::st_drop_geometry(parcels_dev) |>
      dplyr::filter(MAP_PAR_ID %in% sel)
    gross   <- sum(df$gross_acres,             na.rm = TRUE)
    net_unc <- sum(df$net_unconstrained_acres, na.rm = TRUE)
    net_eas <- sum(df[[dev_acres_col()]],      na.rm = TRUE)
    frnt    <- sum(df$frontage_ft,             na.rm = TRUE)
    fmt_ac  <- function(x) formatC(round(x, 1), big.mark = ",", format = "f", digits = 1)
    fmt_int <- function(x) formatC(round(x), big.mark = ",", format = "d")
    tagList(
      tags$div(
        class = "stats-row",
        style = "grid-template-columns: repeat(4, 1fr); margin: 0;",
        stat_tile("Selected parcels", formatC(nrow(df), big.mark = ",")),
        stat_tile("Gross acres", fmt_ac(gross), "selected total"),
        stat_tile("Net easy-to-build",
                  paste0(fmt_ac(net_eas), " ac"),
                  paste0(round(100 * net_eas / max(gross, 1), 1),
                         "% of selected gross")),
        stat_tile("Estimated frontage",
                  paste0(fmt_int(frnt), " ft"),
                  paste0("~", round(frnt / 5280, 2), " mi"))
      ),
      tags$div(
        class = "helper-text",
        style = "margin-top: 12px; font-size: 12px;",
        "Net unconstrained: ", tags$strong(fmt_ac(net_unc), "ac"),
        " (gross minus wetlands and permanent open space). Net easy-to-build ",
        "additionally subtracts the wetland buffer (", dev_scenario_lbl(),
        ") and any NHESP Priority Habitat overlap. See the Layer guide for ",
        "full methodology."
      )
    )
  })

  output$dev_table <- renderDT({
    if (is.null(parcels_dev)) return(NULL)
    df <- sf::st_drop_geometry(parcels_dev) |>
      dplyr::select(TOWN, MAP_PAR_ID, SITE_ADDR,
                    gross_acres, net_unconstrained_acres,
                    net_easy_acres, net_easy_strict_acres,
                    pct_developable, frontage_ft, frontage_roads) |>
      dplyr::arrange(dplyr::desc(net_easy_acres))
    DT::datatable(
      df,
      colnames = c("Town", "Parcel", "Address",
                   "Gross ac", "Net unconstr. ac",
                   "Net easy (100ft)", "Net easy (200ft)",
                   "% dev.", "Frontage (ft)", "Roads"),
      rownames = FALSE,
      selection = "none",
      options = list(pageLength = 25, dom = "tip", scrollX = TRUE,
                     columnDefs = list(list(className = "dt-right",
                                            targets = c(3, 4, 5, 6, 7, 8))))
    ) |>
      DT::formatRound(c("gross_acres", "net_unconstrained_acres",
                        "net_easy_acres", "net_easy_strict_acres"),
                      digits = 2) |>
      DT::formatRound("pct_developable", digits = 1) |>
      DT::formatCurrency("frontage_ft", currency = "", digits = 0)
  }, server = FALSE)

  # ===== End Developable land tab =====

  # ---- Download handlers ----
  safe_cols <- function(d) {
    keep <- intersect(export_cols, names(d))
    d[, keep, drop = FALSE]
  }
  
  today_tag <- function() format(Sys.Date(), "%Y%m%d")
  
  output$dl_csv_filtered <- downloadHandler(
    filename = function() paste0("hampshire_parcels_filtered_", today_tag(), ".csv"),
    content  = function(file) {
      d <- sf::st_drop_geometry(filtered())
      write.csv(safe_cols(d), file, row.names = FALSE, na = "")
    }
  )
  
  output$dl_csv_selected <- downloadHandler(
    filename = function() paste0("hampshire_parcels_selected_", today_tag(), ".csv"),
    content  = function(file) {
      d <- selected()
      if (is.null(d)) d <- filtered()[0, ]
      write.csv(safe_cols(sf::st_drop_geometry(d)), file, row.names = FALSE, na = "")
    }
  )
  
  output$dl_geojson_filtered <- downloadHandler(
    filename = function() paste0("hampshire_parcels_filtered_", today_tag(), ".geojson"),
    content  = function(file) {
      d <- filtered()
      d$row_id <- NULL; d$popup_html <- NULL
      d$enc_badge_bg <- NULL; d$enc_badge_fg <- NULL
      sf::st_write(d, file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
    }
  )
  
  output$dl_geojson_selected <- downloadHandler(
    filename = function() paste0("hampshire_parcels_selected_", today_tag(), ".geojson"),
    content  = function(file) {
      d <- selected()
      if (is.null(d)) d <- filtered()[0, ]
      d$row_id <- NULL; d$popup_html <- NULL
      d$enc_badge_bg <- NULL; d$enc_badge_fg <- NULL
      sf::st_write(d, file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
    }
  )
}

shinyApp(ui, server)