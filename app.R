# Hampshire College Property Explorer
# Shiny app: explore parcels on a map, filter/select in a table,
# and see running totals for acreage and assessed value.

library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(DT)
library(dplyr)
library(htmltools)
library(jsonlite)

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
              tags$td("~16.1 acres of 25A-23 (Amherst) + ~1.66 ac of 8_4_1 (Hadley). Named facilities: Multi-Sports, Day Care, Arts Complex.")
            ),
            tags$tr(
              tags$td(tags$strong("2016")),
              tags$td("BankUnited, N.A."),
              tags$td("Mass. Dev. Finance Agency Revenue Bonds, Hampshire College Issue, Series 2016"),
              tags$td(style = "text-align: right; font-variant-numeric: tabular-nums;", "$12,564,929"),
              tags$td(style = "text-align: right; font-variant-numeric: tabular-nums;", "2.8%"),
              tags$td("2026"),
              tags$td("Short-term"),
              tags$td("17.6-acre carve-out of 22D-15, per Plan Book 235 Plan 100. Contains Library, Kern, Cole, Crown, Enfield.")
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