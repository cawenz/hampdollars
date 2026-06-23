# Hampshire Next — land-use & housing capacity explorer
# Click any area (or DRAW one) to assign it a land use — Development (housing,
# with a density), Conservation, or Core Campus — or remove it. Homes, the
# Amherst/Hadley split, and tax revenue update live.  Run: shiny::runApp("capacity")
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(leaflet); library(leaflet.extras)
  library(sf); library(htmltools)
})

DATA <- normalizePath(file.path(dirname(getwd()), "data"), mustWork = FALSE)
if (!dir.exists(DATA)) DATA <- "../data"
KML  <- file.path(DATA, "concepts/hampshire_next.kml")
CRS_MA <- 26986; SQM_ACRE <- 4046.8564224

DENS <- c("Row homes (6.4/ac)" = 6.4, "Townhouses (12/ac)" = 12,
          "Low-rise multifamily (20/ac)" = 20, "Exclude (0)" = 0)
abbr  <- function(d) c("6.4"="row homes","12"="townhouses","20"="multifamily","0"="excluded")[[as.character(d)]]
dname <- function(d) names(DENS)[match(d, DENS)]
AMH_COL <- "#C0392B"; HAD_COL <- "#1F6F8B"; CONS_COL <- "#2E7D32"; CORE_COL <- "#607D8B"
CATS <- c("Development", "Conservation", "Core Campus")
TAX <- c(Amherst = 0.01795, Hadley = 0.01163)
# Average assessed value per home, by housing type (editable in the app)
HOME_VALS <- c("row homes" = 395000, "townhouses" = 325000, "multifamily" = 250000)
dollar0 <- function(x) paste0("$", format(round(x), big.mark = ","))

towns_m <- st_transform(st_make_valid(st_read(file.path(DATA, "amherst_hadley.geojson"), quiet = TRUE)), CRS_MA)
ac_in <- function(g_m, tn) { x <- suppressWarnings(st_intersection(g_m, towns_m[towns_m$town == tn, ]))
  if (!nrow(x)) 0 else as.numeric(sum(st_area(x))) / SQM_ACRE }
area_color <- function(rec) switch(rec$category,
  "Development" = if (rec$town == "Hadley") HAD_COL else AMH_COL,
  "Conservation" = CONS_COL, "Core Campus" = CORE_COL)

# Fill interior rings of a (multi)polygon sfc — keep only exterior rings.
remove_holes <- function(g_sfc) {
  polys <- suppressWarnings(st_cast(st_cast(g_sfc, "MULTIPOLYGON"), "POLYGON"))
  ext <- lapply(seq_along(polys), function(i) st_polygon(list(polys[[i]][[1]])))
  st_union(st_sfc(ext, crs = st_crs(g_sfc)))
}

read_theme <- function(lyr) {
  g <- tryCatch(st_read(KML, layer = lyr, quiet = TRUE), error = function(e) NULL)
  if (is.null(g) || !nrow(g)) return(NULL)
  g <- st_make_valid(st_zm(st_transform(g, 4326)))
  nm <- if ("Name" %in% names(g)) g$Name else rep("(area)", nrow(g))
  g$.nm <- trimws(ifelse(is.na(nm), "(area)", nm))
  g[st_geometry_type(g) %in% c("POLYGON", "MULTIPOLYGON"), ]
}
# build a record (geometry + town split + label point) for one polygon
make_rec <- function(id, name, geom, category) {
  pm <- st_transform(st_sfc(geom, crs = 4326), CRS_MA)
  sg <- st_sf(geometry = pm)
  acA <- ac_in(sg, "Amherst"); acH <- ac_in(sg, "Hadley")
  ct <- st_coordinates(st_transform(suppressWarnings(st_point_on_surface(pm)), 4326))
  list(id = id, name = name, category = category, geom = geom,
       ac_amh = acA, ac_had = acH, lng = ct[1, 1], lat = ct[1, 2],
       town = if (acH > acA) "Hadley" else "Amherst")
}
CLEAN <- file.path(DATA, "concepts/clean")   # tightened shapes (data-prep/tighten_shapes.R)
load_cat <- function(layer, category, prefix, cleanfile) {
  cp <- file.path(CLEAN, cleanfile)
  if (file.exists(cp)) { g <- st_make_valid(st_transform(st_read(cp, quiet = TRUE), 4326))
    nmv <- if ("Name" %in% names(g)) g$Name else paste0(category, seq_len(nrow(g)))
  } else { g <- read_theme(layer); if (is.null(g)) return(list()); nmv <- g$.nm }
  g <- g[st_geometry_type(g) %in% c("POLYGON", "MULTIPOLYGON"), ]
  nmv <- trimws(ifelse(is.na(nmv), "(area)", nmv))
  nm <- ave(nmv, nmv, FUN = function(x) if (length(x) > 1) paste0(x, " #", seq_along(x)) else x)
  lapply(seq_len(nrow(g)), function(i) make_rec(paste0(prefix, i), nm[i], st_geometry(g)[[i]], category))
}
init_areas <- c(
  load_cat("Development", "Development", "d", "development.geojson"),
  load_cat("Conservation - trails + habitat", "Conservation", "c", "conservation.geojson"),
  load_cat("Hampshire Next - Retained facilities", "Core Campus", "k", "core_campus.geojson"))

# ---- minor context layers (toggles): parcels, wetlands, recreation/envelope --
CTX <- data.frame(stringsAsFactors = FALSE,
  layer = c("Town of Amherst Recreation", "Recreation/Public Facilities", "Hampshire Next"),
  label = c("Town of Amherst recreation", "Recreation / public facilities", "Vision envelope & misc."),
  col   = c("#8E44AD", "#B7791F", "#9C7A4A"), op = c(0.35, 0.35, 0.04))
CTX_G <- lapply(CTX$layer, read_theme)
all_m <- st_transform(st_sfc(lapply(init_areas, `[[`, "geom"), crs = 4326), CRS_MA)
FT_TO_M <- 0.3048
envelope <- st_buffer(st_union(st_geometry(all_m)), 300)
wet <- st_read(file.path(DATA, "dep_wetlands.geojson"), quiet = TRUE) |> st_transform(CRS_MA) |> st_make_valid()
wet <- wet[lengths(st_intersects(wet, envelope)) > 0, ]
wet_u   <- st_union(st_geometry(wet))
ring100 <- st_make_valid(st_difference(st_buffer(wet_u, 100 * FT_TO_M, nQuadSegs = 30), wet_u))
ring200 <- st_make_valid(st_difference(st_buffer(wet_u, 200 * FT_TO_M, nQuadSegs = 30),
                                       st_buffer(wet_u, 100 * FT_TO_M, nQuadSegs = 30)))
clipw   <- function(g) st_sf(geometry = st_transform(st_intersection(g, envelope), 4326))
wet_w   <- st_transform(st_intersection(wet, envelope), 4326)
r100_w  <- clipw(ring100); r200_w <- clipw(ring200)
parcels <- st_read(file.path(DATA, "hampshire_college_parcels_combined.geojson"), quiet = TRUE) |>
  st_transform(4326) |> st_make_valid()
WET_GROUPS <- c("Wetlands (DEP)", "Wetland buffer · 100 ft (state WPA)", "Wetland buffer · +200 ft (Amherst rule)")
bb <- st_bbox(st_transform(st_sfc(lapply(init_areas, `[[`, "geom"), crs = 4326), 4326))

add_area <- function(map, rec) map |> addPolygons(
  data = st_sf(geometry = st_sfc(rec$geom, crs = 4326)), layerId = rec$id, group = "areas",
  fillColor = area_color(rec), color = "#222", weight = 1, fillOpacity = 0.5,
  options = pathOptions(pane = "devPane"),
  label = HTML(sprintf("<b>%s</b> · %s · click to assign", rec$name, rec$category)),
  highlightOptions = highlightOptions(weight = 3, color = "#000", fillOpacity = 0.7, bringToFront = TRUE))

ui <- page_fluid(
  theme = bs_theme(primary = "#015B4C", base_font = font_google("Work Sans"),
                   heading_font = font_google("Barlow")),
  tags$h3("Hampshire Next — land use & housing capacity", style = "color:#015B4C;font-weight:700;margin-top:8px;"),
  tags$p(class = "text-muted",
    "Click an area — or draw a new one — to assign its land use (Development, Conservation, Core Campus) or ",
    "remove it. Only Development counts toward homes; set its density in the same popup. Homes, the ",
    "Amherst / Hadley split, and tax revenue update live."),
  layout_columns(col_widths = c(8, 4), gap = "12px",
    card(card_header("Campus areas — click or draw an area to assign land use"),
         div(style = "margin-bottom:6px;display:flex;gap:8px;",
             actionButton("reset", "↺ Reset to original areas", class = "btn-sm btn-outline-secondary"),
             downloadButton("export_pdf", "⤓ Export PDF", class = "btn-sm btn-outline-primary")),
         leafletOutput("map", height = 470)),
    layout_columns(col_widths = 12, gap = "8px",
      value_box("Total homes", textOutput("tot"), theme = "primary"),
      value_box("In Amherst", textOutput("tot_amh"), theme = value_box_theme(bg = AMH_COL, fg = "white")),
      value_box("In Hadley",  textOutput("tot_had"), theme = value_box_theme(bg = HAD_COL, fg = "white")),
      uiOutput("acreage"))),
  card(card_header("Annual property-tax revenue at build-out"),
    tags$label("Average assessed value per home, by housing type",
               style = "font-weight:600;color:#015B4C;"),
    layout_columns(col_widths = c(4, 4, 4), gap = "10px",
      numericInput("v_row",   "Row homes",            value = HOME_VALS[["row homes"]],   min = 100000, max = 1500000, step = 10000),
      numericInput("v_town",  "Townhouses",           value = HOME_VALS[["townhouses"]],  min = 100000, max = 1500000, step = 10000),
      numericInput("v_multi", "Low-rise multifamily", value = HOME_VALS[["multifamily"]], min = 100000, max = 1500000, step = 10000)),
    layout_columns(col_widths = c(4, 4, 4), gap = "10px",
      value_box("Amherst tax / yr", textOutput("tax_amh"), theme = value_box_theme(bg = AMH_COL, fg = "white")),
      value_box("Hadley tax / yr", textOutput("tax_had"), theme = value_box_theme(bg = HAD_COL, fg = "white")),
      value_box("Combined / yr", textOutput("tax_tot"), theme = "primary")),
    tags$p(class = "text-muted", style = "font-size:12px;margin:6px 2px 0;",
      "Amherst residential rate 1.795%, Hadley 1.163% (FY2025). Revenue ≈ Σ(homes × value for its housing type) × town rate.")),
  card(card_header("Development areas — density & units"), uiOutput("captable"))
)

server <- function(input, output, session) {
  areas  <- reactiveVal(init_areas)
  dstate <- reactiveVal(setNames(rep(6.4, sum(vapply(init_areas, function(x) x$category == "Development", TRUE))),
                                 vapply(Filter(function(x) x$category == "Development", init_areas), `[[`, "", "id")))
  ndrawn <- reactiveVal(0)
  pop_at <- reactiveVal(NULL)

  dev_areas <- reactive(Filter(function(x) x$category == "Development", areas()))
  calc <- reactive({
    a <- dev_areas(); s <- dstate()
    d <- vapply(a, function(x) { v <- s[[x$id]]; if (is.null(v)) 6.4 else v }, numeric(1))
    amh <- vapply(seq_along(a), function(i) d[i] * a[[i]]$ac_amh, numeric(1))
    had <- vapply(seq_along(a), function(i) d[i] * a[[i]]$ac_had, numeric(1))
    list(a = a, d = d, amh = amh, had = had, tot = amh + had)
  })
  vals <- reactive({                       # editable value per housing type (+ 0 for excluded)
    g <- function(id, def) { v <- input[[id]]; if (is.null(v) || is.na(v) || v <= 0) def else v }
    c("row homes"   = g("v_row",   HOME_VALS[["row homes"]]),
      "townhouses"  = g("v_town",  HOME_VALS[["townhouses"]]),
      "multifamily" = g("v_multi", HOME_VALS[["multifamily"]]),
      "excluded"    = 0)
  })
  taxrev <- reactive({                      # Σ(homes × value-for-its-type) × town rate
    cc <- calc(); val <- unname(vals()[vapply(cc$d, abbr, character(1))])
    amh <- sum(cc$amh * val) * TAX[["Amherst"]]
    had <- sum(cc$had * val) * TAX[["Hadley"]]
    list(amh = amh, had = had, tot = amh + had)
  })

  output$tot     <- renderText(format(round(sum(calc()$tot)), big.mark = ","))
  output$tot_amh <- renderText(format(round(sum(calc()$amh)), big.mark = ","))
  output$tot_had <- renderText(format(round(sum(calc()$had)), big.mark = ","))
  output$tax_amh <- renderText(dollar0(taxrev()$amh))
  output$tax_had <- renderText(dollar0(taxrev()$had))
  output$tax_tot <- renderText(dollar0(taxrev()$tot))
  output$acreage <- renderUI({
    a <- areas()
    accat <- function(cat) round(sum(vapply(Filter(function(x) x$category == cat, a),
                                            function(x) x$ac_amh + x$ac_had, numeric(1))))
    HTML(sprintf(paste0("<div style='font-size:13px;color:#555;padding:2px 4px;'>Acres assigned — ",
      "<span style='color:%s;font-weight:600;'>Development %s</span> · ",
      "<span style='color:%s;font-weight:600;'>Conservation %s</span> · ",
      "<span style='color:%s;font-weight:600;'>Core Campus %s</span></div>"),
      AMH_COL, accat("Development"), CONS_COL, accat("Conservation"), CORE_COL, accat("Core Campus")))
  })

  output$captable <- renderUI({
    cc <- calc(); a <- cc$a
    if (!length(a)) return(tags$p(class = "text-muted", "No Development areas assigned."))
    rows <- lapply(seq_along(a), function(i) tags$tr(
      tags$td(style = "padding:5px 8px;", a[[i]]$name),
      tags$td(style = paste0("padding:5px 8px;font-weight:600;color:", ifelse(a[[i]]$town == "Hadley", HAD_COL, AMH_COL), ";"), a[[i]]$town),
      tags$td(style = "padding:5px 8px;text-align:right;", sprintf("%.1f", a[[i]]$ac_amh + a[[i]]$ac_had)),
      tags$td(style = "padding:5px 8px;", dname(cc$d[i])),
      tags$td(style = "padding:5px 8px;text-align:right;",
              { t <- abbr(cc$d[i]); if (t == "excluded") "—" else dollar0(vals()[[t]]) }),
      tags$td(style = "padding:5px 8px;text-align:right;font-weight:600;",
              if (cc$d[i] == 0) "—" else format(round(cc$tot[i]), big.mark = ","))))
    tags$table(style = "width:100%;border-collapse:collapse;font-size:14px;",
      tags$thead(tags$tr(style = "text-align:left;border-bottom:2px solid #015B4C;color:#015B4C;",
        tags$th(style="padding:6px 8px;","Area"), tags$th(style="padding:6px 8px;","Town"),
        tags$th(style="padding:6px 8px;text-align:right;","Acres"), tags$th(style="padding:6px 8px;","Density"),
        tags$th(style="padding:6px 8px;text-align:right;","Value/home"),
        tags$th(style="padding:6px 8px;text-align:right;","Homes"))),
      tags$tbody(rows))
  })

  # ---- PDF export: static map of the current state + summary + table ---------
  accat <- function(a, cat) round(sum(vapply(Filter(function(x) x$category == cat, a),
                                             function(x) x$ac_amh + x$ac_had, numeric(1))))
  output$export_pdf <- downloadHandler(
    filename = function() paste0("hampshire-next-", Sys.Date(), ".pdf"),
    content = function(file) {
      library(ggplot2)
      a <- areas(); cc <- calc(); vv <- vals(); tr <- taxrev()
      cols <- c("Development · Amherst" = AMH_COL, "Development · Hadley" = HAD_COL,
                "Conservation" = CONS_COL, "Core Campus" = CORE_COL)
      leg <- vapply(a, function(x) if (x$category == "Development")
                      paste0("Development · ", x$town) else x$category, character(1))
      asf <- st_sf(leg = factor(leg, levels = names(cols)),
                   geometry = st_sfc(lapply(a, `[[`, "geom"), crs = 4326))
      bb2 <- st_bbox(asf)
      mp <- ggplot() +
        geom_sf(data = parcels, fill = NA, color = "#9a9a9a", linewidth = 0.15) +
        geom_sf(data = st_transform(towns_m, 4326), fill = NA, color = "#555",
                linewidth = 0.35, linetype = "21") +
        geom_sf(data = asf, aes(fill = leg), color = "#333", linewidth = 0.2, alpha = 0.65) +
        scale_fill_manual(values = cols, drop = FALSE, name = NULL) +
        coord_sf(xlim = c(bb2[[1]], bb2[[3]]), ylim = c(bb2[[2]], bb2[[4]])) +
        theme_void(base_size = 9) +
        theme(legend.position = "bottom", plot.margin = margin(2, 2, 2, 2))
      if (length(cc$a)) {
        dl <- data.frame(
          lng = vapply(cc$a, `[[`, numeric(1), "lng"),
          lat = vapply(cc$a, `[[`, numeric(1), "lat"),
          lab = vapply(seq_along(cc$a), function(i) if (cc$d[i] == 0) "excl."
                       else format(round(cc$tot[i]), big.mark = ","), character(1)))
        mp <- mp + geom_text(data = dl, aes(lng, lat, label = lab),
                             size = 2.4, fontface = "bold", color = "#111")
      }
      money <- function(x) paste0("$", format(round(x), big.mark = ","))
      sumtxt <- paste(
        sprintf("Total homes: %s    (Amherst %s · Hadley %s)",
                format(round(sum(cc$tot)), big.mark = ","),
                format(round(sum(cc$amh)), big.mark = ","),
                format(round(sum(cc$had)), big.mark = ",")),
        sprintf("Acres assigned — Development %s · Conservation %s · Core Campus %s",
                accat(a, "Development"), accat(a, "Conservation"), accat(a, "Core Campus")),
        sprintf("Property tax / yr — Amherst %s · Hadley %s · Combined %s",
                money(tr$amh), money(tr$had), money(tr$tot)),
        sprintf("Home values — row homes %s · townhouses %s · multifamily %s",
                money(vv[["row homes"]]), money(vv[["townhouses"]]), money(vv[["multifamily"]])),
        sep = "\n")
      if (length(cc$a)) {
        td <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
          Area = vapply(cc$a, `[[`, "", "name"),
          Town = vapply(cc$a, `[[`, "", "town"),
          Acres = sprintf("%.1f", vapply(cc$a, function(x) x$ac_amh + x$ac_had, numeric(1))),
          Density = vapply(cc$d, dname, character(1)),
          `Value/home` = vapply(cc$d, function(d) { t <- abbr(d); if (t == "excluded") "—" else money(vv[[t]]) }, character(1)),
          Homes = vapply(seq_along(cc$a), function(i) if (cc$d[i] == 0) "—" else format(round(cc$tot[i]), big.mark = ","), character(1)))
        tg <- gridExtra::tableGrob(td, rows = NULL, theme = gridExtra::ttheme_minimal(base_size = 8))
      } else {
        tg <- grid::textGrob("No Development areas assigned.", gp = grid::gpar(fontsize = 9, col = "#777"))
      }
      title <- grid::textGrob("Hampshire Next — land use & housing capacity", x = 0.01, hjust = 0,
                              gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#015B4C"))
      subt  <- grid::textGrob(paste0("Current scenario · generated ", format(Sys.Date(), "%B %e, %Y")),
                              x = 0.01, hjust = 0, gp = grid::gpar(fontsize = 9, col = "#666"))
      sumg  <- grid::textGrob(sumtxt, x = 0.01, y = grid::unit(1, "npc"), just = c("left", "top"),
                              gp = grid::gpar(fontsize = 9, lineheight = 1.4))
      grDevices::cairo_pdf(file, width = 8.5, height = 11)
      on.exit(grDevices::dev.off())
      gridExtra::grid.arrange(title, subt, mp, sumg, grid::nullGrob(), tg, grid::nullGrob(), ncol = 1,
        heights = grid::unit.c(grid::unit(22, "pt"), grid::unit(16, "pt"),
                               grid::unit(5.4, "in"), grid::unit(84, "pt"), grid::unit(8, "pt"),
                               grid::grobHeight(tg), grid::unit(1, "null")))
    })

  output$map <- renderLeaflet({
    m <- leaflet() |>
      addProviderTiles(providers$CartoDB.Positron, group = "Map") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addMapPane("ctxPane", zIndex = 410) |> addMapPane("wetPane", zIndex = 420) |>
      addMapPane("parcelPane", zIndex = 430) |> addMapPane("devPane", zIndex = 460) |>
      addPolygons(data = st_transform(towns_m, 4326), fill = FALSE, color = "#444", weight = 2,
                  dashArray = "5", label = ~town, group = "Town line",
                  options = pathOptions(pane = "ctxPane", interactive = FALSE))
    for (i in seq_len(nrow(CTX))) { g <- CTX_G[[i]]; if (is.null(g) || !nrow(g)) next
      m <- m |> addPolygons(data = g, group = CTX$label[i], color = CTX$col[i], weight = 2, opacity = 0.9,
             fillColor = CTX$col[i], fillOpacity = CTX$op[i], label = lapply(sprintf("<b>%s</b>", g$.nm), HTML),
             options = pathOptions(pane = "ctxPane")) }
    m <- m |>
      addPolygons(data = wet_w, group = WET_GROUPS[1], color = "#01579B", weight = 1, fillColor = "#4FC3F7", fillOpacity = 0.5, options = pathOptions(pane = "wetPane")) |>
      addPolygons(data = r100_w, group = WET_GROUPS[2], color = "#0277BD", weight = 1, fillColor = "#4FC3F7", fillOpacity = 0.3, options = pathOptions(pane = "wetPane")) |>
      addPolygons(data = r200_w, group = WET_GROUPS[3], color = "#039BE5", weight = 1, fillColor = "#B3E5FC", fillOpacity = 0.3, options = pathOptions(pane = "wetPane")) |>
      addPolygons(data = parcels, group = "Hampshire College parcels", fill = FALSE, color = "#111", weight = 1.2, opacity = 0.7, dashArray = "4", options = pathOptions(pane = "parcelPane", interactive = FALSE))
    for (rec in areas()) m <- add_area(m, rec)
    m |>
      addDrawToolbar(targetGroup = "drawtmp", polylineOptions = FALSE, circleOptions = FALSE,
                     markerOptions = FALSE, circleMarkerOptions = FALSE,
                     polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(color = "#C0392B", weight = 2)),
                     rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(color = "#C0392B", weight = 2)),
                     editOptions = editToolbarOptions(edit = FALSE, remove = FALSE)) |>
      addLayersControl(baseGroups = c("Map", "Satellite"),
        overlayGroups = c(CTX$label, "Hampshire College parcels", WET_GROUPS),
        options = layersControlOptions(collapsed = TRUE)) |>
      hideGroup(c(CTX$label, WET_GROUPS[-1])) |>
      fitBounds(bb[[1]] - .004, bb[[2]] - .004, bb[[3]] + .004, bb[[4]] + .004)
  })

  observe({   # permanent home-count labels on Development areas only
    cc <- calc(); a <- cc$a
    leafletProxy("map") |> clearGroup("ulab")
    if (length(a)) {
      lab <- vapply(seq_along(a), function(i) if (cc$d[i] == 0) "excluded" else sprintf("~%s · %s", format(round(cc$tot[i]), big.mark = ","), abbr(cc$d[i])), character(1))
      leafletProxy("map") |> addLabelOnlyMarkers(lng = vapply(a, `[[`, numeric(1), "lng"), lat = vapply(a, `[[`, numeric(1), "lat"),
        group = "ulab", options = markerOptions(interactive = FALSE), label = lapply(lab, HTML),
        labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
          style = list(color = "#fff", "font-weight" = "700", "font-size" = "11.5px", "text-shadow" = "0 0 3px #000,0 0 3px #000")))
    }
  })

  show_popup <- function(id, lng, lat) {
    rec <- Filter(function(x) x$id == id, areas()); if (!length(rec)) return(); rec <- rec[[1]]
    s <- dstate(); cur_d <- if (id %in% names(s)) s[[id]] else 6.4
    setbtn <- function(inp, key, val, lab, sel, on, off = "#fff", oc = "#222") sprintf(
      paste0("<button onclick=\"Shiny.setInputValue('%s',{area:'%s',%s:%s,n:Math.random()},{priority:'event'})\" ",
             "style='display:inline-block;margin:2px;padding:5px 9px;border:1px solid #cfc8b8;border-radius:6px;",
             "cursor:pointer;font:13px Arial;background:%s;color:%s;'>%s</button>"),
      inp, id, key, if (is.character(val)) paste0("'", val, "'") else val,
      if (sel) on else off, if (sel) "#fff" else oc, lab)
    cats <- paste(vapply(CATS, function(c) setbtn("assigncat", "cat", c, c, rec$category == c, "#015B4C"), ""), collapse = "")
    dens <- if (rec$category == "Development")
      paste0("<div style='margin-top:6px;color:#555;font-size:12px'>Density:</div>",
             paste(vapply(names(DENS), function(nm) setbtn("pick", "dens", unname(DENS[nm]), nm, isTRUE(all.equal(unname(DENS[nm]), cur_d)), "#0487AA"), ""), collapse = "")) else ""
    rm <- sprintf(paste0("<button onclick=\"Shiny.setInputValue('removearea',{area:'%s',n:Math.random()},{priority:'event'})\" ",
             "style='display:block;width:100%%;margin:8px 0 0;padding:6px 9px;border:1px solid #C0392B;border-radius:6px;",
             "cursor:pointer;font:13px Arial;background:#fff;color:#C0392B;font-weight:600;'>\U1F5D1 Remove this area</button>"), id)
    html <- paste0("<div style='min-width:210px;font-family:Arial'><b>", rec$name, "</b><br>",
      "<span style='color:#666;font-size:12px'>", sprintf("%.1f ac · %s · now: %s", rec$ac_amh + rec$ac_had, rec$town, rec$category),
      "</span><div style='margin-top:6px;color:#555;font-size:12px'>Land use:</div>", cats, dens, rm, "</div>")
    leafletProxy("map") |> clearPopups() |> addPopups(lng = lng, lat = lat, popup = html)
  }

  # enclosed unassigned gaps = interior holes of the union of all assigned areas
  gaps <- reactive({
    a <- areas(); if (length(a) < 2) return(NULL)
    u <- st_make_valid(st_union(st_transform(st_sfc(lapply(a, `[[`, "geom"), crs = 4326), CRS_MA)))
    h <- tryCatch(st_make_valid(st_difference(st_make_valid(remove_holes(u)), u)), error = function(e) NULL)
    if (is.null(h) || length(h) == 0 || all(st_is_empty(h))) return(NULL)
    hp <- suppressWarnings(st_cast(st_cast(h, "MULTIPOLYGON"), "POLYGON"))
    sfh <- st_sf(geometry = hp); sfh$ac <- as.numeric(st_area(sfh)) / SQM_ACRE
    sfh <- sfh[sfh$ac > 0.25, ]
    if (!nrow(sfh)) return(NULL)
    sfh$id <- paste0("gap", seq_len(nrow(sfh))); st_transform(sfh, 4326)
  })
  observe({
    g <- gaps()
    leafletProxy("map") |> clearGroup("gaps")
    if (!is.null(g) && nrow(g)) leafletProxy("map") |> addPolygons(
      data = g, layerId = ~id, group = "gaps", fillColor = "#9E9E9E", color = "#333",
      weight = 1, dashArray = "3", fillOpacity = 0.45, options = pathOptions(pane = "devPane"),
      label = lapply(sprintf("Unassigned · %.1f ac · click to assign", g$ac), HTML),
      highlightOptions = highlightOptions(weight = 2, color = "#000", fillOpacity = 0.65, bringToFront = TRUE))
  })
  show_gap_popup <- function(gid, lng, lat) {
    g <- gaps(); if (is.null(g)) return()
    row <- g[g$id == gid, ]; if (!nrow(row)) return()
    catbtn <- function(c) sprintf(paste0("<button onclick=\"Shiny.setInputValue('assigngap',{gap:'%s',cat:'%s',n:Math.random()},{priority:'event'})\" ",
      "style='display:inline-block;margin:2px;padding:5px 9px;border:1px solid #cfc8b8;border-radius:6px;cursor:pointer;font:13px Arial;background:#fff;color:#222;'>%s</button>"), gid, c, c)
    html <- paste0("<div style='min-width:205px;font-family:Arial'><b>Unassigned gap</b><br>",
      "<span style='color:#666;font-size:12px'>", sprintf("%.1f ac — enclosed by assigned areas", row$ac), "</span>",
      "<div style='margin-top:6px;color:#555;font-size:12px'>Assign to:</div>",
      catbtn("Development"), catbtn("Conservation"), catbtn("Core Campus"), "</div>")
    leafletProxy("map") |> clearPopups() |> addPopups(lng = lng, lat = lat, popup = html)
  }
  observeEvent(input$assigngap, {
    gid <- as.character(input$assigngap$gap); cat <- input$assigngap$cat
    g <- gaps(); row <- g[g$id == gid, ]; if (!nrow(row)) return()
    nd <- ndrawn() + 1; ndrawn(nd); id <- paste0("g", nd)
    rec <- make_rec(id, paste("Filled gap", nd), st_geometry(row)[[1]], cat)
    areas(c(areas(), list(rec)))
    if (cat == "Development") { s <- dstate(); s[[id]] <- 6.4; dstate(s) }
    add_area(leafletProxy("map"), rec); leafletProxy("map") |> clearPopups()
  })

  observeEvent(input$map_shape_click, {
    cl <- input$map_shape_click; if (is.null(cl$id)) return()
    id <- as.character(cl$id)
    if (startsWith(id, "gap")) { show_gap_popup(id, cl$lng, cl$lat); return() }
    pop_at(list(id = id, lng = cl$lng, lat = cl$lat)); show_popup(id, cl$lng, cl$lat)
  })

  observeEvent(input$assigncat, {
    id <- as.character(input$assigncat$area); newcat <- input$assigncat$cat
    a <- lapply(areas(), function(x) { if (x$id == id) x$category <- newcat; x })
    areas(a)
    if (newcat == "Development") { s <- dstate(); if (!(id %in% names(s))) { s[[id]] <- 6.4; dstate(s) } }
    rec <- Filter(function(x) x$id == id, a)[[1]]
    leafletProxy("map") |> removeShape(id)
    add_area(leafletProxy("map"), rec)
    p <- pop_at(); if (!is.null(p) && p$id == id) show_popup(id, p$lng, p$lat)
  })

  observeEvent(input$pick, {
    s <- dstate(); s[[as.character(input$pick$area)]] <- as.numeric(input$pick$dens); dstate(s)
    leafletProxy("map") |> clearPopups()
  })

  observeEvent(input$removearea, {
    id <- as.character(input$removearea$area)
    areas(Filter(function(x) x$id != id, areas()))
    s <- dstate(); dstate(s[names(s) != id])
    leafletProxy("map") |> removeShape(id) |> clearPopups()
  })

  observeEvent(input$map_draw_new_feature, {
    f <- input$map_draw_new_feature
    co <- f$geometry$coordinates[[1]]
    mat <- do.call(rbind, lapply(co, function(p) c(p[[1]], p[[2]])))
    geom <- st_polygon(list(mat))
    nd <- ndrawn() + 1; ndrawn(nd); id <- paste0("n", nd)
    rec <- make_rec(id, paste("Drawn area", nd), geom, "Development")
    if (rec$ac_amh + rec$ac_had < 0.05) { leafletProxy("map") |> clearGroup("drawtmp"); return() }
    areas(c(areas(), list(rec)))
    s <- dstate(); s[[id]] <- 6.4; dstate(s)
    leafletProxy("map") |> clearGroup("drawtmp")
    add_area(leafletProxy("map"), rec)
  })

  observeEvent(input$reset, {
    for (x in areas()) leafletProxy("map") |> removeShape(x$id)
    areas(init_areas)
    dstate(setNames(rep(6.4, length(Filter(function(x) x$category == "Development", init_areas))),
                    vapply(Filter(function(x) x$category == "Development", init_areas), `[[`, "", "id")))
    ndrawn(0)
    prox <- leafletProxy("map") |> clearGroup("drawtmp")
    for (rec in init_areas) add_area(prox, rec)
  })
}

shinyApp(ui, server)
