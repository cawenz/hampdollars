# Land context map: how to interpret the layers

This is a working guide to the overlays available on the Land context tab. Every layer is a snapshot of public MassGIS or federal data, fetched into `data/` by `data-prep/fetch_massgis_layers.R`. Re-run that script any time you want fresh data.

## Basemaps (top-right corner of the map)

Three basemap options, switched via the radio in the top-right of the map:

**Satellite.** Esri World Imagery. Best for seeing what is actually on the ground: buildings, fields, forest cover, parking lots, solar arrays. Use this as the default for spatial intuition.

**Light.** CartoDB Positron. A clean, mostly-white basemap with road labels. Best when you want overlay polygons to read clearly without the visual noise of aerial imagery.

**Topographic.** USGS US Topo. The full traditional USGS topographic map with contour lines, hydrography, roads, place names, and selected buildings. Use this when terrain shape is the point. Contour interval in this area is 20 feet. Tiles cap at zoom 16, so they will look slightly stretched if you zoom further in.

## Hampshire College parcels (orange outline)

The parcels owned by the Trustees of Hampshire College, taken from MassGIS Level 3 Standardized Assessors' Parcels for Amherst (FY2024) and Hadley (FY2025). Click any parcel for a popup with parcel ID, address, land use code and description, acreage, FY assessed value, year built, and the colored encumbrance badge showing which creditor (if any) holds collateral on it. Hover gives a quick parcel ID and address.

The orange outline plus translucent yellow fill is chosen for visibility against the satellite basemap; the colors do not encode anything else.

## Hillshade (terrain relief)

A grayscale shaded-relief rendering of statewide LiDAR-derived elevation, served as tiles by Esri. Hills and valleys appear in low-angle sunlight so you can read terrain shape even on a flat-color basemap. There is no quantitative information attached; this is purely a visual aid for reading topography at a glance. Combine it with the Topographic basemap for proper contour lines.

To get an actual elevation number, click anywhere on the map (when no draw tool is active). A red pin drops and a popup shows ground elevation in feet, sourced from the USGS 3DEP 1-meter elevation dataset. Vertical accuracy in Massachusetts is roughly 0.3 m RMSE.

## DEP wetlands

The regulatory wetland resource areas mapped by MassDEP from aerial photo interpretation (mostly 1:12,000-scale orthos). Every shaded polygon is a wetland of some kind; the type comes from the `IT_VALDESC` field that pops up on hover. The cover types you will see locally:

* **Wooded swamp (deciduous, mixed, coniferous).** Forested wetlands; the most common type in this region. Red maple swamps dominate.
* **Shrub swamp.** Low woody vegetation, often along stream margins.
* **Shallow marsh, meadow, or fen.** Emergent vegetation, seasonally flooded.
* **Deep marsh.** Permanently flooded with emergent vegetation such as cattails.
* **Open water.** Pond and lake surfaces.
* **Bog.** Sphagnum peatlands; rare and the most heavily protected.

Regulatory implications, in plain terms:

* Any polygon is a "resource area" under the Wetlands Protection Act (M.G.L. c. 131 §40). The wetland itself, the **100-foot Buffer Zone** around it, and the **200-foot Riverfront Area** along perennial streams are all jurisdictional, even though only the polygon itself is drawn here.
* For development inside or near a polygon, expect to need a wetlands delineation by a Professional Wetland Scientist, then a Notice of Intent and Order of Conditions from the local Conservation Commission (Amherst or Hadley).
* Bogs and deep marshes carry additional protections; expect a tougher review.

Important caveat: this is a planning-grade layer, not a jurisdictional boundary. The flagged boundary on the ground can differ from the mapped polygon by tens of feet (more in flat terrain), and seasonally wet areas may be wetlands even when not shown. Treat the map as "wetlands are present in this neighborhood, get them flagged before doing anything," not as the legal boundary.

## Vernal pools (certified and potential)

Small points marking vernal pools, from the Natural Heritage and Endangered Species Program. Two flavors:

* **Certified vernal pools** (solid teal-green dots) have been formally documented and certified by NHESP. Certification gives them legal standing under several regulatory programs.
* **Potential vernal pools** (hollow dots) were photo-interpreted from aerial imagery and have not been field-verified or certified.

These matter here because Amherst's local Wetlands Protection Bylaw protects vernal pools and their surrounding habitat beyond what the state Wetlands Protection Act covers on its own. A certified vernal pool on or near a parcel is a meaningful development constraint: it typically triggers local Conservation Commission review of work in the pool's vicinity, and certified pools inside Priority Habitat also factor into MESA review. The points mark the pool location, not the regulated habitat envelope around it, so treat a dot as "there is a protected feature here, with a regulated buffer around it that is not drawn."

## Protected and recreational open space

Land that is permanently or partially protected from development, from the MassGIS Protected and Recreational Open Space dataset. Hover shows the site name, the owner type (municipal, state, federal, land trust, private nonprofit, other), and the level of protection.

Three things to read carefully on any one polygon:

**Owner type** tells you who holds the underlying property. Municipal land is owned by the town (Amherst Conservation Commission, Hadley town forest, and so on). State land includes DCR forests and Wildlife Management Areas. Land-trust and private-nonprofit holdings are owned by groups like the Kestrel Land Trust.

**Protection level** matters more than ownership. "Permanent" means the land is locked into protection by deed restriction, conservation restriction (CR), agricultural preservation restriction (APR), or constitutional Article 97 status; it cannot be developed without state-level action. "Limited" means the protection runs for a defined term, often expiring. "None" means the property is currently open space but has no recorded restriction; it could be sold tomorrow.

**Fee versus interest.** A protected area can either be owned outright by a conservation organization (fee), or it can be private land with a recorded restriction (interest). Both are protected, but only the latter is still on the tax rolls and still subject to private ownership decisions.

For Hampshire's purposes the layer answers two questions at once. How much green space surrounds the campus (which shapes the recreational and ecological context). And how many of the parcels next door are off the market permanently (which constrains long-term land-deal options).

## NHESP Priority Habitat of Rare Species (purple dashed)

Polygons mapped by the Massachusetts Natural Heritage and Endangered Species Program (NHESP) where state-listed rare species are documented or strongly inferred. A parcel that intersects a Priority Habitat polygon triggers the Massachusetts Endangered Species Act (MESA, M.G.L. c. 131A); any project that would alter habitat must be reviewed by NHESP before permits issue.

In practice this means an early conversation with NHESP, often a habitat assessment, and sometimes mitigation or project redesign. Priority Habitat does not automatically prohibit development, but it adds a regulatory layer that takes months to clear and can substantially shape what is buildable.

There is a separate, smaller layer called Estimated Habitat of Rare Wildlife, used to trigger Wetlands Protection Act review specifically for projects in wetlands. It is not currently included here; we can add it if it becomes useful.

## Wellhead protection (Zone II and IWPA)

Public-drinking-water-supply protection zones, in two flavors styled differently on the map:

**Zone II (darker blue)** is the area that contributes water to a public water supply well under maximum pumping conditions, mapped by MassDEP after a formal hydrogeological study. Zone II carries the strongest restrictions under the Drinking Water Regulations (310 CMR 22.21). Certain land uses (large fuel storage, junkyards, dry cleaners, intensive industrial uses, road salt storage) are prohibited or tightly controlled.

**IWPA (lighter blue)** is the Interim Wellhead Protection Area, a default radius (usually 400 to 2,500 feet, depending on the well's pumping rate) used until a proper Zone II is delineated. It carries the same regulatory weight as Zone II.

If a parcel sits inside one of these zones, expect extra scrutiny on any use that involves chemicals, fuels, large impervious areas, or significant earth disturbance. Hover gives the well operator and town.

## Areas of Critical Environmental Concern (ACECs)

Areas formally designated by the Secretary of Energy and Environmental Affairs as having unique natural, scenic, historic, or ecological values worth protecting (301 CMR 12.00). Projects inside an ACEC face heightened state environmental review (MEPA), stricter wetlands review thresholds, and ineligibility for certain state actions that would degrade the area.

There are no ACECs in the immediate area around Hampshire College; the closest sit further east and west. The layer is included so it stays available if the map bounds are widened or for reference when discussing land elsewhere in the region.

## Developable land methodology

The Developable land tab combines several of these layers into a per-parcel estimate of how much of each Hampshire parcel could be built on without major regulatory friction. The math runs in `data-prep/compute_developable.R`, which reads the snapshots in `data/` and writes `data/parcels_developable.geojson` for the app to load.

### Three tiers of acreage

For each parcel, the script computes three acreage numbers:

**Gross acres.** The parcel's mapped polygon area, computed in MA State Plane (EPSG:26986). This will be close to but not always identical to the assessor's `ACRES_CALC` field, because assessor acreage is sometimes from the deed rather than from the polygon.

**Net unconstrained acres.** Gross acres minus the parts of the parcel covered by:

* Regulatory wetland polygons (the DEP wetlands layer).
* Permanently protected open space (Protected & Recreational Open Space polygons where `LEV_PROT = "P"`).

The logic is that wetlands cannot be filled without major mitigation, and permanently protected open space is off the development table by deed restriction, CR, or Article 97 status. Subtracting them gives a "best case" ceiling on what could ever be considered for development.

**Net easy-to-build acres.** Net unconstrained acres minus the parts of the parcel covered by:

* The 100-foot Wetlands Protection Act Buffer Zone (a buffer drawn around every regulatory wetland polygon).
* NHESP Priority Habitat of Rare Species.

These are areas where development is technically allowed but triggers significant regulatory review (Notice of Intent for the Buffer Zone, MESA review for Priority Habitat). Subtracting them gives a "easy path" estimate of land where development would face the least regulatory friction.

The gap between net unconstrained and net easy-to-build tells you how much of the developable land carries permitting friction. A parcel that is 80% net unconstrained but only 40% easy-to-build is mostly wetland buffer, and any project would need a wetlands delineation and ConCom review.

### Buffer scenarios: standard vs. strict

The Developable land tab has a "Wetland buffer" toggle with two scenarios:

* **Standard (100 ft)** uses the state Wetlands Protection Act Buffer Zone width. This is the default.
* **Strict (200 ft)** doubles the buffer to a conservative width that approximates the 200-ft Riverfront Area plus a generous reading of local no-disturbance rules.

Amherst's local Wetlands Protection Bylaw is generally stricter than the state WPA. It applies the same 100-ft Buffer Zone but layers a no-disturbance zone inside it and reaches resources (isolated wetlands, vernal pools) the state may not. The exact widths should be confirmed against the current bylaw and regulations, but the practical effect is that the true "easy-to-build" figure for an Amherst parcel sits somewhere between the standard and strict scenarios, likely closer to strict where wetlands are present. Switching the toggle recomputes the green developable polygons, the top stat tiles, the selection summary, and the table. Across all 24 parcels the college-wide net easy-to-build figure drops from roughly 615 acres (standard) to roughly 533 acres (strict).

### Road frontage

For each parcel the script also estimates road frontage, which matters for development because:

* Many zoning bylaws require a minimum frontage on a public way for a buildable lot.
* Curb cuts, utility connections, and emergency access all need road adjacency.
* A parcel with long frontage can often be subdivided; one with little frontage usually cannot.

Method: the script takes the MassDOT Roads centerline layer (excluding ramps), buffers each road outward by about 30 feet (a typical half-right-of-way for local Massachusetts roads), and intersects each parcel's polygon boundary with the resulting corridor. The length of the intersection is the estimated frontage. The script also keeps a deduplicated list of which streets contribute frontage, so the popup can tell you whether a parcel fronts Bay Road, West Street, or some combination.

This is a proxy, not a survey. The 30-foot buffer will over-credit parcels along wider state-highway ROWs and under-credit parcels with deep set-backs from the centerline. For Hampshire's parcels, which sit on a mix of state routes and local roads, the numbers should be within roughly 10 to 20 percent of a careful survey-based count.

### What the script does NOT account for

* **Slope.** Steep terrain is a real developability constraint but requires processing the USGS 3DEP DEM, which is not part of the current pipeline.
* **Existing buildings and impervious surface.** If the question is "how much more could be built," existing structures are context, not exclusions.
* **Setbacks from neighbors, septic, and zoning dimensions.** These are jurisdiction-specific and would typically shave another 5 to 15 percent off the easy-to-build figure in practice.
* **Wellhead protection zones.** Zone II and IWPA restrict use type, not parcel area, so they appear on the map as context but are not subtracted from the developable totals.
* **ACECs.** None exist in the Hampshire area; if the analysis is ever extended to parcels inside an ACEC, that designation should be added as an absolute exclusion.

### How to read the map

Parcels are shaded by either percent developable (red to yellow to green) or absolute net easy-to-build acres (viridis), depending on the "Color by" toggle. The fill opacity is intentionally low so that the satellite basemap shows through, letting you see existing buildings, parking, and field shape inside each parcel.

Toggling overlays adds the constraint layers on top of the basemap but underneath the parcel coloring: wetlands in blue, permanent open space in green, priority habitat in dashed purple, and an optional hillshade for terrain context. Clicking any parcel shows a popup with the three acreage tiers, percent developable, frontage, and which roads contribute the frontage.

### Refreshing the analysis

If you fetch fresh MassGIS snapshots (re-run `data-prep/fetch_massgis_layers.R`) you should also re-run `Rscript data-prep/compute_developable.R` to recompute the per-parcel numbers. Knobs to turn inside that script:

* `HALF_ROW_M` (default 30 ft) for how generously to count frontage.
* `WET_BUFFER_M` (default 100 ft) to widen the wetland buffer toward the 200-foot Riverfront Area.
* The filter `LEV_PROT == "P"` to decide whether "limited" protection should also count as an exclusion.

## Things worth knowing across all layers

**These are planning layers, not deeds.** Treat each polygon as "the state thinks this is here." Almost every regulatory layer has a corresponding on-the-ground process (wetlands delineation, MESA habitat assessment, ACEC nomination paperwork) that produces the legally authoritative boundary. The mapped polygon is a heads-up, not a determination.

**Regulatory reach is usually wider than what is drawn.** Wetlands carry 100-foot Buffer Zones and 200-foot Riverfront Areas, which are jurisdictional but not drawn here. Priority Habitat triggers MESA review for projects within the polygon, but adjacent projects can still attract scrutiny. ACECs raise review thresholds for proposals nearby, not just inside. When a parcel sits within a few hundred feet of any of these layers, assume the regulatory reach is wider than the polygon you see.

**The hover label is your best friend.** Each polygon carries its source attributes from MassGIS. Hover gives the specifics (wetland cover type, site name, well operator, owner type, protection level) that the styled color alone cannot convey.

**Drawing your own shapes.** The toolbar at the top-left of the map (polygon, rectangle, line) lets you sketch study areas; the sidebar shows live totals for acreage, square footage, perimeter, and line length. Use these for back-of-the-envelope sizing of potential development footprints, frontage along roads, or trail lengths. Drawn shapes are local to the browser session and do not persist; export by screenshot if you want to keep them.

**Refreshing the data.** All MassGIS snapshots in `data/` were fetched within a buffered bounding box around the Hampshire parcels. To pull fresh data, run `Rscript data-prep/fetch_massgis_layers.R` from the project root. Widen the buffer in that script if you want to see context further afield.
