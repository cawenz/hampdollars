# Hampshire College Property Explorer

A Shiny app for exploring parcels owned by the Trustees of Hampshire College
in Amherst and Hadley, Massachusetts. Data sourced from the MassGIS Level 3
Assessors' Parcels (Amherst FY2024, Hadley FY2025).

## Features

- Interactive Leaflet map of all 24 parcels, color-coded by land use or town
- Two-way selection: click a parcel on the map or a row in the table and the
  other view updates to match
- Filter by town and land-use code
- Running summary of parcel count, total acreage, and total assessed value
  for the current filter + selection
- Light basemap and satellite imagery as toggleable layers

## Requirements

R 4.1 or later, with these packages:

```r
install.packages(c("shiny", "bslib", "leaflet", "sf", "DT", "dplyr", "htmltools"))
```

`sf` requires GDAL + PROJ system libraries. On macOS (Homebrew):
`brew install gdal proj`. On Ubuntu: `sudo apt install libgdal-dev libproj-dev`.
On Windows the binary package from CRAN includes its own dependencies.

## Run locally

From the `hampshire_college_explorer/` directory:

```r
shiny::runApp()
```

or from any R session:

```r
shiny::runApp("/path/to/hampshire_college_explorer")
```

## Deploy to shinyapps.io

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "...", token = "...", secret = "...")
rsconnect::deployApp("/path/to/hampshire_college_explorer")
```

## Directory layout

```
hampshire_college_explorer/
├── app.R
├── README.md
└── data/
    └── hampshire_college_parcels_combined.geojson
```

## Data refresh

The GeoJSON was extracted from the MassGIS Level 3 shapefiles. To regenerate
with newer vintages, download fresh shapefiles from
<https://www.mass.gov/info-details/massgis-data-property-tax-parcels>, unzip
them, and filter the `Assess` DBF for `OWNER1 LIKE '%HAMPSHIRE COLLEGE%'`,
then join back to the `TaxPar` polygons on `LOC_ID`.

## Notes

- Four Amherst assessor sub-parcels (25A-1, 25A-1-1, 25A-2-1, 25A-47) are
  tracked separately by the town for valuation purposes but share the main
  25A-23 tax parcel polygon on the ground, so they don't appear as distinct
  shapes on the map.
- Hadley uses FY2025 data; Amherst uses FY2024 (the most recent MassGIS
  vintages at time of extraction).
