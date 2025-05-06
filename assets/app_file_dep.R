if (!identical(getwd(), rprojroot::find_root("app.R"))) {
  stop("This script must be run from the top directory of the climr package")
}

# Leaflet vector grid plugin
dir.create("www/htmlwidgets", showWarnings = FALSE, recursive = TRUE)
utils::download.file("https://unpkg.com/leaflet.vectorgrid@latest/dist/Leaflet.VectorGrid.bundled.min.js",
                     "www/htmlwidgets/lfx-vgrid-prod.js")

# Western North-America BEC Subzone coloring from CCISS
dir.create("data", showWarnings = FALSE, recursive = TRUE)
utils::download.file("https://github.com/bcgov/ccissr/raw/refs/heads/main/data-raw/data_tables/WNAv12_3_SubzoneCols.csv",
                     "data/WNAv12_3_SubzoneCols.csv")
