# Setup ----
suppressPackageStartupMessages({
  library(archive)
  library(bslib)
  library(data.table)
  library(DT)
  library(htmltools)
  library(htmlwidgets)
  library(jsonlite)
  library(leafem)
  library(leaflet.extras)
  library(leaflet)
  library(shiny)
  library(terra)
  library(climr)
  library(zip)
  library(plotly)
  source("scripts/utils.R", local = TRUE)
})

# Shiny options
options(shiny.autoreload = TRUE)
options(shiny.maxRequestSize = 1000 * 1024^2)

# MapBox values
mbtk <- Sys.getenv("BCGOV_MAPBOX_TOKEN")
mblbstyle <- Sys.getenv("BCGOV_MAPBOX_LABELS_STYLE")
mbhsstyle <- Sys.getenv("BCGOV_MAPBOX_HILLSHADE_STYLE")

pals <- readRDS("scripts/pals.rds")

# Elevation raster for missing values
elevtif <- c(Sys.getenv("ELEV_RASTER"), "../northamerica_elevation_cec_2023.tif")
if (!length(felev <- which(file.exists(elevtif)))) {
  curl::curl_download("http://www.cec.org/files/atlas_layers/0_reference/0_03_elevation/elevation_tif.zip", "elevation_tif.zip")
  unzip("elevation_tif.zip", files = "Elevation_TIF/NA_Elevation/data/northamerica/northamerica_elevation_cec_2023.tif", junkpaths = TRUE, exdir = "..")
  unlink("elevation_tif.zip")
  cec <- terra::rast("../northamerica_elevation_cec_2023.tif")
  cec <- terra::project(cec, "EPSG:4326")
  terra::writeRaster(cec, "../northamerica_elevation_cec_2023.tif", overwrite = TRUE)
} else {
  cec <- terra::rast(elevtif[felev])
}

# Base map ---- 
l <- leaflet::leaflet(
  options = leaflet::leafletOptions(maxZoom = 25)
) |>
  # base layer
  leaflet::addProviderTiles(
    provider = leaflet::providers$CartoDB.PositronNoLabels,
    options = leaflet::pathOptions(pane = "mapPane", maxZoom = 25, maxNativeZoom = 20),
    group = "Light"
  ) |>
  leaflet::addProviderTiles(
    provider = leaflet::providers$CartoDB.DarkMatterNoLabels,
    options = leaflet::pathOptions(pane = "mapPane", maxZoom = 25, maxNativeZoom = 20),
    group = "Dark"
  ) |>
  leaflet::addProviderTiles(
    provider = leaflet::providers$Esri.WorldImagery,
    options = leaflet::pathOptions(pane = "mapPane", maxZoom = 25, maxNativeZoom = 18),
    group = "Satellite"
  ) |>
  leaflet::addProviderTiles(
    provider = leaflet::providers$OpenStreetMap,
    options = leaflet::pathOptions(pane = "mapPane", maxZoom = 25, maxNativeZoom = 20),
    group = "OpenStreetMap"
  ) |>
  leaflet::addTiles(
    urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mbhsstyle, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
    attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
    options = leaflet::pathOptions(pane = "mapPane", maxZoom = 25, maxNativeZoom = 22),
    group = "Hillshade"
  ) |>
  # overlay layer
  leaflet::addTiles(
    urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbstyle, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
    attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
    options = leaflet::pathOptions(pane = "overlayPane", maxZoom = 25, maxNativeZoom = 22),
    group = "Labels"
  ) |>
  add_custom_render() |>
  # extensions
  leaflet.extras::addSearchOSM(
    options = leaflet.extras::searchOptions(
      collapsed = TRUE,
      hideMarkerOnCollapse = TRUE,
      autoCollapse = TRUE,
      zoom = 11
    )
  ) |>
  leaflet::addLayersControl(
    baseGroups = c("Light", "Dark", "Satellite", "OpenStreetMap", "Hillshade"),
    overlayGroups = c("Labels", "WNA BEC", "Climate"),
    position = "topright"
  ) |>
  leaflet::setView(lng = -125, lat = 55, zoom = 5) |>
  #leaflet::addMiniMap(toggleDisplay = TRUE, minimized = TRUE) |>
  #default_draw_tool() |>
  leaflet::hideGroup(c("WNA BEC", "Climate")) |>
  leaflet::showGroup("Hillshade")

# Shiny App ----
shiny::shinyApp(
  ui = shiny::tagList(
    # Favicon
    tags$head(
      tags$link(rel="apple-touch-icon", href="images/bcid-apple-touch-icon.png", sizes="180x180"),
      tags$link(rel="icon", href="images/bcid-favicon-32x32.png", sizes="32x32", type="image/png"),
      tags$link(rel="icon", href="images/bcid-favicon-16x16.png", sizes="16x16", type="image/png"),
      tags$link(rel="mask-icon", href="images/bcid-apple-icon.svg", color="#036"),
      tags$link(rel="icon", href="images/bcid-favicon-32x32.png")
    ),
    shiny::navbarPage(
      collapsible = TRUE,
      theme = bslib::bs_theme(
        preset = "bcgov",
        "navbar-brand-padding-y" = "0rem",
        "navbar-brand-margin-end" = "4rem"
      ),
      title = shiny::tagList(
        shiny::tags$image(
          src = "images/bcid-logo-rev-en.svg",
          style = "display: inline-block",
          height = "35px",
          alt = "British Columbia"
        ),
        "ClimR"
      ),
      shiny::tabPanel(
        title = "Map",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            style = "height: 84vh; overflow-y: auto; overflow-x: auto;", 
            
            # need to adjust this so the helplink is centered, create the link
            shiny::actionLink(
              inputId = "tutorial",
              label = "Click here for a tutorial"
              ),
            br(), br(),
            strong("Add Sites Using One of the 2 Methods Below:"),
            accordion(
              # need to add help icons to each of the methods
              multiple = FALSE,
              
              accordion_panel(
                title = "Method 1: By selection on map",
                
                accordion(
                  multiple = FALSE,
                  
                  accordion_panel(
                    title = "Click on map to add points",
                    DT::DTOutput("geom_dt", width = "100%"),
                    #shiny::actionButton("add_button", "Enter New", icon("plus")),
                    shiny::actionButton("delete_button_point", "Delete Selected", icon("trash-alt")),
                    value = "acc1_pan1"
                  ),
                
                  accordion_panel(
                    title = "Draw on map to add area-of-interest",
                    ("Choose drawing tool:"),
                    shiny::actionButton("draw_square", icon("square")),
                    shiny::actionButton("draw_circle", icon("circle")),
                    shiny::actionButton("draw_polygon", icon("draw-polygon")), #change this icon
                    DT::DTOutput("points_table", width = "100%"),
                    #shiny::actionButton("add_button", "Enter New", icon("plus")),
                    shiny::actionButton("delete_button_draw", "Delete Selected", icon("trash-alt"))
                  )
                )
              ),
              
              accordion_panel(
                title = "Method 2: Upload a file",
                # shiny::actionButton("upload_button", "Upload", icon("upload"),
                #                     style = "width:100%; background-color:#8f0e7e; color: #FFF"),
                shiny::div(
                  #title = "Upload a csv, a raster or a shape file to add geographies",
                  shiny::fileInput(
                    inputId = "upload",
                    label = "Upload a csv, a raster or a shape file to add geographies"
                  )
                )
                
              )
            ),
            br(), br(),
            shiny::actionButton("downscale_params", "Choose Downscale Parameters",
                                style = "width:100%", disabled = TRUE)
          ),
          shiny::mainPanel(
            # create map as UI element
            leaflet::leafletOutput("climr", width = "100%", height = "84vh") #height needs to be fixed to be adaptive
          )
        )
      ),
     
      shiny::navbarMenu(
        "About",
        "How to use",
        shiny::tabPanel("Map"),
        shiny::tabPanel("Data"),
        "climr package",
        shiny::tabPanel(
          title = "Documentation",
          shiny::tags$iframe(
            src = "https://bcgov.github.io/climr/reference/index.html",
            style = "width: 100%; height: 90vh; border: none;",
            seamless = "seamless"
          )
        ),
        shiny::tabPanel(
          title = "Articles",
          shiny::tags$iframe(
            src = "https://bcgov.github.io/climr/articles/index.html",
            style = "width: 100%; height: 100vh; border: none;",
            seamless = "seamless"
          )
        )
      ),
      header = list(
        shiny::includeCSS("www/style.css"),
        shiny::includeScript("www/script.js")
      )
    ),
    # Footer
    tags$footer(
      class = "footer mt-5",
      tags$nav(
        class = "navbar navbar-expand-lg bottom-static navbar-dark bg-primary-nav",
        tags$div(
          class = "container",
          tags$ul(
            class = "navbar-nav",
            tags$li(class = "nav-item", tags$a(class = "nav-link", href = "https://www2.gov.bc.ca/gov/content/home", "Home", target = "_blank")),
            tags$li(class = "nav-item", tags$a(class = "nav-link", href = "https://www2.gov.bc.ca/gov/content?id=79F93E018712422FBC8E674A67A70535", "Disclaimer", target = "_blank")),
            tags$li(class = "nav-item", tags$a(class = "nav-link", href = "https://www2.gov.bc.ca/gov/content?id=9E890E16955E4FF4BF3B0E07B4722932", "Privacy", target = "_blank")),
            tags$li(class = "nav-item", tags$a(class = "nav-link", href = "https://www2.gov.bc.ca/gov/content?id=E08E79740F9C41B9B0C484685CC5E412", "Accessibility", target = "_blank")),
            tags$li(class = "nav-item", tags$a(class = "nav-link", href = "https://www2.gov.bc.ca/gov/content?id=1AAACC9C65754E4D89A118B875E0FBDA", "Copyright", target = "_blank")),
            tags$li(class = "nav-item", tags$a(class = "nav-link", href = "https://www2.gov.bc.ca/gov/content?id=6A77C17D0CCB48F897F8598CCC019111", "Contact Us", target = "_blank"))
          )
        )
      )
    )
  ),
  
  # Shiny server ----
  server = function(input, output, session) {
    session$allowReconnect("force")
    
    # ---- Modal input storage
    output$climr <- leaflet::renderLeaflet(l)
    
    # ---- Geometry
    source("scripts/geometry_workingcpy.R", local = TRUE)
    sg <- session_geometry()
    
    # ---- Map events
    shiny::observeEvent(input$climr_draw_start, {
      if (shiny::in_devmode()) cat("Event: climr_draw_start", sep = "\n")
      sg$add_point_enabled(FALSE)
    })
    shiny::observeEvent(input$climr_draw_stop, {
      if (shiny::in_devmode()) cat("Event: climr_draw_stop", sep = "\n")
      sg$add_point_enabled(TRUE)
    })
    shiny::observeEvent(input$climr_draw_new_feature, {
      if (shiny::in_devmode()) cat("Event: climr_draw_new_feature", sep = "\n")
      sg$add_draw_poly(input$climr_draw_new_feature)
    })
    shiny::observeEvent(input$climr_click, {
      if (shiny::in_devmode()) cat("Event: climr_click", sep = "\n")
      sg$add_point(input$climr_click$lat, input$climr_click$lng)
    })
    shiny::observeEvent(input$upload_button, {
      if (shiny::in_devmode()) cat("Event: upload_button", sep = "\n")
      sg$add_file(input$upload_button)
    })
    
    # delete a map point (currently using row ID to delete not point ID)
    shiny::observeEvent(input$delete_button_point, {
      if (shiny::in_devmode()) cat("Event: sg_remove", sep = "\n")
      row_num <- input$geom_dt_rows_selected
      point_id <- as.numeric(map_points$dt[row_num,1])
      sg$rm(point_id)
    })
    # pop-up remove button for map points
    shiny::observeEvent(input$sg_remove, {
      if (shiny::in_devmode()) cat("Event: sg_remove", sep = "\n")
      sg$rm(input$sg_remove)
    })

    
    sn <- \(j) setNames(j,j)
    
  }
)

