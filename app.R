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
  library(dplyr)
  library(quarto)
  source("scripts/utils.R", local = TRUE)
})

if (Sys.getenv("SHINY_DEPLOY") == "server") {
  terraOptions(tempdir = "/opt/rtmp/temp")
}

# Tooltip setup
tooltipsIcon <- icon("question-circle")
# Use regular style instead of solid
tooltipsIcon$attribs$class <- gsub("fa ", "far ", tooltipsIcon$attribs$class, fixed = TRUE)
# Wrap in a span to be able to use prompter
tooltipsIcon <- span(tooltipsIcon)

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
    # overlayGroups = c("Labels", "WNA BEC", "Climate"),
    overlayGroups = c("Labels"),
    position = "topright"
  ) |>
  leaflet::setView(lng = -125, lat = 55, zoom = 5) |>
  leaflet::addMiniMap(toggleDisplay = TRUE, minimized = TRUE) |>
  # default_draw_tool() |>
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
        "climr"
      ),
      shiny::tabPanel(
        title = "Get Data",
        prompter::use_prompt(),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            style = "height: 84vh; overflow-y: auto; overflow-x: auto;", # FIX HEIGHT TO BE ADAPTIVE
            
            # create the link!!!
            shiny::div(
              style = "text-align: center;",
              shiny::actionLink(
                inputId = "tutorial",
                label = "What does this page do",
                icon = icon("question-circle")
                )
            ),
            br(),
            splitLayout(
              actionButton("clear_selections", "Clear Selections",
                            style = "width:100%; height:70px; background-color:#c21104; color: #FFF"),
              actionButton(
                "generate_results",
                label = "Generate Results",
                icon = icon("plus-square"),
                style = "width:100%; height:70px; background-color:#003366; color: #FFF",
                disabled = TRUE
              )
            ),
            br(),
            
            strong("Add Sites Using One of the 2 Methods Below:"),
            accordion(
              multiple = FALSE,
              open = FALSE,
              id = "acc_methods",
              
              accordion_panel(
                title = h5("Method 1: By selection on map",
                            prompter::add_prompt(
                              tooltipsIcon,
                              message = HTML(paste("Click on map to add points or draw an area-of-interest using shape tools in left-hand corner of map.")),
                              position = "top",
                              size = "large",
                              shadow = FALSE
                            )
                ),
                DT::DTOutput("geom_dt", width = "100%"),
                shiny::actionButton("delete_button", "Delete Selected", icon("trash-alt")),
                value = "acc_method1"
              ),
              
              accordion_panel(
                title = h5("Method 2: Upload a file",
                           prompter::add_prompt(
                             tooltipsIcon,
                             message = HTML(paste("Upload a csv, a raster or a shape file to add geographies.")),
                             position = "top",
                             size = "large",
                             shadow = FALSE
                           )
                ),
                shiny::div(
                  shiny::fileInput(
                    inputId = "upload",
                    label = "Upload a file:"
                  )
                ),
                value = "acc_method2"
              )
            ),
            br(),
            shiny::actionButton("downscale_parameters", "Choose Downscale Parameters",
                                disabled = TRUE, icon = icon("sliders-h"), style = "width:100%; align:center;"),
            
            br(), br(),
            
            # options for raster preview
            uiOutput("preview_raster_elements", width = "100%"),
          ),
          shiny::mainPanel(
            # create map as UI element
            leaflet::leafletOutput("getdata_map", width = "100%", height = "84vh") #height needs to be fixed to be adaptive
          )
        )
      ),
      
      shiny::tabPanel(
        title = "Visualization",
        prompter::use_prompt(),
        shinyjs::useShinyjs(),
        shiny::mainPanel(
                  id = "main-panel-container",
                  width = "100%",
                  # shinyjs::useShinyjs(),
                  tags$head(
                    tags$script(HTML("
                                Shiny.addCustomMessageHandler('toggle-plot', function(show) {
                                  if (show) {
                                    document.body.classList.add('show-plot');
                                  } else {
                                    document.body.classList.remove('show-plot');
                                  }
                                });
                              ")),
                    tags$style(HTML("
                              #map-container {
                                width: 100%;
                                height: 85vh;
                                float: left;
                                transition: width 0.5s ease-in-out;
                              }
                              
                              .show-plot #map-container {
                                width: 30%;
                              }
                              
                              #plot-container {
                                width: 69%;
                                float: right;
                                height: 85vh;
                                overflow-y: auto;
                                display: none;
                              }
                              
                              .show-plot #plot-container {
                                display: block;
                              }
                              
                              #plot-tabs .nav {
                                display: flex !important;
                                flex-wrap: nowrap !important;
                              }
                              
                              #plot-tabs .nav-item {
                                flex: 1 1 0 !important;
                                min-width: 0 !important;
                                text-align: center;
                              }
                              
                              .tight-card h5, .tight-card h6 {
                                margin-top: 4px;
                                margin-bottom: 4px;
                              }
                            "))
                  ),
                  # Map container
                  shiny::div(id = "map-container",
                      leaflet::leafletOutput("vis_map", width = "100%", height = "84vh"),
                      shiny::absolutePanel(
                        class = "input-control",
                        top = 90,            
                        left = 60,           
                        width = 170,
                        style = "padding: 10px;",
                        # # create the link!!!
                        # shiny::div(
                        #   style = "text-align: center;",
                        #   shiny::actionLink(
                        #     inputId = "tutorial",
                        #     label = "What does this page do",
                        #     icon = icon("question-circle")
                        #     )
                        # ),
                        shiny::radioButtons(
                          inputId = "input_type",
                          label = h4("Visualize by:", style = "margin-bottom: 7px;"),
                          width = "100%",
                          choices = c("FLP Area", "Ecoregion", "Map point"),
                          selected = character(0)
                        ),
                        shiny::actionButton("clear_map", "Clear Map",
                                            style = "width:100%; height:40px; background-color:#c21104; color: #FFF"
                        ),
                        shiny::checkboxInput("show_overlay_controls", "Show climate map"
                        ),
                        shiny::conditionalPanel(
                          condition = "input.show_overlay_controls == true",
                          h4("Overlay Controls"),
                          shiny::uiOutput("overlay_element"),
                          shiny::uiOutput("overlay_period"),
                          shiny::uiOutput("scale_adj"),
                          shiny::actionButton(
                            inputId = "load_overlay",
                            label = "Load",
                            icon = shiny::icon("droplet"),
                            width = "100%"
                          ),
                          shiny::actionButton(
                            inputId = "download_overlay",
                            label = "Download",
                            disabled = TRUE,
                            icon = shiny::icon("map"),
                            width = "100%"
                          ) 
                        )
                      ),
                  ),
                  
                  # Plot container (initially hidden)
                  shiny::conditionalPanel(
                    condition = "input.input_type !== null",
                    shiny::div(id = "plot-container",
                               style = "height: 85vh; display: flex; flex-direction: column;",
                      bslib::navset_card_underline(
                        id = "plot-tabs",
                        bslib::nav_panel("Bivariate",
                                         shiny::fluidRow(
                                           column(
                                             width = 3,
                                             class = "tight-card",
                                             style = "height: 75vh;", 
                                             bslib::card(
                                               title = "Bivariate Plot Variables",
                                               style = "height: 99%; overflow-y: auto;",
                                               shiny::conditionalPanel(
                                                 condition = "input.input_type == 'Map point'",
                                                 shiny::actionButton(
                                                   inputId = "downscale_data_bivariate",
                                                   label = "Downscale Data",
                                                   style = "background-color:#1d8f0e; color: #FFF"
                                                 ),
                                                 h4("Interactive Plot Options", style = "margin-bottom: 5px;"),
                                               ),
                                               h5("X-Axis", style = "margin-bottom: 5px;"),
                                               shiny::selectInput(
                                                 inputId = "bivariate_element_x",
                                                 label = h6("Choose element:"),
                                                 width = "100%",
                                                 choices = unique(climr::variables %>% pull(Code_Element)),
                                                 selected = "MAT"
                                               ),
                                               shiny::uiOutput("bivariate_valid_time_x"),
                                               h5("Y-Axis", style = "margin-bottom: 5px;"),
                                               shiny::selectInput(
                                                 inputId = "bivariate_element_y",
                                                 label = h6("Choose element:"),
                                                 width = "100%",
                                                 choices = unique(climr::variables %>% pull(Code_Element)),
                                                 selected = "MAP"
                                               ),
                                               shiny::uiOutput("bivariate_valid_time_y"),
                                               shiny::radioButtons(
                                                 inputId = "bivariate_period",
                                                 label = h5("Choose time period:"),
                                                 width = "100%",
                                                 inline = TRUE,
                                                 choices = climr::list_gcm_periods(),
                                                 selected = "2041_2060"
                                               ),
                                               shiny::conditionalPanel(
                                                 condition = "input.input_type == 'FLP Area' || input.input_type == 'Ecoregion'",
                                                 shiny::actionButton(
                                                   inputId = "plot_bivariate_flp_er",
                                                   label = "Plot",
                                                   icon = icon("chart-simple"),
                                                   style = "background-color:#1d8f0e; color: #FFF"
                                                 )
                                               ),
                                               shiny::downloadButton(
                                                 outputId = "bivariate_download",
                                                 label = "Download Plot",
                                                 style = "width: 100%;"
                                               )
                                             )
                                           ),
                                           column(
                                             width = 9,
                                             style = "height: 75vh;", 
                                             plotly::plotlyOutput("bivariate_plot", height = "600px")
                                           )
                                         )),
                        bslib::nav_panel("Walter-Lieth",
                                         shiny::fluidRow(
                                           column(
                                             width = 3,
                                             class = "tight-card",
                                             style = "height: 75vh;", 
                                             bslib::card(
                                               title = "Walter-Lieth Variables",
                                               style = "height: 99%; overflow-y: auto;",
                                               shiny::conditionalPanel(
                                                 condition = "input.input_type == 'Map point'",
                                                 shiny::actionButton(
                                                   inputId = "downscale_data_wl",
                                                   label = "Downscale Data",
                                                   style = "background-color:#1d8f0e; color: #FFF"
                                                 ),
                                                 h4("Interactive Plot Options", style = "margin-bottom: 5px;")
                                               ),
                                               shiny::radioButtons(
                                                 inputId = "wl_obs_period",
                                                 label = h5("Choose observed period:"),
                                                 width = "100%",
                                                 inline = TRUE,
                                                 choices = c("1961_1990", climr::list_obs_periods())
                                               ),
                                               shiny::checkboxInput(
                                                 inputId = "wl_diurnal",
                                                 label = tags$span("Show diurnal range", style = "font-size: 0.85em; font-weight: bold;"),
                                               ),
                                               shiny::conditionalPanel(
                                                 condition = "input.input_type == 'FLP Area' || input.input_type == 'Ecoregion'",
                                                 shiny::actionButton(
                                                   inputId = "plot_wl_flp_er",
                                                   label = "Plot",
                                                   icon = icon("chart-simple"),
                                                   style = "background-color:#1d8f0e; color: #FFF"
                                                 )
                                               ),
                                               shiny::downloadButton(
                                                 outputId = "wl_download",
                                                 label = "Download Plot",
                                                 style = "width: 100%;",
                                                 disabled = "disabled"
                                               )
                                             )
                                           ),
                                           column(
                                             width = 9,
                                             style = "height: 75vh;", 
                                             shiny::plotOutput("wl_plot", height = "600px")
                                           )
                                         )),
                        # bslib::nav_panel("Climate Stripes"),
                        # bslib::nav_panel("Boxplot"),
                        bslib::nav_panel("Time Series",
                                         shiny::fluidRow(
                                           column(
                                             width = 3,
                                             class = "tight-card",
                                             style = "height: 75vh;", 
                                             bslib::card(
                                               title = "Time Series Variable",
                                               style = "height: 99%; overflow-y: auto;",
                                               shiny::conditionalPanel(
                                                 condition = "input.input_type == 'Map point'",
                                                 shiny::actionButton(
                                                   inputId = "downscale_data_time_series",
                                                   label = "Downscale Data",
                                                   style = "background-color:#1d8f0e; color: #FFF"
                                                 ),
                                                 h4("Interactive Plot Options", style = "margin-bottom: 5px;"),
                                               ),
                                               shiny::radioButtons(
                                                 inputId = "time_series_dataset",
                                                 label = h5("Choose dataset:"),
                                                 width = "100%",
                                                 inline = TRUE,
                                                 choices = c("MSWX Blend" = "mswx.blend", "ClimateNA" = "climatena", "Climatic Research Unit / Global Precipitation Climatology Centre" = "cru.gpcc"),
                                                 selected = "mswx.blend"
                                               ),
                                               shiny::selectInput(
                                                 inputId = "time_series_element",
                                                 label = h5("Choose element:"),
                                                 width = "100%",
                                                 choices = unique(climr::variables %>% pull(Code_Element)),
                                                 selected = "Tmax"
                                               ),
                                               shiny::uiOutput("time_series_valid_season"),
                                               shiny::conditionalPanel(
                                                 condition = "input.input_type == 'FLP Area' || input.input_type == 'Ecoregion'",
                                                 shiny::actionButton(
                                                   inputId = "plot_ts_flp_er",
                                                   label = "Plot",
                                                   icon = icon("chart-simple"),
                                                   style = "background-color:#1d8f0e; color: #FFF"
                                                 )
                                               ),
                                               shiny::actionButton(
                                                 inputId = "ts_adj_plot",
                                                 label = "Adjust Plot"
                                               ),
                                               shiny::downloadButton(
                                                 outputId = "timeseries_download",
                                                 label = "Download Plot",
                                                 style = "width: 100%;"
                                               )
                                             )
                                           ),
                                           column(
                                             width = 9,
                                             style = "height: 75vh;", 
                                             shiny::plotOutput("timeseries_plot", height = "600px")
                                           )
                                         ))
                      )
                    )
                  )
        )
      ),
     
      shiny::navbarMenu(
        "Documentation",
        "How to use",
        shiny::tabPanel("climr App",
                        tags$iframe(src = "documentation/_book/index.html",
                                    width = "100%", frameborder = "0", height = "900px")
                        ),
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
    source("server/getdata_server.R", local = TRUE)
    source("server/visualization_server.R", local = TRUE)
    
    getdata_server(input, output, session)
    visualization_server(input, output, session)
    
  }
)

