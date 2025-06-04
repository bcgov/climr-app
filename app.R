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
  source("scripts/utils.R", local = TRUE)
})

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
  default_draw_tool() |>
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
            style = "height: 84vh; overflow-y: auto; overflow-x: auto;", 
            
            # create the link!!!
            shiny::div(
              style = "text-align: center;",
              shiny::actionLink(
                inputId = "tutorial",
                label = "Click here for a tutorial",
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

    # initialize sg_dt as reactive
    sg_dt <- reactiveValues(dt = data.table::data.table(
      id = integer(),
      lat = character(),
      long = character(),
      wkt = character(),
      group = character(),
      source = character(),
      datapath = character()
    ),
      filtered_dt = NULL
    )
    
    # reactive state for raster preview
    show_ui <<- reactiveVal(TRUE)
    
    # ---- Modal input storage
    output$climr <- leaflet::renderLeaflet(l)
    
    downscale_default <- list(
      downscale_which_refmap = "refmap_climr",
      downscale_obs_periods_checkbox = "1961_1990",
      downscale_obs_periods = "NULL",
      downscale_obs_years_checkbox = FALSE,
      downscale_obs_years = NULL,
      downscale_obs_ts_dataset = "NULL",
      downscale_sim_recommended = FALSE,
      downscale_gcms = "NULL",
      downscale_ssps = "NULL",
      downscale_gcm_periods = "NULL",
      downscale_gcm_ssp_years = NULL,
      downscale_gcm_hist_years = NULL,
      downscale_gcm_years_checkbox = FALSE,
      downscale_gcm_years = c(1951:2100),
      downscale_ensemble_mean = TRUE,
      downscale_max_run = 0,
      downscale_run_nm = "NULL",
      downscale_extra_vars = downscale_core_vars,
      downscale_extra_vars_sets = "NULL",
      downscale_custom_elements = NULL,
      downscale_custom_time_periods = NULL,
      downscale_core_ppt_lr = FALSE,
      downscale_return_refperiod = TRUE
    )
    
    vstore <- reactiveValues(
      tifsource = names(climr_tif) |> head(1),
      time = NULL,
      element = NULL,
      climatevar = "NONE",
      downscale_which_refmap = downscale_default[["downscale_which_refmap"]],
      downscale_obs_periods_checkbox = downscale_default[["downscale_obs_periods_checkbox"]],
      downscale_obs_periods = downscale_default[["downscale_obs_periods"]],
      downscale_obs_years_checkbox = downscale_default[["downscale_obs_years_checkbox"]],
      downscale_obs_years = downscale_default[["downscale_obs_years"]],
      downscale_obs_ts_dataset = downscale_default[["downscale_obs_ts_dataset"]],
      downscale_sim_recommended = downscale_default[["downscale_sim_recommended"]],
      downscale_gcms = downscale_default[["downscale_gcms"]],
      downscale_ssps = downscale_default[["downscale_ssps"]],
      downscale_gcm_periods = downscale_default[["downscale_gcm_periods"]],
      downscale_gcm_ssp_years = downscale_default[["downscale_gcm_ssp_years"]],
      downscale_gcm_hist_years = downscale_default[["downscale_gcm_hist_years"]],
      downscale_gcm_years_checkbox = downscale_default[["downscale_gcm_years_checkbox"]],
      downscale_gcm_years = downscale_default[["downscale_gcm_years"]],
      downscale_ensemble_mean = downscale_default[["downscale_ensemble_mean"]],
      downscale_max_run = downscale_default[["downscale_max_run"]],
      downscale_run_nm = downscale_default[["downscale_run_nm"]],
      downscale_extra_vars_sets = downscale_default[["downscale_extra_vars_sets"]],
      downscale_custom_elements = downscale_default[["downscale_custom_elements"]],
      downscale_custom_time_periods = downscale_default[["downscale_custom_time_periods"]],
      downscale_extra_vars = downscale_default[["downscale_extra_vars"]],
      downscale_core_ppt_lr = downscale_default[["downscale_core_ppt_lr"]],
      downscale_return_refperiod = downscale_default[["downscale_return_refperiod"]],
      downscale_output = "csv",
      downscale_resolution = 2500,
      vscale = "none",
      processing = FALSE,
      ds_ras_elements = NULL,
      ds_ras_time_periods = NULL,
      ds_ras_obs_sim = NULL,
      ds_ras_obs_periods = NULL,
      ds_ras_gcms = NULL,
      ds_ras_ssps = NULL,
      ds_ras_run_choices = NULL,
      ds_ras_run = NULL,
      ds_ras_gcm_periods = NULL,
      calculate_diff = FALSE,
      log_transform_raster = TRUE,
      downscale_raster_preview = NULL
    )
    
    # ---- Geometry
    source("scripts/geometry.R", local = TRUE)
    sg <- session_geometry(sg_dt)
    
    # ---- Map events
    
    # add map points and drawing map shapes logic
    shiny::observeEvent(input$climr_draw_start, {
      if (shiny::in_devmode()) cat("Event: climr_draw_start", sep = "\n")
      sg$add_point_enabled(FALSE)
      updateActionButton(session = getDefaultReactiveDomain(),
                         "downscale_parameters", disabled = FALSE)
      updateActionButton(session = getDefaultReactiveDomain(),
                         "generate_results", disabled = FALSE)
    })
    shiny::observeEvent(input$climr_draw_stop, {
      if (shiny::in_devmode()) cat("Event: climr_draw_stop", sep = "\n")
      sg$add_point_enabled(TRUE)
    })
    shiny::observeEvent(input$climr_draw_new_feature, {
      if (shiny::in_devmode()) cat("Event: climr_draw_new_feature", sep = "\n")
      sg$add_draw_poly(input$climr_draw_new_feature)
      bslib::accordion_panel_open("acc_methods", "acc_method1")
    })
    shiny::observeEvent(input$climr_click, {
      if (shiny::in_devmode()) cat("Event: climr_click", sep = "\n")
      sg$add_point(input$climr_click$lat, input$climr_click$lng)
      updateActionButton(session = getDefaultReactiveDomain(),
                         "downscale_parameters", disabled = FALSE)
      updateActionButton(session = getDefaultReactiveDomain(),
                         "generate_results", disabled = FALSE)
      bslib::accordion_panel_open("acc_methods", "acc_method1")
    })
    
    # upload a file
    shiny::observeEvent(input$upload, {
      if (shiny::in_devmode()) cat("Event: upload", sep = "\n")
      sg$add_file(input$upload)
      updateActionButton(session = getDefaultReactiveDomain(),
                         "downscale_parameters", disabled = FALSE)
      updateActionButton(session = getDefaultReactiveDomain(),
                         "generate_results", disabled = FALSE)
    })
    
    # pop-up remove button for map points
    shiny::observeEvent(input$sg_remove, {
      if (shiny::in_devmode()) cat("Event: sg_remove", sep = "\n")
      sg$rm(input$sg_remove)
      if (nrow(sg_dt$dt) < 1) {
        updateActionButton(session = getDefaultReactiveDomain(),
                          "downscale_parameters", disabled = TRUE)
        updateActionButton(session = getDefaultReactiveDomain(),
                           "generate_results", disabled = TRUE)
      }
    })
    
    # ---- Data table events
    
    # delete a map point via data table
    shiny::observeEvent(input$delete_button, {
      if (shiny::in_devmode()) cat("Event: sg_remove", sep = "\n")
      row_num <- input$geom_dt_rows_selected
      point_id <- sg_dt$filtered_dt[row_num,1] 
      if (length(point_id) != 0) {
        sg$rm(point_id)
        if (nrow(sg_dt$dt) < 1) {
        updateActionButton(session = getDefaultReactiveDomain(),
                          "downscale_parameters", disabled = TRUE)
        updateActionButton(session = getDefaultReactiveDomain(),
                          "generate_results", disabled = TRUE)
      }
      } else {
        showModal(
          modalDialog(
            title = "Warning",
            paste("Please select row(s)." ),
            easyClose = TRUE
          )
        )
      } 
    })
    
    # clear all selections (map and file) logic
    shiny::observeEvent(input$clear_selections, {
      sg$clear_all()
    })

    sn <- \(j) setNames(j,j)
    
    # ---- Downscale events
    downscale_modal <- function() {
      shiny::showModal(
        shiny::modalDialog(
          title = "Downscale Parameters", size = "l", fade = FALSE, class = "modal-dialog-scrollable",
          
          # Reference map selection
          shiny::div(
            shiny::radioButtons(
              inputId = "downscale_which_refmap",
              label = h5("Choose reference map:", 
                         prompter::add_prompt(
                           tooltipsIcon,
                           message = HTML(paste("Which map of 1961-1990 climatological normals to use as the high-resolution reference climate map for downscaling.")),
                           position = "top",
                           size = "large",
                           shadow = FALSE
                         )
              ),
              # choices = c(local({z <- climr::list_refmaps(); substr(z, 8L, z |> nchar()) |> tools::toTitleCase() |> setNames(object = z, nm = _)})),
              choices = c("climr" = "refmap_climr", "ClimateNA" = "refmap_climatena"),
              selected = vstore[["downscale_which_refmap"]],
              inline = TRUE,
              width = "100%",
            )
          ),
          br(),
          
          accordion(
            open = FALSE,
            
            # Observed climate data parameters
            accordion_panel(
              title = h5("Observed Climate Data"),
              value = "acc_observed",
              
              shiny::div(
                shiny::checkboxGroupInput(
                  inputId = "downscale_obs_periods_checkbox",
                  label = h5("Choose observed periods:",
                             prompter::add_prompt(
                               tooltipsIcon,
                               message = HTML(paste("Historical period for observed climate data, averaged over this period.")),
                               position = "top",
                               size = "large",
                               shadow = FALSE
                             )
                  ),
                  inline = TRUE,
                  width = "100%",
                  choices = c("1961_1990", climr::list_obs_periods() |> sn()),
                  selected = vstore[["downscale_obs_periods_checkbox"]]
                )
              ),
              br(),
              shiny::div(
                shiny::checkboxInput(
                  inputId = "observed_years_checkbox",
                  label = tags$span("Specify Observed Years", style = "font-size: 0.85em; font-weight: bold;"),
                  value = vstore[["downscale_obs_years_checkbox"]],
                  width = "100%"
                )
              ),
              shiny::conditionalPanel(
                condition = "input.observed_years_checkbox == true",
                shiny::div(
                  shiny::sliderInput(
                    inputId = "downscale_obs_years",
                    label = h5("Choose observed years range:",
                               prompter::add_prompt(
                                 tooltipsIcon,
                                 message = HTML(paste("Choose years to obtain individual years or time series of observational climate data.")),
                                 position = "top",
                                 size = "large",
                                 shadow = FALSE
                               )
                    ),
                    min = min(climr::list_obs_years()),
                    max = max(climr::list_obs_years()),
                    value = c(1951,2024),
                    width = "100%",
                    step = 1,
                    sep = ""
                  ),
                  shiny::radioButtons(
                    inputId = "downscale_obs_ts_dataset",
                    label = h5("Choose observation time-series data:",
                               prompter::add_prompt(
                                 tooltipsIcon,
                                 message = HTML(paste("Dataset for observational time series data. ClimateNA gridded time series, CRU/GPCC for CRU TS (temperature) and GPCC (precipitation),")),
                                 position = "top",
                                 size = "large",
                                 shadow = FALSE
                               )
                    ),
                    width = "100%",
                    selected = vstore[["downscale_obs_ts_dataset"]],
                    choices = c("ClimateNA" = "climatena", "Climatic Research Unit / Global Precipitation Climatology Centre" = "cru.gpcc")
                  )
                )
              )
            ),
            
            # Simulated climate data parameters
            accordion_panel(
              title = h5("Simulated Climate Data"),
              value = "acc_simulated",
              
              shiny::div(
                shiny::checkboxInput(
                  inputId = "sim_data_default",
                  label = "Use recommended default settings for Simulated Climate Data",
                  value = vstore[["downscale_sim_recommended"]],
                  width = "100%"
                ),
                shiny::checkboxGroupInput(
                  inputId = "downscale_gcms",
                  label = h5("Choose Global Climate Model (GCM):",
                             prompter::add_prompt(
                               tooltipsIcon,
                               message = HTML(paste("Global climate models to downscale. Select multiple GCMs for ensemble outputs.")),
                               position = "top",
                               size = "large",
                               shadow = FALSE
                             )
                  ),
                  width = "100%",
                  inline = TRUE,
                  choices = climr::list_gcms() |> sn(),
                  selected = vstore[["downscale_gcms"]]
                )
              ),
              shiny::div(
                shiny::checkboxGroupInput(
                  inputId = "downscale_ssps",
                  label = h5("Choose Shared Socio-economic Pathways (SSP) scenarios:",
                             prompter::add_prompt(
                               tooltipsIcon,
                               message = HTML(paste("SSP scenarios pairing shared socioeconomic pathways with representative concentration pathways (only necessary if choosing periods/years past 2014).")),
                               position = "top",
                               size = "large",
                               shadow = FALSE
                             )
                  ),
                  width = "100%",
                  inline = TRUE,
                  choices = climr::list_ssps() |> sn(),
                  selected = vstore[["downscale_ssps"]]
                )
              ),
              shiny::div(
                shiny::checkboxGroupInput(
                  inputId = "downscale_gcm_periods",
                  label = h5("Choose GCM periods:",
                             prompter::add_prompt(
                               tooltipsIcon,
                               message = HTML(paste("20-year reference periods for GCM simulations.")),
                               position = "top",
                               size = "large",
                               shadow = FALSE
                             )
                  ),
                  width = "100%",
                  inline = TRUE,
                  choices = c(climr::list_gcm_periods() |> sn()),
                  selected = vstore[["downscale_gcm_periods"]]
                )
              ),
              br(),
              shiny::div(
                shiny::checkboxInput(
                  inputId = "gcm_years_checkbox",
                  label = tags$span("Specify GCM Years", style = "font-size: 0.85em; font-weight: bold;"),
                  value = vstore[["downscale_gcm_years_checkbox"]],
                  width = "100%"
                )
              ),
              shiny::conditionalPanel(
                condition = "input.gcm_years_checkbox == true",
                shiny::div(
                  shiny::sliderInput(
                    inputId = "downscale_gcm_years",
                    label = h5("Choose GCM years:",
                               prompter::add_prompt(
                                 tooltipsIcon,
                                 message = HTML(paste("Choose time series years for GCM simulations of the historical scenario and future SSP scenarios.")),
                                 position = "top",
                                 size = "large",
                                 shadow = FALSE
                               )
                    ),
                    width = "100%",
                    min = min(climr::list_gcm_hist_years()),
                    max = max(climr::list_gcm_ssp_years()),
                    value = c(1951, 2100),
                    step = 1,
                    sep = ""
                  ),
                )
              ),
              
              shiny::div(
                shiny::checkboxInput(
                  inputId = "downscale_ensemble_mean",
                  label = tags$span("Use ensemble mean", style = "font-size: 0.85em; font-weight: bold;",
                                    prompter::add_prompt(
                                      tooltipsIcon,
                                      message = HTML(paste("Something helpful about ensemble mean.")),
                                      position = "top",
                                      size = "large",
                                      shadow = FALSE
                                    )
                  ),
                  value = vstore[["downscale_ensemble_mean"]],
                  width = "100%"
                )
              ),
              shiny::div(
                shiny::numericInput(
                  inputId = "downscale_max_run",
                  label = h5("Choose maximum number of model runs:",
                             prompter::add_prompt(
                               tooltipsIcon,
                               message = HTML(paste("More helpful things about model runs... Make sure to mention that 0 defaults to using ensemble mean.")),
                               position = "top",
                               size = "large",
                               shadow = FALSE
                             )
                  ),
                  value = vstore[["downscale_max_run"]],
                  width = "100%",
                  min = 0,
                  max = 10,
                  step = 1
                )
              ),
              # # leave out for now -- too technical for app
              # shiny::div(
              #   title = "Names of specific runs to return instead of using max_run. Overrides max_run if specified.",
              #   shiny::selectInput(
              #     inputId = "downscale_run_nm",
              #     label = "Name of specified runs",
              #     width = "100%",
              #     choices = list("Options" = {
              #       gcms <- vstore[["downscale_gcms"]]
              #       ssps <- vstore[["downscale_ssps"]]
              #       if (!length(gcms) && !length(ssps)) {
              #         c()
              #       } else if (length(gcms) && !length(ssps)) {
              #         climr::list_runs_historic(gcm = gcms) |> sn()
              #       } else if (length(gcms) && length(ssps)) {
              #         climr::list_runs_ssp(gcm = gcms, ssp = ssps) |> sn()
              #       }
              #     }, "Remove all" = c("null" = "NULL")),
              #     multiple = TRUE,
              #     selected = vstore[["downscale_run_nm"]]
              #   )
              # ),
            )
          ),
          br(),
          
          shiny::div(
            shiny::checkboxGroupInput(
              inputId = "downscale_extra_vars_sets",
              label = h5("Choose extra climate variables:",
                         prompter::add_prompt(
                           tooltipsIcon,
                           message = HTML(paste("Extra climate variables to compute. Select a set which contains all variables of that category, and/or create a custom set. Defaults to monthly PPT, Tmax, Tmin if not specified.")),
                           position = "top",
                           size = "large",
                           shadow = FALSE
                         )
              ),
              width = "100%",
              choices = c("Monthly", "Seasonal", "Annual", "Custom"),
              selected = vstore[["downscale_extra_vars_sets"]],
              inline = TRUE
              ),
            shiny::conditionalPanel(
              condition = "input.downscale_extra_vars_sets && input.downscale_extra_vars_sets.includes('Custom')",
              shiny::div(
                shiny::checkboxGroupInput(
                  inputId = "downscale_custom_elements",
                  label = h5("Choose elements:"),
                  width = "100%",
                  inline = TRUE,
                  choices = unique(climr::variables %>% pull(Code_Element)),
                  selected = vstore[["downscale_custom_elements"]]
                ),
                shiny::checkboxGroupInput(
                  inputId = "downscale_custom_time_periods",
                  label = h5("Choose time periods:"),
                  width = "100%",
                  inline = TRUE,
                  choices = unique(climr::variables %>% pull(Time)), ## BUG - some annuals are showing up as ANY ##
                  selected = vstore[["downscale_custom_time_periods"]]
                )
              )
            )
          ),
          br(),
          
          shiny::div(
            shiny::checkboxInput(
              inputId = "downscale_core_ppt_lr",
              label = tags$span("Apply elevation adjustment to precipitation values during downscaling", style = "font-size: 0.85em; font-weight: bold;",
                          prompter::add_prompt(
                            tooltipsIcon,
                            message = HTML(paste("Elevation adjustments are cool, but why??")),
                            position = "top",
                            size = "large",
                            shadow = FALSE
                          )
              ),
              value = vstore[["downscale_core_ppt_lr"]],
              width = "100%"
            )
          ),
          footer = shiny::tagList(
            shiny::actionButton(
              inputId = "downscale_reset",
              label = "Reset"
            ),           
            shiny::actionButton(
              inputId = "downscale_apply",
              label = "Apply",
              style = "background-color:#1d8f0e; color: #FFF",
              icon = icon("check")
            ), 
          ),
          easyClose = TRUE
        )
      )
    }
    shiny::observeEvent(input$sim_data_default, {
      if (shiny::in_devmode()) cat("Event: downscale_sim_recommended", sep = "\n")
      vstore[["downscale_sim_recommended"]] <- input$sim_data_default
    })
    shiny::observe(
      if (vstore[["downscale_sim_recommended"]] == TRUE) {
        # GCMs
        vstore[["downscale_gcms"]] <- climr::list_gcms()[c(1,4:7,10:12)]
        shiny::updateCheckboxGroupInput(
          inputId = "downscale_gcms",
          choices = climr::list_gcms() |> sn(),
          selected = vstore[["downscale_gcms"]],
          inline = TRUE
        )
        
        # SSPs
        vstore[["downscale_ssps"]] <- climr::list_ssps()[c(1:3)]
        shiny::updateCheckboxGroupInput(
          inputId = "downscale_ssps",
          choices = climr::list_ssps() |> sn(),
          selected = vstore[["downscale_ssps"]],
          inline = TRUE
        )
        
        # GCM periods
        vstore[["downscale_gcm_periods"]] <- climr::list_gcm_periods()[c(1:5)]
        shiny::updateCheckboxGroupInput(
          inputId = "downscale_gcm_periods",
          choices = c(climr::list_gcm_periods() |> sn()),
          selected = vstore[["downscale_gcm_periods"]],
          inline = TRUE
        )
      }
    )
    
    shiny::observeEvent(input$downscale_parameters, {
      if (shiny::in_devmode()) cat("Event: downscale_parameters", sep = "\n")
      temp_dt <- sg_dt$dt
      
      if (!is.null(temp_dt) && nrow(temp_dt) > 0) {
        sources <- unique(na.omit(temp_dt$source))
        
        # ensure all data sources are the same before opening Downscale Parameters
        if (length(unique((sources))) == 1) {
          downscale_modal()
        } else {
          showModal(
            modalDialog(
              title = "Warning",
              paste("Please ensure input is points OR area-of-interest OR file input."),
              easyClose = TRUE
            )
          )
        }
      } else {
        showModal(
          modalDialog(
            title = "Warning",
            paste("There is no data to downscale."),
            easyClose = TRUE
          )
        )
      }
    
    })
    
    # applies all user specified downscale parameters
    shiny::observeEvent(input$downscale_apply, {
      
      ## refmap ##
      vstore[["downscale_which_refmap"]] <- input$downscale_which_refmap
      
      ## observed periods ##
      vstore[["downscale_obs_periods_checkbox"]] <- input$downscale_obs_periods_checkbox
      if ("1961_1990" %in% vstore[["downscale_obs_periods_checkbox"]]) {
        vstore[["downscale_return_refperiod"]] <- TRUE
      } 
      vstore[["downscale_obs_periods"]] <- input$downscale_obs_periods_checkbox[input$downscale_obs_periods_checkbox != "1961_1990"]
      
      ## observed years ##
      vstore[["downscale_obs_years_checkbox"]] <- input$observed_years_checkbox
      if (vstore[["downscale_obs_years_checkbox"]] == TRUE) {
        date_range <- c(min(input$downscale_obs_years):max(input$downscale_obs_years))
        vstore[["downscale_obs_years"]] <- date_range
      }
      
      ## time series dataset ##
      vstore[["downscale_obs_ts_dataset"]] <- input$downscale_obs_ts_dataset
      
      ## GCMs ##
      vstore[["downscale_gcms"]] <- input$downscale_gcms
      
      ## SSPs ##
      vstore[["downscale_ssps"]] <- input$downscale_ssps
      
      ## GCM periods ##
      vstore[["downscale_gcm_periods"]] <- input$downscale_gcm_periods
      
      ## GCM years ##
      vstore[["downscale_gcm_years_checkbox"]] <- input$gcm_years_checkbox
      if (vstore[["downscale_gcm_years_checkbox"]] == TRUE) {
        # add selected range
        date_range <- (min(input$downscale_gcm_years):max(input$downscale_gcm_years))
        if (2015 %in% date_range & (min(date_range) != 2015)) {
          hist_range <- (min(input$downscale_gcm_years):2014)
          ssp_range <- (2015:max(input$downscale_gcm_years))
        } else if (min(date_range) >= 2015) {
          hist_range <- "NULL"
          ssp_range <- (min(input$downscale_gcm_years):max(input$downscale_gcm_years))
        } else {
          hist_range <- (min(input$downscale_gcm_years):max(input$downscale_gcm_years))
          ssp_range <- NULL
        }
        vstore[["downscale_gcm_years"]] <- input$downscale_gcm_years
        vstore[["downscale_gcm_hist_years"]] <- hist_range
        vstore[["downscale_gcm_ssp_years"]] <- ssp_range
      }
      
      ## ensemble mean / max model runs ##
      vstore[["downscale_ensemble_mean"]] <- as.logical(input$downscale_ensemble_mean)
      vstore[["downscale_max_run"]] <- input$downscale_max_run
      
      ## extra climate variables ##
      # handle sets
      extra_var_handler()
      
      # handle custom
      vstore[["downscale_custom_elements"]] <- input$downscale_custom_elements
      vstore[["downscale_custom_time_periods"]] <- input$downscale_custom_time_periods
      
      ## elev adjustment ##
      vstore[["downscale_core_ppt_lr"]] <- input$downscale_core_ppt_lr
      
      # ensure there are valid element/time period matches selected
      compatible_periods <- climr::variables[Code_Element %in% vstore[["downscale_custom_elements"]] & Time %in% vstore[["downscale_custom_time_periods"]]]
      
      # collect simulated inputs
      selections <- list(input$downscale_gcms, input$downscale_ssps, input$downscale_gcm_periods)
      lengths <- sapply(selections, length)
      
      if (nrow(compatible_periods) == 0 && !is.null(input$downscale_custom_elements) && !is.null(input$downscale_custom_time_periods)) {
        showModal(
          modalDialog(
            title = "Warning",
            paste("Please select valid time period(s) for selected variable(s)." ),
            easyClose = TRUE
          )
        )
      } else if (!(all(lengths == 0) || all(lengths > 0))) {
        showModal(
          modalDialog(
            title = "Warning",
            paste("Please select a GCM AND an SSP AND a GCM period." ),
            easyClose = TRUE
          )
        )
      } else {
        removeModal()
      }
    })
    
    update_vstore_and_notify <- function(vstore_key, input_value, msg_format) {
      vpl <- 30
      current_value <- vstore[[vstore_key]]
      if ("NULL" %in% input_value) {
        if (length(input_value) > 1) {
          if ("NULL" %in% current_value) {
            shiny::updateSelectInput(inputId = vstore_key, selected = setdiff(input_value, "NULL"))
          } else {
            shiny::updateSelectInput(inputId = vstore_key, selected = "NULL")
          }
          return()
        }
      }

      additions <- setdiff(input_value, current_value)
      deletions <- setdiff(current_value, input_value)
      vstore[[vstore_key]] <- input_value
      if (length(additions) > 0) {
        diff_value <- substr(paste(additions, collapse = ", "), 1, vpl)
        shiny::showNotification(
          sprintf("%s added [%s]", msg_format, diff_value),
          duration = 2
        )
      } else if (length(deletions) > 0) {
        diff_value <- substr(paste(deletions, collapse = ", "), 1, vpl)
        shiny::showNotification(
          sprintf("%s removed [%s]", msg_format, diff_value),
          duration = 2
        )
      }
    }
    
    remove_from_vstore <- function(vstore_key, vars, label) {
      current_value <- vstore[[vstore_key]]
      updated_value <- setdiff(current_value, vars)
      deletions <- setdiff(updated_value, current_value)
      vstore[[vstore_key]] <- updated_value
      
      shiny::showNotification(
        sprintf(label),
        duration = 2
      )
    }
    
    # handler for extra climate variable sets
    extra_var_handler <- function() {
      # add new variables
      vstore[["downscale_extra_vars_sets"]] <- input$downscale_extra_vars_sets
      
      # remove core vars if sets selected
      if (!is.null(input$downscale_extra_vars_sets)) {
        vstore[["downscale_extra_vars"]] <- NULL
      }

      # handle added variables
      if ("Monthly" %in% vstore[["downscale_extra_vars_sets"]]) {
        monthly_vars <- climr::variables %>% filter(Category == "Monthly") %>% pull(Code)
        update_vstore_and_notify("downscale_extra_vars", monthly_vars, "Monthly vars")
      }
      if ("Seasonal" %in% vstore[["downscale_extra_vars_sets"]]) {
        seasonal_vars <- climr::variables %>% filter(Category == "Seasonal") %>% pull(Code)
        update_vstore_and_notify("downscale_extra_vars", seasonal_vars, "Seasonal vars")
      }
      if ("Annual" %in% vstore[["downscale_extra_vars_sets"]]) {
        annual_vars <- climr::variables %>% filter(Category == "Annual") %>% pull(Code)
        update_vstore_and_notify("downscale_extra_vars", annual_vars, "Annual vars")
      }
    }

    # reset
    shiny::observeEvent(input$downscale_reset, {
      if (shiny::in_devmode()) cat("Event: downscale_reset", sep = "\n")
      shiny::showModal(
        shiny::modalDialog(
          title = "Confirm Reset",
          "Are you sure you want to reset the downscale preferences to default values?",
          footer = tagList(
            shiny::actionButton("confirm_reset_yes", "Yes", class = "btn btn-danger"),
            shiny::actionButton("confirm_reset_no", "No")
          ),
          easyClose = TRUE
        )
      )
    })

    shiny::observeEvent(input$confirm_reset_yes, {
      if (shiny::in_devmode()) cat("Event: confirm_reset_yes", sep = "\n")
      lapply(names(downscale_default), \(x) {
        vstore[[x]] <- downscale_default[[x]]
      })
      downscale_modal()
    })
    
    shiny::observeEvent(input$confirm_reset_no, {
      if (shiny::in_devmode()) cat("Event: confirm_reset_no", sep = "\n")
      downscale_modal()
    })
    
    # update_run_nm_select <- function() {
    #   gcms <- vstore[["downscale_gcms"]]
    #   ssps <- vstore[["downscale_ssps"]]
    #   if (!length(gcms) && !length(ssps)) {
    #     opt_choices <- c()
    #   } else if (length(gcms) && !length(ssps)) {
    #     opt_choices <- climr::list_runs_historic(gcm = gcms) |> sn()
    #   } else if (length(gcms) && length(ssps)) {
    #     opt_choices <- climr::list_runs_ssp(gcm = gcms, ssp = ssps) |> sn()
    #   }
    #   choices <- list("Options" = opt_choices, "Remove all" = c("null" = "NULL"))
    #   if (all(vstore[["downscale_run_nm"]] %in% unlist(choices))) {
    #     select <- vstore[["downscale_run_nm"]]
    #   } else {
    #     select <- NULL
    #   }
    #   shiny::updateSelectInput(inputId = "downscale_run_nm", choices = choices, selected = select)
    # }
    
    shiny::observeEvent(input$generate_results, {
      if (shiny::in_devmode()) cat("Event: generate_results", sep = "\n")
      
      # check that it is possible to downscale the data 
      temp_dt <- sg_dt$dt
      
      if (!is.null(temp_dt) && nrow(temp_dt) > 0) {
        sources <- unique(na.omit(temp_dt$source))
        
        # ensure all data sources are the same before opening Downscale Launch window
        if (length(unique((sources))) == 1) {
          vstore[["processing"]] <- FALSE
          output$downscale_points_count_estimate <- shiny::renderUI({
            pce <- sg$process_count(vstore[["downscale_resolution"]])
            bslib::card(
              full_screen = FALSE,
              height = "auto",
              bslib::card_header("Load estimation"),
              class = "bg-warning",
              fill = TRUE,
              bslib::card_body(
                shiny::tags$span(
                  if (pce$marker_count > 0) "[%s] points from [%s] markers geometries." |> sprintf(format(pce$marker_count, big.mark = ","), format(pce$marker, big.mark = ",")),
                  shiny::br(),
                  if (pce$shape_count > 0) "[%s] points from [%s] shapes geometries." |> sprintf(format(pce$shape_count, big.mark = ","), format(pce$shape, big.mark = ","))
                )
              )
            )
          })
          shiny::showModal(
            shiny::modalDialog(
              title = "Preferences for Downscale Processing", size = "l",
              shiny::uiOutput("downscale_output_buttons"),
              shiny::uiOutput("downscale_points_count_estimate"),
              shiny::actionButton(
                inputId = "downscale_process_launch",
                label = "Launch Downscale Process",
                class = "btn btn-primary btn-lg",
                icon = shiny::icon("play"),
                width = "100%"
              ),
              
              # preview for csv results
              DT::DTOutput("preview_table", width = "100%"),
              br(),
              
              shiny::downloadButton(
                outputId = "downscale_download",
                label = "Download Downscaled Data",
                style = "width: 100%;"
              ),
            )
          )
        } else {
          showModal(
            modalDialog(
              title = "Warning",
              paste("Please ensure input is points OR area-of-interest OR file input."),
              easyClose = TRUE
            )
          )
        }
      } else {
        showModal(
          modalDialog(
            title = "Warning",
            paste("There is no data to downscale."),
            easyClose = TRUE
          )
        )
      }
    })
    shiny::observeEvent(input$downscale_output, {
      if (shiny::in_devmode()) cat("Event: downscale_output", sep = "\n")
      vstore[["downscale_output"]] <- input$downscale_output
      if (input$downscale_output == "csv") {
        show_ui(FALSE)
      }
    })
    shiny::observeEvent(input$downscale_resolution, {
      if (shiny::in_devmode()) cat("Event: downscale_resolution", sep = "\n")
      vstore[["downscale_resolution"]] <- input$downscale_resolution
    })
    shiny::observeEvent(input$downscale_process_launch, {
      if (shiny::in_devmode()) cat("Event: downscale_process_launch", sep = "\n")
      if (vstore[["processing"]]) return()
      
      # concatenate custom extra climate variables
      if (!is.null(vstore[["downscale_custom_elements"]]) & !is.null(vstore[["downscale_custom_time_periods"]])) {
        codes <- climr::variables[Code_Element %in% vstore[["downscale_custom_elements"]] & Time %in% vstore[["downscale_custom_time_periods"]], Code]
        vstore[["downscale_extra_vars"]] <- unique(c(vstore[["downscale_extra_vars"]], codes))
      }
      
      # set obs and GCM years, TS dataset to null if not selected
      if (vstore[["downscale_obs_years_checkbox"]] == FALSE) {
        vstore[["downscale_obs_years"]] <- NULL
        vstore[["downscale_obs_ts_dataset"]] <- NULL
      }
      if (vstore[["downscale_gcm_years_checkbox"]] == FALSE) {
        vstore[["downscale_gcm_hist_years"]] <- NULL
        vstore[["downscale_gcm_ssp_years"]] <- NULL
      }
      
      sg$process()
      
      show_ui(TRUE)
    })
    shiny::observeEvent(input$ds_ras_elements, {
      if (shiny::in_devmode()) cat("Event: ds_ras_elements", sep = "\n")
      update_vstore_and_notify("ds_ras_elements", input$ds_ras_elements, "Raster element")
    })
    shiny::observeEvent(input$ds_ras_time_periods, {
      if (shiny::in_devmode()) cat("Event: ds_ras_time_periods", sep = "\n")
      update_vstore_and_notify("ds_ras_time_periods", input$ds_ras_time_periods, "Raster time period")
    })
    shiny::observeEvent(input$ds_ras_ref_periods, {
      if (shiny::in_devmode()) cat("Event: ds_ras_ref_periods", sep = "\n")
      update_vstore_and_notify("ds_ras_ref_periods", input$ds_ras_ref_periods, "Raster ref/GCM/SSP period")
    })
    shiny::observeEvent(input$log_scale, {
      if (shiny::in_devmode()) cat("Event: log_transform_raster", sep = "\n")
      update_vstore_and_notify("log_transform_raster", input$log_scale, "Log transform")
    })
    shiny::observeEvent(input$calculate_diff, {
      if (shiny::in_devmode()) cat("Event: calculate_diff", sep = "\n")
      update_vstore_and_notify("calculate_diff", input$calculate_diff, "Calculate change")
    })
    shiny::observeEvent(input$preview_raster, {
      if (shiny::in_devmode()) cat("Event: downscale_raster_preview", sep = "\n")
      if (vstore[["downscale_output"]] == "tif") {
        
        # clear previous raster and legend
        if (!is.null(vstore[["downscale_raster_preview"]])) {
          leaflet::removeImage(mp, "rast_layer")
          leaflet::clearControls(mp)
        }
        
        # browser()
        # concatenate raster layer preview
        code <- raster_layers[Code_Element == vstore[["ds_ras_elements"]] & Time == vstore[["ds_ras_time_periods"]], Code]
        raster_names <- names(preview_raster)
        
        if (input$ds_ras_obs_sim == "Observed") {
          #browser()
          if (input$ds_ras_obs_periods == "1961_1990") {
            period <- "REFPERIOD"
          } else {
            period <- "OBS"
          }
          time_period <- input$ds_ras_obs_periods
          years <- input$ds_ras_obs_periods
          keywords <- c(code, period, time_period)
          
          layer_match <- raster_names[
            Reduce(`&`, lapply(keywords, function(k) grepl(k, raster_names)))
          ]
        }
        
        if (input$ds_ras_obs_sim == "Simulated") {
          gcm <- input$ds_ras_gcms
          ssp <- input$ds_ras_ssps
          run <- input$ds_ras_run
          time_period <- input$ds_ras_gcm_periods
          years <- input$ds_ras_gcm_periods
          keywords <- c(code, gcm, ssp, run, time_period)
          
          layer_match <- raster_names[
            Reduce(`&`, lapply(keywords, function(k) grepl(k, raster_names)))
          ]
        }
        
        # extract data for legend
        legend_title <- climr::variables[Code == code, Variable] |> tools::toTitleCase()
        if (grepl("\\u00b0C", legend_title) | grepl("\\u00b0c", legend_title)) {
          legend_title <- stringi::stri_unescape_unicode(legend_title)
        }
        units <- paste0(" ", climr::variables[Code == code, Unit])
        if (grepl("\\u00b0C", units)) {
          units <- stringi::stri_unescape_unicode(units)
        }
        if (units == "%") {
          units <- "\\%"
        }
      
        update_vstore_and_notify("downscale_raster_preview", layer_match, "Preview raster")
        
        # to display calculated difference if selected
        if (vstore[["calculate_diff"]]) {
          #browser()
          type <- raster_layers[Code_Element == vstore[["ds_ras_elements"]] & Time == vstore[["ds_ras_time_periods"]], Type]
          keywords <- c("REFPERIOD", code)
          ref_period_raster <- raster_names[
            Reduce(`&`, lapply(keywords, function(k) grepl(k, raster_names)))
          ]
          
          # set palettes
          col_scheme <- if (grepl("PPT", layer_match)) {
            rev(hcl.colors(5,"Blue-Red 3"))
          } else {
            hcl.colors(5,"Blue-Red 3")
          }
          
          # display raster and legend
          if (type == "interval") {
            display_raster <- preview_raster[[layer_match]] - preview_raster[[ref_period_raster]]
            pal <- colorNumeric(
              palette = col_scheme,
              domain = values(display_raster),
              na.color = "transparent"
            )
            
            # update legend title
            # browser()
            legend_title <- glue::glue("Change in {legend_title} from 1961_1990 to {years}")
            
            leaflet::addRasterImage(mp, display_raster, layerId = "rast_layer", colors = pal)
            leaflet::addLegend(mp, pal = pal, values = values(display_raster), title = legend_title, labFormat = labelFormat(suffix = units))
          }
          if (type == "ratio") {
            # browser()
            display_raster <- preview_raster[[layer_match]]/preview_raster[[ref_period_raster]]
            pal <- colorNumeric(
              palette = col_scheme,
              domain = values(display_raster),
              na.color = "transparent"
            )
            
            # update legend title
            legend_title <- glue::glue("Change in {legend_title} from 1961_1990 to {years}")
            
            leaflet::addRasterImage(mp, display_raster, layerId = "rast_layer", colors = pal)
            leaflet::addLegend(mp, pal = pal, values = values(display_raster), title = legend_title, labFormat = labelFormat(suffix = units))
          }
        } else {
          # # extract data for legend
          # legend_title <- climr::variables[Code == code, Variable] |> tools::toTitleCase()
          # if (grepl("\\u00b0C", legend_title) | grepl("\\u00b0c", legend_title)) {
          #   legend_title <- stringi::stri_unescape_unicode(legend_title)
          # }
          # units <- paste0(" ", climr::variables[Code == code, Unit])
          # if (grepl("\\u00b0C", units)) {
          #   units <- stringi::stri_unescape_unicode(units)
          # }
          # if (units == "%") {
          #   units <- "\\%"
          # }
          
          variable_type <- climr::variables[Code == code, Type]
          if (variable_type == "ratio" & vstore[["log_transform_raster"]] == TRUE) {
            # log transform
            raster_layer_values <- log2(preview_raster[[layer_match]] + 1)
          } else {
            raster_layer_values <- preview_raster[[layer_match]]
          }
  
          # set palettes
          #browser()
          col_scheme <- if (grepl("PPT", layer_match)) {
            RColorBrewer::brewer.pal(9, "YlGnBu")
          } else {
            rev(RColorBrewer::brewer.pal(11, "RdYlBu"))
          }
          pal <- colorNumeric(
            palette = col_scheme,
            domain = values(raster_layer_values),
            na.color = "transparent"
          )
  
          # add raster image
          leaflet::addRasterImage(mp, raster_layer_values, layerId = "rast_layer", colors = pal)
  
          # label formatters for legend
          inv_log2_formatter <- labelFormat(
            transform = function(x) round((2^x) - 1),  # inverse of log2(x + 1)
            suffix = units
          )
  
          # add legend
          leaflet::addLegend(mp, pal = pal, values = values(raster_layer_values), title = legend_title, labFormat = if (vstore[["log_transform_raster"]] & variable_type == "ratio") inv_log2_formatter else labelFormat(suffix = units))
          }
        }  
    })
    
    # reactive output for outputs options
    output$downscale_output_buttons <- shiny::renderUI({
      if ("marker" %in% (sg_dt$dt)$group) {
        shiny::radioButtons(
          inputId = "downscale_output",
          label = h5("Choose downscale output format:",
                     prompter::add_prompt(
                       tooltipsIcon,
                       message = HTML(paste("Shapes/rasters can be returned as csv or GeoTIFF. All points are returned in csv.")),
                       position = "top",
                       size = "large",
                       shadow = FALSE
                     )
          ),
          choices = c("Comma Separated Value (csv)" = "csv"),
          inline = TRUE,
          selected = "csv"
        )
      } else if ("shape" %in% (sg_dt$dt)$group) {
        shiny::div(
          shiny::radioButtons(
            inputId = "downscale_output",
            label = h5("Choose downscale output format:",
                       prompter::add_prompt(
                         tooltipsIcon,
                         message = HTML(paste("tif: Shapes/rasters are returned as GeoTIFF. csv: all points are returned in csv.")),
                         position = "top",
                         size = "large",
                         shadow = FALSE
                       )
            ),
            choices = c("Geographic Tag Image File Format (GeoTIFF)" = "tif", "Comma Separated Value (csv)" = "csv"),
            inline = TRUE,
            selected = "tif"
          ),
          shiny::sliderInput(
            inputId = "downscale_resolution",
            label = h5("Choose downscale resolution (m):",
                       prompter::add_prompt(
                         tooltipsIcon,
                         message = HTML(paste("Target resolution for shapes drawn on map or added using file upload. Does not apply to points, raster or csv files.")),
                         position = "top",
                         size = "large",
                         shadow = FALSE
                       )
            ),
            value = vstore[["downscale_resolution"]],
            width = "100%",
            min = 250,
            max = 10000,
            step = 50,
            post = "m",
            ticks = FALSE
          )
        )
      } else {
        NULL
      }
    })
    
  }
)

