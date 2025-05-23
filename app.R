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
    overlayGroups = c("Labels", "WNA BEC", "Climate"),
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
        "ClimR"
      ),
      shiny::tabPanel(
        title = "Map",
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
                label = "Generate results",
                icon = icon("plus-square"),
                style = "width:100%; height:70px; background-color:#003366; color: #FFF",
                disabled = TRUE
              )
            ),
            br(),
            
            strong("Add Sites Using One of the 2 Methods Below:"),
            accordion(
              # need to add help icons to each of the methods
              multiple = FALSE,
              
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
                p("Click on map to add points or draw an area-of-interest using shape tools."),
                DT::DTOutput("geom_dt", width = "100%"),
                shiny::actionButton("delete_button", "Delete Selected", icon("trash-alt")),
                value = "acc1"
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
                    label = "Upload a csv, a raster or a shape file to add geographies"
                  )
                ),
                value = "acc2"
              )
            ),
            br(), br(),
            shiny::actionButton("downscale_parameters", "Choose Downscale Parameters",
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
    
    # ---- Modal input storage
    output$climr <- leaflet::renderLeaflet(l)
    
    downscale_default <- list(
      downscale_which_refmap = "refmap_climr",
      downscale_obs_periods_checkboxes = "2001_2020",
      downscale_obs_periods = "2001_2020",
      downscale_obs_years_radio = "No",
      downscale_obs_years = "NULL",
      downscale_obs_ts_dataset = "NULL",
      downscale_gcms = "NULL",
      downscale_ssps = "NULL",
      downscale_gcm_periods = "NULL",
      downscale_gcm_ssp_years = "NULL",
      downscale_gcm_hist_years = "NULL",
      downscale_gcm_years_radio = "No",
      downscale_gcm_years = "NULL",
      downscale_ensemble_mean = 0, # is default for YES
      downscale_max_run = 0,
      downscale_run_nm = "NULL",
      downscale_extra_vars_packages = "NULL",
      downscale_extra_vars = "NULL",
      downscale_extra_vars_custom_monthly = "NULL",
      downscale_extra_vars_custom_seasonal = "NULL",
      downscale_extra_vars_custom_annual = "NULL",
      downscale_core_ppt_lr = FALSE,
      downscale_return_refperiod = FALSE
    )
    
    vstore <- reactiveValues(
      tifsource = names(climr_tif) |> head(1),
      time = NULL,
      element = NULL,
      climatevar = "NONE",
      downscale_which_refmap = downscale_default[["downscale_which_refmap"]],
      downscale_obs_periods_checkboxes = downscale_default[["downscale_obs_periods_checkboxes"]],
      downscale_obs_periods = downscale_default[["downscale_obs_periods"]],
      downscale_obs_years_radio = downscale_default[["downscale_obs_years_radio"]],
      downscale_obs_years = downscale_default[["downscale_obs_years"]],
      downscale_obs_ts_dataset = downscale_default[["downscale_obs_ts_dataset"]],
      downscale_gcms = downscale_default[["downscale_gcms"]],
      downscale_ssps = downscale_default[["downscale_ssps"]],
      downscale_gcm_periods = downscale_default[["downscale_gcm_periods"]],
      downscale_gcm_ssp_years = downscale_default[["downscale_gcm_ssp_years"]],
      downscale_gcm_hist_years = downscale_default[["downscale_gcm_hist_years"]],
      downscale_gcm_years_radio = downscale_default[["downscale_gcm_years_radio"]],
      downscale_gcm_years = downscale_default[["downscale_gcm_years"]],
      downscale_ensemble_mean = downscale_default[["downscale_ensemble_mean"]],
      downscale_max_run = downscale_default[["downscale_max_run"]],
      downscale_run_nm = downscale_default[["downscale_run_nm"]],
      downscale_extra_vars_packages = downscale_default[["downscale_extra_vars_packages"]],
      downscale_extra_vars = downscale_default[["downscale_extra_vars"]],
      downscale_extra_vars_custom_monthly = downscale_default[["downscale_extra_vars_custom_monthly"]],
      downscale_extra_vars_custom_seasonal = downscale_default[["downscale_extra_vars_custom_seasonal"]],
      downscale_extra_vars_custom_annual = downscale_default[["downscale_extra_vars_custom_annual"]],
      downscale_core_ppt_lr = downscale_default[["downscale_core_ppt_lr"]],
      downscale_return_refperiod = downscale_default[["downscale_return_refperiod"]],
      downscale_output = "csv",
      downscale_resolution = 2500,
      vscale = "none",
      processing = FALSE,
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
    })
    shiny::observeEvent(input$climr_click, {
      if (shiny::in_devmode()) cat("Event: climr_click", sep = "\n")
      sg$add_point(input$climr_click$lat, input$climr_click$lng)
      updateActionButton(session = getDefaultReactiveDomain(),
                         "downscale_parameters", disabled = FALSE)
      updateActionButton(session = getDefaultReactiveDomain(),
                         "generate_results", disabled = FALSE)
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
      updateActionButton(session = getDefaultReactiveDomain(),
                         "downscale_parameters", disabled = TRUE)
      updateActionButton(session = getDefaultReactiveDomain(),
                         "generate_results", disabled = TRUE)
      lapply(names(downscale_default), \(x) {
        vstore[[x]] <- downscale_default[[x]]
      })
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
              label = h5("Choose Reference Map:", 
                         prompter::add_prompt(
                           tooltipsIcon,
                           message = HTML(paste("Which map of 1961-1990 climatological normals to use as the high-resolution reference climate map for downscaling.")),
                           position = "top",
                           size = "large",
                           shadow = FALSE
                         )
              ),
              choices = c(local({z <- climr::list_refmaps(); substr(z, 8L, z |> nchar()) |> tools::toTitleCase() |> setNames(object = z, nm = _)})
              ),
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
                  inputId = "downscale_obs_periods_checkboxes",
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
                  selected = vstore[["downscale_obs_periods_checkboxes"]]
                )
              ),
              shiny::div(
                shiny::radioButtons(
                  inputId = "observed_years_radio",
                  label = h5("Would you like to specify observed years?"),
                  choices = c("Yes", "No"),
                  selected = vstore[["downscale_obs_years_radio"]],
                  width = "100%"
                ),
              ),
              
              # reactive output for selecting Observed Years
              uiOutput("observed_years_radio")
            ),
            
            # Simulated climate data parameters
            accordion_panel(
              title = h5("Simulated Climate Data"),
              value = "acc_simulated",
              
              shiny::div(
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
              shiny::div(
                shiny::radioButtons(
                  inputId = "gcm_years_radio",
                  label = h5("Would you like to specify GCM years?"),
                  choices = c("Yes", "No"),
                  selected = vstore[["downscale_gcm_years_radio"]],
                  width = "100%"
                ),
              ),
              
              # reactive output for selecting GCM Years
              uiOutput("gcm_years_radio"),

              # reactive output for selecting SSPs
              uiOutput("downscale_gcm_years"),
              
              shiny::div(
                shiny::radioButtons(
                  inputId = "downscale_max_run",
                  label = h5("Use ensemble mean for maxinum number of model runs to include?"),
                  width = "100%",
                  choices = c("Yes" = 0, "No" = "NULL"), # THIS MAY CREATE ISSUES WHEN DOWNSCALING
                  inline = TRUE,
                  selected = vstore[["downscale_max_run"]]
                )
              ),
              shiny::div(
                shiny::numericInput(
                  inputId = "downscale_max_run",
                  label = h5("Choose maximum number of model runs:"),
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
              inputId = "downscale_extra_vars_packages",
              label = h5("Choose extra climate variables:",
                         prompter::add_prompt(
                           tooltipsIcon,
                           message = HTML(paste("Extra climate variables to compute. Select a package which contains all variables of that category, and/or create a custom package. Defaults to monthly PPT, Tmax, Tmin if not specified.")),
                           position = "top",
                           size = "large",
                           shadow = FALSE
                         )
              ),
              width = "100%",
              choices = c("Monthly", "Seasonal", "Annual", "Custom"),
              selected = vstore[["downscale_extra_vars_packages"]],
              inline = TRUE
              ),
            uiOutput("downscale_extra_vars_custom")
          ),
          br(),
          
          shiny::div(
            shiny::checkboxInput(
              inputId = "downscale_core_ppt_lr",
              label = "Apply elevation adjustment to precipitation values during downscaling",
              value = vstore[["downscale_core_ppt_lr"]],
              width = "100%"
            )
          ),
          footer = shiny::tagList(
            shiny::actionButton(
              inputId = "downscale_reset",
              label = "Reset",
              class = "btn btn-warning"
            ),           
            shiny::modalButton("Close")
          )
        )
      )
    }
    
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
    
    shiny::observeEvent(input$downscale_which_refmap, {
      if (shiny::in_devmode()) cat("Event: downscale_which_refmap", sep = "\n")
      update_vstore_and_notify("downscale_which_refmap", input$downscale_which_refmap, "Ref map")
    })
    shiny::observeEvent(input$downscale_obs_periods_checkboxes, {
      update_vstore_and_notify("downscale_obs_periods_checkboxes", input$downscale_obs_periods_checkboxes, "Obs periods")
      if (shiny::in_devmode()) cat("Event: downscale_obs_periods", sep = "\n")
      if ("1961_1990" %in% input$downscale_obs_periods_checkboxes) {
        update_vstore_and_notify("downscale_return_refperiod", TRUE, "Return ref period")
      } else {
        update_vstore_and_notify("downscale_return_refperiod", FALSE, "Return ref period")
      }
      update_vstore_and_notify("downscale_obs_periods", input$downscale_obs_periods_checkboxes[input$downscale_obs_periods_checkboxes != "1961_1990"], "Obs periods")
    })
    shiny::observeEvent(input$observed_years_radio,{
      if (shiny::in_devmode()) cat("Event: downscale_obs_years_radio", sep = "\n")
      update_vstore_and_notify("downscale_obs_years_radio", input$observed_years_radio, "Obs radios")
      
      if (input$observed_years_radio == "No") {
        update_vstore_and_notify("downscale_obs_years", "NULL", "Obs years")
      }
    })
    shiny::observeEvent(input$downscale_obs_years, {
        if (shiny::in_devmode()) cat("Event: downscale_obs_years", sep = "\n")
        date_range <- min(input$downscale_obs_years):max(input$downscale_obs_years)
        update_vstore_and_notify("downscale_obs_years", date_range, "Obs years")
    })
    shiny::observe({
      vstore[["downscale_obs_years_radio"]]
      updateSliderInput(session = getDefaultReactiveDomain(),
                        inputId = "downscale_obs_years",
                        value = c(min(vstore[["downscale_obs_years"]]), max(vstore[["downscale_obs_years"]]))
                        )
    })
    shiny::observeEvent(input$downscale_obs_ts_dataset, {
      if (shiny::in_devmode()) cat("Event: downscale_obs_ts_dataset", sep = "\n")
      update_vstore_and_notify("downscale_obs_ts_dataset", input$downscale_obs_ts_dataset, "Obs dataset")
    })
    shiny::observeEvent(input$downscale_gcms, {
      if (shiny::in_devmode()) cat("Event: downscale_gcms", sep = "\n")
      update_vstore_and_notify("downscale_gcms", input$downscale_gcms, "GCMs")
      update_run_nm_select()
    })
    shiny::observeEvent(input$downscale_ssps, {
      if (shiny::in_devmode()) cat("Event: downscale_ssps", sep = "\n")
      update_vstore_and_notify("downscale_ssps", input$downscale_ssps, "SSPs")
      update_run_nm_select()
    })
    shiny::observeEvent(input$downscale_gcm_periods, {
      if (shiny::in_devmode()) cat("Event: downscale_gcm_periods", sep = "\n")
      update_vstore_and_notify("downscale_gcm_periods", input$downscale_gcm_periods, "GCM periods")
    })
    shiny::observeEvent(input$gcm_years_radio, {
      if (shiny::in_devmode()) cat("Event: downscale_gcm_years_radio", sep = "\n")
      update_vstore_and_notify("downscale_gcm_years_radio", input$gcm_years_radio, "GCM radios")
      
      if (input$gcm_years_radio == "No") {
        update_vstore_and_notify("downscale_gcm_years", "NULL", "GCM years")
        update_vstore_and_notify("downscale_gcm_hist_years", "NULL", "GCM hist years")
        update_vstore_and_notify("downscale_gcm_ssp_years", "NULL", "GCM SSP years")
      }
    })
    shiny::observeEvent(input$downscale_gcm_years, {
      if (shiny::in_devmode()) cat("Event: downscale_gcm_years", sep = "\n")
      
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
        update_vstore_and_notify("downscale_gcm_hist_years", hist_range, "GCM hist years")
        update_vstore_and_notify("downscale_gcm_ssp_years", ssp_range, "GCM SSP years")
    })
    shiny::observe({
      vstore[["downscale_gcm_years_radio"]]
      updateSliderInput(session = getDefaultReactiveDomain(),
                        inputId = "downscale_gcm_years",
                        value = c(min(vstore[["downscale_gcm_years"]]), max(vstore[["downscale_gcm_years"]]))
      )
    })
    shiny::observeEvent(input$downscale_max_run, {
      if (shiny::in_devmode()) cat("Event: downscale_max_run", sep = "\n")
      update_vstore_and_notify("downscale_max_run", input$downscale_max_run, "Max run")
    })
    # shiny::observeEvent(input$downscale_run_nm, {
    #   if (shiny::in_devmode()) cat("Event: downscale_run_nm", sep = "\n")
    #   update_vstore_and_notify("downscale_run_nm", input$downscale_run_nm, "Run name")
    # })
    shiny::observeEvent(input$downscale_extra_vars_packages, {
      if (shiny::in_devmode()) cat("Event: downscale_extra_vars", sep = "\n")
      if ("Monthly" %in% input$downscale_extra_vars_packages) {
        update_vstore_and_notify("downscale_extra_vars_packages", input$downscale_extra_vars_packages, "Monthly package")
        update_vstore_and_notify("downscale_extra_vars", downscale_extra_vars$Monthly, "Monthly vars")
      }
      if ("Seasonal" %in% input$downscale_extra_vars_packages) {
        update_vstore_and_notify("downscale_extra_vars_packages", input$downscale_extra_vars_packages, "Seasonal package")
        update_vstore_and_notify("downscale_extra_vars", downscale_extra_vars$Seasonal, "Seasonal vars")
      }
      if ("Annual" %in% input$downscale_extra_vars_packages) {
        update_vstore_and_notify("downscale_extra_vars_packages", input$downscale_extra_vars_packages, "Annual package")
        update_vstore_and_notify("downscale_extra_vars", downscale_extra_vars$Annual, "Annual vars")
      }
      if ("Custom" %in% input$downscale_extra_vars_packages) {
        update_vstore_and_notify("downscale_extra_vars_packages", input$downscale_extra_vars_packages, "Custom package")
      }
    })
    shiny::observeEvent(input$monthly_extra_vars, {
      update_vstore_and_notify("downscale_extra_vars_custom_monthly", input$monthly_extra_vars, "Monthly custom vars")
    })
    shiny::observeEvent(input$seasonal_extra_vars, {
      update_vstore_and_notify("downscale_extra_vars_custom_seasonal", input$seasonal_extra_vars, "Seasonal custom vars")
    })
    shiny::observeEvent(input$annual_extra_vars, {
      update_vstore_and_notify("downscale_extra_vars_custom_annual", input$annual_extra_vars, "Annual custom vars")
    })
    shiny::observeEvent(input$downscale_core_ppt_lr, {
      if (shiny::in_devmode()) cat("Event: downscale_core_ppt_lr", sep = "\n")
      update_vstore_and_notify("downscale_core_ppt_lr", input$downscale_core_ppt_lr, "Core PPT LR")
    })

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
    
    update_run_nm_select <- function() {
      gcms <- vstore[["downscale_gcms"]]
      ssps <- vstore[["downscale_ssps"]]
      if (!length(gcms) && !length(ssps)) {
        opt_choices <- c()
      } else if (length(gcms) && !length(ssps)) {
        opt_choices <- climr::list_runs_historic(gcm = gcms) |> sn()
      } else if (length(gcms) && length(ssps)) {
        opt_choices <- climr::list_runs_ssp(gcm = gcms, ssp = ssps) |> sn()
      }
      choices <- list("Options" = opt_choices, "Remove all" = c("null" = "NULL"))
      if (all(vstore[["downscale_run_nm"]] %in% unlist(choices))) {
        select <- vstore[["downscale_run_nm"]]
      } else {
        select <- NULL
      }
      shiny::updateSelectInput(inputId = "downscale_run_nm", choices = choices, selected = select)
    }
    
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
                  selected = vstore[["downscale_output"]]
                )
              ),
              shiny::div(
                shiny::sliderInput(
                  inputId = "downscale_resolution",
                  label = h5("Downscale Resolution (m)",
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
                  max = 50000,
                  step = 250,
                  post = "m",
                  ticks = FALSE
                )
              ),
              shiny::uiOutput("downscale_points_count_estimate"),
              shiny::actionButton(
                inputId = "downscale_process_launch",
                label = "Launch Downscale Process",
                title = "Trigger a downscale processing run. At the end of the run, the download button on the main control panel will be enabled.",
                class = "btn btn-primary btn-lg",
                icon = shiny::icon("play"),
                width = "100%"
              ),
              br(), br(),
              
              # preview for csv results
              DT::DTOutput("preview_table", width = "100%"),
              
              # options for raster preview
              uiOutput("preview_raster_options", width = "100%"),
              
              shiny::downloadButton(
                outputId = "downscale_download",
                label = "Download Downscaled Data",
                title = "Download downscaled geographies archive",
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
    })
    shiny::observeEvent(input$downscale_resolution, {
      if (shiny::in_devmode()) cat("Event: downscale_resolution", sep = "\n")
      vstore[["downscale_resolution"]] <- input$downscale_resolution
    })
    shiny::observeEvent(input$downscale_process_launch, {
      if (shiny::in_devmode()) cat("Event: downscale_process_launch", sep = "\n")
      if (vstore[["processing"]]) return()
      sg$process()
    })
    shiny::observeEvent(input$ds_ras_prev_options, {
      if (shiny::in_devmode()) cat("Event: downscale_raster_preview", sep = "\n")
      update_vstore_and_notify("downscale_raster_preview", input$ds_ras_prev_options, "Preview raster")
      leaflet::addRasterImage(mp, preview_raster[[input$ds_ras_prev_options]])
    })
    
    # reactive output for selecting Observed Years
    output$observed_years_radio <- renderUI({
      if (input$observed_years_radio == "Yes") {
        if ("NULL" %in% vstore$downscale_obs_years) {
          date_range_preset <- c(min(climr::list_obs_years()):max(climr::list_obs_years()))
          update_vstore_and_notify("downscale_obs_years", date_range_preset, "Obs years")
        }
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
            value = c(min(vstore[["downscale_obs_years"]]), max(vstore[["downscale_obs_years"]])),
            width = "100%",
            sep = ""
          ),
          selected = c(min(vstore[["downscale_obs_years"]]), max(vstore[["downscale_obs_years"]])),
        )
      } else {
        NULL
      }
    })
    
    # reactive output for selecting GCM Years
    output$gcm_years_radio <- renderUI({
      if (input$gcm_years_radio == "Yes") {
        if ("NULL" %in% vstore$downscale_gcm_years) {
          date_range_preset <- c(min(climr::list_gcm_hist_years()):(max(climr::list_gcm_hist_years())-100))
          update_vstore_and_notify("downscale_gcm_years", date_range_preset, "GCM years")
          update_vstore_and_notify("downscale_gcm_hist_years", date_range_preset, "GCM hist years")
          
        }
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
            value = c(min(vstore[["downscale_gcm_years"]]), max(vstore[["downscale_gcm_years"]])),
            step = 1,
            sep = ""
          ),
          selected = c(min(vstore[["downscale_gcm_years"]]), max(vstore[["downscale_gcm_years"]])),
        )
      } else {
        NULL
      }
    })
    
    # reactive output for selecting SSPs
    output$downscale_gcm_years <- renderUI({
      if (any(min(climr::list_gcm_ssp_years()):max(climr::list_gcm_ssp_years()) %in% input$downscale_gcm_years)) {
        shiny::div(
          shiny::checkboxGroupInput(
            inputId = "downscale_ssps",
            label = h5("Choose Shared Socio-economic Pathways (SSP) scenarios:",
                       prompter::add_prompt(
                         tooltipsIcon,
                         message = HTML(paste("SSP scenarios pairing shared socioeconomic pathways with representative concentration pathways (only necessary if choosing years past 2014).")),
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
        )
      } else {
        NULL
      }
    })
    
    # reactive output for selecting a custom package for extra climate variables
    output$downscale_extra_vars_custom <- renderUI({
      if ("Custom" %in% input$downscale_extra_vars_packages) {
        accordion(
          id = "climate_vars_acc",
          accordion_panel(
            title = h5("Monthly Variables"),
            value = "monthly_acc",
            shiny::checkboxGroupInput(
              inputId = "monthly_extra_vars",
              label = h5("Choose monthly variables:"),
              width = "100%",
              inline = TRUE,
              choices = c(downscale_extra_vars$Monthly),
              selected = vstore[["downscale_extra_vars_custom_monthly"]]
            )
          ),
          accordion_panel(
            title = h5("Seasonal Variables"),
            value = "seasonal_acc",
            shiny::checkboxGroupInput(
              inputId = "seasonal_extra_vars",
              label = h5("Choose seasonal variables:"),
              width = "100%",
              inline = TRUE,
              choices = c(downscale_extra_vars$Seasonal),
              selected = vstore[["downscale_extra_vars_custom_seasonal"]]
            )
          ),
          accordion_panel(
            title = h5("Annual Variables"),
            value = "annual_acc",
            shiny::checkboxGroupInput(
              inputId = "annual_extra_vars",
              label = h5("Choose annual variables:"),
              width = "100%",
              inline = TRUE,
              choices = c(downscale_extra_vars$Annual),
              selected = vstore[["downscale_extra_vars_custom_annual"]]
            )
          )
        )
      }
    })
  }
)

