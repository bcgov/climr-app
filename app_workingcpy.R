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
                p("Click on map to add points or draw an area-of-interest using shape tools."),
                DT::DTOutput("geom_dt", width = "100%"),
                shiny::actionButton("delete_button_point", "Delete Selected", icon("trash-alt")),
                value = "acc1"
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
    
    # ---- Modal input storage
    output$climr <- leaflet::renderLeaflet(l)
    
    downscale_default <- list(
      downscale_which_refmap = "refmap_climr",
      downscale_obs_periods = "2001_2020",
      downscale_obs_years = "NULL",
      downscale_obs_ts_dataset = "NULL",
      downscale_gcms = "NULL",
      downscale_ssps = "NULL",
      downscale_gcm_periods = "NULL",
      downscale_gcm_ssp_years = "NULL",
      downscale_gcm_hist_years = "NULL",
      downscale_max_run = 0,
      downscale_run_nm = "NULL",
      downscale_extra_vars = "NULL",
      downscale_core_ppt_lr = FALSE
    )
    
    vstore <- reactiveValues(
      tifsource = names(climr_tif) |> head(1),
      time = NULL,
      element = NULL,
      climatevar = "NONE",
      downscale_which_refmap = downscale_default[["downscale_which_refmap"]],
      downscale_obs_periods = downscale_default[["downscale_obs_periods"]],
      downscale_obs_years = downscale_default[["downscale_obs_years"]],
      downscale_obs_ts_dataset = downscale_default[["downscale_obs_ts_dataset"]],
      downscale_gcms = downscale_default[["downscale_gcms"]],
      downscale_ssps = downscale_default[["downscale_ssps"]],
      downscale_gcm_periods = downscale_default[["downscale_gcm_periods"]],
      downscale_gcm_ssp_years = downscale_default[["downscale_gcm_ssp_years"]],
      downscale_gcm_hist_years = downscale_default[["downscale_gcm_hist_years"]],
      downscale_max_run = downscale_default[["downscale_max_run"]],
      downscale_run_nm = downscale_default[["downscale_run_nm"]],
      downscale_extra_vars = downscale_default[["downscale_extra_vars"]],
      downscale_core_ppt_lr = downscale_default[["downscale_core_ppt_lr"]],
      downscale_output = "tif",
      downscale_resolution = 2500,
      vscale = "none",
      processing = FALSE
    )
    
    # ---- Geometry
    source("scripts/geometry_workingcpy.R", local = TRUE)
    sg <- session_geometry()
    
    # ---- Map events
    shiny::observeEvent(input$climr_draw_start, {
      if (shiny::in_devmode()) cat("Event: climr_draw_start", sep = "\n")
      sg$add_point_enabled(FALSE)
      updateActionButton(session = getDefaultReactiveDomain(),
                         "downscale_parameters", disabled = FALSE)
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
    })
    shiny::observeEvent(input$upload_button, {
      if (shiny::in_devmode()) cat("Event: upload_button", sep = "\n")
      sg$add_file(input$upload_button)
      updateActionButton(session = getDefaultReactiveDomain(),
                         "downscale_parameters", disabled = FALSE)
    })
    
    # pop-up remove button for map points
    shiny::observeEvent(input$sg_remove, {
      if (shiny::in_devmode()) cat("Event: sg_remove", sep = "\n")
      sg$rm(input$sg_remove)
      if (nrow(map_points$dt) == 1) {
        updateActionButton(session = getDefaultReactiveDomain(),
                          "downscale_parameters", disabled = TRUE)
      }
    })
    
    # ---- Data table events
    
    # delete a map point via data table
    shiny::observeEvent(input$delete_button_point, {
      if (shiny::in_devmode()) cat("Event: sg_remove", sep = "\n")
      row_num <- input$geom_dt_rows_selected
      point_id <- as.numeric(map_points$dt[row_num,1])
      if (length(point_id) != 0) {
        sg$rm(point_id)
        if (nrow(map_points$dt) == 1) {
        updateActionButton(session = getDefaultReactiveDomain(),
                          "downscale_parameters", disabled = TRUE)
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

    sn <- \(j) setNames(j,j)
    
    # ---- Downscale events
    downscale_modal <- function() {
      shiny::showModal(
        shiny::modalDialog(
          title = "Downscale Parameters", size = "l", fade = FALSE, class = "modal-dialog-scrollable",
          shiny::div(
            title = "Which map of 1961-1990 climatological normals to use as the high-resolution reference climate map for downscaling. 'auto' selects the best available map per point.",
            shiny::selectInput(
              inputId = "downscale_which_refmap",
              label = "Reference Map",
              width = "100%",
              choices = c(
                local({z <- climr::list_refmaps(); substr(z, 8L, z |> nchar()) |> tools::toTitleCase() |> setNames(object = z, nm = _)})
              ),
              selected = vstore[["downscale_which_refmap"]]
            )
          ),
          shiny::div(
            title = "Historical period for observational climate data, averaged over this period. Select 'Null' for no observational periods.",
            shiny::selectInput(
              inputId = "downscale_obs_periods",
              label = "Observation periods",
              width = "100%",
              choices = list("Options" = climr::list_obs_periods() |> sn(), "Remove all" = c("null" = "NULL")),
              selected = vstore[["downscale_obs_periods"]]
            )
          ),
          shiny::div(
            title = "Years to obtain individual years or time series of observational climate data.",
            shiny::selectInput(
              inputId = "downscale_obs_years",
              label = "Observation years",
              width = "100%",
              choices = list("Options" = climr::list_obs_years() |> sn(), "Remove all" = c("null" = "NULL")),
              multiple = TRUE,
              selected = vstore[["downscale_obs_years"]]
            )
          ),
          shiny::div(
            title = "Dataset for observational time series data. Options: 'climatena' for ClimateNA gridded time series, 'cru.gpcc' for CRU TS (temperature) and GPCC (precipitation), or 'Null' for none.",
            shiny::selectInput(
              inputId = "downscale_obs_ts_dataset",
              label = "Observation time-series data",
              width = "100%",
              selected = vstore[["downscale_obs_ts_dataset"]],
              choices = c("ClimateNA" = "climatena", "Climatic Research Unit / Global Precipitation Climatology Centre" = "cru.gpcc", "null" = "NULL")
            )
          ),
          shiny::div(
            title = "Global climate models to downscale. Select multiple GCMs for ensemble outputs.",
            shiny::selectInput(
              inputId = "downscale_gcms",
              label = "Global climate model",
              width = "100%",
              choices = list("Options" = climr::list_gcms() |> sn(), "Remove all" = c("null" = "NULL")),
              multiple = TRUE,
              selected = vstore[["downscale_gcms"]]
            )
          ),
          shiny::div(
            title = "SSP-RCP scenarios pairing shared socioeconomic pathways with representative concentration pathways.",
            shiny::selectInput(
              inputId = "downscale_ssps",
              label = "Shared Socio-economic Pathways (SSP) - Representative Concentration Pathways (RCP) Scenarios",
              width = "100%",
              choices = list("Options" = climr::list_ssps() |> sn(), "Remove all" = c("null" = "NULL")),
              multiple = TRUE,
              selected = vstore[["downscale_ssps"]]
            )
          ),
          shiny::div(
            title = "20-year reference periods for GCM simulations.",
            shiny::selectInput(
              inputId = "downscale_gcm_periods",
              label = "General Circulation Model (GCM) Periods",
              width = "100%",
              choices = list("Options" = climr::list_gcm_periods() |> sn(), "Remove all" = c("null" = "NULL")),
              multiple = TRUE,
              selected = vstore[["downscale_gcm_periods"]]
            )
          ),
          shiny::div(
            title = "Time series years for GCM simulations of future SSP scenarios.",
            shiny::selectInput(
              inputId = "downscale_gcm_ssp_years",
              label = "General circulation model (GCM) Shared Socio-economic Pathways (SSP) Years",
              width = "100%",
              choices = list("Options" = climr::list_gcm_ssp_years() |> sn(), "Remove all" = c("null" = "NULL")),
              multiple = TRUE,
              selected = vstore[["downscale_gcm_ssp_years"]]
            )
          ),
          shiny::div(
            title = "Time series years for GCM simulations of the historical scenario.",
            shiny::selectInput(
              inputId = "downscale_gcm_hist_years",
              label = "General circulation model (GCM) Historical Years",
              width = "100%",
              choices = list("Options" = climr::list_gcm_hist_years() |> sn(), "Remove all" = c("null" = "NULL")),
              multiple = TRUE,
              selected = vstore[["downscale_gcm_hist_years"]]
            )
          ),
          shiny::div(
            title = "Maximum number of model runs to include. 0 returns only the ensemble mean.",
            shiny::selectInput(
              inputId = "downscale_max_run",
              label = "Maximum number of model runs",
              width = "100%",
              choices = c("ensembleMean" = 0, 1:10),
              multiple = FALSE,
              selected = vstore[["downscale_max_run"]]
            )
          ),
          shiny::div(
            title = "Names of specific runs to return instead of using max_run. Overrides max_run if specified.",
            shiny::selectInput(
              inputId = "downscale_run_nm",
              label = "Name of specified runs",
              width = "100%",
              choices = list("Options" = {
                gcms <- vstore[["downscale_gcms"]]
                ssps <- vstore[["downscale_ssps"]]
                if (!length(gcms) && !length(ssps)) {
                  c()
                } else if (length(gcms) && !length(ssps)) {
                  climr::list_runs_historic(gcm = gcms) |> sn()
                } else if (length(gcms) && length(ssps)) {
                  climr::list_runs_ssp(gcm = gcms, ssp = ssps) |> sn()
                }
              }, "Remove all" = c("null" = "NULL")),
              multiple = TRUE,
              selected = vstore[["downscale_run_nm"]]
            )
          ),
          shiny::div(
            title = "Extra Climate variables to compute. Defaults to monthly PPT, Tmax, Tmin if not specified.",
            shiny::selectizeInput(
              inputId = "downscale_extra_vars",
              label = "Extra Climate variables",
              width = "100%",
              choices = c(downscale_extra_vars, list("Remove all" = c("null" = "NULL"))),
              multiple = TRUE,
              selected = vstore[["downscale_extra_vars"]]
            )
          ),
          shiny::div(
            title = "Apply elevation adjustment to precipitation values during downscaling.",
            shiny::checkboxInput(
              inputId = "downscale_core_ppt_lr",
              label = "Precipitation elevation adjustment",
              value = vstore[["downscale_core_ppt_lr"]]
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
      # before displaying the popup window, use the source column in the map_points table to make sure they are all the same
      downscale_modal()
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
    shiny::observeEvent(input$downscale_obs_periods, {
      if (shiny::in_devmode()) cat("Event: downscale_obs_periods", sep = "\n")
      update_vstore_and_notify("downscale_obs_periods", input$downscale_obs_periods, "Obs periods")
    })
    shiny::observeEvent(input$downscale_obs_years, {
      if (shiny::in_devmode()) cat("Event: downscale_obs_years", sep = "\n")
      update_vstore_and_notify("downscale_obs_years", input$downscale_obs_years, "Obs years")
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
    shiny::observeEvent(input$downscale_gcm_ssp_years, {
      if (shiny::in_devmode()) cat("Event: downscale_gcm_ssp_years", sep = "\n")
      update_vstore_and_notify("downscale_gcm_ssp_years", input$downscale_gcm_ssp_years, "GCM SSP years")
    })
    shiny::observeEvent(input$downscale_gcm_hist_years, {
      if (shiny::in_devmode()) cat("Event: downscale_gcm_hist_years", sep = "\n")
      update_vstore_and_notify("downscale_gcm_hist_years", input$downscale_gcm_hist_years, "GCM hist years")
    })
    shiny::observeEvent(input$downscale_max_run, {
      if (shiny::in_devmode()) cat("Event: downscale_max_run", sep = "\n")
      update_vstore_and_notify("downscale_max_run", input$downscale_max_run, "Max run")
    })
    shiny::observeEvent(input$downscale_run_nm, {
      if (shiny::in_devmode()) cat("Event: downscale_run_nm", sep = "\n")
      update_vstore_and_notify("downscale_run_nm", input$downscale_run_nm, "Run name")
    })
    shiny::observeEvent(input$downscale_extra_vars, {
      if (shiny::in_devmode()) cat("Event: downscale_extra_vars", sep = "\n")
      update_vstore_and_notify("downscale_extra_vars", input$downscale_extra_vars, "Core vars")
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
    
    shiny::observeEvent(input$downscale_process, {
      if (shiny::in_devmode()) cat("Event: downscale_process", sep = "\n")
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
            title = "tif: Shapes/rasters are returned as GeoTIFF. csv: all points are returned in csv.",
            shiny::radioButtons(
              inputId = "downscale_output",
              label = "Downscale Output Format Priority",
              choices = c("Geographic Tag Image File Format (GeoTIFF)" = "tif", "Comma Separated Value (csv)" = "csv"),
              inline = TRUE,
              selected = vstore[["downscale_output"]]
            )
          ),
          shiny::div(
            title = "Target resolution for shapes drawn on map or added using file upload. Does not apply to points, raster or csv files.",
            shiny::sliderInput(
              inputId = "downscale_resolution",
              label = "Downscale Resolution (m)",
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
          )
        )
      )
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
    
  }
)

