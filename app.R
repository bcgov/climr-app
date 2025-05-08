# COOP DEVL VERSION

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
  leaflet::setView(lng = -100, lat = 50, zoom = 5) |>
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
        shiny::div(
          class = "outer",
          leaflet::leafletOutput("climr", width = "100%", height = "100%"),
          shiny::absolutePanel(
            class = "input-control",
            shiny::div(class = "input-control-header", shiny::h4("Controls")),
            shiny::div(
              class = "input-control-body",
              shiny::div(
                title = "Upload a csv, a raster or a shape file to add geographies",
                shiny::fileInput(
                  inputId = "upload",
                  label = "Upload geometry or raster file"
                )
              ),
              # Downscale parameters
              shiny::actionButton(
                inputId = "downscale_parameters",
                label = "Downscale Options",
                title = "Open advanced downscale parameters selection",
                class = "btn btn-primary btn-sm",
                icon = shiny::icon("gear"),
                width = "62%"
              ),
              shiny::actionButton(
                inputId = "downscale_process",
                label = "",
                title = "Open downscale process launch window with currently active geographies",
                class = "btn btn-secondary btn-sm",
                icon = shiny::icon("play"),
                width = "17%",
                disabled = TRUE
              ),
              shiny::downloadButton(
                outputId = "downscale_download",
                label = "",
                title = "Download downscaled geographies archive",
                class = "btn btn-secondary btn-sm"
              ),
              # Overlay parameters
              shiny::hr(),
              shiny::actionButton(
                inputId = "select_overlay",
                label = "Select Overlay",
                title = "Open map climate overlay selection",
                class = "btn btn-primary btn-sm",
                icon = shiny::icon("droplet"),
                width = "62%"
              ),
              shiny::actionButton(
                inputId = "download_overlay",
                label = "Download",
                title = "Download currently active overlay raster (tif)",
                class = "btn btn-secondary btn-sm",
                disabled = TRUE,
                width = "36%",
                icon = shiny::icon("map")
              ),
              shiny::div(
                title = "Adjust the opacity of the currently active overlay",
                shiny::sliderInput(
                  inputId = "opacity",
                  label = "Overlay opacity",
                  value = 80,
                  min = 0,
                  max = 100,
                  step = 1,
                  post = "%",
                  ticks = FALSE
                )
              ),
              shiny::tags$div(
                title = "Adjust the resolution of the currently active overlay",
                shiny::sliderInput(
                  inputId = "resolution",
                  label = "Overlay resolution",
                  value = 96,
                  min = 24,
                  max = 384,
                  step = 12,
                  post = "px",
                  ticks = FALSE
                )
              ),
              shiny::div(
                style = "display: inline-flex; gap: 8px",
                shiny::div(
                  title = "Adjust color palette of the currently active overlay",
                  shiny::selectizeInput(
                    inputId = "palette",
                    label = NULL,
                    choices = pals$select,
                    selected = "Roma",
                    width = "225px",
                    options = list(render = I('{option: function(item, escape) {return item.label;},item: function(item, escape) {return item.label;}}'))
                  )
                ),
                shiny::div(
                  title = "Invert color palette value association of the currently active overlay",
                  shiny::checkboxInput("inverse", "Invert", width = "72px")
                )
              )
            )
          )
        )
      ),
      shiny::navbarMenu(
        "Data",
        "Locations",
        shiny::tabPanel(
          title = "Geometry",
          shiny::div(
            class = "outer2",
            DT::DTOutput(outputId = "geom_dt")
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
    source("scripts/geometry.R", local = TRUE)
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
    shiny::observeEvent(input$upload, {
      if (shiny::in_devmode()) cat("Event: upload", sep = "\n")
      sg$add_file(input$upload)
    })
    shiny::observeEvent(input$sg_remove, {
      if (shiny::in_devmode()) cat("Event: sg_remove", sep = "\n")
      sg$rm(input$sg_remove)
    })
    shiny::observeEvent(input$sg_view, {
      if (shiny::in_devmode()) cat("Event: sg_view", sep = "\n")
      sg$view(input$sg_view)
    })
    shiny::observeEvent(input$sg_bivariate, {
      if (shiny::in_devmode()) cat("Event: sg_bivariate", sep = "\n")
      sg$bivariate(input$sg_bivariate)
    })
    shiny::observeEvent(input$sg_timeseries, {
      if (shiny::in_devmode()) cat("Event: sg_timeseries", sep = "\n")
      sg$timeseries(input$sg_timeseries)
    })
    shiny::observeEvent(input$sg_climate_diagram, {
      if (shiny::in_devmode()) cat("Event: sg_climate_diagram", sep = "\n")
      sg$climate_diagram(input$sg_climate_diagram)
    })
    shiny::observeEvent(input$sg_boxplot, {
      if (shiny::in_devmode()) cat("Event: sg_boxplot", sep = "\n")
      sg$boxplot(input$sg_boxplot)
    })
    shiny::observeEvent(input$sg_climate_stripes, {
      if (shiny::in_devmode()) cat("Event: sg_climate_stripes", sep = "\n")
      sg$climate_stripes(input$sg_climate_stripes)
    })

    sn <- \(j) setNames(j,j)

    # ---- Downscale events
    downscale_modal <- function() {
      shiny::showModal(
        shiny::modalDialog(
          title = "Downscale Parameters", size = "xl", fade = FALSE, class = "modal-dialog-scrollable",
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

    # ---- Overlay events
    shiny::observeEvent(input$select_overlay, {
      if (shiny::in_devmode()) cat("Event: select_overlay", sep = "\n")
      output$vscale_overlay <- NULL
      shiny::showModal(
        shiny::modalDialog(
          title = "Climate Overlay Selection",
          size = "xl",
          shiny::selectInput(
            inputId = "tifsource",
            label = "Source",
            width = "100%",
            choices = names(climr_tif),
            selected = vstore[["tifsource"]]
          ),
          shiny::selectInput(
            inputId = "element",
            label = "Climate Element",
            width = "100%",
            choices = {
              dt <- climr_tif[[vstore[["tifsource"]]]]
              elements <- unique(dt[, list(element, category, label)])
              basic <- elements[category %in% "Basic elements", setNames(element, label)]
              derived <- elements[category %in% "Derived elements", setNames(element, label)]
              annual <- elements[category %in% "Annual elements" & !(element %in% derived), setNames(element, label)]
              list(
                "Basic elements" = basic,
                "Derived elements" = derived,
                "Annual elements" = annual
              )
            },
            selected = vstore[["element"]]
          ),
          shiny::selectInput(
            inputId = "time",
            label = "Time Period",
            width = "100%",
            choices = {
              dt <- climr_tif[[vstore[["tifsource"]]]]
              available_times <- dt[element %in% input$element, unique(time_code)]
              annual <- setNames("aa"["aa" %in% available_times], "Annual"["aa" %in% available_times])
              season <- time_labels_season[time_labels_season %in% available_times]
              month <- time_labels_month[time_labels_month %in% available_times]
              list(
                "Default" = annual,
                "Seasons" = season,
                "Months" = month
              )
            },
            selected = vstore[["time"]]
          ),
          shiny::uiOutput("vscale_overlay"),
          footer = shiny::tagList(
            shiny::actionButton(
              inputId = "load_overlay",
              label = "Load",
              icon = shiny::icon("droplet"),
              class = "btn btn-primary"
            ),
            shiny::modalButton("Close")
          )
        )
      )
    })

    shiny::observeEvent(input$tifsource, {
      if (shiny::in_devmode()) cat("Event: tifsource", sep = "\n")
      vstore[["tifsource"]] <<- input$tifsource
      dt <- climr_tif[[vstore[["tifsource"]]]]
      elements <- unique(dt[, list(element, category, label)])
      basic <- elements[category %in% "Basic elements", setNames(element, label)]
      derived <- elements[category %in% "Derived elements", setNames(element, label)]
      annual <- elements[category %in% "Annual elements" & !(element %in% derived), setNames(element, label)]
      choices <- list(
        "Basic elements" = basic,
        "Derived elements" = derived,
        "Annual elements" = annual
      )
      shiny::updateSelectInput(inputId = "element", choices = choices)
    })

    shiny::observeEvent(input$element, {
      if (shiny::in_devmode()) cat("Event: element", sep = "\n")
      vstore[["element"]] <<- input$element
      dt <- climr_tif[[vstore[["tifsource"]]]]
      available_times <- dt[element %in% input$element, unique(time_code)]
      annual <- setNames("aa"["aa" %in% available_times], "Annual"["aa" %in% available_times])
      season <- time_labels_season[time_labels_season %in% available_times]
      month <- time_labels_month[time_labels_month %in% available_times]
      choices <- list(
        "Default" = annual,
        "Seasons" = season,
        "Months" = month
      )
      shiny::updateSelectInput(inputId = "time", choices = choices)
    })

    shiny::observeEvent(input$time, {
      if (shiny::in_devmode()) cat("Event: time", sep = "\n")
      vstore[["time"]] <<- input$time
      if (is.null(input$element) || is.null(input$time)) return()
      dt <- climr_tif[[vstore[["tifsource"]]]]
      url <- dt[element == input$element & time_code == input$time, url]
      if (length(url) == 1) {
        vstore[["climatevar"]] <- url
      } else {
        vstore[["climatevar"]] <- "NONE"
      }
    })

    shiny::observeEvent(input$load_overlay, {
      if (shiny::in_devmode()) cat("Event: load_overlay", sep = "\n")
      mp <- leaflet::leafletProxy("climr", deferUntilFlush = FALSE)
      mp |> leaflet::clearGroup("Climate") |> leaflet::hideGroup("Climate")
      session$sendCustomMessage(type="jsCode", list(code= "$('#rasterValues-val').remove();"))
      shiny::updateActionButton(inputId = "download_overlay", disabled = TRUE)
      if ("NONE" %in% vstore[["climatevar"]] | 0 == input$opacity) return()
      shiny::updateActionButton(inputId = "download_overlay", disabled = FALSE)
      prefix <- vstore[["climatevar"]] |> basename() |> tools::file_path_sans_ext()
      if (prefix %in% climr_ratios) {
        vstore[["vscale"]] <- "log2"
      } else {
        vstore[["vscale"]] <- ""
      }
      fpal <- if (isTRUE(input$inverse)) rev else identity
      pal <- pals$colors[[input$palette]] |> fpal()
      mp |> leafem::addGeotiff(
        url = vstore[["climatevar"]],
        group = "Climate",
        layerId = "val",
        project = FALSE,
        opacity = input$opacity / 100,
        resolution = input$resolution,
        colorOptions = leafem::colorOptions(
          palette = pal,
          na.color = "transparent"
        ),
        ## pixelValuesToColorFn evaluation scope is preventing us from
        ## accessing values needed to redefine pixelValuesToColorFn function
        ## using georaster min/max. Since we are feeding a URL, these
        ## values are not accessible from R
        ## Sending a custom message to redraw the layer has delay issue
        ## since the custom message is processed before Leaflet has
        ## finished drawing the geotiff layer.
        ## So we fall back to uiOutput hacky way.
        # pixelValuesToColorFn = "scoping issue"
        imagequery = TRUE,
        imagequeryOptions = leafem::imagequeryOptions(
          prefix = prefix
        ),
        autozoom = FALSE,
        options = leaflet::tileOptions(maxZoom = 25, maxNativeZoom = 20)
      ) |> leaflet::showGroup("Climate")
      
      output$vscale_overlay <- shiny::renderUI({
        vstore[["vscale"]] <- NULL
        shiny::selectInput(
          inputId = "vscale",
          label = "Scale Adjustement",
          width = "100%",
          choices = {
            if (prefix %in% climr_ratios) {
              c("None" = "none", "Log" = "log1p")
            } else {
              c("None" = "none")
            }
          }
        )
      })

      shiny::showNotification("Rendering %s values" |> sprintf(prefix), duration = 5)
    })

    shiny::observeEvent(input$vscale, {
      if (shiny::in_devmode()) cat("Event: vscale", sep = "\n")
      vstore[["vscale"]] <- input$vscale
    })

    shiny::observeEvent(input$opacity, {
      if (shiny::in_devmode()) cat("Event: opacity", sep = "\n")
      session$sendCustomMessage(type="updateOpacity", list(category = "image", layerId = "val", opacity = input$opacity / 100))
    })
    shiny::observeEvent(shiny::debounce(input$resolution, 500), {
      if (shiny::in_devmode()) cat("Event: resolution (debounced)", sep = "\n")
      session$sendCustomMessage(type="updateResolution", list(category = "image", layerId = "val", resolution = input$resolution))
    })
    shiny::observeEvent(input$inverse, {
      if (shiny::in_devmode()) cat("Event: inverse", sep = "\n")
      if (isTRUE(input$inverse)) {
        session$sendCustomMessage(type="jsCode", list(code= "$('.palselect').addClass('palselect-invert');"))
      } else {
        session$sendCustomMessage(type="jsCode", list(code= "$('.palselect').removeClass('palselect-invert');"))
      }
    })
    shiny::observe({
      fpal <- if (isTRUE(input$inverse)) rev else identity
      session$sendCustomMessage(type="updateClimatePalette", list(
        category = "image", layerId = "val", vscale = vstore[["vscale"]], colorOptions = leafem::colorOptions(
          palette = pals$colors[[input$palette]] |> fpal(),
          na.color = "transparent"
        )
      ))
    })
    shiny::observeEvent(input$download_overlay, {
      if (shiny::in_devmode()) cat("Event: download_overlay", sep = "\n")
      session$sendCustomMessage(type="jsCode", list(code = "window.location.assign('%s');" |> sprintf(vstore[["climatevar"]])))
    })
  }
)