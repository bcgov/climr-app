# Visualization tab server ----

visualization_server <- function(input, output, session) {
  
  # initialize vis_sg_dt as reactive
  vis_sg_dt <- reactiveValues(dt = data.table::data.table(
    id = integer(),
    wkt = character(),
    group = character(),
    source = character(),
    datapath = character()
  ),
  filtered_dt = NULL
  )
  
  # ---- Modal input storage
  output$vis_map <- leaflet::renderLeaflet(l)
  
  # allow map to be modified instead of re-rendering
  vis_mp <- leaflet::leafletProxy("vis_map")
  
  # ---- Plot data info
  vis_by_map <- reactiveVal(FALSE)
  bivariate_data <- c()
  timeseries_data <- c()
  
  # mapping for months/seasons
  time_labels <- c(
    "Annual" = "Ann",
    "Winter" = "Wt",
    "Spring" = "Sp",
    "Summer" = "Sm",
    "Autumn" = "At",
    "January" = "Jan",
    "February" = "Feb",
    "March" = "Mar",
    "April" = "Apr",
    "May" = "May",
    "June" = "Jun",
    "July" = "Jul",
    "August" = "Aug",
    "September" = "Sep",
    "October" = "Oct",
    "November" = "Nov",
    "December" = "Dec"
  )
  
  vstore <- reactiveValues(
    tifsource = names(climr_tif) |> head(1),
    time = NULL,
    element = NULL,
    climatevar = NULL,
    vscale = NULL
  )
  
  # ---- Geometry
  source("scripts/geometry_visualization.R", local = TRUE)
  vis_sg <- visualization_geometry(vis_sg_dt, vis_mp)
  
  # ---- Visualization Map events
  
  shiny::observeEvent(input$vis_map_click, {
    if (shiny::in_devmode()) cat("Event: vis_map_click", sep = "\n")
    vis_sg$add_point(input$vis_map_click$lat, input$vis_map_click$lng, vis_by_map)
  })
  
  # pop-up remove button for map points
  shiny::observeEvent(input$sg_remove, {
    if (shiny::in_devmode()) cat("Event: sg_remove", sep = "\n")
    vis_sg$rm(input$sg_remove)
  })
  
  # clear map
  shiny::observeEvent(input$clear_map, {
    if (shiny::in_devmode()) cat("Event: clear_map", sep = "\n")
    vis_sg$clear_all(vis_mp)
    leaflet::removeImage(vis_mp, layerId = "val")
  })
  
  # ---- Visualization data events
  shiny::observeEvent(input$input_type, {
    if (input$input_type == "Map point") {
      vis_by_map(TRUE)
    } else {
      vis_by_map(FALSE)
      vis_sg$clear_all(vis_mp)
    }
    show_plot <- input$input_type != ""
    session$sendCustomMessage("toggle-plot", show_plot)
  })
  
  # ---- Visualization Plot events
  
  shiny::observeEvent(input$bivariate_element_x, {
    if (shiny::in_devmode()) cat("Event: bivariate_element_x", sep = "\n")
    if (!is.null(bivariate_data)) {
      vis_sg$bivariate(bivariate_data)
    }
  })
  shiny::observeEvent(input$bivariate_time_x, {
    if (shiny::in_devmode()) cat("Event: bivariate_time_x", sep = "\n")
    if (!is.null(bivariate_data)) {
      vis_sg$bivariate(bivariate_data)
    }    
  })
  shiny::observeEvent(input$bivariate_element_y, {
    if (shiny::in_devmode()) cat("Event: bivariate_element_y", sep = "\n")
    if (!is.null(bivariate_data)) {
      vis_sg$bivariate(bivariate_data)
    }    
  })
  shiny::observeEvent(input$bivariate_time_y, {
    if (shiny::in_devmode()) cat("Event: bivariate_time_y", sep = "\n")
    if (!is.null(bivariate_data)) {
      vis_sg$bivariate(bivariate_data)
    }    
  })
  shiny::observeEvent(input$downscale_data_bivariate, {
    if (shiny::in_devmode()) cat("Event: downscale_data_bivariate", sep = "\n")
    if (nrow(vis_sg_dt$dt) < 1 & input$input_type == "Map point") {
      showModal(
        modalDialog(
          title = "Warning",
          paste("Please select a map point!"),
          easyClose = TRUE
        )
      )
    } else {
      withCallingHandlers(
        message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
        warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
        error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
        {
          g <- terra::vect((vis_sg_dt$dt)[1,][["wkt"]], crs = "EPSG:4326")
          coords <- terra::crds(g)
          elevs <- terra::extract(cec, g, method = "bilinear", ID = FALSE, raw = TRUE)[,1]
          xyz <- data.table::data.table(
            id = 1,
            lon = coords[, 1],
            lat = coords[, 2],
            elev = elevs
          )
          bivariate_data <- climr::plot_bivariate_input(xyz)
          vis_sg$bivariate(bivariate_data)
        }
      )
    }
  })
  shiny::observeEvent(input$time_series_dataset, {
    if (shiny::in_devmode()) cat("Event: time_series_dataset", sep = "\n")
    if (!is.null(timeseries_data)) {
      vis_sg$timeseries(timeseries_data)
    }    
  })
  shiny::observeEvent(input$time_series_gcms, {
    if (shiny::in_devmode()) cat("Event: time_series_gcm", sep = "\n")
    if (!is.null(timeseries_data)) {
      vis_sg$timeseries(timeseries_data)
    }    
  })
  shiny::observeEvent(input$time_series_ssps, {
    if (shiny::in_devmode()) cat("Event: time_series_ssp", sep = "\n")
    if (!is.null(timeseries_data)) {
      vis_sg$timeseries(timeseries_data)
    }    
  })
  shiny::observeEvent(input$downscale_data_time_series, {
    if (shiny::in_devmode()) cat("Event: downscale_data_time_series", sep = "\n")
    if (nrow(vis_sg_dt$dt) < 1 & input$input_type == "Map point") {
      showModal(
        modalDialog(
          title = "Warning",
          paste("Please select a map point!"),
          easyClose = TRUE
        )
      )
    } else {
      withCallingHandlers(
        message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
        warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
        error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
        {
          g <- terra::vect((vis_sg_dt$dt)[1,][["wkt"]], crs = "EPSG:4326")
          coords <- terra::crds(g)
          elevs <- terra::extract(cec, g, method = "bilinear", ID = FALSE, raw = TRUE)[,1]
          xyz <- data.table::data.table(
            id = 1,
            lon = coords[, 1],
            lat = coords[, 2],
            elev = elevs
          )
          showModal(
            modalDialog(
              title = "Just a note!",
              paste("There is a lot of data being downscaled right now to set up an interactive plot, so thank you for being patient! :)"),
              easyClose = TRUE
            )
          )
          timeseries_data <- climr::plot_timeSeries_input(xyz, gcms = input$time_series_gcms, ssps = input$time_series_ssps, obs_ts_dataset = c("mswx.blend", "cru.gpcc", "climatena"))
          vis_sg$timeseries(timeseries_data)
          removeModal()
        }
      )
    }
  })
  shiny::observeEvent(input$time_series_element, {
    if (shiny::in_devmode()) cat("Event: time_series_element", sep = "\n")
    if (!is.null(timeseries_data)) {
      vis_sg$timeseries(timeseries_data)
    }
  })
  shiny::observeEvent(input$time_series_season, {
    if (shiny::in_devmode()) cat("Event: time_series_season", sep = "\n")
    if (!is.null(timeseries_data)) {
      vis_sg$timeseries(timeseries_data)
    }    
  })
  
  # reactive outputs
  output$bivariate_valid_time_x <- shiny::renderUI({
    if (!is.null(input$bivariate_element_x)) {
      shiny::radioButtons(
        inputId = "bivariate_time_x",
        label = h5("Choose x-axis season/month:"),
        width = "100%",
        inline = TRUE,
        choices = setNames(climr::variables[Code_Element == input$bivariate_element_x,] %>% pull(Time), time_labels[climr::variables[Code_Element == input$bivariate_element_x,] %>% pull(Time)])
      )
    }
  })
  
  output$bivariate_valid_time_y <- shiny::renderUI({
    if (!is.null(input$bivariate_element_y)) {
      shiny::radioButtons(
        inputId = "bivariate_time_y",
        label = h5("Choose y-axis season/month:"),
        width = "100%",
        inline = TRUE,
        choices = setNames(climr::variables[Code_Element == input$bivariate_element_y,] %>% pull(Time), time_labels[climr::variables[Code_Element == input$bivariate_element_y,] %>% pull(Time)])
      )
    }
  })
  
  output$time_series_valid_season <- shiny::renderUI({
    if (!is.null(input$time_series_element)) {
      shiny::radioButtons(
        inputId = "time_series_season",
        label = h5("Choose season/month:"),
        width = "100%",
        inline = TRUE,
        choices = setNames(climr::variables[Code_Element == input$time_series_element,] %>% pull(Time), time_labels[climr::variables[Code_Element == input$time_series_element,] %>% pull(Time)])
      )
    }
  })
  
  # ---- Visualization Overlay events
  shiny::observeEvent(input$load_overlay, {
    if (shiny::in_devmode()) cat("Event: load_overlay", sep = "\n")
    
    # update selected options
    vstore[["time"]] <- input$time
    vstore[["element"]] <- input$element
    if ("ratio" %in% climr::variables[Code_Element == input$element & Time == input$time, Type]) {
      vstore[["vscale"]] <- input$vscale
    } else {
      vstore[["vscale"]] <- FALSE
    }
    
    # create URL
    if (is.null(input$element) || is.null(input$time)) return()
    dt <- climr_tif[[vstore[["tifsource"]]]]
    get_time_code <- function(label) {
      if (label %in% names(time_labels_season)) {
        return(time_labels_season[[label]])
      } else if (label %in% names(time_labels_month)) {
        return(time_labels_month[[label]])
      } else {
        return()
      }
    }
    url <- dt[element == input$element & time_code == get_time_code(input$time), url]
    if (length(url) == 1) {
      vstore[["climatevar"]] <- url
    } else {
      vstore[["climatevar"]] <- NULL
    }
    
    # render overlay
    mp <- leaflet::leafletProxy("vis_map", deferUntilFlush = FALSE)
    mp |> leaflet::clearGroup("Climate") |> leaflet::hideGroup("Climate")
    shiny::updateActionButton(inputId = "download_overlay", disabled = TRUE)
    if (is.null(vstore[["climatevar"]])) return()
    shiny::updateActionButton(inputId = "download_overlay", disabled = FALSE)
    
    # get scaling
    if (isTRUE(vstore[["vscale"]])) {
      vstore$vscale <- "log2"
    } else {
      vstore$vscale <- ""
    }
    
    # set up palettes/breaks (this needs speeding up - loading full raster in R right now)
    vals <- values(rast(url))
    vals <- vals[is.finite(vals)]
    q <- quantile(vals, c(0.005, 0.995))
    inc <- diff(q) / 500
    breaks <- seq(q[1] - inc, q[2] + inc, by = inc)
    
    if (grepl("PPT", vstore[["element"]])) {
      pal <- RColorBrewer::brewer.pal(9, "YlGnBu")
    } else {
      pal <- rev(RColorBrewer::brewer.pal(11, "RdYlBu"))
    }
    
    mp |> leafem::addGeotiff(
      url = vstore[["climatevar"]],
      group = "Climate",
      layerId = "val",
      project = FALSE,
      # opacity =
      # resolution =
      colorOptions = leafem::colorOptions(
        palette = pal,
        breaks = breaks,
        na.color = "transparent"
      ),
      imagequery = TRUE,
      autozoom = FALSE,
      options = leaflet::tileOptions(maxZoom = 25, maxNativeZoom = 20)
    ) |> leaflet::showGroup("Climate")
    
    session$sendCustomMessage(type="updateClimatePalette", list(
      category = "image", layerId = "val", vscale = vstore[["vscale"]], colorOptions = leafem::colorOptions(
        palette = pal,
        na.color = "transparent"
      )
    ))
    
    shiny::showNotification("Rendering %s values" |> sprintf(vstore[["element"]]), duration = 5)
  })
  shiny::observeEvent(input$download_overlay, {
    if (shiny::in_devmode()) cat("Event: download_overlay", sep = "\n")
    session$sendCustomMessage(type="jsCode", list(code = "window.location.assign('%s');" |> sprintf(vstore[["climatevar"]])))
  })
  
  # reactive outputs
  output$overlay_element <- shiny::renderUI({
    shiny::selectInput(
      inputId = "element",
      label = "Choose element:",
      choices = {
        dt <- climr_tif[[vstore[["tifsource"]]]]
        unique(dt[, element])
      }
    )
  })
  
  output$overlay_period <- shiny::renderUI({
    if (!is.null(input$element) & input$element != "elev" & input$element != "lat" & input$element != "PET") {
      shiny::selectInput(
        inputId = "time",
        label = "Choose season/months:",
        choices = climr::variables[Code_Element == input$element, Time]
      )
    }
  })
  
  output$scale_adj <- shiny::renderUI({
    if (!is.null(input$element) & "ratio" %in% climr::variables[Code_Element == input$element, Type]) {
      shiny::checkboxInput(
        inputId = "vscale",
        label = "Apply scale adjustment",
        value = reactive({
          req(input$element)
          "ratio" %in% climr::variables[Code_Element == input$element, Type]
        })()
      )
    }
  })
  
}