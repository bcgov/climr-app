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
  
  # allow map to be modified instead of re-rendering
  vis_mp <- leaflet::leafletProxy("vis_map")
  
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
  })
  
  # ---- Visualization data events
  shiny::observeEvent(input$input_type, {
    if (input$input_type == "Map point") {
      vis_by_map(TRUE)
    } else {
      vis_by_map(FALSE)
      vis_sg$clear_all(vis_mp)
    }
    show_plot <- input$input_type != "" && input$input_type != "Overlay"
    session$sendCustomMessage("toggle-plot", show_plot)
    show_overlay <- input$input_type == "Overlay"
    session$sendCustomMessage("toggle-overlay", show_overlay)
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
  shiny::observeEvent(input$time_series_dataset, {
    if (shiny::in_devmode()) cat("Event: time_series_dataset", sep = "\n")
    if (!is.null(timeseries_data)) {
      vis_sg$timeseries(timeseries_data)
    }    
  })
  shiny::observeEvent(input$time_series_gcm, {
    if (shiny::in_devmode()) cat("Event: time_series_gcm", sep = "\n")
    if (!is.null(timeseries_data)) {
      vis_sg$timeseries(timeseries_data)
    }    
  })
  shiny::observeEvent(input$time_series_ssp, {
    if (shiny::in_devmode()) cat("Event: time_series_ssp", sep = "\n")
    if (!is.null(timeseries_data)) {
      vis_sg$timeseries(timeseries_data)
    }    
  })
  shiny::observeEvent(input$downscale_data_time_series, {
    if (shiny::in_devmode()) cat("Event: downscale_data_time_series", sep = "\n")
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
        timeseries_data <- climr::plot_timeSeries_input(xyz)
        vis_sg$timeseries(timeseries_data)
        removeModal()
      }
    )
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
  
}