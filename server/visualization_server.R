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
  output$vis_map <- leaflet::renderLeaflet(l |> addDistricts() |>  
                                             htmlwidgets::onRender("
                                                  function(el, x) {
                                                    window.map = this;
                                                    console.log('Map assigned to window.map');
                                                  }
                                                ")
                                           ) 
  
  # allow map to be modified instead of re-rendering
  vis_mp <- leaflet::leafletProxy("vis_map")
  
  # ---- Plot data info
  vis_by_map <- reactiveVal(FALSE)
  show_plots <- reactiveVal(FALSE)
  bivariate_data <- data.table()
  timeseries_data <- data.table()
  wl_data <- data.table()
  
  # mapping for database queries
  gcm_id <- data.table(gcm = list_gcms(), gcm_id = seq_along(list_gcms()))
  ssp_id <- data.table(ssp = list_ssps(), ssp_id = seq_along(list_ssps()))
  var_id <- data.table(var = list_vars(), var_id = seq_along(list_vars()))
  dataset_id <- data.table(dataset = c("mswx.blend","cru.gpcc","climatena"), dataset_id = 1:3)
  
  vstore <- reactiveValues(
    tifsource = names(climr_tif) |> head(1),
    time = NULL,
    element = NULL,
    climatevar = NULL,
    vscale = NULL,
    flp_area = NULL,
    ecoregion = NULL
  )
  
  # ---- Geometry
  source("scripts/geometry_visualization.R", local = TRUE)
  vis_sg <- visualization_geometry(vis_sg_dt, vis_mp)
  
  # ---- Visualization Map events
  shiny::observeEvent(input$tutorial_vis, {
    showModal(modalDialog(
      title = "What does this page do?",
      easyClose = TRUE,
      size = "xl",
      footer = NULL,
      tags$iframe(
        src = "How_to_use_visualization.pdf#toolbar=0",
        width = "100%",
        height = "700px",
        style = "border:none;"
      )
    ))
  })
  # click on map points
  shiny::observeEvent(input$vis_map_click, {
    if (shiny::in_devmode()) cat("Event: vis_map_click", sep = "\n")
    vis_sg$add_point(input$vis_map_click$lat, input$vis_map_click$lng, vis_by_map)
    lapply(
      c("downscale_data_time_series", "downscale_data_bivariate", "downscale_data_wl", "ts_adj_plot"),
      function(btn_id) {
        shiny::updateActionButton(
          inputId = btn_id,
          disabled = FALSE
        )
      }
    )
  })
  
  # pop-up remove button for map points
  shiny::observeEvent(input$sg_remove, {
    if (shiny::in_devmode()) cat("Event: sg_remove", sep = "\n")
    vis_sg$rm(input$sg_remove)
  })
  
  # clear map function
  clear_all <- function() {
    # remove all map points/shapes and overlays
    vis_sg$clear_all(vis_mp)
    vis_by_map(FALSE)
    leaflet::removeImage(vis_mp, layerId = "val")
    leaflet::clearControls(vis_mp)
    leaflet::hideGroup(vis_mp, "Climate")
    session$sendCustomMessage("clear_district","Waddles")
    
    # remove all plots
    if (!is.null(input$input_type)) {
      if (input$input_type != "") {
        session$sendCustomMessage("toggle-plot", FALSE)
      }
    }
    shiny::updateActionButton(
      inputId = "download_overlay",
      disabled = TRUE
    )
    shiny::updateRadioButtons(
      inputId = "input_type",
      selected = character(0)
    )
    shiny::updateCheckboxInput(
      inputId = "show_overlay_controls",
      value = FALSE
    )
    show_plots(FALSE)
    
    # disable downscale/plot buttons
    lapply(
      c("plot_ts_flp_er", "plot_bivariate_flp_er", "plot_wl_flp_er", "downscale_data_time_series", "downscale_data_bivariate", "downscale_data_wl", "ts_adj_plot"),
      function(btn_id) {
        shiny::updateActionButton(
          inputId = btn_id,
          disabled = TRUE
        )
      }
    )
  }
  clear_inputs <- function() {
    # clear map icons
    vis_sg$clear_all(vis_mp)
    vis_by_map(FALSE)
    
    # remove plots
    show_plots(FALSE)
    
    # disable downscale/plot buttons
    lapply(
      c("plot_ts_flp_er", "plot_bivariate_flp_er", "plot_wl_flp_er", "downscale_data_time_series", "downscale_data_bivariate", "downscale_data_wl", "ts_adj_plot"),
      function(btn_id) {
        shiny::updateActionButton(
          inputId = btn_id,
          disabled = TRUE
        )
      }
    )
  }
  shiny::observeEvent(input$clear_map, {
    if (shiny::in_devmode()) cat("Event: clear_map", sep = "\n")
    clear_all()
  })
  # click on ecoregion/FLP area
  shiny::observeEvent(input$dist_click,{
    if (input$input_type == "FLP Area") {
      vstore[["flp_area"]] <- input$dist_click
    } else if (input$input_type == "Ecoregion") {
      vstore[["ecoregion"]] <- input$dist_click
    }
    lapply(
      c("plot_ts_flp_er", "plot_bivariate_flp_er", "plot_wl_flp_er", "ts_adj_plot"),
      function(btn_id) {
        shiny::updateActionButton(
          inputId = btn_id,
          disabled = FALSE
        )
      }
    )
  })
  
  # ---- Visualization data events
  timeseries_modal <- function() {
    shiny::showModal(
      shiny::modalDialog(
        title = "Adjust Plot Options", size = "l", fade = FALSE, class = "modal-dialog-scrollable",
        shiny::checkboxGroupInput(
          inputId = "time_series_dataset",
          label = h5("Choose observational dataset:"),
          width = "100%",
          inline = TRUE,
          choices = c("MSWX Blend" = "mswx.blend", "ClimateNA" = "climatena", "Climatic Research Unit / Global Precipitation Climatology Centre" = "cru.gpcc"),
          selected = c("mswx.blend", "climatena", "cru.gpcc")
        ),
        shiny::checkboxGroupInput(
          inputId = "time_series_gcms",
          label = h5("Choose GCMs:"),
          width = "100%",
          inline = TRUE,
          choices = climr::list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
          selected = climr::list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)]
        ),
        shiny::checkboxGroupInput(
          inputId = "time_series_ssps",
          label = h5("Choose SSPs:"),
          width = "100%",
          inline = TRUE,
          choices = climr::list_ssps()[c(1:3)],
          selected = climr::list_ssps()[c(1:3)]
        ),
        footer = shiny::tagList(
          shiny::actionButton(
            inputId = "timeseries_ok",
            label = "OK!",
            style = "background-color:#1d8f0e; color: #FFF",
            icon = icon("check")
          ), 
        ),
        easyClose = TRUE
      )
    )
  }
  shiny::observeEvent(input$input_type, {
    type <- input$input_type
    # clear previous inputs
    clear_inputs()
    if (input$input_type == "Map point") {
      vis_by_map(TRUE)
      session$sendCustomMessage("clear_district","Waddles")
    } else if (input$input_type == "FLP Area") {
      vis_by_map(FALSE)
      vis_sg$clear_all(vis_mp)
      dat <- list(url = "https://tileserver.thebeczone.ca/data/flp_bnd/{z}/{x}/{y}.pbf", name = "flp", id = "ORG_UNIT", inputType = type)
      session$sendCustomMessage("addRegionTile",dat)
      session$sendCustomMessage("reset_district","Luna")
    } else if (input$input_type == "Ecoregion") {
      vis_by_map(FALSE)
      vis_sg$clear_all(vis_mp)
      dat <- list(url = "https://tileserver.thebeczone.ca/data/ecoregions/{z}/{x}/{y}.pbf", name = "Ecoregions", id = "NA_L3CODE", inputType = type)
      session$sendCustomMessage("addRegionTile",dat)
      session$sendCustomMessage("reset_region","Luna")
    } else {
      session$sendCustomMessage("clear_district","Waddles")
      vis_by_map(FALSE)
      vis_sg$clear_all(vis_mp)
    }
    show_plot <- input$input_type != ""
    session$sendCustomMessage("toggle-plot", show_plot)
    
    # disable the download buttons
    shinyjs::disable("bivariate_download")
    shinyjs::disable("wl_download")
    shinyjs::disable("timeseries_download")
  })
  
  # ---- Visualization Plot events
  shiny::observeEvent(input$ts_plot_info, {
    showModal(modalDialog(
      title = "What does this plot mean?",
      easyClose = TRUE,
      size = "l",
      shiny::p("Time series plots of 20th and 21st century climate change for user-selected locations and climate variables."),
      shiny::p("Purposes of the plot:"),
      shiny::tags$ul(
        shiny::tags$li("View differences in interannual variability and climate change trends among global climate models (GCMs)"),
        shiny::tags$li("View the differences between multiple simulations of each model"),
        shiny::tags$li("Compare simulated and observed climate change from 1901 to present"),
        shiny::tags$li("Compare time series of two different variables")
      ),
      shiny::p("All global climate model anomalies are bias-corrected to the 1961-1990 reference period normals."),
      #HTML('<a href="documentation/_book/Instructions.html#step-2.-visualize-by-plots" target="_blank">Click here for documentation.</a>')
    ))
  })
  shiny::observeEvent(input$biv_plot_info, {
    showModal(modalDialog(
      title = "What does this plot mean?",
      easyClose = TRUE,
      size = "l",
      shiny::p("Bivariate plots showing 21st century climate change for user-selected locations and climate variables."),
      shiny::p("Purposes of the plot:"),
      shiny::tags$ol(
        shiny::tags$li("Show differences in climate change trends among global climate models (GCMs)"),
        shiny::tags$li("Show the differences between multiple simulations of each model"),
        shiny::tags$li("Compare simulated climate change to observed climate change in the 2001-2020 period")
      ),
      shiny::p("All climate changes are relative to the 1961-1990 reference period normals."),
      #HTML('<a href="documentation/_book/Instructions.html#step-2.-visualize-by-plots" target="_blank">Click here for documentation.</a>')
    ))
  })
  shiny::observeEvent(input$wl_plot_info, {
    showModal(modalDialog(
      title = "What does this plot mean?",
      easyClose = TRUE,
      size = "l",
      shiny::p("Purposes of the Walter-Lieth Climate Diagram:"),
      shiny::tags$ul(
        shiny::tags$li("Allow identification of humid and drought periods over a year"),
        shiny::tags$li("Allow for an easy climate comparison between geographic locations")
      ),
      shiny::p("All global climate model anomalies are bias-corrected to the 1961-1990 reference period normals."),
      #HTML('<a href="documentation/_book/Instructions.html#step-2.-visualize-by-plots" target="_blank">Click here for documentation.</a>')
    ))
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
    } else if (nrow(vis_sg_dt$dt) > 0 & input$input_type == "Map point") {
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
          bivariate_data <<- climr::plot_bivariate_input(xyz)
          show_plots(TRUE)
          vis_sg$bivariate(bivariate_data)
          shinyjs::enable("bivariate_download")
        }
      )
    }
  })
  shiny::observeEvent(input$plot_bivariate_flp_er, {
    if (shiny::in_devmode()) cat("Event: plot_bivariate_flp_er", sep = "\n")
    if (!is.null(input$input_type)) {
      show_plots(TRUE)
      if ((!is.null(vstore[["flp_area"]]) & input$input_type == "FLP Area") | (!is.null(vstore[["ecoregion"]]) & input$input_type == "Ecoregion")) {
        withCallingHandlers(
          message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
          warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
          error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
          {
            # set up db query (only view ssp245)
            region <- vstore[[if (input$input_type == "FLP Area") "flp_area" else "ecoregion"]]
            gcms <- paste(gcm_id[,gcm_id], collapse = ",")
            code_x <- paste(climr::variables[Code_Element == input$bivariate_element_x & Time == input$bivariate_time_x, Code], collapse = ",")
            var_x <- var_id[var == code_x, var_id]
            code_y <- paste(climr::variables[Code_Element == input$bivariate_element_y & Time == input$bivariate_time_y, Code], collapse = ",")
            var_y <- var_id[var == code_y, var_id]

            query <- sprintf("SELECT * FROM ds_bivariate WHERE region = '%s'
                            AND (gcm_id IN (%s) OR gcm_id IS NULL)
                            AND (ssp_id = 2 OR ssp_id IS NULL)
                            AND (var_id = %s OR var_id = %s)
                            ORDER BY gcm_id, period, run_id, ssp_id", region, gcms, var_x, var_y)
            dat <- climr:::db_safe_query(query)
            dat <- as.data.table(dat)

            # reformat data
            dat[gcm_id, gcm := i.gcm, on = "gcm_id"]
            dat[ssp_id, ssp := i.ssp, on = "ssp_id"]
            dat[var_id, var := i.var, on = "var_id"]
            dat[, run_id := as.character(run_id)]
            dat[run_id == "1", run_id := "ensembleMean"]
            dat2 <- dcast(dat, gcm + ssp + run_id + period ~ var)
            setnames(dat2, old = c("gcm", "ssp", "run_id", "period"), new = c("GCM", "SSP", "RUN", "PERIOD"))
            bivariate_data <<- dat2
            vis_sg$bivariate(bivariate_data)
            shinyjs::enable("bivariate_download")
          }
        )
      }
    }
  })
  shiny::observeEvent(input$ts_adj_plot, {
    if (shiny::in_devmode()) cat("Event: ts_adj_plot", sep = "\n")
    if (nrow(vis_sg_dt$dt) < 1 & input$input_type == "Map point") {
      showModal(
        modalDialog(
          title = "Warning",
          paste("Please select a map point!"),
          easyClose = TRUE
        )
      )
    } else if (nrow(vis_sg_dt$dt) > 0 & input$input_type == "Map point") {
      timeseries_modal()
    } else if (input$input_type == "FLP Area" & is.null(vstore[["flp_area"]])) {
      showModal(
        modalDialog(
          title = "Warning",
          paste("Please select an FLP area!"),
          easyClose = TRUE
        )
      )
    } else if (input$input_type == "Ecoregion" & is.null(vstore[["ecoregion"]])) {
      showModal(
        modalDialog(
          title = "Warning",
          paste("Please select an ecoregion!"),
          easyClose = TRUE
        )
      )
    } else if ((!is.null(vstore[["flp_area"]]) & input$input_type == "FLP Area") | (!is.null(vstore[["ecoregion"]]) & input$input_type == "Ecoregion")) {
      timeseries_modal()
    }
  })
  shiny::observeEvent(input$timeseries_ok, {
    removeModal()
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
    } else if (nrow(vis_sg_dt$dt) > 0 & input$input_type == "Map point") {
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
          if (!input$ts_adj_plot) {
            datasets <- c("mswx.blend", "cru.gpcc", "climatena")
            gcms <- climr::list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)]
            ssps <- climr::list_ssps()[c(1:3)]
          } else {
            datasets <- input$time_series_dataset
            gcms <- input$time_series_gcms
            ssps <- input$time_series_ssps
          }
          timeseries_data <<- climr::plot_timeSeries_input(xyz, gcms = gcms, ssps = ssps, obs_ts_dataset = datasets)
          show_plots(TRUE)
          vis_sg$timeseries(timeseries_data)
          removeModal()
          shinyjs::enable("timeseries_download")
        }
      )
    }
  })
  shiny::observeEvent(input$plot_ts_flp_er, {
    if (shiny::in_devmode()) cat("Event: plot_ts_flp_er", sep = "\n")
    if (!is.null(input$input_type)) {
      if (input$input_type == "FLP Area" & is.null(vstore[["flp_area"]])) {
        showModal(
          modalDialog(
            title = "Warning",
            paste("Please select an FLP area!"),
            easyClose = TRUE
          )
        )
      } else if (input$input_type == "Ecoregion" & is.null(vstore[["ecoregion"]])) {
        showModal(
          modalDialog(
            title = "Warning",
            paste("Please select an ecoregion!"),
            easyClose = TRUE
          )
        )
      } else if ((!is.null(vstore[["flp_area"]]) & input$input_type == "FLP Area") | (!is.null(vstore[["ecoregion"]]) & input$input_type == "Ecoregion")) {
        show_plots(TRUE)
        withCallingHandlers(
          message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
          warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
          error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
          {
            # set up db query
            region <- vstore[[if (input$input_type == "FLP Area") "flp_area" else "ecoregion"]]
            if (!input$ts_adj_plot) {
              dataset <- paste(dataset_id[dataset %in% c("mswx.blend", "cru.gpcc", "climatena"), dataset_id], collapse = ",")
              gcms <- paste(gcm_id[gcm %in% climr::list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)], gcm_id], collapse = ",")
              ssps <- paste(ssp_id[ssp %in% climr::list_ssps()[c(1:3)], ssp_id], collapse = ",")
            } else {
              dataset <- paste(dataset_id[dataset %in% input$time_series_dataset, dataset_id], collapse = ",")
              gcms <- paste(gcm_id[gcm %in% input$time_series_gcms, gcm_id], collapse = ",")
              ssps <- paste(ssp_id[ssp %in% input$time_series_ssps, ssp_id], collapse = ",")
            }
            code <- paste(climr::variables[Code_Element == input$time_series_element & Time == input$time_series_season, Code], collapse = ",")
            var <- var_id[var == code, var_id]

            query <- sprintf("SELECT * FROM ds_timeseries WHERE region = '%s' 
                            AND (gcm_id IN (%s) OR gcm_id IS NULL)
                            AND (ssp_id IN (%s) OR ssp_id IS NULL)
                            AND (dataset_id IN (%s) OR dataset_id IS NULL)
                            AND var_id = %s
                            ORDER BY gcm_id, period, run_id, ssp_id", region, gcms, ssps, dataset, var)
            dat <- climr:::db_safe_query(query)
            dat <- as.data.table(dat)

            # reformat data
            dat[gcm_id, gcm := i.gcm, on = "gcm_id"]
            dat[ssp_id, ssp := i.ssp, on = "ssp_id"]
            dat[dataset_id, dataset := i.dataset, on = "dataset_id"]
            dat[, run_id := as.character(run_id)]
            dat[run_id == "1", run_id := "ensembleMean"]
            dat <- dat[,-c("gcm_id","ssp_id", "dataset_id", "region", "var_id")]
            setnames(dat, old = c("run_id", "period", "value", "gcm", "ssp", "dataset"), new = c("RUN", "PERIOD", code, "GCM", "SSP", "DATASET"))
            timeseries_data <<- dat
            vis_sg$timeseries(timeseries_data)
            shinyjs::enable("timeseries_download")
          }
        )
      }
    }
  })
  shiny::observeEvent(input$downscale_data_wl, {
    if (shiny::in_devmode()) cat("Event: downscale_data_wl", sep = "\n")
    if (nrow(vis_sg_dt$dt) < 1 & input$input_type == "Map point") {
      showModal(
        modalDialog(
          title = "Warning",
          paste("Please select a map point!"),
          easyClose = TRUE
        )
      )
    } else if (nrow(vis_sg_dt$dt) > 0 & input$input_type == "Map point") {
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
          wl_data <<- climr::plot_WalterLieth_input(xyz, obs_period = climr::list_obs_periods())
          show_plots(TRUE)
          vis_sg$walter_lieth(wl_data)
          shinyjs::enable("wl_download")
        }
      )
    }
  })
  shiny::observeEvent(input$plot_wl_flp_er, {
    if (shiny::in_devmode()) cat("Event: plot_wl_flp_er", sep = "\n")
    if (!is.null(input$input_type)) {
      if (input$input_type == "FLP Area" & is.null(vstore[["flp_area"]])) {
        showModal(
          modalDialog(
            title = "Warning",
            paste("Please select an FLP area!"),
            easyClose = TRUE
          )
        )
      } else if (input$input_type == "Ecoregion" & is.null(vstore[["ecoregion"]])) {
        showModal(
          modalDialog(
            title = "Warning",
            paste("Please select an ecoregion!"),
            easyClose = TRUE
          )
        )
      } else if ((!is.null(vstore[["flp_area"]]) & input$input_type == "FLP Area") | (!is.null(vstore[["ecoregion"]]) & input$input_type == "Ecoregion")) {
        show_plots(TRUE)
        withCallingHandlers(
          message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
          warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
          error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
          {
            # set up db query
            region <- vstore[[if (input$input_type == "FLP Area") "flp_area" else "ecoregion"]]
            codes <- c(sprintf("PPT_%02d", 1:12), sprintf("Tmax_%02d", 1:12), sprintf("Tmin_%02d", 1:12))
            var <- paste(var_id[var %in% codes, var_id], collapse = ",")
            
            query <- sprintf("SELECT * FROM ds_bivariate WHERE region = '%s'
                            AND gcm_id IS NULL
                            AND ssp_id IS NULL
                            AND run_id IS NULL
                            AND var_id IN (%s)
                            ORDER BY period", region, var)
            dat <- climr:::db_safe_query(query)
            dat <- as.data.table(dat)
            
            # reformat data
            dat[var_id, var := i.var, on = "var_id"]
            dat2 <- dcast(dat, period ~ var, value.var = "value")
            setnames(dat2, old = c("period"), new = c("PERIOD"))
            wl_data <<- dat2
            vis_sg$walter_lieth(wl_data)
            shinyjs::enable("wl_download")
          }
        )
      }
    }
  })
  
  # reactive outputs
  output$bivariate_valid_time_x <- shiny::renderUI({
    if (!is.null(input$bivariate_element_x)) {
      shiny::selectInput(
        inputId = "bivariate_time_x",
        label = h6("Season/month:"),
        width = "100%",
        choices = climr::variables[Code_Element == input$bivariate_element_x,] %>% pull(Time)
      )
    }
  })
  
  output$bivariate_valid_time_y <- shiny::renderUI({
    if (!is.null(input$bivariate_element_y)) {
      shiny::selectInput(
        inputId = "bivariate_time_y",
        label = h6("Season/month:"),
        width = "100%",
        choices = climr::variables[Code_Element == input$bivariate_element_y,] %>% pull(Time)
      )
    }
  })
  
  output$time_series_valid_season <- shiny::renderUI({
    if (!is.null(input$time_series_element)) {
      shiny::selectInput(
        inputId = "time_series_season",
        label = h5("Season/month:"),
        width = "100%",
        choices = climr::variables[Code_Element == input$time_series_element,] %>% pull(Time)
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
    
    # render overlay, making sure any old controls are cleared
    mp <- leaflet::leafletProxy("vis_map", deferUntilFlush = FALSE)
    mp |> leaflet::clearGroup("Climate") |> leaflet::hideGroup("Climate") |> leaflet::clearControls()
    shiny::updateActionButton(inputId = "download_overlay", disabled = TRUE)
    if (is.null(vstore[["climatevar"]])) return()
    shiny::updateActionButton(inputId = "download_overlay", disabled = FALSE)
    
    # get scaling
    if (isTRUE(vstore[["vscale"]])) {
      vstore$vscale <- "log1p"
    } else {
      vstore$vscale <- ""
    }

    # set up palettes/breaks
    r <- rast(url)
    bounds <- terra::minmax(r)
    q <- quantile(bounds, c(0.005, 0.995), na.rm = TRUE)
    inc <- diff(q) / 500
    breaks <- seq(q[1] - inc, q[2] + inc, by = inc)
    
    if (grepl("PPT|MSP|PAS", vstore[["element"]])) {
      pal <- RColorBrewer::brewer.pal(9, "YlGnBu")
    } else {
      pal <- rev(RColorBrewer::brewer.pal(11, "RdYlBu"))
    }
    
    mp |> leafem::addGeotiff(
      url = vstore[["climatevar"]],
      group = "Climate",
      layerId = "val",
      project = FALSE,
      colorOptions = leafem::colorOptions(
        palette = pal,
        breaks = breaks,
        na.color = "transparent"
      ),
      imagequery = FALSE,
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
    
    
    # extract data for legend
    if (!(vstore[["element"]] %in% c("elev", "lat"))) {
      legend_title <- climr::variables[Code_Element == vstore[["element"]] & Time == vstore[["time"]], Variable] |> tools::toTitleCase()
      if (grepl("\\u00b0C", legend_title) | grepl("\\u00b0c", legend_title)) {
        legend_title <- stringi::stri_unescape_unicode(legend_title)
      }
      units <- paste0(" ", climr::variables[Code_Element == vstore[["element"]] & Time == vstore[["time"]], Unit])
      if (grepl("\\u00b0C", units)) {
        units <- stringi::stri_unescape_unicode(units)
      }
      if (units == "%") {
        units <- "\\%"
      }
    }
    if (vstore[["element"]] == "elev") {
      legend_title <- paste("Elevation")
      units <- "m"
    }
    
    # label formatters for legend
    inv_log1p_formatter <- labelFormat(
      transform = function(x) round(exp(x) - 1),
      suffix = units
    )
    
    if ((vstore[["vscale"]]) == "log1p") {
      log_bounds <- log1p(pmax(bounds, 0))

      pal_leg <- leaflet::colorNumeric(palette = pal, domain = log_bounds, na.color = "transparent")
      
      leaflet::addLegend(
        mp,
        position = "topright",
        pal = pal_leg,
        values = seq(log_bounds[1], log_bounds[2], length.out = 6),
        title = HTML(sprintf("<div style='width: 100px;'>%s</div>", legend_title)),
        labFormat = inv_log1p_formatter
      )
    } else {
      pal_leg <- leaflet::colorNumeric(palette = pal, domain = bounds, na.color = "transparent")
      
      leaflet::addLegend(
        mp,
        position = "topright",
        pal = pal_leg,
        values = seq(bounds[1], bounds[2], length.out = 6),
        title = HTML(sprintf("<div style='width: 100px;'>%s</div>", legend_title)),
        labFormat = labelFormat(suffix = units)
      )
    }
  })
  shiny::observeEvent(input$download_overlay, {
    if (shiny::in_devmode()) cat("Event: download_overlay", sep = "\n")
    session$sendCustomMessage(type="jsCode", list(code = "window.location.assign('%s');" |> sprintf(vstore[["climatevar"]])))
  })
  
  # reactive outputs
  output$overlay_element <- shiny::renderUI({
    shiny::selectInput(
      inputId = "element",
      label = "Element:",
      choices = {
        dt <- climr_tif[[vstore[["tifsource"]]]]
        unique(dt[!element %in% c("PET", "lat", "CMI", "EXT", "EMT", "MAP", "MAT", "RH", "MSP", "AHM", "SHM"), element])
      },
      selected = "Tave"
    )
  })
  
  output$overlay_period <- shiny::renderUI({
    if (!is.null(input$element)) {
      if (input$element != "elev" & input$element != "lat" & input$element != "PET") {
        shiny::selectInput(
          inputId = "time",
          label = "Season:",
          choices = climr::variables[Code_Element == input$element & Category != "Monthly", Time]
        )
      }
    }
  })
  
  output$scale_adj <- shiny::renderUI({
    if (!is.null(input$element)) {
      if (input$element %in% c("PPT", "CMD", "PAS")) {
        shiny::checkboxInput(
          inputId = "vscale",
          label = "Apply scale adjustment",
          value = reactive({
            req(input$element)
            "ratio" %in% climr::variables[Code_Element == input$element, Type]
          })()
        )
      }
    }
  })
  
  # download handling for plots
  output$bivariate_download <- shiny::downloadHandler(
    filename = function() {
      paste0("bivariate_plot_", Sys.time(), ".png")
    },
    content = function(file) {
      pixelratio <- session$clientData$pixelratio
      width  <- session$clientData$output_bivariate_plot_width
      height <- session$clientData$output_bivariate_plot_height
      
      png(file, width = width*pixelratio*3, height = height*pixelratio*3, res = 120*pixelratio)
      print(climr::plot_bivariate(
              X = bivariate_data,
              xvar = climr::variables[Code_Element == input$bivariate_element_x & Time == input$bivariate_time_x, Code],
              yvar = climr::variables[Code_Element == input$bivariate_element_y & Time == input$bivariate_time_y, Code],
              period_focal = input$bivariate_period,
              interactive = FALSE
              ))
      dev.off()
    }
  )
  
  output$wl_download <- shiny::downloadHandler(
    filename = function() {
      paste0("walter_lieth_plot_", Sys.time(), ".png")
    },
    content = function(file) {
      pixelratio <- session$clientData$pixelratio
      width  <- session$clientData$output_wl_plot_width
      height <- session$clientData$output_wl_plot_height
      
      png(file, width = width*pixelratio*3, height = height*pixelratio*3, res = 120*pixelratio)
      print(climr::plot_WalterLieth(
        X = wl_data,
        diurnal = input$wl_diurnal,
        obs_period = input$wl_obs_period
      ))
      dev.off()
    }
  )
  
  output$timeseries_download <- shiny::downloadHandler(
    filename = function() {
      paste0("timeseries_plot_", Sys.time(), ".png")
    },
    content = function(file) {
      pixelratio <- session$clientData$pixelratio
      width  <- session$clientData$output_timeseries_plot_width
      height <- session$clientData$output_timeseries_plot_height
      
      png(file, width = width*pixelratio*2, height = height*pixelratio*2, res = 120*pixelratio)
      print(climr::plot_timeSeries(
        X = timeseries_data,
        var1 = climr::variables[Code_Element == input$time_series_element & Time == input$time_series_season, Code],
        obs_ts_dataset = if (!input$ts_adj_plot) c("mswx.blend", "cru.gpcc", "climatena") else input$time_series_datasetinput$time_series_dataset
        )
      )
      dev.off()
    }
  )
}