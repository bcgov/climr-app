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
  bivariate_data <- c()
  timeseries_data <- c()
  wl_data <- c()
  
  # mapping for database connections
  gcm_id <- data.table(gcm = list_gcms(), gcm_id = seq_along(list_gcms()))
  ssp_id <- data.table(ssp = list_ssps(), ssp_id = seq_along(list_ssps()))
  var_id <- data.table(var = list_vars(), var_id = seq_along(list_vars()))
  dataset_id <- data.table(dataset = c("mswx.blend","cru.gpcc","climatena"), dataset_id = 1:3)
  
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
    vscale = NULL,
    flp_area = NULL
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
    vis_by_map(FALSE)
    leaflet::removeImage(vis_mp, layerId = "val")
    leaflet::clearControls(vis_mp)
    leaflet::hideGroup(vis_mp, "Climate")
    session$sendCustomMessage("clear_district","Waddles")
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
    output$bivariate_plot <- renderPlot({ NULL })
    output$timeseries_plot <- renderPlot({ NULL })
    output$wl_plot <- renderPlot({ NULL })
  })
  
  shiny::observeEvent(input$dist_click,{
    vstore[["flp_area"]] <- input$dist_click
  })
  
  # ---- Visualization data events
  timeseries_modal <- function() {
    shiny::showModal(
      shiny::modalDialog(
        title = "Simulated Data Options", size = "l", fade = FALSE, class = "modal-dialog-scrollable",
        shiny::checkboxGroupInput(
          inputId = "time_series_gcms",
          label = h5("Choose GCMs:"),
          width = "100%",
          inline = TRUE,
          choices = climr::list_gcms(),
          selected = climr::list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)]
        ),
        shiny::checkboxGroupInput(
          inputId = "time_series_ssps",
          label = h5("Choose SSPs:"),
          width = "100%",
          inline = TRUE,
          choices = climr::list_ssps(),
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
    if (input$input_type == "Map point") {
      vis_by_map(TRUE)
      session$sendCustomMessage("clear_district","Waddles")
    } else if (input$input_type == "FLP Area") {
      dat <- list(url = "https://tileserver.thebeczone.ca/data/flp_bnd/{z}/{x}/{y}.pbf", name = "flp", id = "ORG_UNIT")
      session$sendCustomMessage("addRegionTile",dat)
      session$sendCustomMessage("reset_district","Luna")
    } else {
      session$sendCustomMessage("clear_district","Waddles")
      vis_by_map(FALSE)
      vis_sg$clear_all(vis_mp)
    }
    show_plot <- input$input_type != ""
    session$sendCustomMessage("toggle-plot", show_plot)
  })
  
  # ---- Visualization Plot events
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
          bivariate_data <- climr::plot_bivariate_input(xyz)
          vis_sg$bivariate(bivariate_data)
        }
      )
    }
  })
  shiny::observeEvent(input$plot_bivariate_flp, {
    if (shiny::in_devmode()) cat("Event: plot_bivariate_flp", sep = "\n")
    if (!is.null(input$input_type)) {
      if (!is.null(vstore[["flp_area"]]) & input$input_type == "FLP Area") {
        withCallingHandlers(
          message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
          warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
          error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
          {
            # set up db query
            region <- vstore[["flp_area"]]
            gcms <- paste(gcm_id[,gcm_id], collapse = ",")
            ssps <- paste(ssp_id[,ssp_id], collapse = ",")
            code_x <- paste(climr::variables[Code_Element == input$bivariate_element_x & Time == input$bivariate_time_x, Code], collapse = ",")
            var_x <- var_id[var == code_x, var_id]
            code_y <- paste(climr::variables[Code_Element == input$bivariate_element_y & Time == input$bivariate_time_y, Code], collapse = ",")
            var_y <- var_id[var == code_y, var_id]

            query <- sprintf("SELECT * FROM ds_bivariate WHERE region = '%s'
                            AND (gcm_id IN (%s) OR gcm_id IS NULL)
                            AND (ssp_id IN (%s) OR ssp_id IS NULL)
                            AND (var_id = %s OR var_id = %s)
                            ORDER BY gcm_id, period, run_id, ssp_id", region, gcms, ssps, var_x, var_y)
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
            vis_sg$bivariate(dat2)
          }
        )
      }
    }
  })
  shiny::observeEvent(input$sim_data_ts, {
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
      timeseries_modal()
    } else if (input$input_type == "FLP Area" & is.null(vstore[["flp_area"]])) {
      showModal(
        modalDialog(
          title = "Warning",
          paste("Please select an FLP area!"),
          easyClose = TRUE
        )
      )
    } else if (input$input_type == "FLP Area" & !is.null(vstore[["flp_area"]])) {
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
          if (!input$sim_data_ts) {
            gcms <- climr::list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)]
            ssps <- climr::list_ssps()[c(1:3)]
          } else {
            gcms <- input$time_series_gcms
            ssps <- input$time_series_ssps
          }
          timeseries_data <- climr::plot_timeSeries_input(xyz, gcms = gcms, ssps = ssps, obs_ts_dataset = c("mswx.blend", "cru.gpcc", "climatena"))
          vis_sg$timeseries(timeseries_data)
          removeModal()
        }
      )
    }
  })
  shiny::observeEvent(input$plot_ts_flp, {
    if (shiny::in_devmode()) cat("Event: plot_ts_flp", sep = "\n")
    if (!is.null(input$input_type)) {
      if (!is.null(vstore[["flp_area"]]) & input$input_type == "FLP Area") {
        withCallingHandlers(
          message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
          warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
          error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
          {
            # set up db query
            region <- vstore[["flp_area"]]
            if (!input$sim_data_ts) {
              gcms <- paste(gcm_id[gcm %in% climr::list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)], gcm_id], collapse = ",")
              ssps <- paste(ssp_id[ssp %in% climr::list_ssps()[c(1:3)], ssp_id], collapse = ",")
            } else {
              gcms <- paste(gcm_id[gcm %in% input$time_series_gcms, gcm_id], collapse = ",")
              ssps <- paste(ssp_id[ssp %in% input$time_series_ssps, ssp_id], collapse = ",")
            }
            dataset <- paste(dataset_id[dataset %in% input$time_series_dataset, dataset_id], collapse = ",")
            code <- paste(climr::variables[Code_Element == input$time_series_element & Time == input$time_series_season, Code], collapse = ",")
            var <- var_id[var == code, var_id]

            query <- sprintf("SELECT * FROM ds_timeseries WHERE region = '%s' 
                            AND (gcm_id IN (%s) OR gcm_id IS NULL)
                            AND (ssp_id IN (%s) OR ssp_id IS NULL)
                            AND (dataset_id = %s OR dataset_id IS NULL)
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
            vis_sg$timeseries(dat)
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
          wl_data <- climr::plot_WalterLieth_input(xyz, obs_period = climr::list_obs_periods())
          vis_sg$walter_lieth(wl_data)
        }
      )
    }
  })
  shiny::observeEvent(input$plot_wl_flp, {
    if (shiny::in_devmode()) cat("Event: plot_wl_flp", sep = "\n")
    if (!is.null(input$input_type)) {
      if (!is.null(vstore[["flp_area"]]) & input$input_type == "FLP Area") {
        withCallingHandlers(
          message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
          warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
          error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
          {
            # set up db query
            region <- vstore[["flp_area"]]
            codes <- c(sprintf("PPT_%02d", 1:12), sprintf("Tmax_%02d", 1:12), sprintf("Tmin_%02d", 1:12))
            var <- paste(var_id[var %in% codes, var_id], collapse = ",")
            
            query <- sprintf("SELECT * FROM ds_bivariate WHERE region = '%s' 
                            AND var_id IN (%s)
                            AND run_id = 1
                            ORDER BY period", region, var)
            dat <- climr:::db_safe_query(query)
            dat <- as.data.table(dat)
            
            # reformat data
            dat[var_id, var := i.var, on = "var_id"]
            dat[, run_id := as.character(run_id)]
            dat[run_id == "1", run_id := "ensembleMean"]
            dat2 <- dcast(dat, run_id + period ~ var)
            setnames(dat2, old = c("run_id", "period"), new = c("RUN", "PERIOD"))
            vis_sg$walter_lieth(dat2)
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
        label = h6("Choose season/month:"),
        width = "100%",
        choices = setNames(climr::variables[Code_Element == input$bivariate_element_x,] %>% pull(Time), time_labels[climr::variables[Code_Element == input$bivariate_element_x,] %>% pull(Time)])
      )
    }
  })
  
  output$bivariate_valid_time_y <- shiny::renderUI({
    if (!is.null(input$bivariate_element_y)) {
      shiny::selectInput(
        inputId = "bivariate_time_y",
        label = h6("Choose season/month:"),
        width = "100%",
        choices = setNames(climr::variables[Code_Element == input$bivariate_element_y,] %>% pull(Time), time_labels[climr::variables[Code_Element == input$bivariate_element_y,] %>% pull(Time)])
      )
    }
  })
  
  output$time_series_valid_season <- shiny::renderUI({
    if (!is.null(input$time_series_element)) {
      shiny::selectInput(
        inputId = "time_series_season",
        label = h5("Choose season/month:"),
        width = "100%",
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
    
    # render overlay, making sure any old controls are cleared
    mp <- leaflet::leafletProxy("vis_map", deferUntilFlush = FALSE)
    mp |> leaflet::clearGroup("Climate") |> leaflet::hideGroup("Climate") |> leaflet::clearControls()
    shiny::updateActionButton(inputId = "download_overlay", disabled = TRUE)
    if (is.null(vstore[["climatevar"]])) return()
    shiny::updateActionButton(inputId = "download_overlay", disabled = FALSE)
    
    # get scaling
    if (isTRUE(vstore[["vscale"]])) {
      vstore$vscale <- "log2"
    } else {
      vstore$vscale <- ""
    }
    # set up palettes/breaks
    r <- rast(url)
    bounds <- minmax(r)
    q <- quantile(bounds, c(0.005, 0.995), na.rm = TRUE)
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
    pal_leg <- leaflet::colorNumeric(palette = pal, domain = bounds, na.color = "transparent")
    leaflet::addLegend(mp, position = "topright", pal = pal_leg, values = bounds, title = HTML(sprintf("<div style='width: 100px;'>%s</div>", legend_title)), labFormat = labelFormat(suffix = units))
    
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
      },
      selected = "Tave"
    )
  })
  
  output$overlay_period <- shiny::renderUI({
    if (!is.null(input$element)) {
      if (input$element != "elev" & input$element != "lat" & input$element != "PET") {
        shiny::selectInput(
          inputId = "time",
          label = "Choose season/months:",
          choices = climr::variables[Code_Element == input$element, Time]
        )
      }
    }
  })
  
  output$scale_adj <- shiny::renderUI({
    if (!is.null(input$element)) {
      if ("ratio" %in% climr::variables[Code_Element == input$element, Type]) {
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
  
}