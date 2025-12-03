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
  ssp_id2 <- data.table(ssp = c("historical", list_ssps()[1:3]), ssp_id = seq_along(list_ssps()))
  type_id <- data.table(type = c("ensmin", "ensmax", "ensmean", "dataset"), type_id = 1:4)
  
  defaults <- list(
    ts_datasets = c("mswx.blend"),
    ts_gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
    ts_ssps = list_ssps()[1:3]
  )
  
  vstore <- reactiveValues(
    tifsource = NULL,
    rescale = FALSE,
    time = NULL,
    element = NULL,
    climatevar = NULL,
    vscale = NULL,
    flp_area = NULL,
    ecoregion = NULL,
    ts_datasets = c("mswx.blend", "cru.gpcc", "climatena"),
    ts_gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
    ts_ssps = list_ssps()[1:3],
    ts_time = "Annual",
    biv_time_x = "Annual",
    biv_time_y = "Annual"
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
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "PDF",
          tags$iframe(
            src = "How_to_use_visualization.pdf#toolbar=0",
            width = "100%",
            height = "700px",
            style = "border:none;"
          )
        ),
        tabPanel(
          "Video",
          accordion(
            multiple = FALSE,
            open = FALSE,
            id = "vis_videos",
            
            accordion_panel(
              title = h5("How to use the climate map overlays:"),
              tags$div(style = "margin-top: 10px;"),
              tags$iframe(
                style = "width: 100%; height: 600px; border: none;",
                src = "https://youtube.com/embed/hY8VbLCBA3k"
              ),
              value = "vis_vid1"
            ),
            
            accordion_panel(
              title = h5("How to use climate plots:"),
              tags$div(style = "margin-top: 10px;"),
              tags$iframe(
                style = "width: 100%; height: 600px; border: none;",
                src = "https://youtube.com/embed/SiIbLHNgvrw"
              ),
              value = "vis_vid2"
            ),
          )
        )
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
    show_plots(FALSE)
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
    
    # reset time series defaults
    vstore[["ts_datasets"]] == defaults[["ts_datasets"]]
    vstore[["ts_gcms"]] == defaults[["ts_gcms"]]
    vstore[["ts_ssps"]] == defaults[["ts_ssps"]]
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
    
    # reset time series defaults
    vstore[["ts_datasets"]] <- defaults[["ts_datasets"]]
    vstore[["ts_gcms"]] <- defaults[["ts_gcms"]]
    vstore[["ts_ssps"]] <- defaults[["ts_ssps"]]
    
    show_plots(FALSE)
  })
  
  # ---- Visualization data events
  timeseries_modal <- function() {
    shiny::showModal(
      shiny::modalDialog(
        title = "Adjust Plot Options", size = "l", fade = FALSE, class = "modal-dialog-scrollable",
        shiny::checkboxGroupInput(
          inputId = "time_series_dataset",
          label = h5(
            tags$span(HTML("Choose <a href='https://vonuma.com/climr-docs/Definitions.html#glossary-of-terms' target='_blank'>observational time-series dataset</a>:"))
          ),
          width = "100%",
          inline = TRUE,
          choices = c("MSWX Blend" = "mswx.blend", "ClimateNA" = "climatena", "Climatic Research Unit / Global Precipitation Climatology Centre" = "cru.gpcc"),
          selected = vstore[["ts_datasets"]]
        ),
        shiny::conditionalPanel(
          condition = "input.input_type == 'Map point'",
          shiny::checkboxGroupInput(
            inputId = "time_series_gcms",
            label = h5(
              tags$span(HTML("Choose <a href='https://vonuma.com/climr-docs/Definitions.html#glossary-of-terms' target='_blank'>GCMs</a>:"))
            ),
            width = "100%",
            inline = TRUE,
            choices = climr::list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
            selected = vstore[["ts_gcms"]]
          )
        ),
        shiny::checkboxGroupInput(
          inputId = "time_series_ssps",
          label = h5(
            tags$span(HTML("Choose <a href='https://vonuma.com/climr-docs/Definitions.html#glossary-of-terms' target='_blank'>SSPs</a>:"))
          ),
          width = "100%",
          inline = TRUE,
          choices = climr::list_ssps()[c(1:3)],
          selected = vstore[["ts_ssps"]]
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
    
    # reset time series defaults
    vstore[["ts_datasets"]] <- defaults[["ts_datasets"]]
    vstore[["ts_gcms"]] <- defaults[["ts_gcms"]]
    vstore[["ts_ssps"]] <- defaults[["ts_ssps"]]
    
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
  shiny::observeEvent(input$time_series_season, {
    vstore[["ts_time"]] <- input$time_series_season
  })
  shiny::observeEvent(input$bivariate_time_x, {
    vstore[["biv_time_x"]] <- input$bivariate_time_x
  })
  shiny::observeEvent(input$bivariate_time_y, {
    vstore[["biv_time_y"]] <- input$bivariate_time_y
  })
  shiny::observeEvent(input$ts_plot_info, {
    showModal(modalDialog(
      title = "What does this plot mean?",
      easyClose = TRUE,
      size = "xl",
      shiny::p("Time series plots of 20th and 21st century climate change for user-selected locations and climate variables."),
      shiny::p("Purposes of the plot:"),
      shiny::tags$ul(
        shiny::tags$li("View differences in interannual variability and climate change trends among global climate models (GCMs)"),
        shiny::tags$li("View the differences between multiple simulations of each model"),
        shiny::tags$li("Compare simulated and observed climate change from 1901 to present"),
        shiny::tags$li("Compare time series of two different variables")
      ),
      shiny::p("All global climate model anomalies are bias-corrected to the 1961-1990 reference period normals."),
      tags$img(src = "timeseries_plot_info.png", style = "max-width:100%; height:auto; margin-top:20px;")
    ))
  })
  shiny::observeEvent(input$biv_plot_info, {
    showModal(modalDialog(
      title = "What does this plot mean?",
      easyClose = TRUE,
      size = "xl",
      shiny::p("The bivariate plots show projections of 21st century climate change for user-selected climate variables. The main purposes of the plot are to:"),
      shiny::tags$ul(
        shiny::tags$li("Show differences in climate change trends among global climate models (GCMs);"),
        shiny::tags$li("Show the differences between multiple simulations of each model; and"),
        shiny::tags$li("Compare simulated climate change to observed climate change in the 2001-2020 period.")
      ),
      shiny::p("We recommend using temperature variables as the x-axis. This provides a representation of how the y-axis variable is projected to change in proportion to regional climate heating."),
      shiny::p("Details:"),
      shiny::tags$ul(
        shiny::tags$li("All climate changes are relative to the mean climate of the 1961-1990 period."),
        shiny::tags$li("The observed climate change in the 2001-2020 period is obtained from the MSWX-blend dataset."),
        shiny::tags$li("Change values are for the SSP2-4.5 emissions scenario only. We have not provided other scenarios as they tend to differ only in magnitude rather than trends and variation, which are the focus of this plot.")
      ),
      tags$img(src = "bivariate_plot_info.png", style = "max-width:100%; height:auto; margin-top:20px;")
    ))
  })
  shiny::observeEvent(input$wl_plot_info, {
    showModal(modalDialog(
      title = "What does this plot mean?",
      easyClose = TRUE,
      size = "xl",
      shiny::p("The Walter-Lieth climate diagram was developed by German climatologists Heinrich Walter and Helmut Lieth in the 1950s–60s as part of their efforts to standardize the visualization of climate data for ecological zoning and global vegetation classification. The diagrams are a simple way to graphically represent seasonal patterns of temperature and precipitation at a given location, and to compare the climates of different regions."),
      shiny::p("The diagram provides an overview of climate seasonality using a dual-axis plot."),
      shiny::tags$ul(
        shiny::tags$li(HTML("<strong>Temperature</strong> (°C) is plotted on the left vertical axis.")),
        shiny::tags$li(HTML("<strong>Precipitation</strong> (mm) is plotted on the right, typically at a scale where <strong>2 mm of precipitation corresponds to 1°C</strong> (the 1:2 ratio)."))
      ),
      shiny::p("The 1:2 scaling allows for a simplified identification of arid periods. However, this is only a rough proxy for climatic moisture deficit and does not directly integrate potential evapotranspiration and relevant factors like wind, humidity, or radiation. Therefore, the diagram should not be interpreted as a quantitative water balance diagram."),
      tags$img(src = "walter_lieth_plot_info1.png", style = "max-width:100%; height:auto; margin-top:20px;"),
      shiny::p("How to interpret climatic trends:"),
      tags$img(src = "walter_lieth_plot_info2.png", style = "max-width:100%; height:auto; margin-top:20px;")
      
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
    # update time series input
    vstore[["ts_datasets"]] <- input$time_series_dataset
    vstore[["ts_gcms"]] <- input$time_series_gcms
    vstore[["ts_ssps"]] <- input$time_series_ssps
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
          timeseries_data <<- climr::plot_timeSeries_input(xyz, gcms = climr::list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)], ssps = climr::list_ssps()[c(1:3)], obs_ts_dataset = c("mswx.blend", "cru.gpcc", "climatena"))
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
      } else if (!is.null(vstore[["flp_area"]]) & input$input_type == "FLP Area") {
        show_plots(TRUE)
        withCallingHandlers(
          message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
          warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
          error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
          {
            # set up db query
            region <- vstore[["flp_area"]]
            if (!input$ts_adj_plot) {
              dataset <- paste(dataset_id[dataset %in% c("mswx.blend", "cru.gpcc", "climatena"), dataset_id], collapse = ",")
              ssps <- paste(ssp_id2[ssp %in% climr::list_ssps()[c(1:3)], ssp_id], collapse = ",")
            } else {
              dataset <- paste(dataset_id[dataset %in% input$time_series_dataset, dataset_id], collapse = ",")
              ssps <- paste(ssp_id2[ssp %in% input$time_series_ssps, ssp_id], collapse = ",")
            }
            code <- paste(climr::variables[Code_Element == input$time_series_element & Time == input$time_series_season, Code], collapse = ",")
            var <- var_id[var == code, var_id]
            
            ## datasets
            query <- sprintf("SELECT region, dataset_id, type_id, var_id, unnest(vals) value 
                            FROM flp_ts_datasets 
                            WHERE region = '%s'
                            AND var_id = %s", region, var)
            dat_ds <- climr:::db_safe_query(query)
            dat_ds <- as.data.table(dat_ds)
            
            # add periods
            dataset_years <- c(1901:2024, 1901:2022, NA, NA, 1902:2023, NA, NA) # NEED TO CHANGE THIS TO BE DYNAMIC
            dat_ds[, period := dataset_years]
            
            # reformat data
            dat_ds[, dataset_id %in% dataset]
            dat_ds[dataset_id, dataset := i.dataset, on = "dataset_id"]
            dat_ds[type_id, type := i.type, on = "type_id"]
            dat_ds <- dat_ds[,-c("var_id", "dataset_id", "type_id")]
            setnames(dat_ds, old = c("region", "dataset", "type", "value", "period"), new = c("REGION", "DATASET", "TYPE", "VAL", "PERIOD"))
            
            ## historical
            query <- sprintf("SELECT region, ssp_id, type_id, var_id, unnest(vals) value 
                            FROM flp_ts_hist 
                            WHERE region = '%s'
                            AND var_id = %s", region, var)
            dat_hist <- climr:::db_safe_query(query)
            dat_hist <- as.data.table(dat_hist)
            
            # add periods
            years_hist <- rep(sort(c(seq(1855, 2015, by=5), 2014)), 3)
            dat_hist[, period := years_hist]
            
            # reformat data
            dat_hist[ssp_id2, ssp := i.ssp, on = "ssp_id"]
            dat_hist[type_id, type := i.type, on = "type_id"]
            dat_hist <- dat_hist[,-c("var_id", "ssp_id", "type_id")]
            setnames(dat_hist, old = c("region", "ssp", "type", "value", "period"), new = c("REGION", "SSP", "TYPE", "VAL", "PERIOD"))
            
            ## projected
            query <- sprintf("SELECT region, ssp_id, type_id, var_id, unnest(vals) value 
                            FROM flp_ts_proj 
                            WHERE region = '%s'
                            AND var_id = %s", region, var)
            dat_proj <- climr:::db_safe_query(query)
            dat_proj <- as.data.table(dat_proj)
            
            # add periods
            years_proj <- rep(c(2014, seq(2015, 2100, by=5)), 3*3)
            dat_proj[, period := years_proj]
            
            # reformat data
            dat_proj[, ssp_id %in% ssps]
            dat_proj[ssp_id2, ssp := i.ssp, on = "ssp_id"]
            dat_proj[type_id, type := i.type, on = "type_id"]
            dat_proj <- dat_proj[,-c("var_id", "ssp_id", "type_id")]
            setnames(dat_proj, old = c("region", "ssp", "type", "value", "period"), new = c("REGION", "SSP", "TYPE", "VAL", "PERIOD"))
            
            # join
            dat <- rbindlist(list(dat_ds, dat_hist, dat_proj), fill = TRUE)
            
            timeseries_data <<- dat
            vis_sg$timeseries(timeseries_data)
            shinyjs::enable("timeseries_download")
          }
        )
      } else if (!is.null(vstore[["ecoregion"]]) & input$input_type == "Ecoregion") {
        show_plots(TRUE)
        withCallingHandlers(
          message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
          warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
          error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
          {
            # set up db query
            region <- vstore[["ecoregion"]]
            if (!input$ts_adj_plot) {
              dataset <- paste(dataset_id[dataset %in% c("mswx.blend", "cru.gpcc", "climatena"), dataset_id], collapse = ",")
              ssps <- paste(ssp_id2[ssp %in% climr::list_ssps()[c(1:3)], ssp_id], collapse = ",")
            } else {
              dataset <- paste(dataset_id[dataset %in% input$time_series_dataset, dataset_id], collapse = ",")
              ssps <- paste(ssp_id2[ssp %in% input$time_series_ssps, ssp_id], collapse = ",")
            }
            code <- paste(climr::variables[Code_Element == input$time_series_element & Time == input$time_series_season, Code], collapse = ",")
            var <- var_id[var == code, var_id]
            
            ## datasets
            query <- sprintf("SELECT region, dataset_id, type_id, var_id, unnest(vals) value 
                            FROM ecor_ts_datasets 
                            WHERE region = '%s'
                            AND var_id = %s", region, var)
            dat_ds <- climr:::db_safe_query(query)
            dat_ds <- as.data.table(dat_ds)
            
            # add periods
            dataset_years <- c(1901:2024, 1901:2022, NA, NA, 1902:2023, NA, NA) # NEED TO CHANGE THIS TO BE DYNAMIC
            dat_ds[, period := dataset_years]
            
            # reformat data
            dat_ds[, dataset_id %in% dataset]
            dat_ds[dataset_id, dataset := i.dataset, on = "dataset_id"]
            dat_ds[type_id, type := i.type, on = "type_id"]
            dat_ds <- dat_ds[,-c("var_id", "dataset_id", "type_id")]
            setnames(dat_ds, old = c("region", "dataset", "type", "value", "period"), new = c("REGION", "DATASET", "TYPE", "VAL", "PERIOD"))
            
            ## historical
            query <- sprintf("SELECT region, ssp_id, type_id, var_id, unnest(vals) value 
                            FROM ecor_ts_hist 
                            WHERE region = '%s'
                            AND var_id = %s", region, var)
            dat_hist <- climr:::db_safe_query(query)
            dat_hist <- as.data.table(dat_hist)
            
            # add periods
            years_hist <- rep(sort(c(seq(1855, 2015, by=5), 2014)), 3)
            dat_hist[, period := years_hist]
            
            # reformat data
            dat_hist[ssp_id2, ssp := i.ssp, on = "ssp_id"]
            dat_hist[type_id, type := i.type, on = "type_id"]
            dat_hist <- dat_hist[,-c("var_id", "ssp_id", "type_id")]
            setnames(dat_hist, old = c("region", "ssp", "type", "value", "period"), new = c("REGION", "SSP", "TYPE", "VAL", "PERIOD"))
            
            ## projected
            query <- sprintf("SELECT region, ssp_id, type_id, var_id, unnest(vals) value 
                            FROM ecor_ts_proj 
                            WHERE region = '%s'
                            AND var_id = %s", region, var)
            dat_proj <- climr:::db_safe_query(query)
            dat_proj <- as.data.table(dat_proj)
            
            # add periods
            years_proj <- rep(c(2014, seq(2015, 2100, by=5)), 3*3)
            dat_proj[, period := years_proj]
            
            # reformat data
            dat_proj[, ssp_id %in% ssps]
            dat_proj[ssp_id2, ssp := i.ssp, on = "ssp_id"]
            dat_proj[type_id, type := i.type, on = "type_id"]
            dat_proj <- dat_proj[,-c("var_id", "ssp_id", "type_id")]
            setnames(dat_proj, old = c("region", "ssp", "type", "value", "period"), new = c("REGION", "SSP", "TYPE", "VAL", "PERIOD"))
            
            # join
            dat <- rbindlist(list(dat_ds, dat_hist, dat_proj), fill = TRUE)
            
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
      available_time <- climr::variables[Code_Element == input$bivariate_element_x,] %>% pull(Time)
      selected_time <- if (vstore[["biv_time_x"]] %in% available_time) {
        vstore[["biv_time_x"]]
      } else {
        "Annual"
      }
      shiny::selectInput(
        inputId = "bivariate_time_x",
        label = h6("Season/month:"),
        width = "100%",
        choices = climr::variables[Code_Element == input$bivariate_element_x,] %>% pull(Time),
        selected = selected_time
      )
    }
  })
  
  output$bivariate_valid_time_y <- shiny::renderUI({
    if (!is.null(input$bivariate_element_y)) {
      available_time <- climr::variables[Code_Element == input$bivariate_element_y,] %>% pull(Time)
      selected_time <- if (vstore[["biv_time_y"]] %in% available_time) {
        vstore[["biv_time_y"]]
      } else {
        "Annual"
      }
      shiny::selectInput(
        inputId = "bivariate_time_y",
        label = h6("Season/month:"),
        width = "100%",
        choices = climr::variables[Code_Element == input$bivariate_element_y,] %>% pull(Time),
        selected = selected_time
      )
    }
  })
  
  output$time_series_valid_season <- shiny::renderUI({
    if (!is.null(input$time_series_element)) {
      available_time <- climr::variables[Code_Element == input$time_series_element,] %>% pull(Time)
      selected_time <- if (vstore[["ts_time"]] %in% available_time) {
        vstore[["ts_time"]]
      } else {
        "Annual"
      }
      shiny::selectInput(
        inputId = "time_series_season",
        label = h5("Season/month:"),
        width = "100%",
        choices = climr::variables[Code_Element == input$time_series_element,] %>% pull(Time),
        selected = selected_time
      )
    }
  })
  
  # ---- Visualization Overlay events
  shiny::observe({
    if (!input$show_overlay_controls) {
      leaflet::removeImage(vis_mp, layerId = "val")
      leaflet::clearControls(vis_mp)
    }
  })
  load_overlay <- function(data) {
    # render overlay, making sure any old controls are cleared
    mp <<- leaflet::leafletProxy("vis_map", deferUntilFlush = FALSE)
    mp |> leaflet::clearGroup("Climate") |> leaflet::hideGroup("Climate") |> leaflet::clearControls()
    shiny::updateActionButton(inputId = "download_overlay", disabled = TRUE)
    shiny::updateActionButton(inputId = "rescale_overlay", disabled = TRUE)
    if (is.null(data)) return()
    shiny::updateActionButton(inputId = "download_overlay", disabled = FALSE)
    shiny::updateActionButton(inputId = "rescale_overlay", disabled = FALSE)
    
    # get scaling
    vstore[["vscale"]] <- input$vscale
    if (isTRUE(vstore[["vscale"]]) & input$element %in% climr::variables[Type == "ratio", Code_Element]) {
      vstore[["vscale"]] <- "log1p"
    } else {
      vstore[["vscale"]] <- ""
    }
      
    # set up palettes/breaks
    name <- climr::variables[Code_Element == input$element & Time == input$time, Code]
    if (input$time == "Annual") {
      matching_vars <- c(input$element, paste0(input$element, "_an"))
      if (input$overlay_res == "800m") {
        bounds <- as.numeric(
          overlay_800m[variable %in% matching_vars, .(lower_bound, upper_bound)][1]
        )
      } else if (input$overlay_res == "2500m") {
        bounds <- as.numeric(
          overlay_2500m[variable %in% matching_vars, .(lower_bound, upper_bound)][1]
        )
      }
      
    } else {
      if (input$overlay_res == "800m") {
        bounds <- as.numeric(overlay_800m[variable == name, .(lower_bound, upper_bound)][1])
      } else if (input$overlay_res == "2500m") {
        bounds <- as.numeric(overlay_2500m[variable == name, .(lower_bound, upper_bound)][1])
      } 
    }
    inc <- diff(bounds) / 500
    breaks <- seq(bounds[1] - inc, bounds[2] + inc, by = inc)
    
    if (grepl("PPT|MSP|PAS", vstore[["element"]])) {
      pal <- RColorBrewer::brewer.pal(9, "YlGnBu")
    } else {
      pal <- rev(RColorBrewer::brewer.pal(11, "RdYlBu"))
    }
    
    mp |> clearImages() |> leafem::addGeotiff(
      url = data,
      group = "Climate",
      layerId = "val",
      resolution = 200,
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
    
    if (vstore[["vscale"]] == "log1p") {
      session$sendCustomMessage(type="updateClimatePalette", list(
        category = "image", layerId = "val", vscale = vstore[["vscale"]], colorOptions = leafem::colorOptions(
          palette = pal,
          na.color = "transparent"
        ),
        bounds = if (!vstore[["rescale"]]) bounds else NULL
      ))
    }
    
    shiny::showNotification("Rendering %s values" |> sprintf(vstore[["element"]]), duration = 5)
    
    get_legend(bounds)
  }
  get_legend <- function(bounds) {
    # clear any previous controls
    mp |> leaflet::clearControls()
    
    # extract data for legend
    if (!(vstore[["element"]] %in% c("elev"))) {
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
    
    # colour palette
    if (grepl("PPT|MSP|PAS", vstore[["element"]])) {
      pal <- RColorBrewer::brewer.pal(9, "YlGnBu")
    } else {
      pal <- rev(RColorBrewer::brewer.pal(11, "RdYlBu"))
    }
    
    if ((vstore[["vscale"]]) == "log1p") {
      if (!vstore[["rescale"]]) {
        log_bounds <- log1p(pmax(bounds, 0))
      } else {
        log_bounds <- bounds
      }
      
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
  }
  shiny::observeEvent(input$overlay_res, {
    if (shiny::in_devmode()) cat("Event: overlay_res", sep = "\n")
    if (!is.null(input$overlay_res)) {
      if (input$overlay_res == "800m") {
        vstore[["tifsource"]] <- names(climr_tif)[2]
      } else if (input$overlay_res == "2500m") {
        vstore[["tifsource"]] <- names(climr_tif) |> head(1)
      }
    }
  })
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
        return(time_labels_season[label])
      } else {
        return()
      }
    }
    if (input$overlay_res == "800m") {
      url <- dt[element == input$element & time_code == get_time_code(input$time) & grepl("cropped", name), url]
    } else {
      url <- dt[element == input$element & time_code == get_time_code(input$time), url] 
    }
    if (length(url) == 1) {
      vstore[["climatevar"]] <- url
    } else {
      vstore[["climatevar"]] <- NULL
    }
    
    # reset rescale
    vstore[["rescale"]] <- FALSE
    
    load_overlay(vstore[["climatevar"]])
  })
  shiny::observeEvent(input$download_overlay, {
    if (shiny::in_devmode()) cat("Event: download_overlay", sep = "\n")
    session$sendCustomMessage(type="jsCode", list(code = "window.location.assign('%s');" |> sprintf(vstore[["climatevar"]])))
  })
  shiny::observeEvent(input$rescale_overlay, {
    if (grepl("PPT|MSP|PAS", vstore[["element"]])) {
      pal <- RColorBrewer::brewer.pal(9, "YlGnBu")
    } else {
      pal <- rev(RColorBrewer::brewer.pal(11, "RdYlBu"))
    }
    if ("ratio" %in% climr::variables[Code_Element == input$element & Time == input$time, Type]) {
      if (input$vscale) {
        vstore[["vscale"]] <- "log1p"
      } else {
        vstore[["vscale"]] <- ""
      }
    } else {
      vstore[["vscale"]] <- FALSE
    }
    
    # set rescale
    vstore[["rescale"]] <- TRUE
    
    session$sendCustomMessage(type="updateClimatePalette", list(
      category = "image", layerId = "val", vscale = vstore[["vscale"]], colorOptions = leafem::colorOptions(
        palette = pal,
        na.color = "transparent"
      )
    )
    )
  })
  shiny::observeEvent(input$overlay_domain, {
    bounds <- input$overlay_domain
    if (vstore[["rescale"]]) {
      get_legend(bounds)
    }
  })
  
  # overlay element and time
  shiny::observe({
    dt <- climr_tif[[vstore[["tifsource"]]]]
    valid_choices <- unique(dt[!element %in% c("PET", "lat", "CMI", "EXT", "EMT", "MAP", "MAT", "RH", "MSP", "AHM", "SHM"), element])
    current <- input$element
    shiny::updateSelectInput(session, "element",
                             choices = valid_choices,
                             selected = if (!is.null(current) && current %in% valid_choices) current else "Tave"
    )
  })
  shiny::observe({
    req(input$element)
    if (!input$element %in% c("elev", "lat", "PET")) {
      time_choices <- climr::variables[Code_Element == input$element & Category != "Monthly", Time]
      current_time <- input$time
      shiny::updateSelectInput(session, "time",
                               choices = time_choices,
                               selected = if (!is.null(current_time) && current_time %in% time_choices) current_time else NULL
      )
    } else {
      shiny::updateSelectInput(session, "time", choices = character(0))
    }
  })
  
  # reactive output for scale adj
  output$scale_adj <- shiny::renderUI({
    if (!is.null(input$element)) {
      if (input$element %in% climr::variables[Type == "ratio", Code_Element]) {
        shiny::checkboxInput(
          inputId = "vscale",
          label = tags$span("Apply scale adj.", style = "font-size: 15px; display: inline-block; max-width: 160px; white-space: normal;"),
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
      paste0("bivariate_plot_", if (input$input_type != "Map Point") input$dist_click else "", "_", climr::variables[Code_Element == input$bivariate_element_x & Time == input$bivariate_time_x, Code], "_", climr::variables[Code_Element == input$bivariate_element_y & Time == input$bivariate_time_y, Code], ".png")
    },
    content = function(file) {
      pixelratio <- session$clientData$pixelratio
      width  <- session$clientData$output_bivariate_plot_width
      height <- session$clientData$output_bivariate_plot_height
      
      png(file, width = width*pixelratio*1.5, height = height*pixelratio*1.5, res = 120*pixelratio)
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
      paste0("walter_lieth_plot_", if (input$input_type != "Map point") input$dist_click else "mappoint", ".png")
    },
    content = function(file) {
      pixelratio <- session$clientData$pixelratio
      width  <- session$clientData$output_wl_plot_width
      height <- session$clientData$output_wl_plot_height
      
      png(file, width = width*pixelratio*1.75, height = height*pixelratio*1.5, res = 120*pixelratio)
      if (input$input_type == "Map point") {
        print(climr::plot_WalterLieth(
          X = wl_data,
          diurnal = input$wl_diurnal,
          obs_period = input$wl_obs_period,
          location = vis_sg_dt$dt[,wkt],
          app = TRUE
        ))
      } else if (input$input_type == "Ecoregion") {
        print(climr::plot_WalterLieth(
          X = wl_data,
          diurnal = input$wl_diurnal,
          obs_period = input$wl_obs_period,
          location = er_codes[input$dist_click],
          app = TRUE
        ))
      } else {
        print(climr::plot_WalterLieth(
          X = wl_data,
          diurnal = input$wl_diurnal,
          obs_period = input$wl_obs_period,
          location = input$dist_click,
          app = TRUE
        ))
      }
      dev.off()
    }
  )
  
  output$timeseries_download <- shiny::downloadHandler(
    filename = function() {
      paste0("timeseries_plot_", if (input$input_type != "Map Point") input$dist_click else "", "_", climr::variables[Code_Element == input$time_series_element & Time == input$time_series_season, Code], ".png")
    },
    content = function(file) {
      pixelratio <- session$clientData$pixelratio
      width  <- session$clientData$output_timeseries_plot_width
      height <- session$clientData$output_timeseries_plot_height
      
      png(file, width = width*pixelratio*1.75, height = height*pixelratio*1.5, res = 120*pixelratio)
      if (input$input_type == "Map point") {
        print(climr::plot_timeSeries(
          X = timeseries_data,
          var1 = climr::variables[Code_Element == input$time_series_element & Time == input$time_series_season, Code],
          obs_ts_dataset = vstore[["ts_datasets"]],
          gcms = vstore[["ts_gcms"]],
          ssps = vstore[["ts_ssps"]],
          app = TRUE
        )
        )
      } else {
        print(climr::plot_timeSeries_preprocess(
          X = timeseries_data,
          var1 = climr::variables[Code_Element == input$time_series_element & Time == input$time_series_season, Code],
          obs_ts_dataset = vstore[["ts_datasets"]],
          ssps = vstore[["ts_ssps"]],
          app = TRUE
        )
        )
      }
      dev.off()
    }
  )
}