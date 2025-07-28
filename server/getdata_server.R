# Get Data tab server ----

getdata_server <- function(input, output, session) {
  
  # initialize getdata_sg_dt as reactive
  getdata_sg_dt <- reactiveValues(dt = data.table::data.table(
    id = integer(),
    lat = character(),
    long = character(),
    wkt = character(),
    group = character(),
    source = character(),
    datapath = character(),
    area = character()
  ),
  filtered_dt = NULL
  )
  
  # reactive state for csv/raster preview
  show_csv_dt <<- reactiveVal(TRUE)
  csv_data <<- reactiveVal(NULL)
  show_raster_ui <<- reactiveVal(TRUE)
  
  # ---- Modal input storage
  output$getdata_map <- leaflet::renderLeaflet(l |> default_draw_tool())
  
  # allow map to be modified instead of re-rendering
  getdata_mp <- leaflet::leafletProxy("getdata_map")
  
  downscale_default <- list(
    downscale_which_refmap = "refmap_climr",
    downscale_obs_periods_checkbox = "1961_1990",
    downscale_obs_periods = NULL,
    downscale_obs_years_checkbox = FALSE,
    downscale_obs_years = NULL,
    downscale_obs_ts_dataset = NULL,
    downscale_sim_recommended = FALSE,
    downscale_gcms = NULL,
    downscale_ssps = NULL,
    downscale_gcm_periods = NULL,
    downscale_gcm_ssp_years = NULL,
    downscale_gcm_hist_years = NULL,
    downscale_gcm_years_checkbox = FALSE,
    downscale_gcm_years = c(1951:2100),
    downscale_ensemble_mean = TRUE,
    downscale_max_run = 0,
    downscale_run_nm = NULL,
    downscale_extra_vars = downscale_core_vars,
    downscale_extra_vars_sets = NULL,
    downscale_custom_elements = NULL,
    downscale_custom_time_periods = NULL,
    downscale_core_ppt_lr = FALSE,
    downscale_return_refperiod = TRUE,
    ds_ras_elements = NULL,
    ds_ras_time_periods = NULL,
    ds_ras_obs_sim = NULL,
    ds_ras_obs_periods = NULL,
    ds_ras_gcms = NULL,
    ds_ras_ssps = NULL,
    ds_ras_run = "ensembleMean",
    ds_ras_gcm_periods = NULL,
    calculate_diff = FALSE,
    calculate_percent_diff = FALSE,
    log_transform_raster = TRUE,
    downscale_raster_preview = NULL
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
    ds_ras_elements = downscale_default[["ds_ras_elements"]],
    ds_ras_time_periods = downscale_default[["ds_ras_time_periods"]],
    ds_ras_obs_sim = downscale_default[["ds_ras_obs_sim"]],
    ds_ras_obs_periods = downscale_default[["ds_ras_obs_periods"]],
    ds_ras_gcms = downscale_default[["ds_ras_gcms"]],
    ds_ras_ssps = downscale_default[["ds_ras_ssps"]],
    ds_ras_run = downscale_default[["ds_ras_run"]],
    ds_ras_gcm_periods = downscale_default[["ds_ras_gcm_periods"]],
    calculate_diff = downscale_default[["calculate_diff"]],
    calculate_percent_diff = downscale_default[["calculate_percent_diff"]],
    log_transform_raster = downscale_default[["log_transform_raster"]],
    downscale_raster_preview = downscale_default[["downscale_raster_preview"]],
    downscale_output = "csv",
    downscale_resolution = 2500,
    vscale = "none",
    processing = FALSE,
    include_dem = TRUE
  )
  
  # ---- Geometry
  source("scripts/geometry_getdata.R", local = TRUE)
  getdata_sg <- session_geometry(getdata_sg_dt, getdata_mp)
  
  # ---- Get Data Map events
  # How to use Get Data page
  shiny::observeEvent(input$tutorial, {
    showModal(modalDialog(
      title = "What does this page do?",
      easyClose = TRUE,
      size = "xl",
      footer = NULL,
      tags$iframe(
        src = "How_to_use_getdata.pdf#toolbar=0",
        width = "100%",
        height = "700px",
        style = "border:none;"
      )
    ))
  })
  # add map points and drawing map shapes logic
  shiny::observeEvent(input$getdata_map_draw_start, {
    if (shiny::in_devmode()) cat("Event: getdata_map_draw_start", sep = "\n")
    getdata_sg$add_point_enabled(FALSE)
    updateActionButton(session = getDefaultReactiveDomain(),
                       "downscale_parameters", disabled = FALSE)
    updateActionButton(session = getDefaultReactiveDomain(),
                       "generate_results", disabled = FALSE)
  })
  shiny::observeEvent(input$getdata_map_draw_stop, {
    if (shiny::in_devmode()) cat("Event: getdata_map_draw_stop", sep = "\n")
    getdata_sg$add_point_enabled(TRUE)
  })
  shiny::observeEvent(input$getdata_map_draw_new_feature, {
    if (shiny::in_devmode()) cat("Event: getdata_map_draw_new_feature", sep = "\n")
    getdata_sg$add_draw_poly(input$getdata_map_draw_new_feature)
    bslib::accordion_panel_open("acc_methods", "acc_method1")
  })
  shiny::observeEvent(input$getdata_map_click, {
    if (shiny::in_devmode()) cat("Event: getdata_map_click", sep = "\n")
    getdata_sg$add_point(input$getdata_map_click$lat, input$getdata_map_click$lng)
    updateActionButton(session = getDefaultReactiveDomain(),
                       "downscale_parameters", disabled = FALSE)
    updateActionButton(session = getDefaultReactiveDomain(),
                       "generate_results", disabled = FALSE)
    bslib::accordion_panel_open("acc_methods", "acc_method1")
  })
  
  # upload a file
  shiny::observeEvent(input$upload, {
    if (shiny::in_devmode()) cat("Event: upload", sep = "\n")
    getdata_sg$add_file(input$upload)
    updateActionButton(session = getDefaultReactiveDomain(),
                       "downscale_parameters", disabled = FALSE)
    updateActionButton(session = getDefaultReactiveDomain(),
                       "generate_results", disabled = FALSE)
  })
  
  # pop-up remove button for map points
  shiny::observeEvent(input$sg_remove, {
    if (shiny::in_devmode()) cat("Event: sg_remove", sep = "\n")
    getdata_sg$rm(input$sg_remove)
    if (nrow(getdata_sg_dt$dt) < 1) {
      updateActionButton(session = getDefaultReactiveDomain(),
                         "downscale_parameters", disabled = TRUE)
      updateActionButton(session = getDefaultReactiveDomain(),
                         "generate_results", disabled = TRUE)
    }
  })
  
  # ---- Get Data - Data table events
  # delete a map point via data table
  shiny::observeEvent(input$delete_button, {
    if (shiny::in_devmode()) cat("Event: sg_remove", sep = "\n")
    row_num <- input$geom_dt_rows_selected
    point_id <- getdata_sg_dt$filtered_dt[row_num,1] 
    if (length(point_id) != 0) {
      getdata_sg$rm(point_id)
      if (nrow(getdata_sg_dt$dt) < 1) {
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
    getdata_sg$clear_all(getdata_mp)
  })
  
  sn <- \(j) setNames(j,j)
  
  # ---- Get Data Downscale events
  downscale_modal <- function() {
    shiny::showModal(
      shiny::modalDialog(
        title = "Downscale Parameters", size = "l", fade = FALSE, class = "modal-dialog-scrollable",
        
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
                             message = HTML(paste("Historical period for observed climate data, averaged over this period. Ref period 1961_1990 will always be included.")),
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
            shiny::uiOutput("observed_years")
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
            shiny::uiOutput("gcm_years"),
            shiny::uiOutput("gcm_max_run")
          )
        ),
        tags$div(style = "margin-top: 10px;"),
        
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
                label = h5("Choose elements:", 
                           prompter::add_prompt(
                             tooltipsIcon,
                             message = HTML("See Definitions section in Documentation for more information on climate variables."),
                             position = "top",
                             size = "large",
                             shadow = FALSE
                           )),
                width = "100%",
                inline = TRUE,
                choices = unique(climr::variables %>% filter(!Code_Element %in% c("CMI", "EXT", "EMT", "MAP", "MAT", "RH", "MSP", "AHM", "SHM")) %>% pull(Code_Element)),
                selected = vstore[["downscale_custom_elements"]]
              ),
              shiny::checkboxGroupInput(
                inputId = "downscale_custom_time_periods",
                label = h5("Choose seasons/months:"),
                width = "100%",
                inline = TRUE,
                choices = unique(climr::variables %>% pull(Time)), 
                selected = vstore[["downscale_custom_time_periods"]]
              )
            )
          )
        ),
        tags$div(style = "margin-top: 20px;"),
        
        shiny::div(
          shiny::checkboxInput(
            inputId = "downscale_core_ppt_lr",
            label = tags$span("Apply elevation adjustment to precipitation values during downscaling", style = "font-size: 0.85em; font-weight: bold;"),
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
    if (vstore[["downscale_sim_recommended"]]) {
      # GCMs
      #vstore[["downscale_gcms"]] <- climr::list_gcms()[c(1,4:7,10:12)]
      shiny::updateCheckboxGroupInput(
        inputId = "downscale_gcms",
        choices = climr::list_gcms() |> sn(),
        selected = climr::list_gcms()[c(1,4:7,10:12)],
        inline = TRUE
      )
      
      # SSPs
      #vstore[["downscale_ssps"]] <- climr::list_ssps()[c(1:3)]
      shiny::updateCheckboxGroupInput(
        inputId = "downscale_ssps",
        choices = climr::list_ssps() |> sn(),
        selected = climr::list_ssps()[c(1:3)],
        inline = TRUE
      )
      
      # GCM periods
      #vstore[["downscale_gcm_periods"]] <- climr::list_gcm_periods()[c(1:5)]
      shiny::updateCheckboxGroupInput(
        inputId = "downscale_gcm_periods",
        choices = c(climr::list_gcm_periods() |> sn()),
        selected = climr::list_gcm_periods()[c(1:5)],
        inline = TRUE
      )
    } else {
      # GCMs
      shiny::updateCheckboxGroupInput(
        inputId = "downscale_gcms",
        choices = climr::list_gcms() |> sn(),
        selected = character(0),
        inline = TRUE
      )
      
      # SSPs
      shiny::updateCheckboxGroupInput(
        inputId = "downscale_ssps",
        choices = climr::list_ssps() |> sn(),
        selected = character(0),
        inline = TRUE
      )
      
      # GCM periods
      shiny::updateCheckboxGroupInput(
        inputId = "downscale_gcm_periods",
        choices = c(climr::list_gcm_periods() |> sn()),
        selected = character(0),
        inline = TRUE
      )
    }
  )
  
  shiny::observeEvent(input$downscale_parameters, {
    if (shiny::in_devmode()) cat("Event: downscale_parameters", sep = "\n")
    temp_dt <- getdata_sg_dt$dt
    
    if (!is.null(temp_dt) & nrow(temp_dt) > 0) {
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
    # remove any existing csv or raster previews
    show_csv_dt(FALSE)
    csv_data(NULL)
    show_raster_ui(FALSE)
    lapply(c("ds_ras_elements", "ds_ras_time_periods", "ds_ras_obs_sim", "ds_ras_obs_periods", "ds_ras_gcms", "ds_ras_ssps", "ds_ras_gcm_periods", "calculate_diff", "calculate_percent_diff", "log_transform_raster", "downscale_raster_preview"), \(x) {
      vstore[[x]] <- downscale_default[[x]]
    })
    leaflet::removeImage(getdata_mp, "rast_layer")
    leaflet::clearControls(getdata_mp)
    
    ## observed periods ##
    vstore[["downscale_obs_periods_checkbox"]] <- input$downscale_obs_periods_checkbox
    if ("1961_1990" %in% vstore[["downscale_obs_periods_checkbox"]]) {
      vstore[["downscale_return_refperiod"]] <- TRUE
    } 
    vstore[["downscale_obs_periods"]] <- input$downscale_obs_periods_checkbox[input$downscale_obs_periods_checkbox != "1961_1990"]
    
    ## observed years ##
    if (!is.null(input$observed_years_checkbox)) {
      vstore[["downscale_obs_years_checkbox"]] <- input$observed_years_checkbox
      if (vstore[["downscale_obs_years_checkbox"]]) {
        date_range <- c(min(input$downscale_obs_years):max(input$downscale_obs_years))
        vstore[["downscale_obs_years"]] <- date_range
      }
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
    if (!is.null(input$gcm_years_checkbox)) {
      vstore[["downscale_gcm_years_checkbox"]] <- input$gcm_years_checkbox
      if (vstore[["downscale_gcm_years_checkbox"]]) {
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
    }
    
    ## ensemble mean / max model runs ##
    if (!is.null(input$downscale_ensemble_mean)) {
      vstore[["downscale_ensemble_mean"]] <- as.logical(input$downscale_ensemble_mean)
      
    }
    if (!is.null(input$downscale_max_run)) {
      vstore[["downscale_max_run"]] <- input$downscale_max_run
    }
    
    ## extra climate variables ##
    # handle sets
    extra_var_handler()
    
    # handle custom
    vstore[["downscale_custom_elements"]] <- input$downscale_custom_elements
    vstore[["downscale_custom_time_periods"]] <- input$downscale_custom_time_periods
    
    ## elev adjustment ##
    vstore[["downscale_core_ppt_lr"]] <- input$downscale_core_ppt_lr
    
    # ensure inherently annual variables are always displayed if selected
    annual_vars <- climr::variables[Code == Code_Element & Time == "Annual", Code]
    if (any(annual_vars %in% vstore[["downscale_custom_elements"]])) {
      vstore[["downscale_custom_time_periods"]] <- c(vstore[["downscale_custom_time_periods"]], "Annual")
    }
    
    # ensure there are valid element/time period matches selected
    compatible_periods <- climr::variables[Code_Element %in% vstore[["downscale_custom_elements"]] & Time %in% vstore[["downscale_custom_time_periods"]]]
    
    # collect simulated inputs
    selections <- list(input$downscale_gcms, input$downscale_ssps, input$downscale_gcm_periods)
    lengths <- sapply(selections, length)
    
    # calculate number of raster layers
    if ((all(getdata_sg_dt$dt$group != "marker"))) {
      n_layers <- calculate_layers()
    } else {
      n_layers <- 0
    }
    
    if ("Custom" %in% vstore[["downscale_extra_vars_sets"]] & (is.null(vstore[["downscale_custom_elements"]]) | is.null(vstore[["downscale_custom_time_periods"]]))) {
      showModal(
        modalDialog(
          title = "Warning",
          paste("Please select both custom element(s) and season/month(s)."),
          easyClose = TRUE
        )
      )
    } else if (nrow(compatible_periods) == 0 & !is.null(input$downscale_custom_elements) & !is.null(input$downscale_custom_time_periods)) {
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
    } else if (n_layers > 40000) {
      showModal(
        modalDialog(
          title = "Warning - Exceeds job size limit!",
          HTML("Too many output raster layers estimated to run the downscale process. We recommend running the downscale one GCM at a time, or running climate variables packages one at a time. For large-scale downscaling, use the <a href='https://bcgov.github.io/climr/' target='_blank'>climr</a> R package."),
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
  
  append_vstore_and_notify <- function(vstore_key, input_value, msg_format) {
    vpl <- 30
    current_value <- isolate(vstore[[vstore_key]])
    
    # If NULL or uninitialized, treat as empty
    if (is.null(current_value)) current_value <- character()
    
    # Special handling for "NULL" sentinel value logic
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
    
    # Compute additions to current value (avoiding duplicates)
    additions <- setdiff(input_value, current_value)
    
    # Append (union ensures no duplicates)
    updated_value <- union(current_value, input_value)
    vstore[[vstore_key]] <- updated_value
    
    # Notification for additions
    if (length(additions) > 0) {
      diff_value <- substr(paste(additions, collapse = ", "), 1, vpl)
      shiny::showNotification(
        sprintf("%s added [%s]", msg_format, diff_value),
        duration = 2
      )
    }
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
      monthly_vars <- climr::variables %>% filter(Category == "Monthly") %>% filter(!Code_Element %in% c("CMI", "EXT", "EMT", "MAP", "MAT", "RH", "MSP", "AHM", "SHM")) %>% pull(Code)
      append_vstore_and_notify("downscale_extra_vars", monthly_vars, "Monthly vars")
    }
    if ("Seasonal" %in% vstore[["downscale_extra_vars_sets"]]) {
      seasonal_vars <- climr::variables %>% filter(Category == "Seasonal") %>% filter(!Code_Element %in% c("CMI", "EXT", "EMT", "MAP", "MAT", "RH", "MSP", "AHM", "SHM")) %>% pull(Code)
      append_vstore_and_notify("downscale_extra_vars", seasonal_vars, "Seasonal vars")
    }
    if ("Annual" %in% vstore[["downscale_extra_vars_sets"]]) {
      annual_vars <- climr::variables %>% filter(Category == "Annual") %>% filter(!Code_Element %in% c("CMI", "EXT", "EMT", "MAP", "MAT", "RH", "MSP", "AHM", "SHM")) %>% pull(Code)
      append_vstore_and_notify("downscale_extra_vars", annual_vars, "Annual vars")
    }
  }
  
  # estimate number of raster layers that will be in output
  safe_length <- function(x) if (is.null(x)) 1 else length(x)
  
  calculate_layers <- function() {
    # extract variables for layer count
    params <- c("downscale_obs_periods_checkbox", "downscale_obs_years", "downscale_obs_ts_dataset", "downscale_gcms", "downscale_ssps", "downscale_gcm_periods", "downscale_gcm_ssp_years", "downscale_gcm_hist_years", "downscale_max_run", "downscale_extra_vars")
    layer_factors <- sapply(params, function(p) {
      safe_length(vstore[[p]])
    })
    
    # for derived variables, add in months and seasons
    if (!is.null(vstore[["downscale_custom_time_periods"]]) & "Custom" %in% vstore[["downscale_extra_vars_sets"]]) {
      if ("Annual" %in% vstore[["downscale_custom_time_periods"]]) {
        layer_factors <- c(layer_factors, Annual = 16)
      }
      if (any(c("Winter", "Spring", "Summer", "Fall") %in% vstore[["downscale_custom_time_periods"]])) {
        layer_factors <- c(layer_factors, Seasonal = 4)
      }
    }
    
    return(prod(layer_factors))
  }
  
  # set raster resolution based on estimated number of layers
  get_resolution_min <- function(n_layers) {
    n_layers <- calculate_layers()

    # predefine point caps based on the number of layers to downscale
    if (n_layers <= 1000) cap <- 150000
    else if (n_layers <= 2000) cap <- 75000
    else if (n_layers <= 4000) cap <- 37500
    else if (n_layers <= 8000) cap <- 20000
    else if (n_layers <= 16000) cap <- 10000
    else if (n_layers <= 32000) cap <- 5000
    else cap <- 2500
    
    # base cell area on AOI area in m^2 / number of allowed points
    cell_area <- as.numeric((getdata_sg_dt$dt)$area)/cap
    # resolution is sqrt(area) rounded to the nearest 50m
    res <- ceiling(sqrt(cell_area)/50)*50
    
    if (res < 250) return(250)
    return(res)
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
    show_raster_ui(FALSE)
    leaflet::removeImage(getdata_mp, "rast_layer")
    leaflet::clearControls(getdata_mp)
  })
  
  shiny::observeEvent(input$confirm_reset_no, {
    if (shiny::in_devmode()) cat("Event: confirm_reset_no", sep = "\n")
    downscale_modal()
  })
  
  shiny::observeEvent(input$include_dem, {
    if (shiny::in_devmode()) cat("Event: include_dem", sep = "\n")
    vstore[["include_dem"]] <- input$include_dem
  })
  shiny::observeEvent(input$generate_results, {
    if (shiny::in_devmode()) cat("Event: generate_results", sep = "\n")
    # check that it is possible to downscale the data 
    temp_dt <- getdata_sg_dt$dt
    n_layers <- calculate_layers()
    
    if (!is.null(temp_dt) & nrow(temp_dt) > 0 & n_layers < 40000) {
      
      sources <- unique(na.omit(temp_dt$source))
      
      # ensure all data sources are the same before opening Downscale Launch window
      if (length(unique((sources))) == 1) {
        # ensure only 1 area has been selected
        if (any(c("map_draw", "file_upload") %in% sources) & nrow(temp_dt) > 1 & unique(temp_dt$group) != "marker") {
          showModal(
            modalDialog(
              title = "Warning",
              paste("Please ensure only a single area is selected."),
              easyClose = TRUE
            )
          )
        }
        vstore[["processing"]] <- FALSE
        output$downscale_points_count_estimate <- shiny::renderUI({
          if (vstore[["downscale_output"]] == "tif") {
            get_resolution_min(calculate_layers())
          }
          pce <- getdata_sg$process_count(vstore[["downscale_resolution"]])
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
            shiny::conditionalPanel(
              condition = "input.downscale_output == 'tif'",
              tags$div(style = "margin-top: 10px;"),
              shiny::checkboxInput(
                inputId = "include_dem",
                label = "Include elevation map in results",
                value = TRUE
              )
            ),
            shiny::actionButton(
              inputId = "downscale_process_launch",
              label = "Launch Downscale Process",
              class = "btn btn-primary btn-lg",
              icon = shiny::icon("play"),
              width = "100%"
            ),
            
            # preview for csv results
            shiny::uiOutput("preview_table_ui"),
            
            shiny::conditionalPanel(
              condition = "input.downscale_output == 'csv'",
              tags$div(style = "margin-top: 10px;"),
              shiny::downloadButton(
                outputId = "downscale_download",
                label = "Download Downscaled Data",
                style = "width: 100%;"
              )
            )
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
    } else if (n_layers >= 40000) {
      showModal(
        modalDialog(
          title = "Warning - Exceeds job size limit!",
          HTML("Too many output raster layers estimated to run the downscale process. We recommend running the downscale one GCM at a time, or running climate variables packages one at a time. For large-scale downscaling, use the <a href='https://bcgov.github.io/climr/' target='_blank'>climr</a> R package."),
          easyClose = TRUE
        )
      )
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
      show_raster_ui(FALSE)
    } else {
      show_csv_dt(FALSE)
    }
  })
  shiny::observeEvent(input$downscale_resolution, {
    if (shiny::in_devmode()) cat("Event: downscale_resolution", sep = "\n")
    vstore[["downscale_resolution"]] <- input$downscale_resolution
  })
  shiny::observeEvent(input$downscale_process_launch, {
    if (shiny::in_devmode()) cat("Event: downscale_process_launch", sep = "\n")
    if (vstore[["processing"]]) return()

    if (input$downscale_output == "csv") {
      show_csv_dt(TRUE)
    } else {
      show_raster_ui(TRUE)
    }
    
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
    
    # model run handling
    if (vstore[["downscale_max_run"]] == 0) {
      vstore[["downscale_ensemble_mean"]] <- TRUE
    }
     
    # set obs period as ref period in modal if none selected
    if (is.null(vstore[["downscale_obs_periods_checkbox"]])) {
      vstore[["downscale_obs_periods_checkbox"]] <- "1961_2020"
    }
    getdata_sg$process()
    
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
  shiny::observeEvent(input$log_transform_raster, {
    if (shiny::in_devmode()) cat("Event: log_transform_raster", sep = "\n")
    update_vstore_and_notify("log_transform_raster", input$log_transform_raster, "Log transform")
  })
  shiny::observeEvent(input$calculate_diff, {
    if (shiny::in_devmode()) cat("Event: calculate_diff", sep = "\n")
    update_vstore_and_notify("calculate_diff", input$calculate_diff, "Calculate difference")
  })
  shiny::observeEvent(input$calculate_percent_diff, {
    if (shiny::in_devmode()) cat("Event: calculate_percent_diff", sep = "\n")
    update_vstore_and_notify("calculate_percent_diff", input$calculate_percent_diff, "Calculate percent change")
  })
  shiny::observeEvent(input$preview_raster, {
    if (shiny::in_devmode()) cat("Event: downscale_raster_preview", sep = "\n")
    if (vstore[["downscale_output"]] == "tif") {

      # update all preview options in vstore
      vstore[["ds_ras_elements"]] = input$ds_ras_elements
      vstore[["ds_ras_time_periods"]] = input$ds_ras_time_periods
      vstore[["ds_ras_obs_sim"]] = input$ds_ras_obs_sim
      vstore[["ds_ras_obs_periods"]] = input$ds_ras_obs_periods
      vstore[["ds_ras_gcms"]] = input$ds_ras_gcms
      vstore[["ds_ras_ssps"]] = input$ds_ras_ssps
      vstore[["ds_ras_gcm_periods"]] = input$ds_ras_gcm_periods
      vstore[["calculate_diff"]] = input$calculate_diff
      if (!is.null(input$log_transform_raster)) {
        vstore[["log_transform_raster"]] = input$log_transform_raster
      }
      
      # clear previous raster and legend
      if (!is.null(vstore[["downscale_raster_preview"]])) {
        leaflet::removeImage(getdata_mp, "rast_layer")
        leaflet::clearControls(getdata_mp)
      }
      
      # concatenate raster layer preview
      code <- raster_layers[Code_Element == vstore[["ds_ras_elements"]] & Time == vstore[["ds_ras_time_periods"]], Code]
      raster_names <- names(preview_raster)
      
      if (vstore[["ds_ras_obs_sim"]] == "Observed") {
        if (vstore[["ds_ras_obs_periods"]] == "1961_1990") {
          period <- "REFPERIOD"
        } else {
          period <- "OBS"
        }
        time_period <- vstore[["ds_ras_obs_periods"]]
        keywords <- c(code, period, time_period)
        
        layer_match <- raster_names[
          Reduce(`&`, lapply(keywords, function(k) {
            pattern <- if (k %in% keywords[1]) paste0("(^|_)", k, "(_|$)") else k
            grepl(pattern, raster_names)
          }))
        ]
      }
      
      if (vstore[["ds_ras_obs_sim"]] == "Simulated") {
        gcm <- vstore[["ds_ras_gcms"]]
        ssp <- vstore[["ds_ras_ssps"]]
        run <- vstore[["ds_ras_run"]]
        time_period <- vstore[["ds_ras_gcm_periods"]]
        keywords <- c(code, gcm, ssp, run, time_period)
        
        layer_match <- raster_names[
          Reduce(`&`, lapply(keywords, function(k) {
            pattern <- if (k %in% keywords[1]) paste0("(^|_)", k, "(_|$)") else k
            grepl(pattern, raster_names)
          }))
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
      
      # error handling for comparing ref period against itself
      if (vstore[["ds_ras_obs_periods"]] == "1961_1990" & vstore[["ds_ras_obs_sim"]] == "Observed") {
        vstore[["calculate_diff"]] = FALSE
      }
      # to display calculated difference if selected
      if (vstore[["calculate_diff"]]) {
        type <- raster_layers[Code_Element == vstore[["ds_ras_elements"]] & Time == vstore[["ds_ras_time_periods"]], Type]
        keywords <- c("REFPERIOD", code)
        ref_period_raster <- raster_names[
          Reduce(`&`, lapply(keywords, function(k) {
            pattern <- if (k %in% keywords[2]) paste0("(^|_)", k, "(_|$)") else k
            grepl(pattern, raster_names)
          }))
        ]
        
        # set palettes
        col_scheme <- if (grepl("PPT|MSP|MAP|PAS", layer_match)) {
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
          legend_title <- glue::glue("Change in {legend_title} from 1961_1990 to {time_period}")
          
          leaflet::addRasterImage(getdata_mp, display_raster, layerId = "rast_layer", colors = pal)
          leaflet::addLegend(getdata_mp, pal = pal, values = values(display_raster), title = HTML(sprintf("<div style='width: 200px;'>%s</div>", legend_title)), labFormat = labelFormat(suffix = units))
        }
        if (type == "ratio") {
          display_raster <- preview_raster[[layer_match]] - preview_raster[[ref_period_raster]]
          
          if (vstore[["calculate_percent_diff"]]) {
            # error handling for division by 0
            display_raster <- terra::ifel(preview_raster[[ref_period_raster]] != 0, (preview_raster[[layer_match]]-preview_raster[[ref_period_raster]])/preview_raster[[ref_period_raster]], 0)
            
            if (all(values(display_raster) == 0, na.rm = TRUE)) {
              showModal(
                modalDialog(
                  title = "Warning",
                  paste("Selected raster is not valid, ratio contains division by zero."),
                  easyClose = TRUE
                )
              )
            } else {
              display_raster <- display_raster*100
              
              pal <- colorNumeric(
                palette = col_scheme,
                domain = values(display_raster),
                na.color = "transparent"
              )
              
              # update legend title
              legend_title <- glue::glue("Percent change in {legend_title} from 1961_1990 to {time_period}")
              
              leaflet::addRasterImage(getdata_mp, display_raster, layerId = "rast_layer", colors = pal)
              leaflet::addLegend(getdata_mp, pal = pal, values = values(display_raster), title = HTML(sprintf("<div style='width: 200px;'>%s</div>", legend_title)), labFormat = labelFormat(suffix = "%"))
            }
          } else {
            pal <- colorNumeric(
              palette = col_scheme,
              domain = values(display_raster),
              na.color = "transparent"
            )
            
            # update legend title
            legend_title <- glue::glue("Change in {legend_title} from 1961_1990 to {time_period}")
            
            leaflet::addRasterImage(getdata_mp, display_raster, layerId = "rast_layer", colors = pal)
            leaflet::addLegend(getdata_mp, pal = pal, values = values(display_raster), title = HTML(sprintf("<div style='width: 100px;'>%s</div>", legend_title)), labFormat = labelFormat(suffix = units))
          }
        }
      } else {
        variable_type <- climr::variables[Code == code, Type]
        if (variable_type == "ratio" & vstore[["log_transform_raster"]]) {
          # log transform - this may need more error handling for negative numbers?
          raster_layer_values <- log2(preview_raster[[layer_match]] + 1)
        } else {
          raster_layer_values <- preview_raster[[layer_match]]
        }
        
        # set palettes
        col_scheme <- if (grepl("PPT|MSP|MAP|PAS", layer_match)) {
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
        leaflet::addRasterImage(getdata_mp, raster_layer_values, layerId = "rast_layer", colors = pal)
        
        # label formatters for legend
        inv_log2_formatter <- labelFormat(
          transform = function(x) round((2^x) - 1),  # inverse of log2(x + 1)
          suffix = units
        )
        
        # if log-transformed, set legend steps - need to do this!
        
        # add legend
        leaflet::addLegend(getdata_mp, pal = pal, values = values(raster_layer_values), title = HTML(sprintf("<div style='width: 100px;'>%s</div>", legend_title)), labFormat = if (vstore[["log_transform_raster"]] & variable_type == "ratio") inv_log2_formatter else labelFormat(suffix = units))
      }
    }  
  })
  
  # reactive output for observed years
  output$observed_years <- shiny::renderUI({
    if (all(getdata_sg_dt$dt$group == "marker")) {
      tagList(
        shiny::checkboxInput(
          inputId = "observed_years_checkbox",
          label = tags$span("Specify Observed Years", style = "font-size: 0.85em; font-weight: bold;"),
          value = vstore[["downscale_obs_years_checkbox"]],
          width = "100%"
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
              label = h5("Choose observational time-series dataset:",
                         prompter::add_prompt(
                           tooltipsIcon,
                           message = HTML(paste("Dataset for observational time series data. MSWX Blend for Multi-Source Weather, ClimateNA gridded time series, CRU/GPCC for CRU TS (temperature) and GPCC (precipitation).")),
                           position = "top",
                           size = "large",
                           shadow = FALSE
                         )
              ),
              width = "100%",
              selected = vstore[["downscale_obs_ts_dataset"]],
              choices = c("MSWX Blend" = "mswx.blend", "ClimateNA" = "climatena", "Climatic Research Unit / Global Precipitation Climatology Centre" = "cru.gpcc")
            )
          )
        )
      )
    }
  })
  
  # reactive output for GCM years
  output$gcm_years <- shiny::renderUI({
    if (all(getdata_sg_dt$dt$group == "marker")) {
      tagList(
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
      )
    }
  })
  
  # reactive output for GCM max runs
  output$gcm_max_run <- shiny::renderUI({
    if (all(getdata_sg_dt$dt$group == "marker")) {
      tagList(
        shiny::div(
          shiny::checkboxInput(
            inputId = "downscale_ensemble_mean",
            label = tags$span("Use ensemble mean", style = "font-size: 0.85em; font-weight: bold;",
                              prompter::add_prompt(
                                tooltipsIcon,
                                message = HTML(paste("Include ensemble mean of model runs.")),
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
                         message = HTML(paste("Selecting 0 will default to ensemble mean.")),
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
        )  
      )
    }
  })
  
  # reactive output for outputs options
  output$downscale_output_buttons <- shiny::renderUI({
    if ("marker" %in% (getdata_sg_dt$dt)$group) {
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
    } else if ("shape" %in% (getdata_sg_dt$dt)$group) {
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
        shiny::uiOutput("resolution_slider")
      )
    }
  })
  
  # reactive output for slider resolution
  output$resolution_slider <- shiny::renderUI({
    if (all((getdata_sg_dt$dt)$source != "map_click")) {
      res_min <- get_resolution_min(calculate_layers())
      shiny::sliderInput(
        inputId = "downscale_resolution",
        label = h5("Choose downscale resolution (m):",
                   prompter::add_prompt(
                     tooltipsIcon,
                     message = HTML(paste("Target resolution for shapes drawn on map or added using file upload. Does not apply to csv files. Available range will depend on how many raster output layers are estimated.")),
                     position = "top",
                     size = "large",
                     shadow = FALSE
                   )
        ),
        value = if (res_min > vstore[["downscale_resolution"]]) res_min else vstore[["downscale_resolution"]],
        width = "100%",
        min = res_min,
        max = 10000,
        step = 50,
        post = "m",
        ticks = FALSE
      )
    }
  })
  
  # reactive output for csv preview
  output$preview_table_ui <- shiny::renderUI({
    if(show_csv_dt()) {
      DT::DTOutput("preview_table", width = "100%")
    }
  })
}