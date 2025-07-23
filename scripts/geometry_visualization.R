# Geometry input logic ----
visualization_geometry <- function(dt, mp) {
  
  # deal with file upload data
  fg <- list()
  fg_ <- function(d0, ...) {
    to_rem <- dt$dt[source %in% c("file_upload", "raster_upload")]$id
    if (length(to_rem)) {
      shiny::showNotification("Replacing previous file upload geometries.", type = "warning")
      rem(to_rem)
    }
    fg[[d0]] <<- list(...)
    return()
  }
  
  # pop-up remove button on map clicks
  rem_popup <- function(id) {
    
    shiny::actionButton(
      "sg_remove_%s" |> sprintf(id),
      "Remove: %s" |> sprintf(id),
      class = "btn btn-sm btn-danger action-button",
      onclick = 'Shiny.setInputValue(\"sg_remove\", %s, {priority: \"event\"})' |> sprintf(id)
    ) |> 
      as.character()
  }
  
  update_map_marker <- function(mp) {
    mg <- dt$dt[group == "marker" & grepl("POINT", wkt)]
    mp |> leaflet::clearGroup("sg_marker")
    if (nrow(mg)) {
      mp |> leaflet::addAwesomeMarkers(
        data = terra::vect(mg$wkt),
        group = "sg_marker",
        popup = lapply(mg$id, rem_popup),
        icon = default_icon
      )
    }
  }
  
  # update_map_shape <- function(mp) {
  #   mg <- dt$dt[group == "shape" | grepl("POLYGON", wkt)]
  #   mp |> leaflet::clearGroup("sg_shape") |>
  #     leaflet.extras::removeDrawToolbar(clearFeatures = TRUE) |>
  #     default_draw_tool()
  #   if (nrow(mg)) {
  #     mp |> leaflet::addPolygons(
  #       data = terra::vect(mg$wkt),
  #       group = "sg_shape",
  #       popup = lapply(mg$id, rem_popup),
  #       fillColor = "#fcba19",
  #       color = "#036",
  #       opacity = 0.8,
  #       weight = 2
  #     )
  #   }
  # }
  
  modal_map <- function(wkt, g) {
    if ("marker" %in% g & grepl("POINT", wkt)) {
      m <- mview |> leaflet::addAwesomeMarkers(
        data = terra::vect(wkt),
        group = "sg_marker",
        icon = default_icon
      )
    } else if ("shape" %in% g | grepl("POLYGON", wkt)) {
      m <- mview |> leaflet::addPolygons(
        data = terra::vect(wkt),
        fillColor = "#fcba19",
        color = "#036",
        opacity = 0.8,
        weight = 2
      )
    }
    shiny::showModal( 
      shiny::modalDialog( 
        title = NULL, 
        easyClose =  TRUE, 
        leaflet::renderLeaflet(m)
      )
    )
  }
  
  refresh <- function(g) {
    if ("marker" %in% g) update_map_marker(mp)
    # if ("shape" %in% g) update_map_shape(mp)
  }
  
  # add new geometries to dt$dt
  push <- function(new, g, s, d = NA_character_) {
    id <- max(c(0L,(dt$dt)$id))+1L
    
    # check if the point is within valid space
    e <- terra::ext(-179.0625, -51.5625, 14.375, 83.125)
    e_poly <- as.polygons(e)
    crs(e_poly) <- "EPSG:4326"
    
    if (grepl("POINT", new)) {
      
      # Extract long and lat coordinates
      coords <- gsub("POINT \\(|\\)", "", new)
      coords_split <- strsplit(coords, " ")[[1]]
      
      lon <- round(as.numeric(coords_split[1]), 5)
      lat <- round(as.numeric(coords_split[2]), 5)
      
      # check if the point is within valid space
      p <- vect(cbind(lon, lat), crs = crs(e_poly))
      intersection <- terra::intersect(e_poly, p)
      
      if (nrow(intersection) == 0) {
        showModal(
          modalDialog(
            title = "Warning",
            paste("Please select a point or area within North America." ),
            easyClose = TRUE
          )
        )
        return()
      }
    }
    # only allow one map point at a time
    dt$dt <- data.table::data.table(id = id, wkt = new, group = g, source = s, datapath = d)
    
    # To show hull when npoints > 100
    if (!grepl("POINT", new)) g <- "shape"
    refresh(g)
    session$sendCustomMessage(type="jsCode", list(code = "$('.input-control-body a.shiny-download-link').removeClass('btn-success');"))
  }
  
  rem <- function(rid) {
    
    # select rows from dt$dt based on row id
    t <- (dt$dt)[id %in% rid, list(group, source, datapath)]
    
    # Drop datapath from fileuploads if any
    d <- unique(t$datapath)
    d <- d[!is.na(d)]
    if (length(d)) {
      fg[[d]] <<- NULL
      unlink(d, recursive = TRUE)
    }
    
    # Refresh geometries
    g <- unique(t$group)
    
    # remove the selected rows and refresh DT
    dt$dt <- (dt$dt)[!id %in% rid]
    
    # refresh reactive DT in sidebar
    refresh(g)
  }
  
  # clear all map point/shapes & files
  clear <- function(mp) {
    # remove points and drawn AOIs
    if ((nrow(dt$dt) > 0) | (length(fg) != 0)) {
      for (id in ((dt$dt)$id)) {
        rem(id)
      }
    }
  }
  
  click_enabled <- TRUE
  click_ignore_next <- FALSE
  
  plot_bivariate <- function(bivariate_data) {
    output$bivariate_plot <- plotly::renderPlotly({
      req(show_plots())
      if (input$input_type == "Map point") {
        if (nrow(dt$dt) > 0 && !is.null(bivariate_data)) {
          withCallingHandlers(
            message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
            warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
            error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
            {
              tryCatch({
                climr::plot_bivariate(
                  X = bivariate_data,
                  xvar = climr::variables[Code_Element == input$bivariate_element_x & Time == input$bivariate_time_x, Code],
                  yvar = climr::variables[Code_Element == input$bivariate_element_y & Time == input$bivariate_time_y, Code],
                  period_focal = input$bivariate_period,
                  interactive = TRUE
                )
              }, error = function(e) {
                if (grepl("Error: Non-zero values found in", e$message)) {
                  showModal(
                    modalDialog(
                      title = "Error!",
                      paste("The values for the reference period 1961-1990 are 0, while there are non-zero values for the variables selected. Unable to calculate percent change. If interested in this variable, please view the Time Series plot."),
                      easyClose = TRUE
                    )
                  )
                  return()
                } else {
                  stop(e)  # Re-throw if not the expected error
                }
              })
            }
          )
        }
      } else if (input$input_type == "FLP Area" | input$input_type == "Ecoregion") {
        withCallingHandlers(
          message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
          warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
          error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
          {
            tryCatch({
              isolate({
                climr::plot_bivariate(
                  X = bivariate_data,
                  xvar = climr::variables[Code_Element == input$bivariate_element_x & Time == input$bivariate_time_x, Code],
                  yvar = climr::variables[Code_Element == input$bivariate_element_y & Time == input$bivariate_time_y, Code],
                  period_focal = input$bivariate_period,
                  interactive = TRUE
                )
              })
            }, error = function(e) {
              if (grepl("Error: Non-zero values found in", e$message)) {
                showModal(
                  modalDialog(
                    title = "Error!",
                    paste("The values for the reference period 1961-1990 are 0, while there are non-zero values for the variables selected. Unable to calculate percent change. If interested in this variable, please view the Time Series plot."),
                    easyClose = TRUE
                  )
                )
                return()
              } else {
                stop(e)  # Re-throw if not the expected error
              }
            })
          }
        )
      }
    })
  }
  
  plot_timeseries <- function(timeseries_data) {
    output$timeseries_plot <- shiny::renderPlot({
      req(show_plots())
      if (input$input_type == "Map point") {
        withCallingHandlers(
          message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
          warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
          error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
          {
            climr::plot_timeSeries(
              X = timeseries_data,
              var1 = climr::variables[Code_Element == input$time_series_element & Time == input$time_series_season, Code],
              obs_ts_dataset = vstore[["ts_datasets"]],
              gcms = vstore[["ts_gcms"]],
              ssps = vstore[["ts_ssps"]],
              app = TRUE
            )
          }
        )
      } else if (input$input_type == "FLP Area") {
        withCallingHandlers(
          message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
          warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
          error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
          {
            isolate({
              climr::plot_timeSeries(
                X = timeseries_data,
                var1 = climr::variables[Code_Element == input$time_series_element & Time == input$time_series_season, Code],
                obs_ts_dataset = vstore[["ts_datasets"]],
                ssps = vstore[["ts_ssps"]],
                app = TRUE
              )
            })
          }
        )
      } else if (input$input_type == "Ecoregion") {
        withCallingHandlers(
          message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
          warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
          error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
          {
            isolate({
              climr::plot_timeSeries_preprocess(
                X = timeseries_data,
                var1 = climr::variables[Code_Element == input$time_series_element & Time == input$time_series_season, Code],
                obs_ts_dataset = vstore[["ts_datasets"]],
                ssps = vstore[["ts_ssps"]],
                app = TRUE
              )
            })
          }
        )
      }
    })     
  }
  
  plot_walter_lieth <- function(wl_data) {
    output$wl_plot <- shiny::renderPlot({
      req(show_plots())
      if (input$input_type == "Map point") {
        withCallingHandlers(
          message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
          warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
          error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
          {
          climr::plot_WalterLieth(
              X = wl_data,
              diurnal = input$wl_diurnal,
              obs_period = input$wl_obs_period,
              location = dt$dt[,wkt],
              app = TRUE
            )
          }
        )
      } else if (input$input_type == "FLP Area") {
        withCallingHandlers(
          message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
          warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
          error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
          {
            isolate({
              climr::plot_WalterLieth(
                X = wl_data,
                diurnal = input$wl_diurnal,
                obs_period = input$wl_obs_period,
                location = input$dist_click,
                app = TRUE
              )
            })
          }
        )
      } else if (input$input_type == "Ecoregion") {
        withCallingHandlers(
          message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
          warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
          error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
          {
            isolate({
              climr::plot_WalterLieth(
                X = wl_data,
                diurnal = input$wl_diurnal,
                obs_period = input$wl_obs_period,
                location = er_codes[input$dist_click],
                app = TRUE
              )
            })
          }
        )
      }
    })     
  }
  
  sg_methods <- list(
    add_point = function(lat,lng,map_val) {
      if (!map_val()) return()
      if (!click_enabled) return()
      if (click_ignore_next) {click_ignore_next <<- FALSE; return()}
      new_p <- "POINT (%s %s)" |> sprintf(lng, lat)
      push(new_p, "marker", "map_click")
    },
    add_draw_poly = function(poly) {
      ft <- poly$properties$feature_type
      if (ft %in% c("polygon","rectangle")) {
        new_p <- paste0(
          "POLYGON ((",
          paste(
            lapply(
              poly$geometry$coordinates[[1]],
              \(x) unlist(x) |> paste(collapse = " ")
            ),
            collapse = ","),
          "))"
        )
      } else if (ft == "circle") {
        new_p <- do.call(sprintf, c("POINT (%s %s)", poly$geometry$coordinates)) |>
          terra::vect(crs = "EPSG:4326") |>
          terra::buffer(poly$properties$radius) |>
          terra::geom(wkt = TRUE)
      }
      push(new_p, "shape", "map_draw")
      click_ignore_next <<- TRUE
    },
    process = function() {
      vstore[["processing"]] <- TRUE
      shiny::updateActionButton(inputId = "generate_results", disabled = TRUE)
      withCallingHandlers(
        message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
        warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
        error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
        {
          
          run_id <- generate_run_id()
          
          output_files <- process_downscale(dt$dt, cec, vstore, fg, run_id)
          
          if (!length(output_files)) {
            vstore[["processing"]] <- FALSE
            shiny::updateActionButton(inputId = "generate_results", disabled = FALSE)
            shiny::showNotification("No output generated.", type = "warning")
            return()
          } 
          
          session$sendCustomMessage(type="jsCode", list(code = "$('.input-control-body a.shiny-download-link').addClass('btn-success');"))
          shiny::showNotification("Downscale process completed. You can now download the results.", type = "message")
        }
      )
      vstore[["processing"]] <- FALSE
    },
    get = function() {
      return(dt$dt)
    },
    rm = function(rid) {
      rem(rid)
    },
    clear_all = function(mp) {
      clear(mp)
    },
    view = function(rid) {
      view_map(rid)
    },
    bivariate = function(bivariate_data) {
      plot_bivariate(bivariate_data)
    },
    timeseries = function(timeseries_data) {
      plot_timeseries(timeseries_data)
    },
    walter_lieth = function(wl_data) {
      plot_walter_lieth(wl_data)
    },
    # boxplot = function(rid) {
    #   plot_boxplot(rid)
    # },
    # climate_stripes = function(rid) {
    #   plot_climate_stripes(rid)
    # },
    add_point_enabled = function(val) {
      if (missing(val)) return(click_enabled)
      else click_enabled <<- val
    },
    process_count = function(resolution = 2500) {
      # Process all loose points first
      marker <- 0
      marker_count <- 0
      if ("marker" %in% (dt$dt)[["group"]]) {
        marker_count <- sum((dt$dt)$group == "marker" & (dt$dt)$source == "map_click")
        marker <- marker_count
        file_idx <- which((dt$dt)$group == "marker" & (dt$dt)$source == "file_upload")
        if (length(file_idx)) {
          marker <- sum(marker, length(file_idx))
          marker_count <- vapply(file_idx, \(i) {
            curf <- fg[[(dt$dt)[["datapath"]][i]]]
            nrow(curf$table)  
          }, FUN.VALUE = integer(1)) |> sum(marker_count, na.rm = TRUE)
        }
      }
      
      # Process str8 raster
      shape <- 0
      shape_count <- 0
      if ("raster_upload" %in% (dt$dt)$source) {
        raster_idx <- which((dt$dt)$source == "raster_upload")
        for (i in raster_idx) {
          shape <- shape + 1
          shape_count <- shape_count + {fg[[(dt$dt)[["datapath"]][i]]]$raster |> terra::ncell()}
        }
      }
      
      # Process shapes
      if ("shape" %in% (dt$dt)[!source %in% "raster_upload"][["group"]]) {
        map_shape_idx <- which((dt$dt)$group %in% "shape" & (dt$dt)$source %in% "map_draw")
        file_upload_idx <- which((dt$dt)$group %in% "shape" & (dt$dt)$source %in% "file_upload")
        # Do map draw since no need to loop within for shape list
        for (i in map_shape_idx) {
          shape <- shape + 1
          shape_count <- shape_count + {terra::vect((dt$dt)$wkt[i], crs = "EPSG:4326") |>
              rastmakerg(resolution) |>
              terra::ncell()}
        }
        
        # Do file upload with loop
        for (i in file_upload_idx) {
          for (j in seq_along(fg[[(dt$dt)[["datapath"]][i]]]$shape)) {
            shape <- shape + 1
            shape_count <- shape_count + {fg[[(dt$dt)[["datapath"]][i]]]$shape[j] |>
                rastmakerg(resolution) |>
                terra::ncell()}
          }
        }
      }
      
      return(
        list(
          marker = marker,
          marker_count = marker_count,
          shape = shape,
          shape_count = shape_count
        )
      )
    }
  )
  return(sg_methods)
}