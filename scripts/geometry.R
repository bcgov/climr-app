# Geometry input logic ----
session_geometry <- function(sg_dt) {
  
  observe ({
    req(sg_dt$dt)
    sg_dt$filtered_dt <- (sg_dt$dt)[source %in% c("map_click", "map_draw")]
  })
  
  # deal with file upload data
  fg <- list()
  fg_ <- function(d0, ...) {
    to_rem <- sg_dt$dt[source %in% c("file_upload", "raster_upload")]$id
    if (length(to_rem)) {
      shiny::showNotification("Replacing previous file upload geometries.", type = "warning")
      rem(to_rem)
    }
    fg[[d0]] <<- list(...)
    return()
  }
  
  # allow map to be modified instead of re-rendering
  mp <<- leaflet::leafletProxy("climr")
  
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
  
  
  refresh_DT <- function() {
    
    output$geom_dt <- DT::renderDT(server = TRUE, {
      req(sg_dt$filtered_dt)
      
      if (nrow(sg_dt$filtered_dt) > 0) {
      
      # create data table to display
      gdt <- data.table::copy((sg_dt$filtered_dt)[,1:3])
      
      # change ID to a character to match alignment
      gdt$id <- as.character(gdt$id)
      
      data.table::setnames(gdt, tools::toTitleCase(names(gdt)))
      DT::datatable(gdt, rownames = FALSE, escape = FALSE, selection = 'single', options = list(
        dom = 'ltp',
        pageLength = 5,
        lengthMenu = c(5, 10, 25, 50, 100),
        rowCallback = DT::JS("
          function(row, data, index) {
            if (data[3] === \"map_click\") {
              $(row).addClass(\"table-primary\");
            } else if (data[3] === \"map_draw\") {
              $(row).addClass(\"table-warning\");
            }
          }
        ")
      ))
      } else {
        NULL
      }
    })
  }
  refresh_DT()
  
  update_map_marker <- function() {
    mg <- sg_dt$dt[group == "marker" & grepl("POINT", wkt)]
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
  
  update_map_shape <- function() {
    mg <- sg_dt$dt[group == "shape" | grepl("POLYGON", wkt)]
    mp |> leaflet::clearGroup("sg_shape") |>
      leaflet.extras::removeDrawToolbar(clearFeatures = TRUE) |>
      default_draw_tool()
    if (nrow(mg)) {
      mp |> leaflet::addPolygons(
        data = terra::vect(mg$wkt),
        group = "sg_shape",
        popup = lapply(mg$id, rem_popup),
        fillColor = "#fcba19",
        color = "#036",
        opacity = 0.8,
        weight = 2
      )
    }
  }
  
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
    refresh_DT()
    shiny::updateActionButton(inputId = "generate_results", disabled = {nrow(sg_dt$dt) <= 0})
    if ("marker" %in% g) update_map_marker()
    if ("shape" %in% g) update_map_shape()
  }
  
  # add new geometries to sg_dt$dt
  push <- function(new, g, s, d = NA_character_) {
    id <- max(c(0L,(sg_dt$dt)$id))+1L
    
    if (grepl("POINT", new)) {
      
      # Extract long and lat coordinates
      coords <- gsub("POINT \\(|\\)", "", new)
      coords_split <- strsplit(coords, " ")[[1]]
      
      lon <- round(as.numeric(coords_split[1]), 5)
      lat <- round(as.numeric(coords_split[2]), 5)
    
      } else {
        
      # Extract list of long and lat to find centroid coords
      coords <- gsub("POLYGON \\(\\(|\\)\\)", "", new)
      coords_split <- strsplit(coords, "[, ]+")[[1]]
      lon_coords <- numeric()
      lat_coords <- numeric()
      index = 1
      for (c in coords_split) {
        if (index %% 2 != 0) {
          lon_coords[index] = as.numeric(c)
        } else {
          lat_coords[index] = as.numeric(c)
        }
        index = index +1
      } 
      lon <- round(mean(lon_coords[!is.na(lon_coords)]), 5)
      lat <- round(mean(lat_coords[!is.na(lat_coords)]), 5)
    }
    
    sg_dt$dt <- rbind(sg_dt$dt, data.table::data.table(id = id, lat = lat, long = lon, wkt = new, group = g, source = s, datapath = d))
    # To show hull when npoints > 100
    if (!grepl("POINT", new)) g <- "shape"
    refresh(g)
    #refresh_DT()
    session$sendCustomMessage(type="jsCode", list(code = "$('.input-control-body a.shiny-download-link').removeClass('btn-success');"))
  }
  
  rem <- function(rid) {
    
    # select rows from sg_dt$dt based on row id
    t <- (sg_dt$dt)[id %in% rid, list(group, source, datapath)]

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
    sg_dt$dt <- (sg_dt$dt)[!id %in% rid]
    
    # refresh reactive DT in sidebar
    #refresh_DT()
    refresh(g)
  }
  
  # clear all map point/shapes & files
  clear <- function() {
    # remove points and drawn AOIs
    if ((nrow(sg_dt$dt) > 0) | (length(fg) != 0)) {
      for (id in ((sg_dt$dt)$id)) {
        rem(id)
      }
      # remove file uploads
      rem((sg_dt$dt)[source %in% c("file_upload", "raster_upload")]$id)
    }
    
    # disable buttons
    updateActionButton(session = getDefaultReactiveDomain(),
                       "downscale_parameters", disabled = TRUE)
    updateActionButton(session = getDefaultReactiveDomain(),
                       "generate_results", disabled = TRUE)
    
    # remove any previewed rasters and legends
    if (!is.null(vstore[["downscale_raster_preview"]])) {
      leaflet::removeImage(mp, "rast_layer")
    }
    leaflet::clearControls(mp)
    
    # remove preview raster accordion
    show_ui(FALSE)
    
    # reset all parameters to defaults
    lapply(names(downscale_default), \(x) {
      vstore[[x]] <- downscale_default[[x]]
    })
  }
  
  click_enabled <- TRUE
  click_ignore_next <- FALSE
  
  sg_methods <- list(
    add_point = function(lat,lng) {
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
    add_file = function(f) {
      f <- as.list(f)
      f0 <- f$datapath
      d0 <- dirname(f$datapath)
      
      if (tolower(tools::file_ext(f0)) %in% c("shp")) {
        shiny::showNotification("Shape file needs to be uploaded as an archive with .shx, .dbf, .shp and other optional files like .prj.", type = "error")
        return()
      }
      
      # does it need unzipping before continuing processing?
      if (tolower(tools::file_ext(f$name)) %in% c("zip","tar","gz","xz","7z","bz2")) {
        farch <- try(archive::archive_extract(f0, d0), silent = TRUE)
        if (inherits(farch, "try-error")) {
          shiny::showNotification("Unable to read archive.", type = "error")
          unlink(d0, recursive = TRUE)
          return()
        }
        f0 <- file.path(d0, farch)
        # check if it's multifile archive (bin for raster, shp for polygons)
        if (length(f0) > 1) {
          f0 <- grep("bin$|shp$", f0, value = TRUE, ignore.case = TRUE) |> head(1)
        }
      }
      
      # Text file upload logic bloc
      if (tolower(tools::file_ext(f0)) %in% c("csv", "txt")) {
        
        res <- try(data.table::fread(f0), silent = TRUE)
        if (inherits(res, "try-error")) {
          shiny::showNotification("Unable to read input file (csv/txt). [data.table::fread(\"%s\")]" |> sprintf(f$name), type = "error")
          unlink(d0, recursive = TRUE)
          return()
        }
        
        nm <- names(res)
        geom_j <- head(grep("^geom|geometry", nm, ignore.case = TRUE), 1)
        elev_j <- head(grep("^elev|elevation", nm, ignore.case = TRUE), 1)
        lon_j <- head(grep("^lng|^long|^lon|longitude", nm, ignore.case = TRUE), 1)
        lat_j <- head(grep("^lat|latitude", nm, ignore.case = TRUE), 1)
        id_j <- head(grep("^id|id$|^site", nm, ignore.case = TRUE), 1)
        
        if (length(geom_j)) {
          
          shiny::showNotification("Found columns in file. [%s : %s]" |> sprintf(f$name, paste(nm[id_j], nm[geom_j], sep = ", ")), type = "message")
          
          shape <- try(terra::vect(res[[geom_j]], crs = "EPSG:4326"), silent = TRUE)
          if (inherits(shape, "try-error")) {
            shiny::showNotification("Unable to read input file geometry column. [pos: %s]" |> sprintf(geom_j), type = "error")
            unlink(d0, recursive = TRUE)
            return()
          }
          
          # Add to file geometries
          fg_(d0,
              "datapath" = f0,
              "type" = "text",
              "id" = id_j,
              "geom" = geom_j,
              "elev" = elev_j,
              "table" = res[,-geom_j],
              "shape" = shape
          )
          
          new_p <- shape |>
            terra::aggregate() |>
            terra::geom(wkt = TRUE)
          
          if (terra::is.points(shape)) {
            if (length(shape) > 100) {
              shiny::showNotification("Uploaded point geometry has more than 100 points. Displaying convex hull.", type = "message")
              new_p <- shape |>
                terra::aggregate() |>
                terra::convHull() |>
                terra::geom(wkt = TRUE)
            }
            push(new_p, "marker", "file_upload", d0)
          } else {
            push(new_p, "shape", "file_upload", d0)
          }
          
          return()
          
        } else if (length(lat_j) && length(lon_j)) {
          shiny::showNotification("Found columns in file. [%s : %s]" |> sprintf(f$name, paste(nm[id_j], nm[lat_j], nm[lon_j], nm[elev_j], sep = ", ")), type = "message")
        } else {
          shiny::showNotification("Column detection could not find latitude and longitude pair in file. [%s]" |> sprintf(f$name), type = "error")
          return()
        }
        
        # Add to file geometries
        fg_(d0,
            "datapath" = f0,
            "type" = "text",
            "id" = id_j,
            "lon" = lon_j,
            "lat" = lat_j,
            "elev" = elev_j,
            "table" = res
        )
        
        new_p <- "MULTIPOINT (%s)" |> sprintf(paste(sprintf("(%s %s)", res[[lon_j]], res[[lat_j]]), collapse = ","))
        if (nrow(res) > 100) {
          shiny::showNotification("Uploaded point csv has more than 100 points. Displaying convex hull.", type = "message")
          new_p <- terra::vect(new_p, "EPSG:4326") |>
            terra::aggregate() |>
            terra::convHull() |>
            terra::geom(wkt = TRUE)
        }
        push(new_p, "marker", "file_upload", d0)
        return()
        
      }
      
      # raster upload logic
      res <- try(terra::rast(f0), silent = TRUE)
      if (!inherits(res, "try-error")) {
        
        if ("" %in% terra::crs(res)) {
          shiny::showNotification("Could not determine the CRS of the raster. [%s]" |> sprintf(f$name), type = "error")
          return()
        }
        
        if (!terra::is.lonlat(res)) {
          res <- terra::project(res, from = terra::crs(res), to = "EPSG:4326")
        }
        
        # Add to file geometries
        fg_(d0,
            "datapath" = f0,
            "type" = "raster",
            "raster" = res
        )
        
        new_p <- res |> 
          terra::ext() |>
          terra::vect() |>
          terra::geom(wkt = TRUE)
        
        push(new_p, "shape", "raster_upload", d0)
        return()
        
      }
      
      # shape upload logic
      res <- try(terra::vect(f0), silent = TRUE)
      if (!inherits(res, "try-error")) {
        
        if ("" %in% terra::crs(res)) {
          shiny::showNotification("Could not determine the CRS of the vector. [%s]" |> sprintf(f$name), type = "error")
          return()
        }
        
        if (!terra::is.lonlat(res)) {
          res <- terra::project(res, from = terra::crs(res), to = "EPSG:4326")
        }
        
        # Add to file geometries
        fg_(d0,
            "datapath" = f0,
            "type" = "shape",
            "shape" = res
        )
        
        new_p <- res |>
          terra::aggregate() |>
          terra::geom(wkt = TRUE)
        
        push(new_p, "shape", "file_upload", d0)
        return()
        
      }
      
      shiny::showNotification("Unable to ingest uploaded file. [%s]" |> sprintf(f$name), type = "error")
      return()
      
    },
    process = function() {
      vstore[["processing"]] <- TRUE
      shiny::updateActionButton(inputId = "generate_results", disabled = TRUE)
      #shiny::updateActionButton(inputId = "downscale_process_launch", disabled = TRUE)
      withCallingHandlers(
        message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
        warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
        error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
        {

          run_id <- generate_run_id()

          output_files <- process_downscale(sg_dt$dt, cec, vstore, fg, run_id)

          if (!length(output_files)) {
            vstore[["processing"]] <- FALSE
            shiny::updateActionButton(inputId = "generate_results", disabled = FALSE)
            #shiny::removeModal()
            shiny::showNotification("No output generated.", type = "warning")
            return()
          } else if (tools::file_ext(output_files) == "csv") {
            output$preview_table <- DT::renderDT(server = TRUE, {
              req(output_files)
              
              dt <- head(read.csv(output_files))
              
              # NEED TO FIGURE OUT HOW TO HAVE CAPTION AT TOP LIKE A TITLE
              
              DT::datatable(dt, rownames = FALSE, escape = FALSE, options = list(
                dom = 't', scrollX = TRUE), caption = "Preview of Downscaled Data")
            })
          } else if (tools::file_ext(output_files) == "tif") {
            
            # close Downscale Processing window
            shiny::removeModal()
            
            # raster previews
            output$preview_raster_options <- shiny::renderUI({
              if (show_ui()) {
                
                # split raster previews
                elements <- c()
                # time_periods <- c()
                for (var in names(preview_raster)) {
                  elements <- c(elements, climr::variables[Code == var, Code_Element])
                  # time_periods <- c(time_periods, climr::variables[Code == var, Time])
                }
                
                # remove duplicates
                elements <- unique(elements)
                # time_periods <- unique(time_periods)
                
                shiny::div(
                  h5("Choose raster layer to preview:"),
                  accordion(
                    accordion_panel(
                      title = h5("Raster Elements:"),
                      value = "preview_acc1",
                      shiny::radioButtons(
                        inputId = "ds_ras_elements",
                        label = h5("Choose element of raster:",
                                   prompter::add_prompt(
                                     tooltipsIcon,
                                     message = HTML(paste("Shows the elements for downscaled raster layers. Choose a element for the layer you would like to preview on the map.")),
                                     position = "top",
                                     size = "large",
                                     shadow = FALSE
                                   )
                        ),
                        width = "100%",
                        inline = TRUE,
                        choices = elements,
                        selected = vstore[["ds_ras_elements"]]
                      )
                    ),
                    accordion_panel(
                      title = h5("Raster Time Periods:"),
                      value = "preview_acc2",
                      uiOutput("preview_raster_periods")
                    ),
                  ),
                  shiny::div(
                    uiOutput("log_transform")
                  ),
                  br(),
                  shiny::actionButton(
                    inputId = "preview_raster",
                    label = "Preview Raster Layer",
                    style = "width: 100%;"
                    ),
                  br(), br(),
                  shiny::downloadButton(
                    outputId = "downscale_download",
                    label = "Download Downscaled Data",
                    title = "Download downscaled geographies archive",
                    style = "width: 100%;"
                  )
                )
              } else {
                NULL
              }
            })
            
            # reactive output for selecting time period for previewed raster
            output$preview_raster_periods <- shiny::renderUI({
              if (show_ui() & !is.null(input$ds_ras_elements)) { 
                
                # match given element to valid raster layers
                matching_layers <- grep(paste0("^", vstore[["ds_ras_elements"]]), names(preview_raster), value = TRUE)
                
                # extract valid time periods for given element
                time_periods <- c()
                for (layer in matching_layers) {
                  time_periods <- c(time_periods, climr::variables[Code == layer, Time])
                }
                
                # remove duplicates
                time_periods <- unique(time_periods)
                
                shiny::radioButtons(
                  inputId = "ds_ras_time_periods",
                  label = h5("Choose time period of raster:",
                             prompter::add_prompt(
                               tooltipsIcon,
                               message = HTML(paste("Shows the time periods for downscaled raster layers. Choose a time period for the layer you would like to preview on the map.")),
                               position = "top",
                               size = "large",
                               shadow = FALSE
                             )
                  ),
                  width = "100%",
                  inline = TRUE,
                  choices = time_periods,
                  selected = vstore[["ds_ras_time_periods"]]
                )
              } else {
                NULL
              }
            })
            
            # reactive output for log transform button
            output$log_transform <- shiny::renderUI({
              if (!is.null(vstore[["ds_ras_elements"]]) & !is.null(vstore[["ds_ras_time_periods"]])) {
                raster_layer <- climr::variables[Code_Element == vstore[["ds_ras_elements"]] & Time == vstore[["ds_ras_time_periods"]], Code]
                variable_type <- climr::variables[Code == raster_layer, Type]
                if (show_ui() & variable_type == "ratio") {
                  shiny::checkboxInput(
                    inputId = "log_scale",
                    label = "Apply log transform to raster preview",
                    value = vstore[["log_transform_raster"]],
                    width = "100%"
                  )
                }
              }
            })
          }

          output$downscale_download <- shiny::downloadHandler(
            filename = function() {
              paste0("downscale_", run_id, ".zip")
            },
            content = function(file) {
              on.exit(unlink(output_files), add = TRUE)
              zip::zipr(file, output_files)
            },
            contentType = "application/zip"
          )

          session$sendCustomMessage(type="jsCode", list(code = "$('.input-control-body a.shiny-download-link').addClass('btn-success');"))
          shiny::showNotification("Downscale process completed. You can now download the results.", type = "message")
        }
      )
      vstore[["processing"]] <- FALSE
      #shiny::updateActionButton(inputId = "downscale_process_launch", disabled = TRUE)
      shiny::updateActionButton(inputId = "generate_results", disabled = FALSE)
      #shiny::removeModal()
    },
    get = function() {
      return(sg_dt$dt)
    },
    rm = function(rid) {
        rem(rid)
    },
    clear_all = function() {
      clear()
    },
    # view = function(rid) {
    #   view_map(rid)
    # },
    # bivariate = function(rid) {
    #   plot_bivariate(rid)
    # },
    # timeseries = function(rid) {
    #   plot_timeseries(rid)
    # },
    # climate_diagram = function(rid) {
    #   plot_climate_diagram(rid)
    # },
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
      if ("marker" %in% (sg_dt$dt)[["group"]]) {
        marker_count <- sum((sg_dt$dt)$group == "marker" & (sg_dt$dt)$source == "map_click")
        marker <- marker_count
        file_idx <- which((sg_dt$dt)$group == "marker" & (sg_dt$dt)$source == "file_upload")
        if (length(file_idx)) {
          marker <- sum(marker, length(file_idx))
          marker_count <- vapply(file_idx, \(i) {
            curf <- fg[[(sg_dt$dt)[["datapath"]][i]]]
            nrow(curf$table)  
          }, FUN.VALUE = integer(1)) |> sum(marker_count, na.rm = TRUE)
        }
      }
      
      # Process str8 raster
      shape <- 0
      shape_count <- 0
      if ("raster_upload" %in% (sg_dt$dt)$source) {
        raster_idx <- which((sg_dt$dt)$source == "raster_upload")
        for (i in raster_idx) {
          shape <- shape + 1
          shape_count <- shape_count + {fg[[(sg_dt$dt)[["datapath"]][i]]]$raster |> terra::ncell()}
        }
      }
      
      # Process shapes
      if ("shape" %in% (sg_dt$dt)[!source %in% "raster_upload"][["group"]]) {
        map_shape_idx <- which((sg_dt$dt)$group %in% "shape" & (sg_dt$dt)$source %in% "map_draw")
        file_upload_idx <- which((sg_dt$dt)$group %in% "shape" & (sg_dt$dt)$source %in% "file_upload")
        # Do map draw since no need to loop within for shape list
        for (i in map_shape_idx) {
          shape <- shape + 1
          shape_count <- shape_count + {terra::vect((sg_dt$dt)$wkt[i], crs = "EPSG:4326") |>
              rastmakerg(resolution) |>
              terra::ncell()}
        }
        
        # Do file upload with loop
        for (i in file_upload_idx) {
          for (j in seq_along(fg[[(sg_dt$dt)[["datapath"]][i]]]$shape)) {
            shape <- shape + 1
            shape_count <- shape_count + {fg[[(sg_dt$dt)[["datapath"]][i]]]$shape[j] |>
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