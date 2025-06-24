# App Theme ----
bcgov_theme <- function(action = c("install","remove")) {
  action <- match.arg(action)

  # Injecting bcgov theme directly into bslib library
  target <- find.package("bslib")
  if (file.access(target,2) < 0) {
    stop("This must be run with write access to the bslib package")
  }

  src <- "./"
  f <- dir(, recursive = TRUE) |> grep("^fonts|^lib", x = _, value = TRUE)

  if (action == "install") {
    lapply(file.path(target, unique(dirname(f))), dir.create, showWarnings = FALSE, recursive = TRUE)
    file.copy(file.path(src, f), file.path(target, f))
  }

  if (action == "remove") {
    unlink(file.path(target, f))
    unlink(file.path(target, "lib/bsw5/dist/bcgov"), recursive = TRUE)
  }

  return(invisible())

}

if (!"bcgov" %in% bslib::bootswatch_themes()) {
  bcgov_theme("install")
}

url_process <- function(tif_url) {
  resp <- list()
  p <- function(prev = NULL) {
    content <- jsonlite::fromJSON(paste(tif_url, prev, sep = "/")) |> data.table::setDT()
    fcontent <- content[!type %in% "directory", list(name, url = paste(tif_url, prev, name, sep = "/"))]
    fcontent <- labelf(fcontent)
    if (nrow(fcontent)) {
      resp[[paste0(prev, "/") |> gsub("^/|/$", "", x = _) |> gsub("/", " - ", x = _) |> gsub("_", " ", x = _)]] <<- fcontent
    }
    for (d in content[type %in% "directory"]$name) {
      p(prev = paste0(prev, "/", d))
    }
  }
  p()
  return(resp)
}

labelf <- function(fcontent) {
  seasons <- c("wt" = "Winter", "sp" = "Spring", "sm" = "Summer", "at" = "Autumn")
  months <- setNames(month.name, sprintf("%02d", 1:12))
  nm <- fcontent$name
  lbl <- basename(nm) |> tools::file_path_sans_ext()
  season_idx <- grep(paste0("_", names(seasons), "$", collapse = "|"), lbl)
  monthly_idx <- grep(paste0("_?", names(months), "$", collapse = "|"), lbl)
  annual_idx <- setdiff(seq_along(lbl), c(season_idx, monthly_idx))
  resp <- data.table::data.table(
    name = c(
      fcontent$name[monthly_idx],
      fcontent$name[season_idx],
      fcontent$name[annual_idx]
    ),
    url = c(
      fcontent$url[monthly_idx],
      fcontent$url[season_idx],
      fcontent$url[annual_idx]
    ),
    label = c(
      {
        s1 <- strsplit(
          lbl[monthly_idx],
          paste0("_?", names(months), "$", collapse = "|")
        ) |> unlist()
        label_climatevars[s1]
      },
      {
        s1 <- strsplit(
          lbl[season_idx],
          paste0("_", names(seasons), "$", collapse = "|")
        ) |> unlist()
        label_climatevars[s1]
      },
      label_climatevars[lbl[annual_idx]]
    ),
    element = c(
      strsplit(
        lbl[monthly_idx],
        paste0("_?", names(months), "$", collapse = "|")
      ) |> unlist(),
      strsplit(
        lbl[season_idx],
        paste0("_", names(seasons), "$", collapse = "|")
      ) |> unlist(),
      lbl[annual_idx]
    ),
    time_code = c(
      substr(lbl[monthly_idx], nchar(lbl[monthly_idx]) - 1, nchar(lbl[monthly_idx])),
      substr(lbl[season_idx], nchar(lbl[season_idx]) - 1, nchar(lbl[season_idx])),
      rep("aa", length(annual_idx))
    ),
    category = c(
      c("Derived elements","Basic elements")[grepl("^PPT|^Tmin|^Tmax", lbl[monthly_idx])+1],
      c("Derived elements","Basic elements")[grepl("^PPT|^Tmin|^Tmax", lbl[season_idx])+1],
      c("Annual elements","Basic elements")[grepl("^PPT|^Tmin|^Tmax", lbl[annual_idx])+1]
    )
  )
  data.table::set(resp, j = "label", value = resp[, "(%s) %s" |> sprintf(element, label)])
  return(resp)
}

label_climatevars <- c(
  "Tave" = "mean temperatures (°C)",
  "Tmax" = "maximum mean temperatures (°C)",
  "Tmin" = "minimum mean temperatures (°C)",
  "PPT" = "precipitation (mm)",
  "Rad" = "solar radiation (MJ m-2 d-1)",
  "MAT" = "mean annual temperature (°C)",
  "MWMT" = "mean warmest month temperature (°C)",
  "MCMT" = "mean coldest month temperature (°C)",
  "TD" = "temperature difference between MWMT and MCMT, or continentality (°C)",
  "MAP" = "mean annual precipitation (mm)",
  "MSP" = "mean annual summer (May to Sept.) precipitation (mm)",
  "AHM" = "annual heat-moisture index (MAT+10)/(MAP/1000))",
  "SHM" = "summer heat-moisture index ((MWMT)/(MSP/1000))",
  "DD_0" = "degree-days below 0°C, chilling degree-days",
  "DDsub0" = "degree-days below 0°C, chilling degree-days",
  "DD5" = "degree-days above 5°C, growing degree-days",
  "DD_18" = "degree-days below 18°C, heating degree-days",
  "DDsub18" = "degree-days below 18°C, heating degree-days",
  "DD18" = "degree-days above 18°C, cooling degree-days",
  "NFFD" = "the number of frost-free days",
  "FFP" = "frost-free period",
  "bFFP" = "Day of the year on which the Frost-Free Period begins",
  "eFFP" = "Day of the year on which the Frost-Free Period ends",
  "PAS" = "precipitation as snow (mm)",
  "PET" = "Potential Evapotranspiration",
  "EMT" = "extreme minimum temperature over 30 years (°C)",
  "EXT" = "extreme maximum temperature over 30 years (°C)",
  "CMD" = "Hargreaves climatic moisture deficit (mm)",
  "CMI" = "Hogg’s climate moisture index (mm)",
  "DD1040" = "degree-days above 10°C and below 40°C",
  "Eref" = "Hargreaves reference evaporation (mm)",
  "RH" = "mean relative humidity (%)",
  "elev" = "North America Elevation CEC 2023",
  "lat" = "Latitude WSG 84"
)

time_labels_season <- c(
  "Annual" = "aa",
  "Winter" = "wt",
  "Spring" = "sp",
  "Summer" = "sm",
  "Autumn" = "at"
)
time_labels_month <- c(
  "January" = "01",
  "February" = "02",
  "March" = "03",
  "April" = "04",
  "May" = "05",
  "June" = "06",
  "July" = "07",
  "August" = "08",
  "September" = "09",
  "October" = "10",
  "November" = "11",
  "December" = "12"
)

# Tiles source
climr_tif <- url_process(Sys.getenv("CLIMR_TIF_URL"))
climr_ratios <- climr::variables[Type %in% "ratio", c(Code, Code_ClimateNA) |> unique() |> sort()]

# Map tiles provider for BGC + vector tiles ----

##javascript source
wna_tileserver <- "https://tileserver.thebeczone.ca/data/WNA_MAP/{z}/{x}/{y}.pbf"
wna_tilelayer <- "WNA_MAP"

plugins <- {
  list(
    vgplugin =
      htmltools::htmlDependency(
        name = "leaflet.vectorgrid",
        version = "1.3.0",
        src = "www/htmlwidgets",
        script = "lfx-vgrid-prod.js"
      )
  )
}

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

add_custom_render <- function(map) {
  subzones_colours_ref <- data.table::fread("data/WNAv12_3_SubzoneCols.csv", key = "classification")
  map <- registerPlugin(map, plugins$vgplugin)
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
      ', paste0("var subzoneColors = {", paste0("'", subzones_colours_ref$classification, "':'", subzones_colours_ref$colour,"'", collapse = ","), "}"), '
      
      var vectorTileOptions=function(layerName, layerId, activ,
                             lfPane, colorMap, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: activ, // makes it able to trigger js events like click
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 0,
                fillColor: colorMap[properties[prop]],
                fill: true,
                fillOpacity: 0.3
              }
            }
          },
          pane : lfPane,
          maxZoom : 25,
          maxNativeZoom : 17,
          getFeatureId: function(f) {
              return f.properties[id];
          }
        }
        
      };
      
      var subzLayer = L.vectorGrid.protobuf(
        "', wna_tileserver, '",
        vectorTileOptions("WNA BEC", "', wna_tilelayer, '", true,
                          "tilePane", subzoneColors, "MAP_LABEL", "MAP_LABEL")
      )
      this.layerManager.addLayer(subzLayer, "tile", "WNA BEC", "WNA BEC");
      
      subzLayer.bindTooltip(function(e) {
        return e.properties.MAP_LABEL
      }, {sticky: true, textsize: "10px", opacity: 1});
      subzLayer.bringToFront();

      var map = this;

      var updateOpacity=function(message) {
        var prefixedLayerId = map.layerManager._layerIdKey(message.category, message.layerId);
        var layer = map.layerManager._byLayerId[prefixedLayerId];
        if (layer !== undefined) {
          layer.setOpacity(message.opacity);
        }
      }

      var updateResolution=function(message) {
        var prefixedLayerId = map.layerManager._layerIdKey(message.category, message.layerId);
        var layer = map.layerManager._byLayerId[prefixedLayerId];
        if (layer !== undefined) {
          const resolution = message.resolution;
          layer.options.resolution = resolution;
          layer.redraw();
        }
      }

      var updateClimatePalette = function(message) {
        var prefixedLayerId = map.layerManager._layerIdKey(message.category, message.layerId);
    
        function tryApplyPalette(attemptsLeft = 10) {
            var layer = map.layerManager._byLayerId[prefixedLayerId];
    
            if (layer) {
                if (layer.options.georaster) {
                    applyColorScale(layer);
                } else {
                    layer.once("load", () => applyColorScale(layer));
                }
            } else if (attemptsLeft > 0) {
                // Wait 300ms and try again
                setTimeout(() => tryApplyPalette(attemptsLeft - 1), 300);
            } else {
                console.warn("Layer not found after multiple attempts:", message.layerId);
            }
        }
    
        function applyColorScale(layer) {
            var georaster = layer.options.georaster;
            var colorOptions = message.colorOptions;
    
            var scaleFunc = ({log: Math.log, log10: Math.log10, log1p: Math.log1p, log2: Math.log2}[message.vscale] || (x => x));
            const cols = colorOptions.palette;
            let scale = chroma.scale(cols);
    
            let dmin = scaleFunc(georaster.mins[0]);
            let dmax = scaleFunc(georaster.maxs[0]);
            let domain = [dmin, dmax];
            let nacol = colorOptions["na.color"];
            let clr = scale.domain(domain);
    
            let pixelValuesToColorFn = values => {
                let val = values[0];
                if (isNaN(val) || val === georaster.noDataValue) return nacol;
                return clr(scaleFunc(val)).hex();
            };
    
            layer.updateColors(pixelValuesToColorFn);
        }
    
        tryApplyPalette(); // kick off the polling
    };

      Shiny.addCustomMessageHandler(\'updateOpacity\', updateOpacity);
      Shiny.addCustomMessageHandler(\'updateResolution\', updateResolution);
      Shiny.addCustomMessageHandler(\'updateClimatePalette\', updateClimatePalette);

    }'
  ))
  map
}

# district tilelayers
district_tileserver <- "https://tileserver.thebeczone.ca/data/Districts/{z}/{x}/{y}.pbf"
district_tilelayer <- "Districts"

addDistricts <- function(map) {
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
            //Now districts regions
            
      map = this;
      district_flag = true;
      Shiny.setInputValue("dist_flag",false);
      var distHL = "DQU";
      var styleHL = {
            weight: 3,
            color: "#fc036f",
            fillColor: "#FFFB00",
            fillOpacity: 1,
            fill: false
      };
      var vectorTileOptionsDist=function(layerName, layerId, activ,
                                     lfPane, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: true,
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 1,
                color: "#000000",
                fill: true,
                fillOpacity: 0
              }
            }
          },
          pane : lfPane, 
          getFeatureId: function(f) {
            return f.properties[id];
          }
        }
      };
      
      distLayer = L.vectorGrid.protobuf(
        "', district_tileserver, '",
        vectorTileOptionsDist("Districts", "', district_tilelayer, '", true,
                          "tilePane", "dist_code", "dist_code")
      )
      //map_2.layerManager.addLayer(distLayer, "tile", "dist_code", "dist_code");
      
      Shiny.addCustomMessageHandler("addRegionTile",function(data){
        //map = window.map;
        //console.log(window.map instanceof L.Map);
        var url = data.url;
        var cname = data.name;
        var cid = data.id;
        console.log(url);
        console.log("HI");
        map.removeLayer(distLayer);
        console.log("Pane exists?", !!map.getPane("tilePane"));
        distLayer = L.vectorGrid.protobuf(url, vectorTileOptionsDist(cname, cname, true,
                          "tilePane", cid, cid)
        )
        map.layerManager.addLayer(distLayer, "tile", cid, cid);
        distLayer.bindTooltip(function(e) {
          const fieldNames = Object.keys(e.properties);
          return e.properties[fieldNames[0]];
        }, {sticky: true, textsize: "12px", opacity: 1});
        distLayer.bringToFront();
        distFlag = true;
        Shiny.setInputValue("dist_flag",distFlag);
        console.log("HI again, regiond should be rendered");

        distLayer.on("click", function(e){
          distLayer.resetFeatureStyle(distHL);
          distHL = e.layer.properties[cid];
          Shiny.setInputValue("dist_click",distHL);
          distLayer.setFeatureStyle(distHL, styleHL);
          flag = false;
          distFlag = false;
          setTimeout(() => {
            Shiny.setInputValue("dist_flag",false);
          }, 600);
          });
      });
      
      Shiny.addCustomMessageHandler("clear_district",function(x){
        map.removeLayer(distLayer);
        distFlag = false;
        Shiny.setInputValue("dist_flag",distFlag);
      });

      Shiny.addCustomMessageHandler("selectDist",function(x){
        distLayer.bringToFront();
        distFlag = true;
        Shiny.setInputValue("dist_flag",distFlag);
      });
      
      Shiny.addCustomMessageHandler("clearTooltips",function(x){
        distLayer.unbindTooltip();
        distFlag = false;
        Shiny.setInputValue("dist_flag",distFlag); 
      });
      
      Shiny.addCustomMessageHandler("reset_district",function(x){
        distLayer.resetFeatureStyle(distHL);
        distFlag = true;
        Shiny.setInputValue("dist_flag",distFlag);
        distLayer.bindTooltip(function(e) {
          const fieldNames = Object.keys(e.properties);
          return e.properties[fieldNames[0]];
        }, {sticky: true, textsize: "12px", opacity: 1});
        //Shiny.setInputValue("dist_click",null);
      });
      
      distLayer.bindTooltip(function(e) {
        const fieldNames = Object.keys(e.properties);
        return e.properties[fieldNames[0]];
      }, {sticky: true, textsize: "12px", opacity: 1});
      
      // end districts
    }'
  ))
  map
}

# # ecoregion tilelayers
# ecoregion_tileserver <- "https://tileserver.thebeczone.ca/data/ecoregions/{z}/{x}/{y}.pbf"
# ecoregion_tilelayer <- "Ecoregions"
# 
# addEcoregions <- function(map) {
#   map <- htmlwidgets::onRender(map, paste0('
#     function(el, x, data) {
#             //Now districts regions
#             
#       ecoregion_flag = true;
#       Shiny.setInputValue("eco_flag",false);
#       var distHL = "DQU";
#       var styleHL = {
#             weight: 3,
#             color: "#fc036f",
#             fillColor: "#FFFB00",
#             fillOpacity: 1,
#             fill: false
#       };
#       var vectorTileOptionsER=function(layerName, layerId, activ,
#                                      lfPane, prop, id) {
#         return {
#           vectorTileLayerName: layerName,
#           interactive: true,
#           vectorTileLayerStyles: {
#             [layerId]: function(properties, zoom) {
#               return {
#                 weight: 1,
#                 color: "#000000",
#                 fill: true,
#                 fillOpacity: 0
#               }
#             }
#           },
#           pane : lfPane, 
#           getFeatureId: function(f) {
#             return f.properties[id];
#           }
#         }
#       };
#       
#       ecoLayer = L.vectorGrid.protobuf(
#         "', ecoregion_tileserver, '",
#         vectorTileOptionsER("Ecoregions", "', ecoregion_tilelayer, '", true,
#                           "tilePane", "dist_code", "dist_code")
#       )
#       //map_2.layerManager.addLayer(ecoLayer, "tile", "dist_code", "dist_code");
#       
#       Shiny.addCustomMessageHandler("addEcoRegionTile",function(data){
#         map = window.map;
#         console.log(window.map instanceof L.Map);
#         var url = data.url;
#         var cname = data.name;
#         var cid = data.id;
#         console.log(url);
#         console.log("HI");
#         map.removeLayer(ecoLayer);
#         ecoLayer = L.vectorGrid.protobuf(url, vectorTileOptionsER(cname, cname, true,
#                           "tilePane", cid, cid)
#         )
#         map.layerManager.addLayer(ecoLayer, "tile", cid, cid);
#         ecoLayer.bindTooltip(function(e) {
#           const fieldNames = Object.keys(e.properties);
#           return e.properties[fieldNames[0]];
#         }, {sticky: true, textsize: "12px", opacity: 1});
#         ecoLayer.bringToFront();
#         ecoFlag = true;
#         Shiny.setInputValue("eco_flag",ecoFlag);
#         console.log("HI");
#         
#         ecoLayer.on("click", function(e){
#           ecoLayer.resetFeatureStyle(distHL);
#           distHL = e.layer.properties[cid];
#           Shiny.setInputValue("er_click",distHL);
#           ecoLayer.setFeatureStyle(distHL, styleHL);
#           flag = false;
#           ecoFlag = false;
#           setTimeout(() => {
#             Shiny.setInputValue("eco_flag",false);
#           }, 600);
#           });
#       });
#       
#       Shiny.addCustomMessageHandler("clear_ecoregion",function(x){
#         map.removeLayer(ecoLayer);
#         ecoFlag = false;
#         Shiny.setInputValue("eco_flag",ecoFlag);
#       });
# 
#       Shiny.addCustomMessageHandler("selectER",function(x){
#         ecoLayer.bringToFront();
#         ecoFlag = true;
#         Shiny.setInputValue("eco_flag",ecoFlag);
#       });
#       
#       Shiny.addCustomMessageHandler("clearTooltips",function(x){
#         ecoLayer.unbindTooltip();
#         ecoFlag = false;
#         Shiny.setInputValue("eco_flag",ecoFlag); 
#       });
#       
#       Shiny.addCustomMessageHandler("reset_ecoregion",function(x){
#         ecoLayer.resetFeatureStyle(distHL);
#         ecoFlag = true;
#         Shiny.setInputValue("eco_flag",ecoFlag);
#         ecoLayer.bindTooltip(function(e) {
#           const fieldNames = Object.keys(e.properties);
#           return e.properties[fieldNames[0]];
#         }, {sticky: true, textsize: "12px", opacity: 1});
#         //Shiny.setInputValue("dist_click",null);
#       });
#       
#       ecoLayer.bindTooltip(function(e) {
#         const fieldNames = Object.keys(e.properties);
#         return e.properties[fieldNames[0]];
#       }, {sticky: true, textsize: "12px", opacity: 1});
#       
#       // end ecoregions
#     }'
#   ))
#   map
# }

default_draw_tool <- function(mp) {
  mp |> leaflet.extras::addDrawToolbar(
    position = "bottomleft",
    polylineOptions = FALSE,
    circleMarkerOptions = FALSE,
    markerOptions = FALSE
  )
}

default_icon <- leaflet::makeAwesomeIcon("record", markerColor = "darkblue", iconColor = "#fcba19")
mview <- leaflet::leaflet() |> leaflet::addProviderTiles(provider = leaflet::providers$CartoDB.PositronNoLabels)

# Function to generate random run number with compressed timestamp
generate_run_id <- function() {
  # Define character set (A-Z, 0-9)
  chars <- c(65:90, 48:57) # ASCII codes for A-Z and 0-9
  # Sample 8 random characters and convert to string
  run_num <- rawToChar(as.raw(sample(chars, 8, replace = TRUE)))
  # Get compressed timestamp (YYYYMMDDHHMM)
  timestamp <- format(Sys.time(), "%Y%m%d%H%M")
  # Combine with underscore
  paste0(run_num, "_", timestamp)
}

# Albers Equal Area CRS (meters-based)
albers_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m"

process_downscale <- function(sg, cec, vstore, fg, run_id) {

  output_files <- c()
  n <- \(x) if (length(x) && !"NULL" %in% x) x
  # Create temporary directory
  temp_dir <- tempdir()

  # Downscale call
  ds <- \(xyz) {
    climr::downscale(
      xyz = xyz,
      which_refmap = vstore[["downscale_which_refmap"]],
      obs_periods = vstore[["downscale_obs_periods"]] |> n(),
      obs_years  = vstore[["downscale_obs_years"]] |> n(),
      obs_ts_dataset = vstore[["downscale_obs_ts_dataset"]] |> n(),
      # return_refperiod = vstore[["downscale_return_refperiod"]],
      return_refperiod = TRUE, # have this hard coded in as true as we need it for calculating difference in rasters - different solution for this?
      gcms = vstore[["downscale_gcms"]] |> n(),
      ssps = vstore[["downscale_ssps"]] |> n(),
      gcm_periods = vstore[["downscale_gcm_periods"]] |> n(),
      gcm_ssp_years = vstore[["downscale_gcm_ssp_years"]] |> n(),
      gcm_hist_years = vstore[["downscale_gcm_hist_years"]] |> n(),
      ensemble_mean = vstore[["downscale_ensemble_mean"]] |> n(),
      max_run = vstore[["downscale_max_run"]] |> n() |> as.integer(),
      run_nm = vstore[["downscale_run_nm"]] |> n(),
      vars = c(vstore[["downscale_extra_vars"]] |> n()),
      ppt_lr = vstore[["downscale_core_ppt_lr"]]
    )
  }
  
  # Process all loose points first
  if ("marker" %in% sg[["group"]]) {
    # Do non file_upload first
    marker_idx <- which(sg$group == "marker" & sg$source == "map_click")

    if (length(marker_idx)) {
      marker_geoms <- terra::vect(sg$wkt[marker_idx], crs = "EPSG:4326")
      coords <- terra::crds(marker_geoms)
      elevs <- terra::extract(cec, marker_geoms, method = "bilinear", ID = FALSE, raw = TRUE)[,1]
      marker_dt <- data.table::data.table(
        sg_id = sg$id[marker_idx],
        id = seq_len(length(marker_idx)) + 9999,
        lon = coords[, 1],
        lat = coords[, 2],
        elev = elevs
      )
    } else {
      marker_dt <- data.table::data.table()
    }

    # Do file_upload second
    file_idx <- which(sg$group == "marker" & sg$source == "file_upload")

    if (length(file_idx)) {
      file_dt <- lapply(file_idx, \(i) {
      
        curf <- fg[[sg[["datapath"]][i]]]
        
        if (length(curf[["id"]])) {
          f_id <- curf$table[[curf[["id"]]]]
        } else { 
          f_id <- seq_len(nrow(curf$table))
        }
        
        if (!is.null(curf$shape)) {
          coords <- terra::crds(curf$shape)
          f_lon = coords[, 1]
          f_lat = coords[, 2]
        } else {
          f_lon <- curf$table[[curf[["lon"]]]]
          f_lat <- curf$table[[curf[["lat"]]]]
        }
        
        if (length(curf[["elev"]])) {
          f_elev <- curf$table[[curf[["elev"]]]]
        } else {
          f_elev <- terra::extract(cec, data.frame(x = f_lon, y = f_lat), method = "bilinear", ID = FALSE, raw = TRUE)[,1]
        }
  
        data.table::data.table(sg_id = i, id = f_id, lon = f_lon, lat = f_lat, elev = f_elev)
          
      }) |> data.table::rbindlist(use.names = TRUE, fill = TRUE)

    } else {
      file_dt <- data.table::data.table()
    }

    xyz <- data.table::rbindlist(list(marker_dt, file_dt), use.names = TRUE, fill = TRUE)

    if (any(duplicated(xyz$id))) {
      warning("Duplicated ids found in points. Replacing.")
      xyz$id <- seq_len(nrow(xyz))
    }

    res <- ds(xyz)
    
    # keep a copy of res for previewing raster
    preview_raster <<- res
    
    # Write the current res to CSV using the same run_id
    csv_file <- file.path(temp_dir, paste0("downscale_", run_id, ".csv"))
    data.table::fwrite(x = res, file = csv_file, row.names = FALSE)

    rm(res, xyz, file_dt, marker_geoms, coords, elevs, marker_dt)

    output_files <- c(output_files, csv_file)

  }

  # Process str8 raster
  if ("raster_upload" %in% sg$source) {
    raster_idx <- which(sg$source == "raster_upload")
    for (i in raster_idx) {
      xyz <- fg[[sg[["datapath"]][i]]]$raster
      res <- ds(xyz)
      # Write the current res to tif using the same run_id
      out_file <- file.path(temp_dir, paste0("downscale_", run_id, "_raster_",i,".%s" |> sprintf(vstore[["downscale_output"]])))
      if (vstore[["downscale_output"]] %in% "tif") {
        # keep a copy of res for previewing raster
        preview_raster <<- res
        
        terra::writeRaster(x = res, filename = out_file, gdal=c("PREDICTOR=2"), datatype="FLT4S", overwrite = TRUE)
      } else {
        data.table::as.data.table(res) |> data.table::fwrite(file = out_file, row.names = TRUE)
      }      
      output_files <- c(output_files, out_file)
      rm(xyz, res)
    }
  }

  # Process shapes
  if ("shape" %in% sg[!source %in% "raster_upload"][["group"]]) {
    map_shape_idx <- which(sg$group %in% "shape" & sg$source %in% "map_draw")
    file_upload_idx <- which(sg$group %in% "shape" & sg$source %in% "file_upload")

    rastmaker <- \(g) {
      ref <- rastmakerg(g, vstore[["downscale_resolution"]]) |>
        terra::resample(x = cec, y = _, method = "bilinear")
      return(ref)
    }

    # Do map draw since no need to loop within for shape list
    for (i in map_shape_idx) {
      g <- terra::vect(sg$wkt[i], crs = "EPSG:4326")
      xyz <- g |> rastmaker()
      res <- ds(xyz)
      res <- terra::mask(res, g)
      
      # Write the current res to tif using the same run_id
      out_file <- file.path(temp_dir, paste0("downscale_", run_id, "_map_draw_",i,".%s" |> sprintf(vstore[["downscale_output"]])))
      if (vstore[["downscale_output"]] %in% "tif") {
        # keep a copy of res for previewing raster
        preview_raster <<- res
        
        terra::writeRaster(x = res, filename = out_file, gdal=c("PREDICTOR=2"), datatype="FLT4S", overwrite = TRUE)
      } else {
        data.table::as.data.table(res) |> data.table::fwrite(file = out_file, row.names = TRUE)
      }      
      output_files <- c(output_files, out_file)
      rm(xyz, res, g)
    }

    # Do file upload with loop
    for (i in file_upload_idx) {
      for (j in seq_along(fg[[sg[["datapath"]][i]]]$shape)) {
        g <- fg[[sg[["datapath"]][i]]]$shape[j]
        xyz <- g |> rastmaker()
        res <- ds(xyz)
        res <- terra::mask(res, g)
        
        # Write the current res to tif using the same run_id
        out_file <- file.path(temp_dir, paste0("downscale_", run_id, "_file_upload_", i,"_shape_", j, ".%s" |> sprintf(vstore[["downscale_output"]])))
        if (vstore[["downscale_output"]] %in% "tif") {
          # keep a copy of res for previewing raster
          preview_raster <<- res
          
          terra::writeRaster(x = res, filename = out_file, gdal=c("PREDICTOR=2"), datatype="FLT4S", overwrite = TRUE)
        } else {
          data.table::as.data.table(res) |> data.table::fwrite(file = out_file, row.names = TRUE)
        }      
        output_files <- c(output_files, out_file)
        rm(xyz, res, g) 
      }
    }

  }

  return(output_files)

}

downscale_core_vars <- sort(sprintf(c("PPT_%02d", "Tmax_%02d", "Tmin_%02d"), sort(rep(1:12, 3))))
downscale_extra_vars <- local({
  lbl <- setdiff(climr::list_vars(), downscale_core_vars)
  seasons <- c("wt" = "Winter", "sp" = "Spring", "sm" = "Summer", "at" = "Autumn")
  months <- setNames(month.name, sprintf("%02d", 1:12))
  season_idx <- grep(paste0("_", names(seasons), "$", collapse = "|"), lbl)
  monthly_idx <- grep(paste0("_?", names(months), "$", collapse = "|"), lbl)
  annual_idx <- setdiff(seq_along(lbl), c(season_idx, monthly_idx))
  list(
    "Monthly" = lbl[monthly_idx],
    "Seasonal" = lbl[season_idx],
    "Annual" = lbl[annual_idx]
  )
})

rastmakerg <- function(g, resolution) {
  hull <- terra::hull(g, type = "rectangle")
  lat <- mean(c(terra::ymin(hull), terra::ymax(hull)))
  y_res <- resolution / 111319  # Latitude resolution
  x_res <- resolution / (111319 * cos(lat * pi / 180))  # Longitude resolution adjusted for latitude
  ref <- terra::rast(hull, resolution = c(x_res, y_res))
  return(ref)
}