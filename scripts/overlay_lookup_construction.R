# Lookup tables scripting for 99% quantile for climate maps

library(climr)
library(terra)
library(data.table)

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
  # season_idx <- grep(paste0("_", names(seasons), "$", collapse = "|"), lbl)
  # monthly_idx <- grep(paste0("_?", names(months), "$", collapse = "|"), lbl)
  season_idx <- grep(paste0("(_|^)", "(", paste(names(seasons), collapse = "|"), ")", "(_|\\b)"), lbl, perl = TRUE)
  monthly_idx <- grep(paste0("(_|^)", "(", paste(names(months), collapse = "|"), ")", "(_|\\b)"), lbl, perl = TRUE)
  
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
      # For monthly
      sub(paste0("(_(", paste(names(months), collapse = "|"), ").*)$"), "", lbl[monthly_idx]),
      
      # For seasonal
      sub(paste0("(_(", paste(names(seasons), collapse = "|"), ").*)$"), "", lbl[season_idx]),
      
      # For annual, just take full label but strip any suffixes after the element name
      sub(paste0("(_.*)$"), "", lbl[annual_idx])
    ),
    time_code = c(
      {
        raw <- regmatches(
          lbl[monthly_idx],
          regexpr("(_|^)([a-z0-9]{2})(_|\\b)", lbl[monthly_idx], perl = TRUE)
        )
        code <- sub(".*(_|^)([a-z0-9]{2})(_|\\b).*", "\\2", raw)
        code[!code %in% c(names(seasons), names(months))] <- "aa"
        code
      },
      {
        raw <- regmatches(
          lbl[season_idx],
          regexpr("(_|^)([a-z0-9]{2})(_|\\b)", lbl[season_idx], perl = TRUE)
        )
        code <- sub(".*(_|^)([a-z0-9]{2})(_|\\b).*", "\\2", raw)
        code[!code %in% c(names(seasons), names(months))] <- "aa"
        code
      },
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

# 2500m maps
tifs_2500 <- url_process("https://vonuma.com/climr-tif/NA/2500m/mosaic/")[[1]]
dt_2500 <- data.table(
  variable = character(),
  lower_bound = numeric(),
  upper_bound = numeric()
)
for (row in 1:nrow(tifs_2500)) {
  if (grepl("_[0-1][0-9]\\.tif$", tifs_2500[row, name])) next
  var <- sub("\\.tif$", "", tifs_2500[row,name])
  tif <- tifs_2500[row, url]
  r <- rast(tif)
  vals <- values(r)
  q <- quantile(vals, c(0.005, 0.995), na.rm = TRUE)
  dt_2500 <- rbind(dt_2500, data.table(
    variable = var,
    lower_bound = q[1],
    upper_bound = q[2]
  ))
}
write.csv(dt_2500, "dt_2500.csv")

# 800m maps
tifs_800 <- url_process("https://vonuma.com/climr-tif/NA800/")[[1]]
dt_800 <- data.table(
  variable = character(),
  lower_bound = numeric(),
  upper_bound = numeric()
)
for (row in 1:nrow(tifs_800)) {
  if (!grepl("cropped", tifs_800[row, name])) next
  var <- sub("\\_cropped.tif$", "", tifs_800[row,name])
  tif <- tifs_800[row, url]
  r <- rast(tif)
  vals <- values(r)
  q <- quantile(vals, c(0.005, 0.995), na.rm = TRUE)
  dt_800 <- rbind(dt_800, data.table(
    variable = var,
    lower_bound = q[1],
    upper_bound = q[2]
  ))
}
write.csv(dt_800, "dt_800.csv")
