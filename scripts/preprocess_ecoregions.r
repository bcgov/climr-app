## Create pre-downscaled datasets for plotting
library(climr)
library(data.table)
library(terra)
library(sf)
library(RPostgres)
library(pool)

bnds <- vect("Ecoregions.gpkg")
dem <- rast("NA_DEM_250.tif")

samps <- spatSample(bnds, size = rep(150, length(bnds)), method = "random")

ssp_id <- data.table(ssp = c("historical", list_ssps()[1:3]), ssp_id = seq_along(list_ssps()))
dataset_id <- data.table(dataset = c("mswx.blend", "cru.gpcc", "climatena"), dataset_id = 1:3)
type_id <- data.table(type = c("ensmin", "ensmax", "ensmean", "dataset"), type_id = 1:4)
var_id <- data.table(var = list_vars(), var_id = seq_along(list_vars()))

# db connection
dbCon <- dbPool(RPostgres::Postgres(), dbname = 'climr',
                   host = '',
                   port = 5432,
                   user = 'postgres',
                   password = '')

# create queries
query_datasets <- "CREATE TABLE IF NOT EXISTS ecor_ts_datasets (
                    region varchar(20),
                    dataset_id SMALLINT,
                    type_id SMALLINT,
                    var_id SMALLINT,
                    vals REAL[124]
                    )"

query_hist <- "CREATE TABLE IF NOT EXISTS ecor_ts_hist (
                    region varchar(20),
                    ssp_id SMALLINT,
                    type_id SMALLINT,
                    var_id SMALLINT,
                    vals REAL[34]
                    )"

query_proj <- "CREATE TABLE IF NOT EXISTS ecor_ts_proj (
                    region varchar(20),
                    ssp_id SMALLINT,
                    type_id SMALLINT,
                    var_id SMALLINT,
                    vals REAL[19]
                    )"

dbExecute(dbCon, query_datasets)
dbExecute(dbCon, query_hist)
dbExecute(dbCon, query_proj)

# function to pad arrays with NAs
idx_ipt_len <- function(index, input, length) {
  x <- NA_integer_
  length(x) <- length
  x[index] <- input
  x
}

for (flp_code in c(unique(samps$NA_L3CODE))) {
  message("Processing ", flp_code)
  sampsub <- samps[samps$NA_L3CODE == flp_code]
  crds <- extract(dem, sampsub, xy = TRUE, ID = TRUE)
  names(crds) <- c("id", "elev", "lon", "lat")

  
  dat2 <- tryCatch({
    dat <- downscale(
            xyz = crds,
            gcms = list_gcms()[c(1, 4, 6, 7, 10, 11, 12)],
            ssps = list_ssps()[1:3],
            max_run = 5,
            return_refperiod = FALSE,
            ensemble_mean = TRUE,
            obs_ts_dataset = c("mswx.blend", "cru.gpcc", "climatena"),
            obs_years = list_obs_years(),
            gcm_hist_years = list_gcm_hist_years(),
            gcm_ssp_years = list_gcm_ssp_years(),
            vars = list_vars(),
            db_option = "database"
          )

    data_agg <- dat[, lapply(.SD, mean, na.rm = TRUE), by = .(GCM, SSP, RUN, PERIOD, DATASET), .SDcols = -c("id", "GCM", "SSP", "RUN", "PERIOD", "DATASET")]

    for (var in list_vars()) {
      # pre-process
      input_data <- plot_timeSeries_input_preprocess(data_agg, var1 = var, obs_ts_dataset = c("mswx.blend", "cru.gpcc", "climatena"))

      # prepare for db
      input_data[ssp_id, on = .(SSP = ssp), SSP := i.ssp_id]
      input_data[dataset_id, on = .(DATASET = dataset), DATASET := i.dataset_id]
      input_data[type_id, on = .(TYPE = type), TYPE := i.type_id]
      input_data[var_id, on = .(VAR = var), VAR := i.var_id]
      setnames(input_data, old = c("PERIOD", "SSP", "DATASET", "TYPE", "VAR", "VAL"), new = c("period", "ssp_id", "dataset_id", "type_id", "var_id", "val"))
      input_data[, region := flp_code]

      ## reformat for db
      # datasets
      max_len <- max(length(input_data[dataset_id == 1, val]), length(input_data[dataset_id == 2, val]), length(input_data[dataset_id == 3, val]))
      insert_datasets <- input_data[!is.na(dataset_id) & is.na(ssp_id), .(val_array = paste0("{", paste0(idx_ipt_len(as.integer(factor(period)), val, max_len), collapse = ","), "}")),
                                    by = .(dataset_id, type_id, var_id, region)][, .(dataset_id, type_id, var_id, region, val_array = gsub("NA", "NULL", val_array, fixed = TRUE))]

      # historical data
      max_len <- max(length(input_data[ssp_id == 1 & type_id == 1, val]), length(input_data[ssp_id == 1 & type_id == 2, val]), length(input_data[ssp_id == 1 & type_id == 3, val]))
      insert_hist <- input_data[is.na(dataset_id) & (ssp_id == 1), .(val_array = paste0("{", paste0(idx_ipt_len(as.integer(factor(period)), val, max_len), collapse = ","), "}")),
                                by = .(type_id, ssp_id, var_id, region)][, .(type_id, ssp_id, var_id, region, val_array = gsub("NA", "NULL", val_array, fixed = TRUE))]

      # projected data
      max_len <- max(length(input_data[ssp_id == 2 & type_id == 1, val]), length(input_data[ssp_id == 2 & type_id == 2, val]), length(input_data[ssp_id == 2 & type_id == 3, val]),
                    length(input_data[ssp_id == 3 & type_id == 1, val]), length(input_data[ssp_id == 3 & type_id == 2, val]), length(input_data[ssp_id == 3 & type_id == 3, val]),
                    length(input_data[ssp_id == 4 & type_id == 1, val]), length(input_data[ssp_id == 4 & type_id == 2, val]), length(input_data[ssp_id == 4 & type_id == 3, val]))

      insert_proj <- input_data[is.na(dataset_id) & (ssp_id %in% c(2, 3, 4)), .(val_array = paste0("{", paste0(idx_ipt_len(as.integer(factor(period)), val, max_len), collapse = ","), "}")),
                                by = .(ssp_id, type_id, var_id, region)][, .(ssp_id, type_id, var_id, region, val_array = gsub("NA", "NULL", val_array, fixed = TRUE))]
      
      # insert queries
      query_insert_ds <- paste0("
          INSERT INTO ecor_ts_datasets (
            region,
            dataset_id,
            type_id,
            var_id,
            vals
          ) VALUES ",
        paste0("(", "'", insert_datasets$region, "'", ", ", insert_datasets$dataset_id, ", ", insert_datasets$type_id, ", ", insert_datasets$var_id, ", ", "'", insert_datasets$val_array, "'", ")", collapse = ", ")
      )

      query_insert_hist <- paste0("
          INSERT INTO ecor_ts_hist (
            region,
            ssp_id,
            type_id,
            var_id,
            vals
          ) VALUES ",
        paste0("(", "'", insert_hist$region, "'", ", ", insert_hist$ssp_id, ", ", insert_hist$type_id, ", ", insert_hist$var_id, ", ", "'", insert_hist$val_array, "'", ")", collapse = ", ")
      )

      query_insert_proj <- paste0("
          INSERT INTO ecor_ts_proj (
            region,
            ssp_id,
            type_id,
            var_id,
            vals
          ) VALUES ",
        paste0("(", "'", insert_proj$region, "'", ", ", insert_proj$ssp_id, ", ", insert_proj$type_id, ", ", insert_proj$var_id, ", ", "'", insert_proj$val_array, "'", ")", collapse = ", ")
      )

      dbExecute(dbCon, query_insert_ds)
      dbExecute(dbCon, query_insert_hist)
      dbExecute(dbCon, query_insert_proj)
    }
  },
  error = function(e) {
    message("Downscaling failed for ", flp_code, " with error ", e)
  })
}

poolClose(dbCon)