## Create pre-downscaled datasets for plotting
library(climr)
library(data.table)
library(terra)
library(sf)

bnds <- vect("Ecoregions.gpkg")
dem <- rast("../Common_Files/NA_DEM_250.tif")

samps <- spatSample(bnds, size = rep(150, length(bnds)), method = "random")

gcm_id <- data.table(gcm = list_gcms(), gcm_id = seq_along(list_gcms()))
ssp_id <- data.table(ssp = list_ssps(), ssp_id = seq_along(list_ssps()))
temp <- list_runs_ssp(gcm = list_gcms(), ssp = list_ssps()) |> unique()
run_id <- data.table(run = temp, run_id = seq_along(temp))
var_id <- data.table(var = list_vars(), var_id = seq_along(list_vars()))
dataset_id <- data.table(dataset = c("mswx.blend","cru.gpcc","climatena"), dataset_id = 1:3)

library(RPostgres)
dbCon <- dbConnect(RPostgres::Postgres(),dbname = 'climr',
                  host = '',
                  port = 5432,
                  user = 'postgres',
                  password = '')


test <- dbGetQuery(dbCon, "select * from ds_timeseries where region = '10.1.3'")


dbExecute(dbCon, "create table ds_timeseries (region varchar(50), gcm_id smallint, ssp_id smallint, run_id smallint,  dataset_id smallint, var_id smallint, period smallint, value real);")

for(flp_code in unique(samps$NA_L3CODE)[-(1:2)]) {
  message("Processing ",flp_code)
  sampsub <- samps[samps$NA_L3CODE == flp_code]
  
  crds <- extract(dem, sampsub, xy = TRUE, ID = TRUE)
  names(crds) <- c("id","elev","lon","lat")
  
  dat <- downscale(
    xyz = crds,
    gcms = list_gcms()[c(1, 4, 6, 7, 10, 11, 12)],
    ssps = list_ssps(),
    max_run = 5,
    return_refperiod = FALSE,
    ensemble_mean = TRUE,
    obs_ts_dataset = c("mswx.blend","cru.gpcc","climatena"),
    obs_years = list_obs_years(),
    gcm_hist_years = list_gcm_hist_years(),
    gcm_ssp_years = list_gcm_ssp_years(),
    vars = list_vars(),
    db_option = "database"
  )
  
  data.agg <- dat[, lapply(.SD, mean), by = .(GCM, SSP, RUN, PERIOD, DATASET), .SDcols = -c("id", "GCM", "SSP", "RUN", "PERIOD", "DATASET")]
  rm (dat)
  setnames(data.agg, old = c("GCM", "SSP", "RUN", "PERIOD", "DATASET"), new = c("gcm","ssp","run","period", "dataset"))
  data.agg[gcm_id, gcm_id := i.gcm_id, on = "gcm"][
    ssp_id, ssp_id := i.ssp_id, on = "ssp"][
      run_id, run_id := i.run_id, on = "run"][
        dataset_id, dataset_id := i.dataset_id, on = "dataset"]
  
  data.agg[,c("gcm","ssp","run","dataset") := NULL]
  dat_l <- melt(data.agg, id.vars = c("gcm_id","ssp_id","run_id","period", "dataset_id"), variable.name = "var",value.name = "value")
  dat_l[var_id, var_id := i.var_id, on = "var"]
  dat_l[,var := NULL]
  dat_l[,region := flp_code]
  setcolorder(dat_l, c("region","gcm_id","ssp_id","run_id","dataset_id","var_id","period","value"))
  dat_l <- dat_l[period != "1961_1990",]
  dat_l[,period := as.integer(period)]
  dbWriteTable(dbCon, "ds_timeseries", dat_l, append = TRUE, row.names = FALSE)
  rm(dat_l, data.agg)
  gc()
}


##bivariate
dbExecute(dbCon, "drop table ds_bivariate")
dbExecute(dbCon, "create table ds_bivariate (region varchar(50), gcm_id smallint, ssp_id smallint, run_id smallint, var_id smallint, period varchar(12), value real);")
samps <- crop(samps, ext(-179.0625, -51.5625, 14.375, 83.125))

for(flp_code in unique(samps$NA_L3CODE)[-c(1:53)]) {
  message("Processing ",flp_code)
  sampsub <- samps[samps$NA_L3CODE == flp_code]
  
  crds <- extract(dem, sampsub, xy = TRUE, ID = TRUE)
  names(crds) <- c("id","elev","lon","lat")
  
  data <- downscale(
    xyz = crds,
    obs_period = list_obs_periods(),
    gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
    ssps = list_ssps(),
    gcm_periods = list_gcm_periods(),
    max_run = 10,
    ensemble_mean = TRUE,
    vars = list_vars()
  )
  
  
  data.agg <- data[, lapply(.SD, mean), by = .(GCM, SSP, RUN, PERIOD), .SDcols = -c("id", "GCM", "SSP", "RUN", "PERIOD")]
  setnames(data.agg, old = c("GCM", "SSP", "RUN", "PERIOD"), new = c("gcm","ssp","run","period"))
  data.agg[gcm_id, gcm_id := i.gcm_id, on = "gcm"][
    ssp_id, ssp_id := i.ssp_id, on = "ssp"][
      run_id, run_id := i.run_id, on = "run"]
  
  data.agg[,c("gcm","ssp","run") := NULL]
  dat_l <- melt(data.agg, id.vars = c("gcm_id","ssp_id","run_id","period"), variable.name = "var",value.name = "value")
  dat_l[var_id, var_id := i.var_id, on = "var"]
  dat_l[,var := NULL]
  dat_l[,region := flp_code]
  setcolorder(dat_l, c("region","gcm_id","ssp_id","run_id","var_id","period","value"))
  dbWriteTable(dbCon, "ds_bivariate", dat_l, append = TRUE, row.names = FALSE)
  rm(dat_l, data, data.agg)
  gc()
}

dbExecute(dbCon, "create index on ds_bivariate (region, var_id, ssp_id)")


dat <- plot_timeSeries_input(crds, gcms = list_gcms()[1:3], max_run = 3,  obs_ts_dataset = c("cru.gpcc","mswx.blend"))

setnames(dat2, c("gcm","ssp","run","period","dataset","var","value"))

plot_timeSeries(dat)
