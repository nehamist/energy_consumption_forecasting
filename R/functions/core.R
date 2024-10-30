#core function to prepare analysis data
getAnalysisData <- function(basedata_path = dirname(getwd()), 
                            start_year=2018,
                            end_year=2020,
                            freq = 15, 
                            data_type = "NO_PV_HP",
                            aggr_by = NULL, 
                            holiday = TRUE,
                            weather = TRUE) { 
  basedata_path <- file.path(basedata_path, "Data")
  year <- seq(start_year, end_year)
  
  # create base table with datetime
  start_time <- paste0(start_year, "-01-01 00:00:00")
  if (freq == 15) {
    end_time <- paste0(end_year, "-12-31 23:45:00")
    seq_freq <- "15 mins"
  } else if (freq == 60) {
    end_time <- paste0(end_year, "-12-31 23:00:00")
    seq_freq <- "hour"
  }
  
  start_index_base <- as.POSIXct(start_time, tz = "UTC")
  end_index_base <- as.POSIXct(end_time, tz = "UTC")
  timestamp_base <- seq(from = start_index_base, to = end_index_base, by = seq_freq)
  dt_base <- data.table(timestamp = timestamp_base)
  dt_base <- dt_base[timestamp >= as.POSIXct("2018-06-01 00:00:00", origin = "1970-01-01 00:00:00", tz = "UTC")]
  dt <- data.table(timestamp = c(), P_TOT_SUM = c())
  
  for (yr in year) {
    dt_base_yr <- dt_base[year(timestamp) == yr]
    
    # get consumption data for household
    file_name <- paste0(yr, "_data_", freq, "min.hdf5")
    dt_h5all <- data.table(h5ls(H5Fopen(file.path(basedata_path, file_name))))
    if (data_type == "NO_PV_HH") {
      where_cond <- str_detect(dt_h5all$group, "NO_PV/SFH\\d+/HOUSEHOLD")
    } else if (data_type == "NO_PV_HP") {
      where_cond <- str_detect(dt_h5all$group, "NO_PV/SFH\\d+/HEATPUMP")
    } else if (data_type == "WITH_PV_HH") {
      where_cond <- str_detect(dt_h5all$group, "WITH_PV/SFH\\d+/HOUSEHOLD")
    } else if (data_type == "WITH_PV_HP") {
      where_cond <- str_detect(dt_h5all$group, "WITH_PV/SFH\\d+/HEATPUMP")
    }
    
    for (i in which(where_cond, arr.ind = TRUE)) {
      SFHN <- as.numeric(gsub("\\D", "", dt_h5all[i][["group"]]))
      if (SFHN %in% c(6,8,11,13,15,17,20,24,25,31,35,37,38,39,40))
        next
      print(paste0("data aggregation from year: ", yr, " and household number: ", SFHN))
      dt_h5group <- h5read(file.path(basedata_path, file_name), name = dt_h5all[i][["group"]])
      dt_h5 <- data.table(dt_h5group$table)
      dt_h5[, timestamp := as.POSIXct(index, origin = "1970-01-01 00:00:00", tz = "UTC")]
      dt_h5 <- dt_h5[, c("timestamp", "P_TOT")]
      dt_h5 <- dt_h5[timestamp >= as.POSIXct("2018-06-01 00:00:00", origin = "1970-01-01 00:00:00", tz = "UTC")]
      dt_h5[, week := wday(timestamp)]
      dt_h5[, hour := hour(timestamp)]
      dt_h5[, minute := minute(timestamp)]
      dt_h5$P_TOT[is.nan(dt_h5$P_TOT)] <- NA
      print(paste0("Inital Total NAs: ", sum(is.na(dt_h5$P_TOT))))
      dt_h5[, P_TOT := na.locf(P_TOT, na.rm = F, fromLast = F), by = c("week", "hour", "minute")]
      print(paste0("NAs left after locf: ", sum(is.na(dt_h5$P_TOT))))
      dt_h5[, P_TOT := na.locf(P_TOT, na.rm = F, fromLast = T), by = c("week", "hour", "minute")]
      print(paste0("NAs left after nocb: ", sum(is.na(dt_h5$P_TOT))))
      dt_h5 <- dt_h5[, -c("week", "hour", "minute")]
      setnames(dt_h5, c("P_TOT"), paste0("P_TOT_", gsub("\\D", "", dt_h5all[i][["group"]])))
      dt_base_yr <- merge(dt_base_yr, dt_h5, by = "timestamp", all.x = TRUE)
    }
    dt_base_yr[, P_TOT_SUM := rowSums(.SD), .SDcols = 2:NCOL(dt_base_yr)]
    dt_base_yr <- dt_base_yr[, c("timestamp", "P_TOT_SUM")]
    
    if (is.null(aggr_by) && weather) {
      # get weather data
      file_name <- paste0(yr, "_weather.hdf5")
      dtwt_group <- h5read(file.path(basedata_path, file_name), 
                           name = "WEATHER_SERVICE/IN/WEATHER_TEMPERATURE_TOTAL",
                           bit64conversion="bit64")
      dt_wt <- data.table(dtwt_group$table)
      dt_wt[, index_substr := substr(index, 1, 10)]
      dt_wt[, timestamp := as.POSIXct(as.integer(index_substr), origin = "1970-01-01 00:00:00", tz = "UTC")]
      dt_wt <- dt_wt[, -c("index_substr", "index")]
      setnames(dt_wt, "TEMPERATURE:TOTAL", "temperature")
      dt_base_yr <- merge(dt_base_yr, dt_wt, by = "timestamp", all.x = T)
      
      #solar irradiance
      dtwsi_group <- h5read(file.path(basedata_path, file_name), 
                            name = "WEATHER_SERVICE/IN/WEATHER_SOLAR_IRRADIANCE_GLOBAL",
                            bit64conversion="bit64")
      dt_wsi <- data.table(dtwsi_group$table)
      dt_wsi[, index_substr := substr(index, 1, 10)]
      dt_wsi[, timestamp := as.POSIXct(as.integer(index_substr), origin = "1970-01-01 00:00:00", tz = "UTC")]
      dt_wsi <- dt_wsi[, -c("index_substr", "index")]
      setnames(dt_wsi, "SOLAR_IRRADIANCE:GLOBAL", "solar_irradiance")
      dt_base_yr <- merge(dt_base_yr, dt_wsi, by = "timestamp", all.x = T)
      
      #precipitation rate
      #dtwpr_group <- h5read(file.path(basedata_path, file_name), 
      #name = "WEATHER_SERVICE/IN/WEATHER_PRECIPITATION_RATE_TOTAL",
      #bit64conversion="bit64")
      #dt_wpr <- data.table(dtwpr_group$table)
      #dt_wpr[, index_substr := substr(index, 1, 10)]
      #dt_wpr[, timestamp := as.POSIXct(as.integer(index_substr), origin = "1970-01-01 00:00:00", tz = "UTC")]
      #dt_wpr <- dt_wpr[, -c("index_substr", "index")]
      #setnames(dt_wpr, "PRECIPITATION_RATE:TOTAL", "precipitation_rate")
      #dt_base_yr <- merge(dt_base_yr, dt_wpr, by = "timestamp", all.x = T)
      
    }
    dt <- rbindlist(list(dt, dt_base_yr))
  }
  
  if (!is.null(aggr_by) && aggr_by == "day" && !holiday) {
    dt[, date := as.Date(timestamp)]
    dt_aggr <- dt[, lapply(.SD,sum), by = c("date")]
    dt_aggr <- dt_aggr[, -c("timestamp")]
    return(dt_aggr)
  } else if (!is.null(aggr_by) && aggr_by == "week") {
    dt[, week := week(timestamp)]
    dt[, year := year(timestamp)]
    dt_aggr <- dt[, lapply(.SD,sum), by = c("year", "week")]
    dt_aggr <- dt_aggr[, -c("timestamp")]
    return(dt_aggr)
  } else if (!is.null(aggr_by) && aggr_by == "month") {
    dt[, month := month(timestamp)]
    dt[, year := year(timestamp)]
    dt_aggr <- dt[, lapply(.SD,sum), by = c("year", "month")]
    dt_aggr <- dt_aggr[, -c("timestamp")]
    return(dt_aggr)
  } else if (is.null(aggr_by) && (holiday || weather)) {
    if (holiday) {
      dt_holdiay <- getHolidayData(start_year, end_year)
      dt[, date := as.Date(timestamp)]
      dt <- merge(dt, dt_holdiay, by = "date")
      dt <- dt[, -c("date")]
    }
    if (weather) {
      dt[, temperature:= na.locf(temperature)]
      dt[, solar_irradiance:= na.locf(solar_irradiance)]
      #dt[, precipitation_rate:= na.locf(precipitation_rate)]
    }
    return(dt)
  } else if (!is.null(aggr_by) && aggr_by == "day" && holiday) {
    dt_holdiay <- getHolidayData(start_year, end_year)
    dt[, date := as.Date(timestamp)]
    dt <- dt[, -c("timestamp")]
    dt_aggr <- dt[, lapply(.SD,sum), by = c("date")]
    dt_aggr <- merge(dt_aggr, dt_holdiay, by = "date")
    return(dt_aggr)
  } else {
    return(dt)
  }
}

# get hour dummies
getHourDummies <- function(timestamp) {
  hr_ <- factor(format(as.POSIXct(timestamp), format = "%H"))
  dummies <- model.matrix(~hr_-1)
  return(dummies)
}

# get minute dummies
getMinuteDummies <- function(timestamp) {
  dummies <- data.table()
  minute_ <- factor(format(as.POSIXct(timestamp), format = "%M"))
  if (!length(unique(minute_)) == 1)
    dummies <- model.matrix(~minute_-1)
  return(dummies)
}

# get weekday dummies
getWDayDummies <- function(timestamp) {
  wday_ <- factor(weekdays(timestamp))
  dummies <- model.matrix(~wday_-1)
  return(dummies)
}

# get month dummies
getMonthDummies <- function(timestamp) {
  mnth_ <- factor(months(timestamp))
  dummies <- model.matrix(~mnth_-1)
  return(dummies)
}

# this function works only for the years 2018, 2019, 2020
getHolidayData <- function(start_year = 2018, end_year = 2020) {
  start_time <- as.POSIXct(paste0(start_year, "-01-01"), tz = "UTC")
  end_time <- as.POSIXct(paste0(end_year, "-12-31"), tz = "UTC")
  timestamp <- seq(from = start_time, to = end_time, by = "day")
  dt <- data.table(date = as.Date(timestamp))
  holiday_vec <- c("neujahr", "karfreitag", "ostermontag", "tag_der_arbeit", 
                   "christi_himmelfahrt", "pfingsmontag", "tag_der_deutschen_einheit",
                   "reformationstag", "weihnachten", "weihnachtstag2")
  for (holiday in holiday_vec) {
    dt[, (holiday) := 0]
    if (holiday == "neujahr") {
      dt[date %in% c("2018-01-01", "2019-01-01", "2020-01-01"), (holiday) := 1]
    } else if (holiday == "karfreitag") {
      dt[date %in% c("2018-03-30", "2019-04-19", "2020-04-10"), (holiday) := 1] 
    } else if (holiday == "ostermontag") {
      dt[date %in% c("2018-04-02", "2019-04-22", "2020-04-13"), (holiday) := 1]
    } else if (holiday == "tag_der_arbeit") {
      dt[date %in% c("2018-05-01", "2019-05-01", "2020-05-01"), (holiday) := 1]
    } else if (holiday == "christi_himmelfahrt") {
      dt[date %in% c("2018-05-10", "2019-05-30", "2020-05-21"), (holiday) := 1]
    } else if (holiday == "pfingsmontag") {
      dt[date %in% c("2018-05-21", "2019-06-10", "2020-06-01"), (holiday) := 1]
    } else if (holiday == "tag_der_deutschen_einheit") {
      dt[date %in% c("2018-10-03", "2019-10-03", "2020-10-03"), (holiday) := 1]
    } else if (holiday == "roformationstag") {
      dt[date %in% c("2018-10-31", "2019-10-31", "2020-10-31"), (holiday) := 1]
    } else if (holiday == "weihnachten") {
      dt[date %in% c("2018-12-25", "2019-12-25", "2020-12-25"), (holiday) := 1]
    } else if (holiday == "weihnachtstag2") {
      dt[date %in% c("2018-12-26", "2019-12-26", "2020-12-26"), (holiday) := 1]
    }
  }
  return(dt)
}