library(data.table)
library(lubridate)

# Adjust these paths if your project structure differs
in_dir <- "data_raw/NOAA_weather_stations"
out_dir <- "data_processed/NOAA_weather_stations_processed"

# Expected column names (23 columns)
headers <- c(
  "WBANNO", "UTC_DATE", "UTC_TIME", "LST_DATE", "LST_TIME",
  "CRX_VN", "LONGITUDE", "LATITUDE", "AIR_TEMPERATURE", "PRECIPITATION",
  "SOLAR_RADIATION", "SR_FLAG", "SURFACE_TEMPERATURE", "ST_TYPE", "ST_FLAG",
  "RELATIVE_HUMIDITY", "RH_FLAG", "SOIL_MOISTURE_5", "SOIL_TEMPERATURE_5",
  "WETNESS", "WET_FLAG", "WIND_1_5", "WIND_FLAG"
)

# Columns that should be coerced to numeric where possible
numeric_cols <- c(
  "LONGITUDE", "LATITUDE", "AIR_TEMPERATURE", "PRECIPITATION",
  "SOLAR_RADIATION", "SURFACE_TEMPERATURE", "RELATIVE_HUMIDITY",
  "SOIL_MOISTURE_5", "SOIL_TEMPERATURE_5", "WETNESS", "WIND_1_5"
)

# Helper: safe read of whitespace-delimited data while skipping comment header lines.
# NOAA files often use leading lines that start with "#"; read.table's comment.char handles that.
read_station_file <- function(path) {
  # read.table with comment.char="#" will ignore comment lines
  df <- tryCatch(
    read.table(path,
               header = FALSE,
               sep = "",
               strip.white = TRUE,
               comment.char = "#",
               fill = TRUE,
               stringsAsFactors = FALSE,
               na.strings = c("", "NA", "-9999", "-9999.0")),
    error = function(e) {
      warning("Failed to read file: ", path, " -- ", conditionMessage(e))
      return(NULL)
    }
  )
  df
}

files <- list.files(in_dir, pattern = "\\.txt$", full.names = TRUE)
if (length(files) == 0) stop("No .txt files found in ", in_dir)

for (f in files) {
  message("Processing: ", f)
  df <- read_station_file(f)
  if (is.null(df)) next
  
  
  expected_n <- length(headers)
  actual_n <- ncol(df)
  
  
  if (actual_n < expected_n) {
    # pad with NA columns
    message(" Found ", actual_n, " columns, padding to ", expected_n)
    for (i in (actual_n+1):expected_n) df[[i]] <- NA
  } else if (actual_n > expected_n) {
    message(" Found ", actual_n, " columns, keeping first ", expected_n)
    df <- df[, 1:expected_n]
  }
  
  
  # assign column names
  colnames(df) <- headers
  
  
  # Trim whitespace from character columns (dates/times/flags)
  df[] <- lapply(df, function(x) if (is.character(x)) trimws(x) else x)
  
  
  # Coerce numeric columns where possible
  for (col in numeric_cols) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }
  
  # Convert UTC_TIME (e.g., 5, 10, 125) into hh:mm format like 00:05, 00:10, 01:25
  df$UTC_TIME <- sprintf("%04d", as.integer(df$UTC_TIME))           # pad to 4 digits
  df$UTC_TIME <- sub("^(\\d{2})(\\d{2})$", "\\1:\\2", df$UTC_TIME)  # insert colon
  #Take UTC_DATE format of YYYYMMDD and turn into YYYY-MM-DD
  df$UTC_DATE <- sub("^(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3", df$UTC_DATE) 
  df$outage_start_dt <- paste(df$UTC_DATE, df$UTC_TIME, ":00")
  df$outage_start_dt <- ymd_hms(df$outage_start_dt)
  
  # Save as .rds using the WBANNO
  out_name <- paste0(tools::file_path_sans_ext(df$WBANNO[1]), ".rds")
  out_path <- file.path(out_dir, out_name)
  saveRDS(df, out_path)
  message(" Saved: ", out_path)
}

message("All done. Processed ", length(files), " files.")