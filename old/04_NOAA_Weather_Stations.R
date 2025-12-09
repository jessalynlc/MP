library(data.table)
library(lubridate)

processed_dir <- "data_processed/NOAA_weather_stations_processed"  # where your per-file .rds live
merged_out_dir <- file.path(processed_dir, "merged_by_station")
backup_dir <- file.path(processed_dir, "backup_before_merge")

dir.create(merged_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)

rds_files <- list.files(processed_dir, pattern = "\\.rds$", full.names = TRUE, recursive = FALSE)
# if you saved subfolders, set recursive = TRUE
if (length(rds_files) == 0) stop("No .rds files found in ", processed_dir)

message("Found ", length(rds_files), " .rds files. Backing them up to: ", backup_dir)
# copy originals to backup (safe)
file.copy(rds_files, backup_dir, overwrite = TRUE)

# helper: generate station id from file or df
infer_station_from_filename <- function(fn) {
  nm <- basename(fn)
  # try to catch digits like 3758 or WBAN patterns
  m <- regmatches(nm, regexpr("\\d{3,6}", nm)) # first run of 3-6 digits
  if (length(m) && nzchar(m)) return(m)
  return(NA_character_)
}

# read all files and annotate
all_loaded <- lapply(rds_files, function(f) {
  df <- tryCatch(readRDS(f), error = function(e) { warning("can't read ", f); return(NULL) })
  if (is.null(df)) return(NULL)
  # ensure it's a data.frame / tibble
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  # try to coerce/ensure outage_start_dt column exists and is POSIXct
  if (!"outage_start_dt" %in% names(df)) {
    # try to construct from UTC_DATE and UTC_TIME
    if ("UTC_DATE" %in% names(df) && "UTC_TIME" %in% names(df)) {
      utc_time_vec <- ifelse(is.na(df$UTC_TIME), NA, sprintf("%04d", as.integer(as.numeric(as.character(df$UTC_TIME)))))
      utc_time_vec <- ifelse(is.na(utc_time_vec), NA, sub("^(\\d{2})(\\d{2})$", "\\1:\\2", utc_time_vec))
      dt_str <- ifelse(is.na(df$UTC_DATE) | is.na(utc_time_vec), NA, paste(df$UTC_DATE, utc_time_vec, ":00", sep = ""))
      df$outage_start_dt <- suppressWarnings(ymd_hms(dt_str, tz = "UTC"))
    } else {
      df$outage_start_dt <- NA
    }
  } else {
    # coerce to POSIXct if it's character
    if (!inherits(df$outage_start_dt, "POSIXct")) {
      df$outage_start_dt <- suppressWarnings(ymd_hms(as.character(df$outage_start_dt), tz = "UTC"))
    }
  }
  
  # ensure WBANNO exists (character)
  if (!"WBANNO" %in% names(df)) {
    df$WBANNO <- NA_character_
  } else {
    df$WBANNO <- as.character(df$WBANNO)
  }
  
  # If WBANNO is missing, infer from filename
  if (all(is.na(df$WBANNO)) || any(is.na(df$WBANNO))) {
    inferred <- infer_station_from_filename(f)
    if (!is.na(inferred)) df$WBANNO[is.na(df$WBANNO)] <- inferred
  }
  
  # attach provenance
  attr(df, "source_file") <- f
  df
})

# drop NULLs
all_loaded <- Filter(Negate(is.null), all_loaded)

# combine into a single big table with file provenance
big <- bind_rows(lapply(all_loaded, function(x) { x$.__source_file <- attr(x, "source_file"); x }))

# ensure WBANNO exists
if (!"WBANNO" %in% names(big)) stop("No WBANNO available in any file; cannot group by station.")

# convert to tibble and standardize types
big <- as_tibble(big)

# create a merge key: use outage_start_dt if available, else UTC_DATE + UTC_TIME
big <- big %>%
  mutate(
    merge_key = ifelse(!is.na(outage_start_dt),
                       format(outage_start_dt, "%Y-%m-%dT%H:%M:%S"),
                       paste0(ifelse(is.na(UTC_DATE), "NA", UTC_DATE), "_", ifelse(is.na(UTC_TIME), "NA", UTC_TIME))
    )
  )

# for each station, combine and deduplicate by merge_key
stations <- unique(big$WBANNO)

message("Merging data for ", length(stations), " stations...")

for (st in stations) {
  sub <- big %>% filter(WBANNO == st)
  before_n <- nrow(sub)
  
  # If you want to prefer rows where more numeric columns are non-NA, compute completeness score:
  score_cols <- c("AIR_TEMPERATURE", "PRECIPITATION", "SOLAR_RADIATION", "SURFACE_TEMPERATURE",
                  "RELATIVE_HUMIDITY", "SOIL_MOISTURE_5", "SOIL_TEMPERATURE_5", "WIND_1_5")
  score_cols <- intersect(score_cols, names(sub))
  if (length(score_cols) > 0) {
    sub <- sub %>% rowwise() %>%
      mutate(completeness = sum(!is.na(across(all_of(score_cols))))) %>% ungroup()
  } else {
    sub$completeness <- 0
  }
  
  # arrange so we keep the "best" row per merge_key (higher completeness), then by outage_start_dt
  sub <- sub %>%
    arrange(merge_key, desc(completeness), ifelse(is.na(outage_start_dt), as.POSIXct("1970-01-01", tz="UTC"), outage_start_dt))
  
  # deduplicate keeping first (best) per merge_key
  merged <- sub %>% distinct(merge_key, .keep_all = TRUE)
  
  # drop helper cols
  merged <- merged %>% select(-merge_key, -completeness)
  
  # sort by outage_start_dt ascending if present
  if ("outage_start_dt" %in% names(merged)) merged <- merged %>% arrange(outage_start_dt)
  
  out_file <- file.path(merged_out_dir, paste0(st, ".rds"))
  saveRDS(merged, out_file)
  message("Station ", st, ": files merged (", before_n, " rows -> ", nrow(merged), " rows). Saved: ", out_file)
}

message("Merge complete. Backups are in: ", backup_dir)
