# reformat_existing_uscrn_files.R
# Parse already-downloaded USCRN subhourly .txt files in outdir -> produce cleaned combined dataset
#
# Requirements:
#   install.packages(c("data.table","lubridate"))
#
library(data.table)
library(lubridate)

# -------- USER: set folder that already contains the .txt files -----------
outdir <- "data_raw/NOAA_weather_stations"   # change if needed
stopifnot(dir.exists(outdir))

# -------- base URL for HEADERS.txt (used to get column names) ----------
base_url <- "https://www.ncei.noaa.gov/pub/data/uscrn/products/subhourly01/"
headers_url <- paste0(base_url, "HEADERS.txt")

# -------- read HEADERS.txt dynamically ----------
hdr_lines <- tryCatch(readLines(headers_url, warn = FALSE), error = function(e) NULL)
if (is.null(hdr_lines)) stop("Failed to fetch HEADERS.txt from NOAA: ", headers_url)
header_line <- hdr_lines[grepl("^WBANNO", hdr_lines)][1]
uscrn_cols <- strsplit(trimws(header_line), "\\s+")[[1]]
message("Using ", length(uscrn_cols), " column names from HEADERS.txt")

# -------- helpers ----------
# Normalize text: replace NBSP and other unicode spaces, collapse multiple spaces, convert CRLF -> LF
normalize_text <- function(txt) {
  # convert to single string if vector
  if (length(txt) > 1) txt <- paste(txt, collapse = "\n")
  # replace common unicode non-breaking and strange spaces with normal space
  txt <- gsub("\u00A0", " ", txt, fixed = TRUE)    # NBSP
  txt <- gsub("\u2002|\u2003|\u2009|\u202F|\u205F", " ", txt, perl = TRUE)
  # normalize CRLF to LF
  txt <- gsub("\r\n", "\n", txt, fixed = TRUE)
  # replace tabs with a single space
  txt <- gsub("\t", " ", txt, fixed = TRUE)
  # collapse multiple spaces (and tabs) into a single space (keeps columns separated)
  txt <- gsub("[ \\t]+", " ", txt, perl = TRUE)
  # trim leading/trailing spaces on each line
  txt <- gsub("(?m)^[ ]+|[ ]+$", "", txt, perl = TRUE)
  return(txt)
}

# Robust state extraction from filename (flexible patterns)
extract_state_from_filename <- function(fname) {
  bn <- basename(fname)
  # try match -XX_ or _XX_ or -XX- or _XX- or year-XX_
  m <- regmatches(bn, regexpr("[-_](AL|AK|AZ|AR|CA|CO|CT|DE|FL|GA|HI|IA|ID|IL|IN|KS|KY|LA|MA|MD|ME|MI|MN|MO|MS|MT|NC|ND|NE|NH|NJ|NM|NV|NY|OH|OK|OR|PA|PR|RI|SC|SD|TN|TX|UT|VA|VT|WA|WI|WV|WY)[-_]", bn, ignore.case = TRUE))
  if (length(m) && nchar(m) > 0) {
    st <- toupper(gsub("(^[-_])|([-_]$)", "", m))
    return(st)
  }
  # fallback: find first uppercase two-letter token
  m2 <- regmatches(bn, regexpr("([A-Z]{2})", bn, perl = TRUE))
  if (length(m2) && nchar(m2) == 2) return(toupper(m2))
  return(NA_character_)
}

# Convert selected columns to numeric (best-effort)
numeric_candidates <- c("LONGITUDE","LATITUDE","AIR_TEMPERATURE","PRECIPITATION","SOLAR_RADIATION",
                        "SURFACE_TEMPERATURE","RELATIVE_HUMIDITY","SOIL_MOISTURE_5","SOIL_TEMPERATURE_5",
                        "WETNESS","WIND_1_5")

# -------- main processing ----------
txt_files <- list.files(outdir, pattern = "\\.txt$", full.names = TRUE)
if (length(txt_files) == 0) stop("No .txt files found in ", outdir)

parsed_list <- list()
error_log <- list()

for (f in txt_files) {
  bn <- basename(f)
  message("Processing: ", bn, " (size=", file.info(f)$size, " bytes)")
  # read raw bytes to avoid encoding surprises
  raw <- tryCatch(readChar(f, nchars = file.info(f)$size, useBytes = TRUE), error = function(e) {
    warning("  readChar failed for ", bn, " : ", conditionMessage(e)); return(NA_character_)
  })
  if (is.na(raw) || nchar(raw) == 0) {
    warning("  Empty or unreadable file: ", bn)
    error_log[[bn]] <- "empty_or_unreadable"
    next
  }
  
  # normalize whitespace & encoding quirks
  raw2 <- normalize_text(raw)
  # split into lines, drop empty lines
  lines <- unlist(strsplit(raw2, "\n", fixed = TRUE))
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) {
    warning("  No non-empty lines after cleaning: ", bn)
    error_log[[bn]] <- "no_lines_after_clean"
    next
  }
  
  # split each line on one-or-more whitespace into character vectors
  spl <- strsplit(lines, "\\s+")
  # convert each vector to a list (unnamed!) so rbindlist with use.names=FALSE works
  rowlist <- lapply(spl, function(v) as.list(v))
  
  # rbindlist with use.names = FALSE to avoid "use.names" warnings you saw
  dt <- tryCatch(rbindlist(rowlist, use.names = FALSE, fill = TRUE), error = function(e) {
    warning("  rbindlist failed for ", bn, " : ", conditionMessage(e)); return(NULL)
  })
  if (is.null(dt)) { error_log[[bn]] <- "rbindlist_failed"; next }
  
  # assign column names: exact match or partial mapping
  ncols <- ncol(dt)
  if (ncols == length(uscrn_cols)) {
    setnames(dt, uscrn_cols)
  } else {
    n_assign <- min(ncols, length(uscrn_cols))
    nm <- c(uscrn_cols[1:n_assign], if (ncols > n_assign) paste0("V", seq_len(ncols - n_assign)) else NULL)
    setnames(dt, nm)
    warning(sprintf("  %s: columns=%d expected=%d (assigned partial names)", bn, ncols, length(uscrn_cols)))
  }
  
  # add provenance and state
  dt[, source_file := bn]
  dt[, state := extract_state_from_filename(bn)]
  
  # datetime parsing if possible
  if (all(c("UTC_DATE","UTC_TIME") %in% names(dt))) {
    dt[, UTC_TIME := sprintf("%04s", as.character(UTC_TIME))]
    dt[, datetime_utc := as.POSIXct(paste0(UTC_DATE, " ", UTC_TIME), format = "%Y%m%d %H%M", tz = "UTC")]
  }
  
  # numeric coercion for likely numeric columns
  for (nc in intersect(numeric_candidates, names(dt))) {
    dt[[nc]] <- as.numeric(dt[[nc]])
  }
  
  # Keep dt
  parsed_list[[bn]] <- dt
  
  # optionally: write a per-file cleaned CSV (uncomment if you want)
  # write_path <- file.path(outdir, paste0(sub("\\.txt$", "", bn), "_cleaned.csv"))
  # fwrite(dt, write_path)
}

# Combine
if (length(parsed_list) == 0) {
  stop("No files parsed successfully. See error_log for details:\n", paste(names(error_log), collapse = ", "))
}

big_dt <- rbindlist(parsed_list, use.names = TRUE, fill = TRUE)

# re-order helpful columns
cols_front <- intersect(c("datetime_utc","WBANNO","state","source_file","LONGITUDE","LATITUDE"), names(big_dt))
setcolorder(big_dt, c(cols_front, setdiff(names(big_dt), cols_front)))

# Save combined files
out_rds <- file.path(outdir, "uscrn_combined_reformatted.rds")
out_csv <- file.path(outdir, "uscrn_combined_reformatted.csv")
saveRDS(big_dt, out_rds)
fwrite(big_dt, out_csv)

# Summary
message("Done. Parsed files: ", length(parsed_list), " Combined rows: ", nrow(big_dt), " columns: ", ncol(big_dt))
if (length(error_log) > 0) {
  message("Some files had issues (see error_log list). First 10 entries:")
  print(head(error_log, 10))
}

# Return the table for interactive use
big_dt
