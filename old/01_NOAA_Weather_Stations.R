library(data.table)
library(lubridate)

# ---------------- USER CONFIG ----------------
states <- c('FL', 'GA', 'IL', 'IN', 'KY', 'NC', 'OH', 'SC', 'VA')           # <- states you want (2-letter USPS codes)
years  <- 2023:2025               # <- year range to fetch
outdir <- "data_raw/NOAA_weather_stations"   # <- where files/save outputs go
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

base_url <- "https://www.ncei.noaa.gov/pub/data/uscrn/products/subhourly01/"

# ---------------- Helpers: listing & parsing directory HTML ----------------
# List .txt filenames in a NOAA HTTPS directory (returns decoded filenames)
list_http_txt_files <- function(year_dir) {
  html <- tryCatch(readLines(year_dir, warn = FALSE), error = function(e) {
    warning("Failed to read index ", year_dir, " : ", conditionMessage(e)); return(NULL)
  })
  if (is.null(html)) return(character(0))
  html <- paste(html, collapse = "\n")
  # extract hrefs ending with .txt (may include encoded characters)
  matches <- gregexpr("href\\s*=\\s*\"([^\"]+\\.txt)\"", html, perl = TRUE)
  if (matches[[1]][1] == -1) return(character(0))
  raw <- regmatches(html, matches)[[1]]
  # extract inner quoted string and URL-decode basename
  files <- vapply(raw, function(m) {
    inner <- sub('^href\\s*=\\s*"(.*?)"$', "\\1", sub('^href\\s*=\\s*"', "", sub('"$', "", m)))
    # basename (strip path) then URL-decode
    utils::URLdecode(basename(inner))
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
  unique(files)
}

# Build a safe (encoded) download URL for a given year dir and filename
make_download_url <- function(year, filename) {
  # encode the filename portion only
  encoded_fname <- utils::URLencode(filename, reserved = TRUE)
  paste0(base_url, year, "/", encoded_fname)
}

# Robust download with retries and basic sanity check (not HTML)
download_with_check <- function(url, dest, retries = 3, pause = 1) {
  for (i in seq_len(retries)) {
    ok <- tryCatch({
      download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      FALSE
    })
    if (!ok) {
      Sys.sleep(pause)
      next
    }
    # basic content check: not tiny and not HTML
    sz <- file.info(dest)$size
    if (is.na(sz) || sz < 200) { # too small: likely error page
      file.remove(dest)
      Sys.sleep(pause); next
    }
    # peek first bytes
    head_txt <- tryCatch({
      con <- file(dest, "rb"); on.exit(close(con), add = TRUE)
      readChar(con, nchars = 400, useBytes = TRUE)
    }, error = function(e) NA_character_)
    if (!is.na(head_txt) && grepl("<!DOCTYPE|<html|HTTP/1\\.|404 Not Found", head_txt, ignore.case = TRUE)) {
      # looks like an HTML/error page
      file.remove(dest)
      Sys.sleep(pause); next
    }
    # success
    return(TRUE)
  }
  return(FALSE)
}

# ---------------- Get HEADERS.txt and column names ----------------
headers_url <- paste0(base_url, "HEADERS.txt")
hdr_lines <- tryCatch(readLines(headers_url, warn = FALSE), error = function(e) NULL)
if (is.null(hdr_lines)) stop("Failed to read HEADERS.txt from NOAA: ", headers_url)
header_line <- hdr_lines[grepl("^WBANNO", hdr_lines)][1]
uscrn_cols <- strsplit(trimws(header_line), "\\s+")[[1]]
message("Using ", length(uscrn_cols), " column names from HEADERS.txt")

# ---------------- Find matching files on server ----------------
message("Searching NOAA directories for years: ", paste(years, collapse = ", "), " and states: ", paste(states, collapse = ", "))
matched_files <- list()  # will hold list of (year, fname)

for (yr in years) {
  yr_dir <- paste0(base_url, yr, "/")
  message("Listing ", yr_dir)
  files <- list_http_txt_files(yr_dir)
  if (length(files) == 0) { message("  No files found for year ", yr); next }
  for (st in states) {
    # flexible pattern: match -ST_ or _ST_ or -ST- or _ST- or ST at start
    pat <- paste0("(^|[-_])", st, "([_-]|$)")
    matched <- files[grepl(pat, files, ignore.case = TRUE)]
    if (length(matched) > 0) {
      for (m in matched) matched_files[[length(matched_files)+1]] <- list(year = yr, fname = m)
      message("  Year ", yr, " found ", length(matched), " file(s) for ", st)
    } else {
      message("  Year ", yr, ": no files for ", st)
    }
  }
}

if (length(matched_files) == 0) stop("No files matched your states & years on NOAA server. Try expanding years or checking state codes.")

# Optional: deduplicate by filename (in case of duplicates)
unique_by_fname <- function(lst) {
  seen <- character(0); out <- list()
  for (it in lst) {
    if (!it$fname %in% seen) { out[[length(out)+1]] <- it; seen <- c(seen, it$fname) }
  }
  out
}
matched_files <- unique_by_fname(matched_files)
message("Total unique matched files: ", length(matched_files))

# ---------------- Download matched files into outdir ----------------
message("Starting downloads into: ", outdir)
downloaded_local_paths <- character(0)
for (it in matched_files) {
  yr <- it$year; fname <- it$fname
  url <- make_download_url(yr, fname)
  dest <- file.path(outdir, fname)
  # If file exists and passes basic sanity, skip download
  need_dl <- TRUE
  if (file.exists(dest)) {
    # quick sanity check
    ok <- tryCatch({
      head_txt <- { con <- file(dest, "rb"); on.exit(close(con), add = TRUE); readChar(con, nchars = 400, useBytes = TRUE) }
      sz <- file.info(dest)$size
      !(is.na(sz) || sz < 200 || grepl("<html|<!DOCTYPE|404 Not Found|HTTP/1\\.", head_txt, ignore.case = TRUE))
    }, error = function(e) FALSE)
    if (ok) {
      message("Already have valid file: ", fname)
      need_dl <- FALSE
    } else {
      message("Existing file looks invalid; will re-download: ", fname)
      try(file.remove(dest), silent = TRUE)
      need_dl <- TRUE
    }
  }
  if (need_dl) {
    message("Downloading: ", fname)
    success <- download_with_check(url, dest, retries = 3, pause = 1)
    if (!success) {
      warning("Failed to download valid file for: ", fname, " (skipping). URL tried: ", url)
      next
    }
    message("  Download OK: ", fname)
  }
  downloaded_local_paths <- c(downloaded_local_paths, dest)
}

if (length(downloaded_local_paths) == 0) stop("No files successfully downloaded.")

# ---------------- Robust parse (split on whitespace) ----------------
# This avoids fread whitespace quirks and NBSP issues
parse_file_whitespace <- function(path) {
  raw_lines <- readLines(path, warn = FALSE)
  # remove completely empty lines
  raw_lines <- raw_lines[nzchar(trimws(raw_lines))]
  if (length(raw_lines) == 0) return(NULL)
  # split each line by one-or-more whitespace
  spl <- strsplit(raw_lines, "\\s+")
  # convert to list-of-lists suitable for rbindlist(fill=TRUE)
  rowlist <- lapply(spl, function(v) as.list(v))
  dt <- rbindlist(rowlist, fill = TRUE)
  # assign column names if possible
  if (ncol(dt) == length(uscrn_cols)) {
    setnames(dt, uscrn_cols)
  } else {
    n_assign <- min(ncol(dt), length(uscrn_cols))
    nm <- c(uscrn_cols[1:n_assign], if (ncol(dt) > n_assign) paste0("V", seq_len(ncol(dt)-n_assign)) else NULL)
    setnames(dt, nm)
    warning("File ", basename(path), " columns=", ncol(dt), " expected=", length(uscrn_cols), " -> assigned partial names.")
  }
  # attempt types: convert numeric-looking cols to numeric (best-effort)
  num_candidates <- c("LONGITUDE","LATITUDE","AIR_TEMPERATURE","PRECIPITATION","SOLAR_RADIATION",
                      "SURFACE_TEMPERATURE","RELATIVE_HUMIDITY","SOIL_MOISTURE_5","SOIL_TEMPERATURE_5",
                      "WETNESS","WIND_1_5")
  for (nc in intersect(num_candidates, names(dt))) set(dt, j = nc, value = as.numeric(dt[[nc]]))
  # datetime
  if (all(c("UTC_DATE","UTC_TIME") %in% names(dt))) {
    dt[, UTC_TIME := sprintf("%04s", as.character(UTC_TIME))]
    dt[, datetime_utc := as.POSIXct(paste0(UTC_DATE, " ", UTC_TIME), format = "%Y%m%d %H%M", tz = "UTC")]
  }
  # provenance and state extraction
  dt[, source_file := basename(path)]
  # try flexible state extraction from filename
  bn <- basename(path)
  st <- toupper(sub(".*[-_](..)[-_ ].*", "\\1", bn))
  if (!grepl("^[A-Z]{2}$", st)) {
    # fallback search for -XX_ or _XX_
    mm <- regmatches(bn, regexpr("[-_](AL|AK|AZ|AR|CA|CO|CT|DE|FL|GA|HI|IA|ID|IL|IN|KS|KY|LA|MA|MD|ME|MI|MN|MO|MS|MT|NC|ND|NE|NH|NJ|NM|NV|NY|OH|OK|OR|PA|PR|RI|SC|SD|TN|TX|UT|VA|VT|WA|WI|WV|WY)[-_]", bn, ignore.case = TRUE))
    if (length(mm) && nchar(mm) > 0) st <- toupper(gsub("(^[-_])|([-_]$)", "", mm))
    else st <- NA_character_
  }
  dt[, state := st]
  return(dt[])
}

# Parse all downloaded files
message("Parsing downloaded files...")
parsed_list <- list()
for (p in downloaded_local_paths) {
  message("Parsing: ", basename(p))
  dtp <- tryCatch(parse_file_whitespace(p), error = function(e) { warning("Parse failed: ", basename(p), " : ", conditionMessage(e)); NULL })
  if (!is.null(dtp)) parsed_list[[length(parsed_list)+1]] <- dtp
}

if (length(parsed_list) == 0) stop("No files parsed successfully.")

big_dt <- rbindlist(parsed_list, use.names = TRUE, fill = TRUE)

# reorder to helpful columns first
cols_front <- intersect(c("datetime_utc","WBANNO","state","source_file","LONGITUDE","LATITUDE"), names(big_dt))
setcolorder(big_dt, c(cols_front, setdiff(names(big_dt), cols_front)))

# Save
out_prefix <- paste0("uscrn_subhourly_states_", paste(states, collapse = "-"), "_", min(years), "-", max(years))
out_rds <- file.path(outdir, paste0(out_prefix, ".rds"))
out_csv <- file.path(outdir, paste0(out_prefix, ".csv"))

saveRDS(big_dt, file = out_rds)
fwrite(big_dt, file = out_csv)

message("Done. Combined rows: ", nrow(big_dt))
message("Saved RDS: ", normalizePath(out_rds))
message("Saved CSV: ", normalizePath(out_csv))

# Return combined DT invisibly (if run interactively)
invisible(big_dt)
