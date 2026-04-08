# Clear console and environment
cat("\014")
rm(list = ls())

# Required packages
packages <- c("httr", "jsonlite", "googlesheets4", "base64enc", "dplyr")
invisible(lapply(packages, library, character.only = TRUE))

# Authenticate Google Sheets
# --- GitHub Actions ---
json_key <- rawToChar(base64decode(Sys.getenv("GCP_SHEETS_KEY_B64")))
temp_json_file <- tempfile(fileext = ".json")
writeLines(json_key, temp_json_file)
gs4_auth(path = temp_json_file)

# --- Local RStudio (comment out above and uncomment below) ---
# gs4_auth(cache = ".secrets", email = "joebond008@gmail.com")

sheet_id <- "1yymwaiKDeqpSrYcQ4Av9vgv59HGxiyIYJ05LYGkog3s"

# =============================================================================
# DATES
# end_date = yesterday (script runs in AM, data through previous day)
# all windows are anchored to end_date
# =============================================================================
end_date              <- Sys.Date() - 1
end_date_str          <- format(end_date, "%Y-%m-%d")
season_start_date     <- as.Date("2026-03-25")

cat(sprintf("End date: %s\n", end_date_str))

# Start date = N days back from end_date (inclusive), floored at season start
get_start_date_str <- function(days_back) {
  start <- end_date - (days_back - 1)
  start <- max(start, season_start_date)
  format(start, "%Y-%m-%d")
}

# =============================================================================
# URL BUILDERS — always 2026, month=1000
# =============================================================================
build_hitting_url <- function(start_date_str) {
  sprintf(
    "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2026&season1=2026&startdate=%s&enddate=%s&month=1000&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR",
    start_date_str, end_date_str
  )
}

build_pitching_url <- function(start_date_str) {
  sprintf(
    "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=pit&lg=all&qual=1&season=2026&season1=2026&startdate=%s&enddate=%s&month=1000&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR",
    start_date_str, end_date_str
  )
}

hitting_1d_url  <- build_hitting_url(get_start_date_str(1))
hitting_7d_url  <- build_hitting_url(get_start_date_str(7))
hitting_14d_url <- build_hitting_url(get_start_date_str(14))
hitting_30d_url <- build_hitting_url(get_start_date_str(30))

pitching_1d_url  <- build_pitching_url(get_start_date_str(1))
pitching_14d_url <- build_pitching_url(get_start_date_str(14))
pitching_30d_url <- build_pitching_url(get_start_date_str(30))

cat("\n--- Hitting URLs ---\n")
cat(hitting_1d_url,  "\n")
cat(hitting_7d_url,  "\n")
cat(hitting_14d_url, "\n")
cat(hitting_30d_url, "\n")
cat("\n--- Pitching URLs ---\n")
cat(pitching_1d_url,  "\n")
cat(pitching_14d_url, "\n")
cat(pitching_30d_url, "\n")

# =============================================================================
# SAFE API CALL
# =============================================================================
safe_api_call <- function(url, label) {
  tryCatch({
    response    <- GET(url)
    raw_text    <- content(response, as = "text", encoding = "UTF-8")
    data_parsed <- fromJSON(raw_text)
    
    if (is.null(data_parsed) || length(data_parsed) == 0) {
      cat(sprintf("No %s data (empty response).\n", label))
      return(NULL)
    }
    
    records <- data_parsed$data
    
    if (is.null(records) || length(records) == 0) {
      cat(sprintf("No %s data (empty data array).\n", label))
      return(NULL)
    }
    
    df <- bind_rows(lapply(records, function(x) {
      x[sapply(x, is.null)] <- NA
      x <- lapply(x, function(v) if (length(v) == 1) as.character(v) else NA_character_)
      as.data.frame(x, stringsAsFactors = FALSE)
    }))
    
    cat(sprintf("[%s] %d rows\n", label, nrow(df)))
    return(df)
    
  }, error = function(e) {
    cat(sprintf("Error fetching %s: %s\n", label, e$message))
    return(NULL)
  })
}

# =============================================================================
# CLEAR SHEET DATA FROM ROW 2
# =============================================================================
clear_sheet_data <- function(sheet_name) {
  tryCatch({
    range_clear(ss = sheet_id, sheet = sheet_name, range = "A2:Z10000")
    cat(sprintf("Cleared: %s\n", sheet_name))
  }, error = function(e) {
    cat(sprintf("Could not clear %s: %s\n", sheet_name, e$message))
  })
}

# =============================================================================
# HITTING WRITER
# =============================================================================
write_hitting <- function(url, sheet_name, label) {
  df <- safe_api_call(url, label)
  
  if (is.null(df) || nrow(df) == 0) {
    cat(sprintf("Skipping %s (no data).\n", sheet_name))
    return(invisible(NULL))
  }
  
  wrc_col <- intersect(c("wRC+", "wRC.", "wRC"), names(df))[1]
  if (is.na(wrc_col)) {
    cat(sprintf("WARNING: No wRC+ column found for %s\n", sheet_name))
    return(invisible(NULL))
  }
  
  filtered <- df %>%
    select(
      ID     = playerid,
      Player = PlayerNameRoute,
      Team   = TeamName,
      AB, H, R, RBI, HR, SB,
      K      = SO,
      BB,
      `wRC+` = all_of(wrc_col)
    ) %>%
    mutate(across(c(AB, H, R, RBI, HR, SB, K, BB, `wRC+`), as.numeric))
  
  clear_sheet_data(sheet_name)
  range_write(ss = sheet_id, data = filtered, sheet = sheet_name,
              range = "A2", col_names = FALSE)
  cat(sprintf("Written %d rows -> %s\n", nrow(filtered), sheet_name))
}

# =============================================================================
# PITCHING WRITER
# =============================================================================
write_pitching <- function(url, sheet_name, label) {
  df <- safe_api_call(url, label)
  
  if (is.null(df) || nrow(df) == 0) {
    cat(sprintf("Skipping %s (no data).\n", sheet_name))
    return(invisible(NULL))
  }
  
  kbb_col <- intersect(c("K-BB%", "K.BB.", "K/BB%", "KBB%"), names(df))[1]
  if (is.na(kbb_col)) {
    cat(sprintf("WARNING: No K/BB%% column found for %s\n", sheet_name))
    return(invisible(NULL))
  }
  
  filtered <- df %>%
    select(
      ID      = playerid,
      Player  = PlayerNameRoute,
      Team    = TeamName,
      GS, IP, W, L, ER, R,
      K       = SO,
      BB,
      `K/BB%` = all_of(kbb_col),
      H, SV, BS, SIERA, WAR
    ) %>%
    mutate(
      across(c(GS, IP, W, L, ER, R, K, BB, H, SV, BS, SIERA, WAR), as.numeric),
      `K/BB%` = as.numeric(`K/BB%`) * 100
    )
  
  clear_sheet_data(sheet_name)
  range_write(ss = sheet_id, data = filtered, sheet = sheet_name,
              range = "A2", col_names = FALSE)
  cat(sprintf("Written %d rows -> %s\n", nrow(filtered), sheet_name))
}

# =============================================================================
# RUN HITTING
# =============================================================================
cat("\n=== HITTING ===\n")
write_hitting(hitting_1d_url,  "MLB Hitter 1d Data",  "1d Hitters")
write_hitting(hitting_7d_url,  "MLB Hitter 7d Data",  "7d Hitters")
write_hitting(hitting_14d_url, "MLB Hitter 14d Data", "14d Hitters")
write_hitting(hitting_30d_url, "MLB Hitter 30d Data", "30d Hitters")

# =============================================================================
# RUN PITCHING
# =============================================================================
cat("\n=== PITCHING ===\n")
write_pitching(pitching_1d_url,  "MLB Pitcher 1d Data",  "1d Pitchers")
write_pitching(pitching_14d_url, "MLB Pitcher 14d Data", "14d Pitchers")
write_pitching(pitching_30d_url, "MLB Pitcher 30d Data", "30d Pitchers")

cat("\nScript completed successfully!\n")
