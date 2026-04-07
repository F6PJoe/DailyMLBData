# Clear console and environment
cat("\014")
rm(list = ls())

# Required packages
packages <- c("httr", "jsonlite", "googlesheets4", "base64enc", "dplyr")
invisible(lapply(packages, library, character.only = TRUE))

# Authenticate Google Sheets
json_key <- rawToChar(base64decode(Sys.getenv("GCP_SHEETS_KEY_B64")))
temp_json_file <- tempfile(fileext = ".json")
writeLines(json_key, temp_json_file)
gs4_auth(path = temp_json_file)

sheet_id <- "1yymwaiKDeqpSrYcQ4Av9vgv59HGxiyIYJ05LYGkog3s"

# =============================================================================
# DATES
# =============================================================================
end_date                <- Sys.Date()
url_end_date            <- end_date - 1
season_start_date       <- as.Date("2026-03-25")
fallback_start_date     <- as.Date("2025-09-28")
days_since_season_start <- as.numeric(end_date - season_start_date)

pull_14d <- days_since_season_start >= 15
pull_30d <- days_since_season_start >= 30

cat(sprintf("Days since season start: %d\n", days_since_season_start))
cat(sprintf("Pull 14d data: %s | Pull 30d data: %s\n", pull_14d, pull_30d))

get_start_date <- function(days_threshold) {
  if (days_threshold == 1) {
    return(url_end_date)
  } else if (days_since_season_start >= days_threshold) {
    return(end_date - (days_threshold - 1))
  } else {
    fallback_days <- days_threshold - days_since_season_start
    return(fallback_start_date - (fallback_days - 1))
  }
}

start_date_1d_str  <- format(get_start_date(1),  "%Y-%m-%d")
start_date_7d_str  <- format(get_start_date(7),  "%Y-%m-%d")
start_date_14d_str <- format(get_start_date(14), "%Y-%m-%d")
start_date_30d_str <- format(get_start_date(30), "%Y-%m-%d")
url_end_date_str   <- format(url_end_date, "%Y-%m-%d")

# =============================================================================
# URL CONSTRUCTORS
# =============================================================================
season_for <- function(threshold) ifelse(days_since_season_start >= threshold, "2026", "2025")

construct_url <- function(start_date_str, season) {
  sprintf(
    "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=%s&season1=%s&startdate=%s&enddate=%s&month=%s&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR",
    season, season, start_date_str, url_end_date_str, ifelse(season == "2026", "1000", "0")
  )
}

construct_pitching_url <- function(start_date_str, season) {
  sprintf(
    "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=pit&lg=all&qual=1&season=%s&season1=%s&startdate=%s&enddate=%s&month=%s&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR",
    season, season, start_date_str, url_end_date_str, ifelse(season == "2026", "1000", "0")
  )
}

hitting_1d_url  <- construct_url(start_date_1d_str,  season_for(1))
hitting_7d_url  <- construct_url(start_date_7d_str,  season_for(7))
hitting_14d_url <- construct_url(start_date_14d_str, season_for(14))
hitting_30d_url <- construct_url(start_date_30d_str, season_for(30))

pitching_1d_url  <- construct_pitching_url(start_date_1d_str,  season_for(1))
pitching_14d_url <- construct_pitching_url(start_date_14d_str, season_for(14))
pitching_30d_url <- construct_pitching_url(start_date_30d_str, season_for(30))

print(hitting_1d_url)
print(hitting_7d_url)
if (pull_14d) print(hitting_14d_url)
if (pull_30d) print(hitting_30d_url)
print(pitching_1d_url)
if (pull_14d) print(pitching_14d_url)
if (pull_30d) print(pitching_30d_url)

# =============================================================================
# SAFE API CALL HELPER
# =============================================================================
safe_api_call <- function(url, label) {
  tryCatch({
    response    <- GET(url)
    data_parsed <- content(response, as = "text", encoding = "UTF-8") %>%
      fromJSON(flatten = TRUE)

    if (is.null(data_parsed) || length(data_parsed) == 0) {
      cat(sprintf("No %s data available (empty response).\n", label))
      return(NULL)
    }

    df <- if ("data" %in% names(data_parsed)) data_parsed$data else data_parsed

    if (is.null(df) || length(df) == 0) {
      cat(sprintf("No %s data available (empty data array).\n", label))
      return(NULL)
    }

    return(as.data.frame(df))

  }, error = function(e) {
    cat(sprintf("Error fetching %s data: %s\n", label, e$message))
    return(NULL)
  })
}

# =============================================================================
# HITTING HELPERS
# =============================================================================
fetch_and_write_hitting <- function(url, sheet_name, csv_name, label) {
  df <- safe_api_call(url, label)

  if (is.null(df) || nrow(df) == 0) {
    cat(sprintf("Skipping %s (no data).\n", sheet_name))
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
      `wRC+` = `wRC.`
    )

  write.csv(filtered, file = csv_name, row.names = FALSE)
  write_sheet(filtered, ss = sheet_id, sheet = sheet_name)
  cat(sprintf("Written: %s | Sheet: %s\n", csv_name, sheet_name))
}

# =============================================================================
# PITCHING HELPERS
# =============================================================================
fetch_and_write_pitching <- function(url, sheet_name, csv_name, label) {
  df <- safe_api_call(url, label)

  if (is.null(df) || nrow(df) == 0) {
    cat(sprintf("Skipping %s (no data).\n", sheet_name))
    return(invisible(NULL))
  }

  filtered <- df %>%
    mutate(`K.BB.` = `K.BB.` * 100) %>%
    select(
      ID     = playerid,
      Player = PlayerNameRoute,
      Team   = TeamName,
      GS, IP, W, L, ER, R,
      K      = SO,
      BB,
      `K/BB%` = `K.BB.`,
      H, SV, BS, SIERA, WAR
    )

  write.csv(filtered, file = csv_name, row.names = FALSE)
  write_sheet(filtered, ss = sheet_id, sheet = sheet_name)
  cat(sprintf("Written: %s | Sheet: %s\n", csv_name, sheet_name))
}

# =============================================================================
# HITTING PULLS
# =============================================================================
fetch_and_write_hitting(hitting_1d_url, "MLB Hitter 1d Data", "hitting_data_1d.csv", "1d Hitters")
fetch_and_write_hitting(hitting_7d_url, "MLB Hitter 7d Data", "hitting_data_7d.csv", "7d Hitters")

if (pull_14d) {
  fetch_and_write_hitting(hitting_14d_url, "MLB Hitter 14d Data", "hitting_data_14d.csv", "14d Hitters")
} else {
  cat("Skipping 14d hitting data — season not yet 15 days old.\n")
}

if (pull_30d) {
  fetch_and_write_hitting(hitting_30d_url, "MLB Hitter 30d Data", "hitting_data_30d.csv", "30d Hitters")
} else {
  cat("Skipping 30d hitting data — season not yet 30 days old.\n")
}

# =============================================================================
# PITCHING PULLS
# =============================================================================
fetch_and_write_pitching(pitching_1d_url, "MLB Pitcher 1d Data", "pitching_data_1d.csv", "1d Pitchers")

if (pull_14d) {
  fetch_and_write_pitching(pitching_14d_url, "MLB Pitcher 14d Data", "pitching_data_14d.csv", "14d Pitchers")
} else {
  cat("Skipping 14d pitching data — season not yet 15 days old.\n")
}

if (pull_30d) {
  fetch_and_write_pitching(pitching_30d_url, "MLB Pitcher 30d Data", "pitching_data_30d.csv", "30d Pitchers")
} else {
  cat("Skipping 30d pitching data — season not yet 30 days old.\n")
}

cat("\nScript completed successfully!\n")
