# Clear console and environment
cat("\014")
rm(list = ls())

# Required packages
packages <- c("httr", "jsonlite", "googlesheets4", "base64enc", "dplyr")

# Install and load missing packages
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# Decode Google Sheets credentials from env var and authenticate
json_key <- rawToChar(base64decode(Sys.getenv("GCP_SHEETS_KEY_B64")))
temp_json_file <- tempfile(fileext = ".json")
writeLines(json_key, temp_json_file)
gs4_auth(path = temp_json_file)

# Set today's date and calculate yesterday's date for the URL
end_date <- Sys.Date()
url_end_date <- end_date - 1  # Use yesterday's date for the URL

# Reference dates
season_start_date <- as.Date("2026-03-25")  # First day of 2026 season
fallback_start_date <- as.Date("2025-09-28")  # Start point for fallback data

# Calculate days since the season started
days_since_season_start <- as.numeric(end_date - season_start_date)

# Function to determine start date based on threshold
get_start_date <- function(days_threshold) {
  if (days_since_season_start >= days_threshold) {
    return(end_date - (days_threshold - 1))
  } else {
    fallback_days <- days_threshold - days_since_season_start
    return(fallback_start_date - (fallback_days - 1))
  }
}

# Compute start dates
start_date_7d <- get_start_date(7)
start_date_14d <- get_start_date(14)
start_date_30d <- get_start_date(30)

# Format dates for URL
start_date_7d_str <- format(start_date_7d, "%Y-%m-%d")
start_date_14d_str <- format(start_date_14d, "%Y-%m-%d")
start_date_30d_str <- format(start_date_30d, "%Y-%m-%d")
url_end_date_str <- format(url_end_date, "%Y-%m-%d")  # End date for URL

# Function to construct the appropriate URL
construct_url <- function(start_date_str, season) {
  sprintf(
    "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=%s&season1=%s&startdate=%s&enddate=%s&month=%s&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR",
    season, season, start_date_str, url_end_date_str, ifelse(season == "2026", "1000", "0")
  )
}

# Generate URLs
hitting_7d_url <- construct_url(start_date_7d_str, ifelse(days_since_season_start >= 7, "2026", "2025"))
hitting_14d_url <- construct_url(start_date_14d_str, ifelse(days_since_season_start >= 14, "2026", "2025"))
hitting_30d_url <- construct_url(start_date_30d_str, ifelse(days_since_season_start >= 30, "2026", "2025"))

# Print URLs for verification
print(hitting_7d_url)
print(hitting_14d_url)
print(hitting_30d_url)

# Hitting URL
hittingurl <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=0&season=2026&season1=2026&startdate=2026-03-01&enddate=2026-11-01&month=0&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"

sheet_id <- "1AAuiHodCcMzOCpC7oW5xzBXIobcavIvh2AV-sy8OaD4"

# Helper function to safely process API response
safe_api_call <- function(url, label) {
  tryCatch({
    response <- GET(url)
    data_parsed <- content(response, as = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE)
    
    # Handle nested data structure
    if (is.null(data_parsed) || length(data_parsed) == 0) {
      cat(sprintf("No %s data available (empty
