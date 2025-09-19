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
season_start_date <- as.Date("2025-03-27")  # First day of 2025 season
fallback_start_date <- as.Date("2024-09-29")  # Start point for fallback data

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
    season, season, start_date_str, url_end_date_str, ifelse(season == "2025", "1000", "0")
  )
}

# Generate URLs
hitting_7d_url <- construct_url(start_date_7d_str, ifelse(days_since_season_start >= 7, "2025", "2024"))
hitting_14d_url <- construct_url(start_date_14d_str, ifelse(days_since_season_start >= 14, "2025", "2024"))
hitting_30d_url <- construct_url(start_date_30d_str, ifelse(days_since_season_start >= 30, "2025", "2024"))

# Print URLs for verification
print(hitting_7d_url)
print(hitting_14d_url)
print(hitting_30d_url)

# Active hitters URL
activehittingurl <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=2&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=1&players=&type=8&postseason=&sortdir=default&sortstat=WAR"

# Hitting URL
hittingurl <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=0&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"

# --- Active Hitters ---
responseactive <- GET(activehittingurl)
dataactive <- content(responseactive, as = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE)
dfactive <- as.data.frame(dataactive)

filtered_dfactive <- dfactive %>%
  select(
    ID = playerid,
    Player = PlayerNameRoute,
    Team = TeamName
  )

sheet_id <- "1AAuiHodCcMzOCpC7oW5xzBXIobcavIvh2AV-sy8OaD4"
write_sheet(filtered_dfactive, ss = sheet_id, sheet = "MLB Active Hitters")
cat("Active hitters saved to Google Sheet.\n")

# --- Full Season Hitters ---
response <- GET(hittingurl)
data <- content(response, as = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE)
df <- as.data.frame(data)

filtered_df <- df %>%
  select(
    Player = PlayerNameRoute,
    ID = playerid,
    Team = TeamName,
    AVG = AVG,
    OBP = OBP,
    OPS = OPS,
    SLG = SLG,
    BABIP = BABIP,
    PA,
    R, HR,
    RBI, SB,
    Barrel_pct = Barrel,
    HardHit_pct = HardHit,
    xBA = xAVG,
    xSLG = xSLG,
    xwOBA = xwOBA
  ) %>%
  mutate(
    AVG = formatC(as.numeric(AVG), format = "f", digits = 3),
    OBP = formatC(as.numeric(OBP), format = "f", digits = 3),
    OPS = formatC(as.numeric(OPS), format = "f", digits = 3),
    SLG = formatC(as.numeric(SLG), format = "f", digits = 3),
    BABIP = formatC(as.numeric(BABIP), format = "f", digits = 3),
    xBA = formatC(as.numeric(xBA), format = "f", digits = 3),
    xSLG = formatC(as.numeric(xSLG), format = "f", digits = 3),
    xwOBA = formatC(as.numeric(xwOBA), format = "f", digits = 3),
    Barrel_pct = formatC(as.numeric(Barrel_pct) * 100, format = "f", digits = 1),
    HardHit_pct = formatC(as.numeric(HardHit_pct) * 100, format = "f", digits = 1)
  )

# Convert numeric columns back to numeric
filtered_df <- filtered_df %>% mutate(across(c(AVG, OBP, OPS, SLG, BABIP, xBA, xSLG, xwOBA, Barrel_pct, HardHit_pct), as.numeric))
filtered_df <- filtered_df %>% mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

write_sheet(filtered_df, ss = sheet_id, sheet = "MLB Hitter Data")
cat("Full season hitters saved to Google Sheet.\n")

# --- 7d, 14d, 30d hitters corrected ---

# 7d
response7d <- GET(hitting_7d_url)
df7d <- as.data.frame(content(response7d, as = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE))
filtered_df7d <- df7d %>%
  select(
    Player = PlayerNameRoute,
    ID = playerid,
    Team = TeamName,
    wOBA7d = wOBA,
    ISO7d = ISO,
    wrc7d = wRC,
    k_pct7d = `K%`
  ) %>%
  mutate(
    wOBA7d = formatC(as.numeric(wOBA7d), format = "f", digits = 3),
    ISO7d = formatC(as.numeric(ISO7d), format = "f", digits = 3),
    k_pct7d = formatC(as.numeric(k_pct7d), format = "f", digits = 1)
  )
colnames(filtered_df7d) <- c("Player", "ID", "Team", "wOBA7d", "ISO7d", "wRC+7d", "K%7d")
write_sheet(filtered_df7d, ss = sheet_id, sheet = "MLB Hitter 7d Data")
cat("7d hitters saved to Google Sheet.\n")

# 14d
response14d <- GET(hitting_14d_url)
df14d <- as.data.frame(content(response14d, as = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE))
filtered_df14d <- df14d %>%
  select(
    Player = PlayerNameRoute,
    ID = playerid,
    Team = TeamName,
    wOBA14d = wOBA,
    ISO14d = ISO,
    wrc14d = wRC,
    k_pct14d = `K%`
  ) %>%
  mutate(
    wOBA14d = formatC(as.numeric(wOBA14d), format = "f", digits = 3),
    ISO14d = formatC(as.numeric(ISO14d), format = "f", digits = 3),
    k_pct14d = formatC(as.numeric(k_pct14d), format = "f", digits = 1)
  )
colnames(filtered_df14d) <- c("Player", "ID", "Team", "wOBA14d", "ISO14d", "wRC+14d", "K%14d")
write_sheet(filtered_df14d, ss = sheet_id, sheet = "MLB Hitter 14d Data")
cat("14d hitters saved to Google Sheet.\n")

# 30d
response30d <- GET(hitting_30d_url)
df30d <- as.data.frame(content(response30d, as = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE))
filtered_df30d <- df30d %>%
  select(
    Player = PlayerNameRoute,
    ID = playerid,
    Team = TeamName,
    wOBA30d = wOBA,
    ISO30d = ISO,
    wrc30d = wRC,
    k_pct30d = `K%`
  ) %>%
  mutate(
    wOBA30d = formatC(as.numeric(wOBA30d), format = "f", digits = 3),
    ISO30d = formatC(as.numeric(ISO30d), format = "f", digits = 3),
    k_pct30d = formatC(as.numeric(k_pct30d), format = "f", digits = 1)
  )
colnames(filtered_df30d) <- c("Player", "ID", "Team", "wOBA30d", "ISO30d", "wRC+30d", "K%30d")
write_sheet(filtered_df30d, ss = sheet_id, sheet = "MLB Hitter 30d Data")
cat("30d hitters saved to Google Sheet.\n")
