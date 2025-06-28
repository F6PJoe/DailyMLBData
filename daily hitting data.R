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
    return(end_date - (days_threshold - 1))  # Standard case: just subtract
  } else {
    fallback_days <- days_threshold - days_since_season_start
    return(fallback_start_date - (fallback_days - 1))  # Keep correct offset
  }
}

# Compute start dates (still based on today's end_date, not URL end_date)
start_date_7d <- get_start_date(7)
start_date_14d <- get_start_date(14)
start_date_30d <- get_start_date(30)

# Format dates for URL
start_date_7d_str <- format(start_date_7d, "%Y-%m-%d")
start_date_14d_str <- format(start_date_14d, "%Y-%m-%d")
start_date_30d_str <- format(start_date_30d, "%Y-%m-%d")
url_end_date_str <- format(url_end_date, "%Y-%m-%d")  # End date for URL (yesterday)

# Function to construct the appropriate URL
construct_url <- function(start_date_str, season) {
  return(sprintf(
    "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=%s&season1=%s&startdate=%s&enddate=%s&month=%s&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR",
    season, season, start_date_str, url_end_date_str, ifelse(season == "2025", "1000", "0")
  ))
}

# Generate URLs
hitting_7d_url <- construct_url(start_date_7d_str, ifelse(days_since_season_start >= 7, "2025", "2024"))
hitting_14d_url <- construct_url(start_date_14d_str, ifelse(days_since_season_start >= 14, "2025", "2024"))
hitting_30d_url <- construct_url(start_date_30d_str, ifelse(days_since_season_start >= 30, "2025", "2024"))

# Print URLs for verification
print(hitting_7d_url)
print(hitting_14d_url)
print(hitting_30d_url)

activehittingurl <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=2&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=1&players=&type=8&postseason=&sortdir=default&sortstat=WAR"

# Define the URL
hittingurl <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=0&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"


# Make the GET request
responseactive <- GET(activehittingurl)

# Parse JSON content
dataactive <- content(responseactive, as = "text", encoding = "UTF-8") %>% 
  fromJSON(flatten = TRUE)

# Convert to a data frame
dfactive <- as.data.frame(dataactive)

# Filter to keep only the desired columns
filtered_dfactive <- dfactive %>%
  select(
    data.playerid,
    data.PlayerNameRoute,
    data.TeamName,
  )

# Rename the columns
colnames(filtered_dfactive) <- c(
  "ID", "Player", "Team")

# Specify the Google Sheet to write to
sheet_id <- "1AAuiHodCcMzOCpC7oW5xzBXIobcavIvh2AV-sy8OaD4"  # Replace with your Google Sheet ID

# Write the data to the Google Sheet
write_sheet(filtered_dfactive, ss = sheet_id, sheet = "MLB Active Hitters")  # Change "Sheet1" to your desired sheet name

# Print a confirmation message
cat("Data has been saved to the Google Sheet:", sheet_id, "\n")


# Make the GET request
response <- GET(hittingurl)

# Parse JSON content
data <- content(response, as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE)

# Convert to a data frame
df <- as.data.frame(data)

# Filter to keep only the desired columns
filtered_df <- df %>%
  select(
    data.PlayerNameRoute,
    data.playerid,
    data.TeamName,
    Avg = `data.AVG`,
    OBP = `data.OBP`,
    OPS = `data.OPS`,
    SLG = `data.SLG`,
    BABIP = `data.BABIP`,
    data.PA,
    data.R, data.HR,
    data.RBI, data.SB,
    Barrel_pct = `data.Barrel.`,
    HardHit_pct = `data.HardHit.`,
    xBA = `data.xAVG`,
    xSLG = `data.xSLG`,
    xwOBA = `data.xwOBA`) %>%
  mutate(
    Avg = formatC(as.numeric(Avg), format = "f", digits = 3),
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

# Ensure no accidental character conversion
filtered_df <- filtered_df %>%
  mutate(across(c(Avg, OBP, OPS, SLG, BABIP, xBA, xSLG, xwOBA, Barrel_pct, HardHit_pct), as.numeric))

# Replace NA values with blanks
filtered_df <- filtered_df %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

# Rename the columns
colnames(filtered_df) <- c(
  "Player", "ID", "Team", "AVG", "OBP", "OPS", "SLG", "BABIP", "PA", "R", "HR", "RBI", "SB",
  "Barrel%", "Hard Hit%", "xBA", "xSLG%", "xwOBA%"
)

# Save the filtered data frame to a CSV file
output_file <- "hitting_data_2024.csv"
write.csv(filtered_df, file = output_file, row.names = FALSE, na = "")

# Print a confirmation message
cat("Data has been saved to", output_file, "\n")

# Specify the Google Sheet to write to
sheet_id <- "1AAuiHodCcMzOCpC7oW5xzBXIobcavIvh2AV-sy8OaD4"  # Replace with your Google Sheet ID

# Write the data to the Google Sheet
write_sheet(filtered_df, ss = sheet_id, sheet = "MLB Hitter Data")  # Change "Sheet1" to your desired sheet name

# Print a confirmation message
cat("Data has been saved to the Google Sheet:", sheet_id, "\n")

# Make the GET request
response7d <- GET(hitting_7d_url)

# Parse JSON content
data7d <- content(response7d, as = "text", encoding = "UTF-8") %>% 
  fromJSON(flatten = TRUE)

# Convert to a data frame
df7d <- as.data.frame(data7d)

df7d$`data.wRC.`

# Filter to keep only the desired columns
filtered_df7d <- df7d %>%
  select(
    data.PlayerNameRoute,
    data.playerid,
    data.TeamName,
    wOBA7d = 'data.wOBA',
    ISO7d = 'data.ISO',
    wrc7d = "data.wRC.",  # Change to 'data.wRC.'
    k_pct7d = 'data.K.'
  ) %>%
  mutate(
    wOBA7d = formatC(as.numeric(wOBA7d), format = "f", digits = 3),
    ISO7d = formatC(as.numeric(ISO7d), format = "f", digits = 3),
    k_pct7d = formatC(as.numeric(k_pct7d) * 100, format = "f", digits = 1)
  )

# Optional: Ensure no accidental character conversion
filtered_df7d <- filtered_df7d %>%
  mutate(across(c(wOBA7d, ISO7d, k_pct7d), as.numeric))

# Rename the columns
colnames(filtered_df7d) <- c(
  "Player", "ID", "Team", "wOBA7d", "ISO7d", "wrc+7d", "K%7d")

# Save the filtered data frame to a CSV file
output_file7d <- "hitting_data_7d_2024.csv"
write.csv(filtered_df7d, file = output_file7d, row.names = FALSE)

# Print a confirmation message
cat("Data has been saved to", output_file7d, "\n")

# Specify the Google Sheet to write to
sheet_id <- "1AAuiHodCcMzOCpC7oW5xzBXIobcavIvh2AV-sy8OaD4"  # Replace with your Google Sheet ID

# Write the data to the Google Sheet
write_sheet(filtered_df7d, ss = sheet_id, sheet = "MLB Hitter 7d Data")  # Change "Sheet1" to your desired sheet name

# Print a confirmation message
cat("Data has been saved to the Google Sheet:", sheet_id, "\n")

# Make the GET request
response14d <- GET(hitting_14d_url)

# Parse JSON content
data14d <- content(response14d, as = "text", encoding = "UTF-8") %>% 
  fromJSON(flatten = TRUE)

# Convert to a data frame
df14d <- as.data.frame(data14d)

# Filter to keep only the desired columns
filtered_df14d <- df14d %>%
  select(
    data.PlayerNameRoute,
    data.playerid,
    data.TeamName,
    wOBA14d = 'data.wOBA',
    ISO14d = 'data.ISO',
    wrc14d = 'data.wRC.',
    k_pct14d = 'data.K.'
  ) %>%
  mutate(
    wOBA14d = formatC(as.numeric(wOBA14d), format = "f", digits = 3),
    ISO14d = formatC(as.numeric(ISO14d), format = "f", digits = 3),
    k_pct14d = formatC(as.numeric(k_pct14d) * 100, format = "f", digits = 1),
  )

# Optional: Ensure no accidental character conversion
filtered_df14d <- filtered_df14d %>%
  mutate(across(c(wOBA14d, ISO14d, k_pct14d), as.numeric))

# Rename the columns
colnames(filtered_df14d) <- c(
  "Player", "ID", "Team", "wOBA14d", "ISO14d", "wrc+14d", "K%14d")

# Save the filtered data frame to a CSV file
output_file14d <- "hitting_data_14d_2024.csv"
write.csv(filtered_df14d, file = output_file14d, row.names = FALSE)

# Print a confirmation message
cat("Data has been saved to", output_file14d, "\n")

# Specify the Google Sheet to write to
sheet_id <- "1AAuiHodCcMzOCpC7oW5xzBXIobcavIvh2AV-sy8OaD4"  # Replace with your Google Sheet ID

# Write the data to the Google Sheet
write_sheet(filtered_df14d, ss = sheet_id, sheet = "MLB Hitter 14d Data")  # Change "Sheet1" to your desired sheet name

# Print a confirmation message
cat("Data has been saved to the Google Sheet:", sheet_id, "\n")

# Make the GET request
response30d <- GET(hitting_30d_url)

# Parse JSON content
data30d <- content(response30d, as = "text", encoding = "UTF-8") %>% 
  fromJSON(flatten = TRUE)

# Convert to a data frame
df30d <- as.data.frame(data30d)

# Filter to keep only the desired columns
filtered_df30d <- df30d %>%
  select(
    data.PlayerNameRoute,
    data.playerid,
    data.TeamName,
    wOBA30d = 'data.wOBA',
    ISO30d = 'data.ISO',
    wrc30d = 'data.wRC.',
    k_pct30d = 'data.K.'
  ) %>%
  mutate(
    wOBA30d = formatC(as.numeric(wOBA30d), format = "f", digits = 3),
    ISO30d = formatC(as.numeric(ISO30d), format = "f", digits = 3),
    k_pct30d = formatC(as.numeric(k_pct30d) * 100, format = "f", digits = 1),
  )

# Optional: Ensure no accidental character conversion
filtered_df30d <- filtered_df30d %>%
  mutate(across(c(wOBA30d, ISO30d, k_pct30d), as.numeric))

# Rename the columns
colnames(filtered_df30d) <- c(
  "Player", "ID", "Team", "wOBA30d", "ISO30d", "wrc+30d", "K%30d")

# Save the filtered data frame to a CSV file
output_file30d <- "hitting_data_30d_2024.csv"
write.csv(filtered_df30d, file = output_file30d, row.names = FALSE)

# Print a confirmation message
cat("Data has been saved to", output_file30d, "\n")


# Specify the Google Sheet to write to
sheet_id <- "1AAuiHodCcMzOCpC7oW5xzBXIobcavIvh2AV-sy8OaD4"  # Replace with your Google Sheet ID

# Write the data to the Google Sheet
write_sheet(filtered_df30d, ss = sheet_id, sheet = "MLB Hitter 30d Data")  # Change "Sheet1" to your desired sheet name

# Print a confirmation message
cat("Data has been saved to the Google Sheet:", sheet_id, "\n")





