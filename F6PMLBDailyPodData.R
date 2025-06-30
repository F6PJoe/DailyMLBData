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


# Specify the Google Sheet to write to
sheet_id <- "1yymwaiKDeqpSrYcQ4Av9vgv59HGxiyIYJ05LYGkog3s"  # Replace with your Google Sheet ID

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
  if (days_threshold == 1) {
      return(url_end_date)
  }
  else if (days_since_season_start >= days_threshold) {
    return(end_date - (days_threshold - 1))  # Standard case: just subtract
  } else {
    fallback_days <- days_threshold - days_since_season_start
    return(fallback_start_date - (fallback_days - 1))  # Keep correct offset
  }
}

# Compute start dates (still based on today's end_date, not URL end_date)
start_date_1d <- get_start_date(1)
start_date_7d <- get_start_date(7)
start_date_14d <- get_start_date(14)
start_date_30d <- get_start_date(30)

# Format dates for URL
start_date_1d_str <- format(start_date_1d, "%Y-%m-%d")
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

# Function to construct the appropriate URL
construct_pitching_url <- function(start_date_str, season) {
  return(sprintf(
    "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=pit&lg=all&qual=1&season=%s&season1=%s&startdate=%s&enddate=%s&month=%s&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR",
    season, season, start_date_str, url_end_date_str, ifelse(season == "2025", "1000", "0")
  ))
}

# Generate URLs
hitting_1d_url <- construct_url(start_date_1d_str, ifelse(days_since_season_start >= 1, "2025", "2024"))
hitting_7d_url <- construct_url(start_date_7d_str, ifelse(days_since_season_start >= 7, "2025", "2024"))
hitting_14d_url <- construct_url(start_date_14d_str, ifelse(days_since_season_start >= 14, "2025", "2024"))
hitting_30d_url <- construct_url(start_date_30d_str, ifelse(days_since_season_start >= 30, "2025", "2024"))
pitching_1d_url <- construct_pitching_url(start_date_1d_str, ifelse(days_since_season_start >= 1, "2025", "2024"))
pitching_14d_url <- construct_pitching_url(start_date_14d_str, ifelse(days_since_season_start >= 14, "2025", "2024"))
pitching_30d_url <- construct_pitching_url(start_date_30d_str, ifelse(days_since_season_start >= 30, "2025", "2024"))

# Print URLs for verification
print(hitting_1d_url)
print(hitting_7d_url)
print(hitting_14d_url)
print(hitting_30d_url)
print(pitching_1d_url)
print(pitching_14d_url)
print(pitching_30d_url)

# Make the GET request
response1d <- GET(hitting_1d_url)

# Parse JSON content
data1d <- content(response1d, as = "text", encoding = "UTF-8") %>% 
  fromJSON(flatten = TRUE)

# Convert to a data frame
df1d <- as.data.frame(data1d)

# Filter to keep only the desired columns
filtered_df1d <- df1d %>%
  select(
    data.playerid,
    data.PlayerNameRoute,
    data.TeamName,
    data.AB, data.H, data.R, data.RBI, data.HR, data.SB,
    data.SO, data.BB, data.wRC.
  )

# Rename the columns
colnames(filtered_df1d) <- c(
  "ID","Player", "Team", "AB", "H", "R", "RBI", "HR", "SB", "K", "BB", "wRC+"
)

# Save the filtered data frame to a CSV file
output_file1d <- "hitting_data_1d_2024.csv"
write.csv(filtered_df1d, file = output_file1d, row.names = FALSE)

# Print a confirmation message
cat("Data has been saved to", output_file1d, "\n")

# Write the data to the Google Sheet
write_sheet(filtered_df1d, ss = sheet_id, sheet = "MLB Hitter 1d Data")  # Change "Sheet1" to your desired sheet name

# Print a confirmation message
cat("Data has been saved to the Google Sheet:", sheet_id, "\n")


# Make the GET request
response7d <- GET(hitting_7d_url)

# Parse JSON content
data7d <- content(response7d, as = "text", encoding = "UTF-8") %>% 
  fromJSON(flatten = TRUE)

# Convert to a data frame
df7d <- as.data.frame(data7d)

# Filter to keep only the desired columns
filtered_df7d <- df7d %>%
  select(
    data.playerid,
    data.PlayerNameRoute,
    data.TeamName,
    data.AB, data.H, data.R, data.RBI, data.HR, data.SB,
    data.SO, data.BB, data.wRC.
  )

# Rename the columns
colnames(filtered_df7d) <- c(
  "ID","Player", "Team", "AB", "H", "R", "RBI", "HR", "SB", "K", "BB", "wRC+"
)

# Save the filtered data frame to a CSV file
output_file7d <- "hitting_data_7d_2024.csv"
write.csv(filtered_df7d, file = output_file7d, row.names = FALSE)

# Print a confirmation message
cat("Data has been saved to", output_file7d, "\n")

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
    data.playerid,
    data.PlayerNameRoute,
    data.TeamName,
    data.AB, data.H, data.R, data.RBI, data.HR, data.SB,
    data.SO, data.BB, data.wRC.
  )

# Rename the columns
colnames(filtered_df14d) <- c(
  "ID","Player", "Team", "AB", "H", "R", "RBI", "HR", "SB", "K", "BB", "wRC+"
)

# Save the filtered data frame to a CSV file
output_file14d <- "hitting_data_14d_2024.csv"
write.csv(filtered_df14d, file = output_file14d, row.names = FALSE)

# Print a confirmation message
cat("Data has been saved to", output_file14d, "\n")

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
    data.playerid,
    data.PlayerNameRoute,
    data.TeamName,
    data.AB, data.H, data.R, data.RBI, data.HR, data.SB,
    data.SO, data.BB, data.wRC.
  )

# Rename the columns
colnames(filtered_df30d) <- c(
  "ID","Player", "Team", "AB", "H", "R", "RBI", "HR", "SB", "K", "BB", "wRC+"
)

# Save the filtered data frame to a CSV file
output_file30d <- "hitting_data_30d_2024.csv"
write.csv(filtered_df30d, file = output_file30d, row.names = FALSE)

# Print a confirmation message
cat("Data has been saved to", output_file30d, "\n")

# Write the data to the Google Sheet
write_sheet(filtered_df30d, ss = sheet_id, sheet = "MLB Hitter 30d Data")  # Change "Sheet1" to your desired sheet name

# Print a confirmation message
cat("Data has been saved to the Google Sheet:", sheet_id, "\n")


#pitchingdata

# Make the GET request
response1d <- GET(pitching_1d_url)

# Parse JSON content
data1d <- content(response1d, as = "text", encoding = "UTF-8") %>% 
  fromJSON(flatten = TRUE)

# Convert to a data frame
df1d <- as.data.frame(data1d)

filtered_df1d <- df1d %>%
  mutate(data.K.BB. = data.K.BB. * 100) %>%
  select(
    data.playerid,
    data.PlayerNameRoute,
    data.TeamName, data.GS,
    data.IP, data.W, data.L, data.ER, data.R, data.SO, data.BB, data.K.BB., data.H, data.SV, data.BS, data.SIERA, data.WAR
  )

# Rename the columns
colnames(filtered_df1d) <- c(
  "ID", "Player", "Team", "GS", "IP", "W", "L", "ER", "R", "K", "BB", "K/BB%", "H", "SV", "BS", "SIERA", "WAR"
)

# Save the filtered data frame to a CSV file
output_file1d <- "pitching_data_1d_2024.csv"
write.csv(filtered_df1d, file = output_file1d, row.names = FALSE)

# Print a confirmation message
cat("Data has been saved to", output_file1d, "\n")

# Write the data to the Google Sheet
write_sheet(filtered_df1d, ss = sheet_id, sheet = "MLB Pitcher 1d Data")  # Change "Sheet1" to your desired sheet name

# Print a confirmation message
cat("Data has been saved to the Google Sheet:", sheet_id, "\n")


# Make the GET request
response14d <- GET(pitching_14d_url)

# Parse JSON content
data14d <- content(response14d, as = "text", encoding = "UTF-8") %>% 
  fromJSON(flatten = TRUE)

# Convert to a data frame
df14d <- as.data.frame(data14d)

filtered_df14d <- df14d %>%
  mutate(data.K.BB. = data.K.BB. * 100) %>%
  select(
    data.playerid,
    data.PlayerNameRoute,
    data.TeamName, data.GS,
    data.IP, data.W, data.L, data.ER, data.R, data.SO, data.BB, data.K.BB., data.H, data.SV, data.BS, data.SIERA, data.WAR
  )

# Rename the columns
colnames(filtered_df14d) <- c(
  "ID", "Player", "Team", "GS", "IP", "W", "L", "ER", "R", "K", "BB", "K/BB%", "H", "SV", "BS", "SIERA", "WAR"
)

# Save the filtered data frame to a CSV file
output_file14d <- "pitching_data_14d_2024.csv"
write.csv(filtered_df14d, file = output_file14d, row.names = FALSE)

# Print a confirmation message
cat("Data has been saved to", output_file14d, "\n")

# Write the data to the Google Sheet
write_sheet(filtered_df14d, ss = sheet_id, sheet = "MLB Pitcher 14d Data")  # Change "Sheet1" to your desired sheet name

# Print a confirmation message
cat("Data has been saved to the Google Sheet:", sheet_id, "\n")

# Make the GET request
response30d <- GET(pitching_30d_url)

# Parse JSON content
data30d <- content(response30d, as = "text", encoding = "UTF-8") %>% 
  fromJSON(flatten = TRUE)

# Convert to a data frame
df30d <- as.data.frame(data30d)

filtered_df30d <- df30d %>%
  mutate(data.K.BB. = data.K.BB. * 100) %>%
  select(
    data.playerid,
    data.PlayerNameRoute,
    data.TeamName, data.GS,
    data.IP, data.W, data.L, data.ER, data.R, data.SO, data.BB, data.K.BB., data.H, data.SV, data.BS, data.SIERA, data.WAR
  )

# Rename the columns
colnames(filtered_df30d) <- c(
  "ID", "Player", "Team", "GS", "IP", "W", "L", "ER", "R", "K", "BB", "K/BB%", "H", "SV", "BS", "SIERA", "WAR"
)

# Save the filtered data frame to a CSV file
output_file30d <- "pitching_data_30d_2024.csv"
write.csv(filtered_df30d, file = output_file30d, row.names = FALSE)

# Print a confirmation message
cat("Data has been saved to", output_file30d, "\n")

# Write the data to the Google Sheet
write_sheet(filtered_df30d, ss = sheet_id, sheet = "MLB Pitcher 30d Data")  # Change "Sheet1" to your desired sheet name

# Print a confirmation message
cat("Data has been saved to the Google Sheet:", sheet_id, "\n")

