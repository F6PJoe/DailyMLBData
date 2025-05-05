# clear console and workspace
cat("\014")
rm(list = ls())

# Required packages
packages <- c("httr", "jsonlite", "googlesheets4", "base64enc", "dplyr", "string", "lubridate")

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
# 
# Calculate the dates
# end_date <- Sys.Date() - 251  # Yesterday's date
# start_date7d <- end_date - 6  # Seven days prior to yesterday
# start_date14d <- end_date - 13  # Seven days prior to yesterday
# start_date30d <- end_date - 29  # Seven days prior to yesterday
# 
# # Convert dates to character format for the URL
# start_date7d <- format(start_date7d, "%Y-%m-%d")
# start_date14d <- format(start_date14d, "%Y-%m-%d")
# start_date30d <- format(start_date30d, "%Y-%m-%d")
# end_date <- format(end_date, "%Y-%m-%d")
# 
# # Define the URL with dynamic dates
# url <- paste0(
#   "https://www.fangraphs.com/api/leaders/major-league/data?",
#   "age=&pos=all&stats=pit&lg=all&qual=0&season=2024&season1=2024",
#   "&startdate=", start_date7d,
#   "&enddate=", end_date,
#   "&month=1000&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0",
#   "&players=&type=c%2C13%2C24%2C6%2C42%2C43%2C120%2C113%2C331%2C62%2C332%2C122",
#   "%2C48%2C49%2C223%2C325%2C420&postseason=&sortdir=default&sortstat=IP"
# )


# Define the start date
start_date <- as.Date("2024-07-21")

#Get today's date
end_date <- Sys.Date()-1

# Check if today's date is before or on May 1st, 2025, updated to 4/22/25 becuase xERA stopped coming over in combined year data
if (end_date <= as.Date("2025-04-22")) {
# If today is before May 1st, 2025, adjust the start date by adding days
  date_diff <- as.integer(end_date - as.Date("2025-03-22"))

# Calculate the new start date based on the difference in days
   new_start_date <- start_date + (date_diff*2)

# Format both the start and end date to match the required format
   new_start_date <- format(new_start_date, "%Y-%m-%d")
   end_date <- format(end_date, "%Y-%m-%d")

# Define the URL with dynamically generated start and end dates
   pitchingurl <- paste0(
     "https://www.fangraphs.com/api/leaders/major-league/data?",
     "age=&pos=all&stats=pit&lg=all&qual=1&",
     "season=2025&season1=2024&",
     "startdate=", new_start_date,
     "&enddate=", end_date,
     "&month=1000&hand=&team=0&",
     "pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&",
     "type=8&postseason=&sortdir=default&sortstat=WAR"
   )
   
   sheetname <- "MLB Pitcher Data w-2024"
   
 } else {
 # If today is on or after May 1st, 2025, use the new URL
   pitchingurl <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=pit&lg=all&qual=1&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
   sheetname <- "MLB Pitchers Data"
 }
#
 # Define the URL
 #pitchingurl <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=pit&lg=all&qual=0&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=c%2C13%2C24%2C6%2C42%2C43%2C120%2C113%2C331%2C62%2C332%2C122%2C48%2C49%2C223%2C325%2C420&postseason=&sortdir=default&sortstat=IP"

print(pitchingurl)

#Make the GET request
response <- GET(pitchingurl)

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
     K_pct = `data.K.`,
     SwStr_pct = `data.SwStr.`,
     data.IP, data.SO,
     data.ERA, data.WHIP,
     data.BABIP, data.SIERA,
     data.xFIP, data.xERA,
     GB_pct = `data.GB.`,
     FB_pct = `data.FB.`,
     Hard_hit_pct = `data.Hard.`,
     Barrel_pct = `data.Barrel.`,
     CSW_pct = `data.C.SwStr.`
   ) %>%
   mutate(
     # Percentages with one decimal place
     K_pct = round(K_pct * 100, 1),
     SwStr_pct = round(SwStr_pct * 100, 1),
     GB_pct = round(GB_pct * 100, 1),
     FB_pct = round(FB_pct * 100, 1),
     Hard_hit_pct = round(Hard_hit_pct * 100, 1),
     Barrel_pct = round(Barrel_pct * 100, 1),
     CSW_pct = round(CSW_pct * 100, 1),

     # ERA, xFIP, xERA, SIERA with two decimal places
     data.ERA = round(data.ERA, 2),
     data.xFIP = round(data.xFIP, 2),
     data.xERA = round(data.xERA, 2),
     data.SIERA = round(data.SIERA, 2),

     # WHIP, BABIP with three decimal places
     data.WHIP = round(data.WHIP, 3),
     data.BABIP = round(data.BABIP, 3)
   )

# Rename the columns
 colnames(filtered_df) <- c(
   "Player", "ID", "Team", "K%", "SwStr%", "IP", "SO", "ERA", "WHIP", "BABIP",
   "SIERA", "xFIP", "xERA", "GB%", "FB%", "Hard%", "Barrel%", "CSW%"
 )

# Save the filtered data frame to a CSV file
# output_file <- "pitching_data.csv"
# write.csv(filtered_df, file = output_file, row.names = FALSE)

# Print a confirmation message
#cat("Data has been saved to", output_file, "\n")

# Specify the Google Sheet to write to
sheet_id <- "1AAuiHodCcMzOCpC7oW5xzBXIobcavIvh2AV-sy8OaD4"  # Replace with your Google Sheet ID

# Write the data to the Google Sheet
write_sheet(filtered_df, ss = sheet_id, sheet = sheetname)  # Change "Sheet1" to your desired sheet name

# Print a confirmation message
cat("Data has been saved to the Google Sheet:", sheet_id, "\n")

#redo logic so this only runs if the date is before the cut off date to not use combined data
# pitchingurl25 <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=pit&lg=all&qual=1&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
# 
# # Make the GET request
# response <- GET(pitchingurl25)
# 
# # Parse JSON content
# data <- content(response, as = "text", encoding = "UTF-8") %>%
#   fromJSON(flatten = TRUE)
# 
# # Convert to a data frame
# df25 <- as.data.frame(data)
# 
# # Filter to keep only the desired columns
# filtered_df25 <- df25 %>%
#   select(
#     data.PlayerNameRoute,
#     data.playerid,
#     data.TeamName,
#     K_pct = `data.K.`,
#     SwStr_pct = `data.SwStr.`,
#     data.IP, data.SO,
#     data.ERA, data.WHIP,
#     data.BABIP, data.SIERA,
#     data.xFIP, data.xERA,
#     GB_pct = `data.GB.`,
#     FB_pct = `data.FB.`,
#     Hard_hit_pct = `data.Hard.`,
#     Barrel_pct = `data.Barrel.`,
#     CSW_pct = `data.C.SwStr.`
#   ) %>%
#   mutate(
#     # Percentages with one decimal place
#     K_pct = round(K_pct * 100, 1),
#     SwStr_pct = round(SwStr_pct * 100, 1),
#     GB_pct = round(GB_pct * 100, 1),
#     FB_pct = round(FB_pct * 100, 1),
#     Hard_hit_pct = round(Hard_hit_pct * 100, 1),
#     Barrel_pct = round(Barrel_pct * 100, 1),
#     CSW_pct = round(CSW_pct * 100, 1),
#     
#     # ERA, xFIP, xERA, SIERA with two decimal places
#     data.ERA = round(data.ERA, 2),
#     data.xFIP = round(data.xFIP, 2),
#     data.xERA = round(data.xERA, 2),
#     data.SIERA = round(data.SIERA, 2),
#     
#     # WHIP, BABIP with three decimal places
#     data.WHIP = round(data.WHIP, 3),
#     data.BABIP = round(data.BABIP, 3)
#   )
# 
# # Rename the columns
# colnames(filtered_df25) <- c(
#   "Player", "ID", "Team", "K%", "SwStr%", "IP", "SO", "ERA", "WHIP", "BABIP",
#   "SIERA", "xFIP", "xERA", "GB%", "FB%", "Hard%", "Barrel%", "CSW%"
# )
# 
# # Save the filtered data frame to a CSV file
# #output_file <- "pitching_data.csv"
# #write.csv(filtered_df25, file = output_file, row.names = FALSE)
# 
# # Print a confirmation message
# #cat("Data has been saved to", output_file, "\n")
# 
# # Specify the Google Sheet to write to
# sheet_id <- "1AAuiHodCcMzOCpC7oW5xzBXIobcavIvh2AV-sy8OaD4"  # Replace with your Google Sheet ID
# 
# # Write the data to the Google Sheet
# write_sheet(filtered_df25, ss = sheet_id, sheet = "MLB Pitchers Data")  # Change "Sheet1" to your desired sheet name
# 
# # Print a confirmation message
# cat("Data has been saved to the Google Sheet:", sheet_id, "\n")

# Define the start date
start_date <- as.Date("2024-07-21")

# Get today's date
end_date <- Sys.Date()

# Check if today's date is before or on May 1st, 2025
if (end_date <= as.Date("2025-05-01")) {
  # If today is before May 1st, 2025, adjust the start date by adding days
  date_diff <- as.integer(end_date - as.Date("2025-03-22"))
  
  # Calculate the new start date based on the difference in days
  new_start_date <- start_date + (date_diff*2)
  
  # Format both the start and end date to match the required format
  new_start_date <- format(new_start_date, "%Y-%m-%d")
  end_date <- format(end_date, "%Y-%m-%d")
  
  # Define the URL with dynamically generated start and end dates
  opppitchingurl <- paste0(
    "https://www.fangraphs.com/api/leaders/major-league/data?",
    "age=&pos=all&stats=bat&lg=all&qual=0&season=2024&season1=2024&",
    "startdate=", new_start_date, "&enddate=", end_date, "&month=1000&hand=&team=0%2Cts&pageitems=30&pagenum=1&ind=0&rost=0&players=0&type=8&postseason=&sortdir=default&sortstat=wRC%2B"
  )
} else {
  # If today is on or after May 1st, 2025, use the new URL
  opppitchingurl <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=0&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0%2Cts&pageitems=30&pagenum=1&ind=0&rost=0&players=0&type=8&postseason=&sortdir=default&sortstat=WAR"
}

# Make the GET request
response <- GET(opppitchingurl)

# Parse JSON content
data <- content(response, as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE)

# Convert to a data frame
df <- as.data.frame(data)

# Filter to keep only the desired columns
filtered_df <- df %>%
  select(data.TeamName,
         K_pct = `data.K.`,
         SwStr_pct = `data.SwStr.`,
         wrc_plus = `data.wRC.`,
         data.wOBA, data.ISO,
         data.TG) %>%
  mutate(
    K_pct = round(K_pct * 100, 1),
    SwStr_pct = round(SwStr_pct * 100, 1),
  )

# Rename the columns
colnames(filtered_df) <- c(
  "Team", "K%", "SwStr%", "wRC+", "wOBA", "ISO", "G")


# Save the filtered data frame to a CSV file
#output_file <- "opp_pitching_data"
#write.csv(filtered_df, file = output_file, row.names = FALSE)

# Print a confirmation message
#cat("Data has been saved to", output_file, "\n")


# Specify the Google Sheet to write to
sheet_id <- "1AAuiHodCcMzOCpC7oW5xzBXIobcavIvh2AV-sy8OaD4"  # Replace with your Google Sheet ID

# Write the data to the Google Sheet
write_sheet(filtered_df, ss = sheet_id, sheet = "MLB Pitchers Opp Data")  # Change "Sheet1" to your desired sheet name

# Print a confirmation message
cat("Data has been saved to the Google Sheet:", sheet_id, "\n")


# Set the date you want to pull
date <- with_tz(Sys.time(), "America/New_York") |> as.Date()

# API URL
url <- paste0("https://statsapi.mlb.com/api/v1/schedule?sportId=1&date=", date, "&hydrate=probablePitcher(note),team")

# Fetch and parse JSON
res <- GET(url)
json <- content(res, as = "text", encoding = "UTF-8")
data <- fromJSON(json, flatten = TRUE)

# Extract games
games <- data$dates$games[[1]]

# Get away pitchers
away_pitchers <- games %>%
  filter(!is.na(teams.away.probablePitcher.fullName)) %>%
  transmute(
    pitcher = stri_trans_general(teams.away.probablePitcher.fullName, "Latin-ASCII"),
    team_abbr = teams.away.team.abbreviation,
    opponent_abbr = teams.home.team.abbreviation
  )

# Get home pitchers
home_pitchers <- games %>%
  filter(!is.na(teams.home.probablePitcher.fullName)) %>%
  transmute(
    pitcher = stri_trans_general(teams.home.probablePitcher.fullName, "Latin-ASCII"),
    team_abbr = teams.home.team.abbreviation,
    opponent_abbr = teams.away.team.abbreviation
  )

# Combine
pitchers <- bind_rows(away_pitchers, home_pitchers)

# Define team abbreviation mapping
team_abbr_map <- c(
  "KC" = "KCR", "SD" = "SDP", "MIN" = "MIN", "TB" = "TBR",
  "CLE" = "CLE", "ATH" = "ATH", "AZ" = "ARI", "HOU" = "HOU",
  "CHC" = "CHC", "SEA" = "SEA", "COL" = "COL", "DET" = "DET",
  "WSH" = "WSN", "LAD" = "LAD", "STL" = "STL", "BAL" = "BAL",
  "PIT" = "PIT", "BOS" = "BOS", "NYY" = "NYY", "TOR" = "TOR",
  "MIA" = "MIA", "PHI" = "PHI", "CWS" = "CHW", "MIL" = "MIL",
  "TEX" = "TEX", "SF" = "SFG", "NYM" = "NYM", "ATL" = "ATL",
  "LAA" = "LAA", "CIN" = "CIN"
)

# Apply mapping
pitchers <- pitchers %>%
  mutate(
    team_abbr = ifelse(team_abbr %in% names(team_abbr_map), team_abbr_map[team_abbr], team_abbr),
    opponent_abbr = ifelse(opponent_abbr %in% names(team_abbr_map), team_abbr_map[opponent_abbr], opponent_abbr)
  )

# OPTIONAL: Export to Excel file locally
#write.csv(pitchers, "probable_pitchers.csv", row.names = FALSE)

# OPTIONAL: Write to Google Sheets
# Specify the Google Sheet to write to
sheet_id <- "1AAuiHodCcMzOCpC7oW5xzBXIobcavIvh2AV-sy8OaD4"  # Replace with your Google Sheet ID

# Write the data to the Google Sheet
write_sheet(pitchers, ss = sheet_id, sheet = "Probable Pitchers")  # Change "Sheet1" to your desired sheet name

cat("Data has been saved to the Google Sheet:", sheet_id, "\n")

