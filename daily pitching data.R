# clear console and workspace
cat("\014")
rm(list = ls())

# Required packages
packages <- c("httr", "jsonlite", "googlesheets4", "base64enc", "dplyr", "stringi", "lubridate")

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
sheet_id <- "1AAuiHodCcMzOCpC7oW5xzBXIobcavIvh2AV-sy8OaD4"

# Helper function to safely process API response
safe_api_call <- function(url, label) {
  tryCatch({
    response <- GET(url)
    data_parsed <- content(response, as = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE)
    
    # Handle nested data structure
    if (is.null(data_parsed) || length(data_parsed) == 0) {
      cat(sprintf("No %s data available (empty response).\n", label))
      return(NULL)
    }
    
    # Extract the data array if it's nested
    if ("data" %in% names(data_parsed)) {
      df <- data_parsed$data
    } else {
      df <- data_parsed
    }
    
    # Check if we actually have data
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

# Define the start date for fallback data (2025 season)
start_date <- as.Date("2025-07-21")

# Get today's date
end_date <- Sys.Date() - 1

# Season start date
season_start_date <- as.Date("2026-03-25")

# Check if today's date is before or on the cutoff date (updated to 4/22/26 because xERA stopped coming over in combined year data)
if (end_date <= as.Date("2026-04-22")) {
  # If today is before the cutoff, use combined 2025-2026 data
  date_diff <- as.integer(end_date - season_start_date)
  
  # Calculate the new start date based on the difference in days
  new_start_date <- start_date + (date_diff * 2)
  
  # Format both the start and end date to match the required format
  new_start_date <- format(new_start_date, "%Y-%m-%d")
  end_date_str <- format(end_date, "%Y-%m-%d")
  
  # Define the URL with dynamically generated start and end dates (combined 2025-2026)
  pitchingurl <- paste0(
    "https://www.fangraphs.com/api/leaders/major-league/data?",
    "age=&pos=all&stats=pit&lg=all&qual=1&",
    "season=2026&season1=2025&",
    "startdate=", new_start_date,
    "&enddate=", end_date_str,
    "&month=1000&hand=&team=0&",
    "pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&",
    "type=8&postseason=&sortdir=default&sortstat=WAR"
  )
  
  sheetname <- "MLB Pitcher Data w-2025"
  
} else {
  # If today is on or after the cutoff date, use 2026 season only
  pitchingurl <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=pit&lg=all&qual=1&season=2026&season1=2026&startdate=2026-03-01&enddate=2026-11-01&month=0&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
  sheetname <- "MLB Pitchers Data"
}

print(pitchingurl)

# Make the GET request
df <- safe_api_call(pitchingurl, "Pitchers")

if (!is.null(df) && nrow(df) > 0) {
  # Filter to keep only the desired columns
  filtered_df <- df %>%
    select(
      PlayerNameRoute,
      playerid,
      TeamName,
      K_pct = `K%`,
      SwStr_pct = `SwStr%`,
      IP, SO,
      ERA, WHIP,
      BABIP, SIERA,
      xFIP, xERA,
      GB_pct = `GB%`,
      FB_pct = `FB%`,
      Hard_hit_pct = `Hard%`,
      Barrel_pct = `Barrel%`,
      CSW_pct = `C+SwStr%`
    ) %>%
    mutate(
      # Percentages with one decimal place
      K_pct = as.numeric(K_pct) * 100,
      SwStr_pct = as.numeric(SwStr_pct) * 100,
      GB_pct = as.numeric(GB_pct) * 100,
      FB_pct = as.numeric(FB_pct) * 100,
      Hard_hit_pct = as.numeric(Hard_hit_pct) * 100,
      Barrel_pct = as.numeric(Barrel_pct) * 100,
      CSW_pct = as.numeric(CSW_pct) * 100,
      
      # ERA, xFIP, xERA, SIERA as numeric (let Google Sheets handle formatting)
      ERA = as.numeric(ERA),
      xFIP = as.numeric(xFIP),
      xERA = as.numeric(xERA),
      SIERA = as.numeric(SIERA),
      
      # WHIP, BABIP as numeric
      WHIP = as.numeric(WHIP),
      BABIP = as.numeric(BABIP),
      
      # IP and SO
      IP = as.numeric(IP),
      SO = as.numeric(SO)
    )
  
  # Rename the columns
  colnames(filtered_df) <- c(
    "Player", "ID", "Team", "K%", "SwStr%", "IP", "SO", "ERA", "WHIP", "BABIP",
    "SIERA", "xFIP", "xERA", "GB%", "FB%", "Hard%", "Barrel%", "CSW%"
  )
  
  # Write the data to the Google Sheet
  write_sheet(filtered_df, ss = sheet_id, sheet = sheetname)
  
  # Print a confirmation message
  cat("Pitcher data has been saved to the Google Sheet:", sheet_id, "\n")
} else {
  cat("Skipping Pitcher Data sheet (no data).\n")
}

# --- Opponent Pitching Data ---

# Get today's date
end_date <- Sys.Date()

# Check if today's date is before or on May 1st, 2026
if (end_date <= as.Date("2026-05-01")) {
  # If today is before May 1st, 2026, use combined 2025-2026 data
  date_diff <- as.integer(end_date - season_start_date)
  
  # Calculate the new start date based on the difference in days
  new_start_date <- start_date + (date_diff * 2)
  
  # Format both the start and end date to match the required format
  new_start_date <- format(new_start_date, "%Y-%m-%d")
  end_date_str <- format(end_date, "%Y-%m-%d")
  
  # Define the URL with dynamically generated start and end dates
  opppitchingurl <- paste0(
    "https://www.fangraphs.com/api/leaders/major-league/data?",
    "age=&pos=all&stats=bat&lg=all&qual=0&season=2025&season1=2025&",
    "startdate=", new_start_date, "&enddate=", end_date_str, "&month=1000&hand=&team=0%2Cts&pageitems=30&pagenum=1&ind=0&rost=0&players=0&type=8&postseason=&sortdir=default&sortstat=wRC%2B"
  )
} else {
  # If today is on or after May 1st, 2026, use 2026 season only
  opppitchingurl <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=0&season=2026&season1=2026&startdate=2026-03-01&enddate=2026-11-01&month=0&hand=&team=0%2Cts&pageitems=30&pagenum=1&ind=0&rost=0&players=0&type=8&postseason=&sortdir=default&sortstat=WAR"
}

print(opppitchingurl)

# Make the GET request
df_opp <- safe_api_call(opppitchingurl, "Opponent Pitching")

if (!is.null(df_opp) && nrow(df_opp) > 0) {
  # Filter to keep only the desired columns
  filtered_df_opp <- df_opp %>%
    select(
      TeamName,
      K_pct = `K%`,
      SwStr_pct = `SwStr%`,
      wrc_plus = `wRC+`,
      wOBA, ISO,
      TG
    ) %>%
    mutate(
      K_pct = as.numeric(K_pct) * 100,
      SwStr_pct = as.numeric(SwStr_pct) * 100,
      wrc_plus = as.numeric(wrc_plus),
      wOBA = as.numeric(wOBA),
      ISO = as.numeric(ISO),
      TG = as.numeric(TG)
    )
  
  # Rename the columns
  colnames(filtered_df_opp) <- c(
    "Team", "K%", "SwStr%", "wRC+", "wOBA", "ISO", "G"
  )
  
  # Write the data to the Google Sheet
  write_sheet(filtered_df_opp, ss = sheet_id, sheet = "MLB Pitchers Opp Data")
  
  # Print a confirmation message
  cat("Opponent pitching data has been saved to the Google Sheet:", sheet_id, "\n")
} else {
  cat("Skipping Opponent Pitching Data sheet (no data).\n")
}

# --- Probable Pitchers ---

# Set the date you want to pull
date <- Sys.Date()

# API URL
url <- paste0("https://statsapi.mlb.com/api/v1/schedule?sportId=1&date=", date, "&hydrate=probablePitcher(note),team")

# Fetch and parse JSON
res <- GET(url)
json <- content(res, as = "text", encoding = "UTF-8")
data <- fromJSON(json, flatten = TRUE)

# Check if there are games scheduled
if (length(data$dates) > 0 && !is.null(data$dates$games[[1]])) {
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
    "CLE" = "CLE", "OAK" = "ATH", "AZ" = "ARI", "HOU" = "HOU",
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
  
  # Write to Google Sheets
  write_sheet(pitchers, ss = sheet_id, sheet = "Probable Pitchers")
  
  cat("Probable pitchers data has been saved to the Google Sheet:", sheet_id, "\n")
} else {
  cat("No games scheduled today - skipping Probable Pitchers sheet.\n")
}

cat("\nScript completed successfully!\n")
