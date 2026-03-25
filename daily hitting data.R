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

# Helper function to get active 26-man rosters from MLB API
get_active_26man_roster <- function() {
  # All 30 MLB team IDs
  team_ids <- c(108, 109, 110, 111, 112, 113, 114, 115, 116, 117,
                118, 119, 120, 121, 133, 134, 135, 136, 137, 138,
                139, 140, 141, 142, 143, 144, 145, 146, 147, 158)
  
  # Team abbreviations mapping (MLB team ID to FanGraphs abbreviation)
  team_abbrev <- c(
    `108` = "LAA", `109` = "ARI", `110` = "BAL", `111` = "BOS", `112` = "CHC",
    `113` = "CIN", `114` = "CLE", `115` = "COL", `116` = "DET", `117` = "HOU",
    `118` = "KCR", `119` = "LAD", `120` = "WSN", `121` = "NYM", `133` = "ATH",
    `134` = "PIT", `135` = "SDP", `136` = "SEA", `137` = "SFG", `138` = "STL",
    `139` = "TBR", `140` = "TEX", `141` = "TOR", `142` = "MIN", `143` = "PHI",
    `144` = "ATL", `145` = "CHW", `146` = "MIA", `147` = "NYY", `158` = "MIL"
  )
  
  all_players <- data.frame()
  
  for (team_id in team_ids) {
    tryCatch({
      url <- sprintf("https://statsapi.mlb.com/api/v1/teams/%s/roster/Active", team_id)
      response <- GET(url)
      data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      
      if (length(data$roster) > 0) {
        roster <- data$roster
        
        # Filter to position players only (exclude pitchers)
        # Position code "1" = pitcher
        position_players <- roster[roster$position$code != "1", ]
        
        if (nrow(position_players) > 0) {
          players <- data.frame(
            mlbam_id = position_players$person$id,
            player_name = position_players$person$fullName,
            team_abbrev = team_abbrev[as.character(team_id)]
          )
          
          all_players <- rbind(all_players, players)
        }
      }
    }, error = function(e) {
      cat(sprintf("Error fetching roster for team %s: %s\n", team_id, e$message))
    })
  }
  
  return(all_players)
}

# --- Full Season Hitters (get this first for mapping) ---
df <- safe_api_call(hittingurl, "Full Season Hitters")

# --- Active Hitters (26-Man Roster from MLB API) ---
cat("Fetching active 26-man rosters from MLB API...\n")
mlb_active_roster <- get_active_26man_roster()

if (!is.null(mlb_active_roster) && nrow(mlb_active_roster) > 0 && !is.null(df) && nrow(df) > 0) {
  # Map MLB roster to FanGraphs IDs using xMLBAMID
  active_hitters <- df %>%
    filter(xMLBAMID %in% mlb_active_roster$mlbam_id) %>%
    select(
      ID = playerid,
      Player = PlayerNameRoute,
      Team = TeamName
    ) %>%
    arrange(Team, Player)
  
  write_sheet(active_hitters, ss = sheet_id, sheet = "MLB Active Hitters")
  cat(sprintf("Active hitters saved to Google Sheet (%d players).\n", nrow(active_hitters)))
} else {
  cat("Skipping Active Hitters sheet (no MLB roster data or FanGraphs data unavailable).\n")
}

# --- Full Season Hitters ---
if (!is.null(df) && nrow(df) > 0) {
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
      AVG = as.numeric(AVG),
      OBP = as.numeric(OBP),
      OPS = as.numeric(OPS),
      SLG = as.numeric(SLG),
      BABIP = as.numeric(BABIP),
      PA = as.numeric(PA),
      R = as.numeric(R),
      HR = as.numeric(HR),
      RBI = as.numeric(RBI),
      SB = as.numeric(SB),
      Barrel_pct = as.numeric(Barrel_pct) * 100,
      HardHit_pct = as.numeric(HardHit_pct) * 100,
      xBA = as.numeric(xBA),
      xSLG = as.numeric(xSLG),
      xwOBA = as.numeric(xwOBA)
    ) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
  
  write_sheet(filtered_df, ss = sheet_id, sheet = "MLB Hitter Data")
  cat("Full season hitters saved to Google Sheet.\n")
} else {
  cat("Skipping Full Season Hitters sheet (no data).\n")
}

# --- 7d, 14d, 30d hitters ---

# 7d
df7d <- safe_api_call(hitting_7d_url, "7d Hitters")

if (!is.null(df7d) && nrow(df7d) > 0) {
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
      wOBA7d = as.numeric(wOBA7d),
      ISO7d = as.numeric(ISO7d),
      wrc7d = as.numeric(wrc7d),
      k_pct7d = as.numeric(k_pct7d) * 100
    )
  colnames(filtered_df7d) <- c("Player", "ID", "Team", "wOBA7d", "ISO7d", "wRC+7d", "K%7d")
  write_sheet(filtered_df7d, ss = sheet_id, sheet = "MLB Hitter 7d Data")
  cat("7d hitters saved to Google Sheet.\n")
} else {
  cat("Skipping 7d Hitters sheet (no data).\n")
}

# 14d
df14d <- safe_api_call(hitting_14d_url, "14d Hitters")

if (!is.null(df14d) && nrow(df14d) > 0) {
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
      wOBA14d = as.numeric(wOBA14d),
      ISO14d = as.numeric(ISO14d),
      wrc14d = as.numeric(wrc14d),
      k_pct14d = as.numeric(k_pct14d) * 100
    )
  colnames(filtered_df14d) <- c("Player", "ID", "Team", "wOBA14d", "ISO14d", "wRC+14d", "K%14d")
  write_sheet(filtered_df14d, ss = sheet_id, sheet = "MLB Hitter 14d Data")
  cat("14d hitters saved to Google Sheet.\n")
} else {
  cat("Skipping 14d Hitters sheet (no data).\n")
}

# 30d
df30d <- safe_api_call(hitting_30d_url, "30d Hitters")

if (!is.null(df30d) && nrow(df30d) > 0) {
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
      wOBA30d = as.numeric(wOBA30d),
      ISO30d = as.numeric(ISO30d),
      wrc30d = as.numeric(wrc30d),
      k_pct30d = as.numeric(k_pct30d) * 100
    )
  colnames(filtered_df30d) <- c("Player", "ID", "Team", "wOBA30d", "ISO30d", "wRC+30d", "K%30d")
  write_sheet(filtered_df30d, ss = sheet_id, sheet = "MLB Hitter 30d Data")
  cat("30d hitters saved to Google Sheet.\n")
} else {
  cat("Skipping 30d Hitters sheet (no data).\n")
}

cat("\nScript completed successfully!\n")
