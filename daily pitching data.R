# clear console and workspace
cat("\014")
rm(list = ls())

# Required packages
packages <- c("httr", "jsonlite", "googlesheets4", "base64enc", "dplyr", "stringi", "lubridate")
invisible(lapply(packages, library, character.only = TRUE))

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
    response    <- GET(url)
    data_parsed <- content(response, as = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE)

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
# DATES
# =============================================================================
start_date        <- as.Date("2025-07-21")
end_date          <- Sys.Date() - 1
season_start_date <- as.Date("2026-03-25")

# =============================================================================
# PITCHER DATA
# =============================================================================
if (end_date <= as.Date("2026-04-22")) {
  date_diff      <- as.integer(end_date - season_start_date)
  new_start_date <- format(start_date + (date_diff * 2), "%Y-%m-%d")
  end_date_str   <- format(end_date, "%Y-%m-%d")

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
  pitchingurl <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=pit&lg=all&qual=1&season=2026&season1=2026&startdate=2026-03-01&enddate=2026-11-01&month=0&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
  sheetname <- "MLB Pitchers Data"
}

print(pitchingurl)

df <- safe_api_call(pitchingurl, "Pitchers")

if (!is.null(df) && nrow(df) > 0) {
  filtered_df <- df %>%
    select(
      PlayerNameRoute,
      playerid,
      TeamName,
      K_pct        = `K%`,
      SwStr_pct    = `SwStr%`,
      IP, SO,
      ERA, WHIP,
      BABIP, SIERA,
      xFIP, xERA,
      GB_pct       = `GB%`,
      FB_pct       = `FB%`,
      Hard_hit_pct = `Hard%`,
      Barrel_pct   = `Barrel%`,
      CSW_pct      = `C+SwStr%`
    ) %>%
    mutate(
      K_pct        = as.numeric(K_pct)        * 100,
      SwStr_pct    = as.numeric(SwStr_pct)    * 100,
      GB_pct       = as.numeric(GB_pct)       * 100,
      FB_pct       = as.numeric(FB_pct)       * 100,
      Hard_hit_pct = as.numeric(Hard_hit_pct) * 100,
      Barrel_pct   = as.numeric(Barrel_pct)   * 100,
      CSW_pct      = as.numeric(CSW_pct)      * 100,
      ERA   = as.numeric(ERA),
      xFIP  = as.numeric(xFIP),
      xERA  = as.numeric(xERA),
      SIERA = as.numeric(SIERA),
      WHIP  = as.numeric(WHIP),
      BABIP = as.numeric(BABIP),
      IP    = as.numeric(IP),
      SO    = as.numeric(SO)
    )

  colnames(filtered_df) <- c(
    "Player", "ID", "Team", "K%", "SwStr%", "IP", "SO", "ERA", "WHIP", "BABIP",
    "SIERA", "xFIP", "xERA", "GB%", "FB%", "Hard%", "Barrel%", "CSW%"
  )

  write_sheet(filtered_df, ss = sheet_id, sheet = sheetname)
  cat("Pitcher data has been saved to the Google Sheet:", sheet_id, "\n")
} else {
  cat("Skipping Pitcher Data sheet (no data).\n")
}

# =============================================================================
# OPPONENT PITCHING DATA
# =============================================================================
end_date <- Sys.Date()

if (end_date <= as.Date("2026-05-01")) {
  date_diff      <- as.integer(end_date - season_start_date)
  new_start_date <- format(start_date + (date_diff * 2), "%Y-%m-%d")
  end_date_str   <- format(end_date, "%Y-%m-%d")

  opppitchingurl <- paste0(
    "https://www.fangraphs.com/api/leaders/major-league/data?",
    "age=&pos=all&stats=bat&lg=all&qual=0&season=2025&season1=2025&",
    "startdate=", new_start_date, "&enddate=", end_date_str,
    "&month=1000&hand=&team=0%2Cts&pageitems=30&pagenum=1&ind=0&rost=0&players=0&type=8&postseason=&sortdir=default&sortstat=wRC%2B"
  )
} else {
  opppitchingurl <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=0&season=2026&season1=2026&startdate=2026-03-01&enddate=2026-11-01&month=0&hand=&team=0%2Cts&pageitems=30&pagenum=1&ind=0&rost=0&players=0&type=8&postseason=&sortdir=default&sortstat=WAR"
}

print(opppitchingurl)

df_opp <- safe_api_call(opppitchingurl, "Opponent Pitching")

if (!is.null(df_opp) && nrow(df_opp) > 0) {
  filtered_df_opp <- df_opp %>%
    select(
      TeamName,
      K_pct    = `K%`,
      SwStr_pct = `SwStr%`,
      wrc_plus = `wRC+`,
      wOBA, ISO,
      TG
    ) %>%
    mutate(
      K_pct     = as.numeric(K_pct)     * 100,
      SwStr_pct = as.numeric(SwStr_pct) * 100,
      wrc_plus  = as.numeric(wrc_plus),
      wOBA      = as.numeric(wOBA),
      ISO       = as.numeric(ISO),
      TG        = as.numeric(TG)
    )

  colnames(filtered_df_opp) <- c("Team", "K%", "SwStr%", "wRC+", "wOBA", "ISO", "G")

  write_sheet(filtered_df_opp, ss = sheet_id, sheet = "MLB Pitchers Opp Data")
  cat("Opponent pitching data has been saved to the Google Sheet:", sheet_id, "\n")
} else {
  cat("Skipping Opponent Pitching Data sheet (no data).\n")
}

# =============================================================================
# PROBABLE PITCHERS
# =============================================================================
date <- Sys.Date()
url  <- paste0("https://statsapi.mlb.com/api/v1/schedule?sportId=1&date=", date, "&hydrate=probablePitcher(note),team")

res  <- GET(url)
json <- content(res, as = "text", encoding = "UTF-8")
data <- fromJSON(json, flatten = TRUE)

if (length(data$dates) > 0 && !is.null(data$dates$games[[1]])) {
  games <- data$dates$games[[1]]

  away_pitchers <- games %>%
    filter(!is.na(teams.away.probablePitcher.fullName)) %>%
    transmute(
      pitcher      = stri_trans_general(teams.away.probablePitcher.fullName, "Latin-ASCII"),
      team_abbr    = teams.away.team.abbreviation,
      opponent_abbr = teams.home.team.abbreviation
    )

  home_pitchers <- games %>%
    filter(!is.na(teams.home.probablePitcher.fullName)) %>%
    transmute(
      pitcher      = stri_trans_general(teams.home.probablePitcher.fullName, "Latin-ASCII"),
      team_abbr    = teams.home.team.abbreviation,
      opponent_abbr = teams.away.team.abbreviation
    )

  pitchers <- bind_rows(away_pitchers, home_pitchers)

  team_abbr_map <- c(
    "KC"  = "KCR", "SD"  = "SDP", "MIN" = "MIN", "TB"  = "TBR",
    "CLE" = "CLE", "OAK" = "ATH", "AZ"  = "ARI", "HOU" = "HOU",
    "CHC" = "CHC", "SEA" = "SEA", "COL" = "COL", "DET" = "DET",
    "WSH" = "WSN", "LAD" = "LAD", "STL" = "STL", "BAL" = "BAL",
    "PIT" = "PIT", "BOS" = "BOS", "NYY" = "NYY", "TOR" = "TOR",
    "MIA" = "MIA", "PHI" = "PHI", "CWS" = "CHW", "MIL" = "MIL",
    "TEX" = "TEX", "SF"  = "SFG", "NYM" = "NYM", "ATL" = "ATL",
    "LAA" = "LAA", "CIN" = "CIN"
  )

  pitchers <- pitchers %>%
    mutate(
      team_abbr     = ifelse(team_abbr     %in% names(team_abbr_map), team_abbr_map[team_abbr],     team_abbr),
      opponent_abbr = ifelse(opponent_abbr %in% names(team_abbr_map), team_abbr_map[opponent_abbr], opponent_abbr)
    )

  write_sheet(pitchers, ss = sheet_id, sheet = "Probable Pitchers")
  cat("Probable pitchers data has been saved to the Google Sheet:", sheet_id, "\n")
} else {
  cat("No games scheduled today - skipping Probable Pitchers sheet.\n")
}

cat("\nScript completed successfully!\n")
