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

sheet_id <- "1AAuiHodCcMzOCpC7oW5xzBXIobcavIvh2AV-sy8OaD4"

# =============================================================================
# DATES
# =============================================================================
end_date                <- Sys.Date()
url_end_date            <- end_date - 1
season_start_date       <- as.Date("2026-03-25")
fallback_start_date     <- as.Date("2025-09-28")
days_since_season_start <- as.numeric(end_date - season_start_date)
thirty_days_in          <- season_start_date + 30  # April 24 — flip to 2026-only

get_start_date <- function(days_threshold) {
  if (days_since_season_start >= days_threshold) {
    return(end_date - (days_threshold - 1))
  } else {
    fallback_days <- days_threshold - days_since_season_start
    return(fallback_start_date - (fallback_days - 1))
  }
}

start_date_7d_str  <- format(get_start_date(7),  "%Y-%m-%d")
start_date_14d_str <- format(get_start_date(14), "%Y-%m-%d")
start_date_30d_str <- format(get_start_date(30), "%Y-%m-%d")
url_end_date_str   <- format(url_end_date, "%Y-%m-%d")

# =============================================================================
# URLS
# =============================================================================
construct_url <- function(start_date_str, season) {
  sprintf(
    "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=%s&season1=%s&startdate=%s&enddate=%s&month=%s&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR",
    season, season, start_date_str, url_end_date_str, ifelse(season == "2026", "1000", "0")
  )
}

hitting_7d_url  <- construct_url(start_date_7d_str,  ifelse(days_since_season_start >= 7,  "2026", "2025"))
hitting_14d_url <- construct_url(start_date_14d_str, ifelse(days_since_season_start >= 14, "2026", "2025"))
hitting_30d_url <- construct_url(start_date_30d_str, ifelse(days_since_season_start >= 30, "2026", "2025"))

# Full season URL — slides forward day by day until 30 days in, then flips to 2026-only
if (end_date >= thirty_days_in) {
  hittingurl <- sprintf(
    "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=0&season=2026&season1=2026&startdate=2026-03-25&enddate=%s&month=1000&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR",
    format(end_date, "%Y-%m-%d")
  )
  cat("30+ days into 2026 season, using 2026 data only.\n")
} else {
  full_season_start_date <- as.Date("2025-08-28") + days_since_season_start
  hittingurl <- sprintf(
    "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=0&season=2025&season1=2025&startdate=%s&enddate=%s&month=1000&hand=&team=0&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR",
    format(full_season_start_date, "%Y-%m-%d"),
    format(end_date, "%Y-%m-%d")
  )
  cat(sprintf("Early season (day %d of 30), blending from %s through today.\n",
              days_since_season_start, format(full_season_start_date, "%Y-%m-%d")))
}

print(hitting_7d_url)
print(hitting_14d_url)
print(hitting_30d_url)
print(hittingurl)

# =============================================================================
# SAFE API CALL HELPER
# =============================================================================
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
# FULL SEASON HITTERS
# =============================================================================
df <- safe_api_call(hittingurl, "Full Season Hitters")

if (!is.null(df) && nrow(df) > 0) {
  filtered_df <- df %>%
    select(
      Player      = PlayerNameRoute,
      ID          = playerid,
      Team        = TeamName,
      AVG, OBP, OPS, SLG, BABIP, PA, R, HR, RBI, SB,
      Barrel_pct  = `Barrel%`,
      HardHit_pct = `HardHit%`,
      xBA         = xAVG,
      xSLG,
      xwOBA
    ) %>%
    mutate(
      AVG         = formatC(as.numeric(AVG),         format = "f", digits = 3),
      OBP         = formatC(as.numeric(OBP),         format = "f", digits = 3),
      OPS         = formatC(as.numeric(OPS),         format = "f", digits = 3),
      SLG         = formatC(as.numeric(SLG),         format = "f", digits = 3),
      BABIP       = formatC(as.numeric(BABIP),       format = "f", digits = 3),
      xBA         = formatC(as.numeric(xBA),         format = "f", digits = 3),
      xSLG        = formatC(as.numeric(xSLG),        format = "f", digits = 3),
      xwOBA       = formatC(as.numeric(xwOBA),       format = "f", digits = 3),
      Barrel_pct  = formatC(as.numeric(Barrel_pct)  * 100, format = "f", digits = 1),
      HardHit_pct = formatC(as.numeric(HardHit_pct) * 100, format = "f", digits = 1)
    ) %>%
    mutate(across(c(AVG, OBP, OPS, SLG, BABIP, xBA, xSLG, xwOBA, Barrel_pct, HardHit_pct), as.numeric)) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

  write_sheet(filtered_df, ss = sheet_id, sheet = "MLB Hitter Data")
  cat("Full season hitters saved to Google Sheet.\n")
} else {
  cat("Skipping Full Season Hitters sheet (no data).\n")
}

# =============================================================================
# 7d HITTERS
# =============================================================================
df7d <- safe_api_call(hitting_7d_url, "7d Hitters")

if (!is.null(df7d) && nrow(df7d) > 0) {
  filtered_df7d <- df7d %>%
    select(Player = PlayerNameRoute, ID = playerid, Team = TeamName,
           wOBA7d = wOBA, ISO7d = ISO, wrc7d = wRC, k_pct7d = `K%`) %>%
    mutate(
      wOBA7d  = as.numeric(wOBA7d),
      ISO7d   = as.numeric(ISO7d),
      wrc7d   = as.numeric(wrc7d),
      k_pct7d = as.numeric(k_pct7d) * 100
    )
  colnames(filtered_df7d) <- c("Player", "ID", "Team", "wOBA7d", "ISO7d", "wRC+7d", "K%7d")
  write_sheet(filtered_df7d, ss = sheet_id, sheet = "MLB Hitter 7d Data")
  cat("7d hitters saved to Google Sheet.\n")
} else {
  cat("Skipping 7d Hitters sheet (no data).\n")
}

# =============================================================================
# 14d HITTERS
# =============================================================================
df14d <- safe_api_call(hitting_14d_url, "14d Hitters")

if (!is.null(df14d) && nrow(df14d) > 0) {
  filtered_df14d <- df14d %>%
    select(Player = PlayerNameRoute, ID = playerid, Team = TeamName,
           wOBA14d = wOBA, ISO14d = ISO, wrc14d = wRC, k_pct14d = `K%`) %>%
    mutate(
      wOBA14d  = as.numeric(wOBA14d),
      ISO14d   = as.numeric(ISO14d),
      wrc14d   = as.numeric(wrc14d),
      k_pct14d = as.numeric(k_pct14d) * 100
    )
  colnames(filtered_df14d) <- c("Player", "ID", "Team", "wOBA14d", "ISO14d", "wRC+14d", "K%14d")
  write_sheet(filtered_df14d, ss = sheet_id, sheet = "MLB Hitter 14d Data")
  cat("14d hitters saved to Google Sheet.\n")
} else {
  cat("Skipping 14d Hitters sheet (no data).\n")
}

# =============================================================================
# 30d HITTERS
# =============================================================================
df30d <- safe_api_call(hitting_30d_url, "30d Hitters")

if (!is.null(df30d) && nrow(df30d) > 0) {
  filtered_df30d <- df30d %>%
    select(Player = PlayerNameRoute, ID = playerid, Team = TeamName,
           wOBA30d = wOBA, ISO30d = ISO, wrc30d = wRC, k_pct30d = `K%`) %>%
    mutate(
      wOBA30d  = as.numeric(wOBA30d),
      ISO30d   = as.numeric(ISO30d),
      wrc30d   = as.numeric(wrc30d),
      k_pct30d = as.numeric(k_pct30d) * 100
    )
  colnames(filtered_df30d) <- c("Player", "ID", "Team", "wOBA30d", "ISO30d", "wRC+30d", "K%30d")
  write_sheet(filtered_df30d, ss = sheet_id, sheet = "MLB Hitter 30d Data")
  cat("30d hitters saved to Google Sheet.\n")
} else {
  cat("Skipping 30d Hitters sheet (no data).\n")
}

# =============================================================================
# ACTIVE HITTERS - MLB API + Roster Reference cross-reference
# Runs last so failures here cannot block any section above
# =============================================================================
tryCatch({

  # Team ID to FanGraphs abbreviation mapping
  team_id_to_fg <- c(
    `108` = "LAA", `109` = "ARI", `110` = "BAL", `111` = "BOS", `112` = "CHC",
    `113` = "CIN", `114` = "CLE", `115` = "COL", `116` = "DET", `117` = "HOU",
    `118` = "KCR", `119` = "LAD", `120` = "WSN", `121` = "NYM", `133` = "ATH",
    `134` = "PIT", `135` = "SDP", `136` = "SEA", `137` = "SFG", `138` = "STL",
    `139` = "TBR", `140` = "TEX", `141` = "TOR", `142` = "MIN", `143` = "PHI",
    `144` = "ATL", `145` = "CHW", `146` = "MIA", `147` = "NYY", `158` = "MIL"
  )

  # Load Roster Reference
  cat("Loading Roster Reference sheet...\n")
  roster_ref <- read_sheet(sheet_id, sheet = "Roster Reference")
  cat(sprintf("Loaded %d players from Roster Reference.\n", nrow(roster_ref)))

  # Get today's schedule and map team IDs to FanGraphs abbreviations
  cat("Fetching today's schedule...\n")
  schedule_resp <- fromJSON(content(
    GET(sprintf("https://statsapi.mlb.com/api/v1/schedule?sportId=1&date=%s", Sys.Date())),
    as = "text", encoding = "UTF-8"
  ))

  teams_today <- NULL
  if (length(schedule_resp$dates) > 0 && !is.null(schedule_resp$dates$games[[1]])) {
    games       <- schedule_resp$dates$games[[1]]
    all_ids     <- unique(c(games$teams$away$team$id, games$teams$home$team$id))
    teams_today <- as.character(team_id_to_fg[as.character(all_ids)])
    teams_today <- teams_today[!is.na(teams_today)]
    cat(sprintf("Teams playing today (%d): %s\n", length(teams_today), paste(teams_today, collapse = ", ")))
  } else {
    cat("No games found for today.\n")
  }

  # Get active 26-man rosters from MLB API (position players only)
  cat("Fetching active rosters...\n")
  team_ids <- c(108, 109, 110, 111, 112, 113, 114, 115, 116, 117,
                118, 119, 120, 121, 133, 134, 135, 136, 137, 138,
                139, 140, 141, 142, 143, 144, 145, 146, 147, 158)

  mlb_active_ids <- c()
  for (team_id in team_ids) {
    tryCatch({
      roster_resp <- fromJSON(content(
        GET(sprintf("https://statsapi.mlb.com/api/v1/teams/%s/roster/Active", team_id)),
        as = "text", encoding = "UTF-8"
      ))
      if (length(roster_resp$roster) > 0) {
        pos_players    <- roster_resp$roster[roster_resp$roster$position$code != "1", ]
        mlb_active_ids <- c(mlb_active_ids, pos_players$person$id)
      }
    }, error = function(e) {
      cat(sprintf("Error fetching roster for team %s: %s\n", team_id, e$message))
    })
  }
  cat(sprintf("Found %d active position players across all rosters.\n", length(mlb_active_ids)))

  # Cross-reference with Roster Reference to get FanGraphs IDs
  # Then filter to only teams playing today
  active_hitters <- roster_ref %>%
    filter(MLBAMID %in% mlb_active_ids) %>%
    select(ID = PlayerId, Player = Name, Team = Team) %>%
    filter(!is.na(Player) & Player != "") %>%
    mutate(ID = as.character(unlist(ID)))

  if (!is.null(teams_today) && length(teams_today) > 0) {
    active_hitters <- active_hitters %>%
      filter(Team %in% teams_today)
  }

  active_hitters <- active_hitters %>% arrange(Team, Player)
  cat(sprintf("Writing %d active hitters (%d teams) to sheet.\n",
              nrow(active_hitters), length(unique(active_hitters$Team))))

  write_sheet(active_hitters, ss = sheet_id, sheet = "MLB Active Hitters")
  cat("Active hitters saved to Google Sheet.\n")

}, error = function(e) {
  cat(sprintf("Error in Active Hitters section: %s\n", e$message))
  cat("Skipping Active Hitters sheet.\n")
})

cat("\nScript completed successfully!\n")
