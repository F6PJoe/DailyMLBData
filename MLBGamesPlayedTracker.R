# MLB Fantasy Games Played Tracker - GitHub Actions Version
# Fetches player games played by position from FanGraphs and writes to Google Sheets

library(httr)
library(jsonlite)
library(dplyr)
library(googlesheets4)
library(base64enc)
library(parallel)

# ── Authentication ────────────────────────────────────────────────────────────
json_key       <- rawToChar(base64decode(Sys.getenv("GCP_SHEETS_KEY_B64")))
temp_json_file <- tempfile(fileext = ".json")
writeLines(json_key, temp_json_file)
gs4_auth(path = temp_json_file)

# ── Config ────────────────────────────────────────────────────────────────────
SHEET_ID   <- Sys.getenv("17ULjP624ARXjCouBZUasFzPqbOB9XlPeJ5x-B94Nc_o")   # set as repo secret
SEASON     <- "2026"
START_DATE <- paste0(SEASON, "-03-01")
END_DATE   <- paste0(SEASON, "-11-01")

BASE_URL <- paste0(
  "https://www.fangraphs.com/api/leaders/major-league/data",
  "?age=&pos=all&stats=bat&lg=all&qual=0",
  "&season=", SEASON, "&season1=", SEASON,
  "&startdate=", START_DATE, "&enddate=", END_DATE,
  "&hand=&team=0&pageitems=2000000000&pagenum=1",
  "&ind=0&rost=0&players=&type=8&postseason=",
  "&sortdir=default&sortstat=WAR"
)

# month codes: 0 = all positions; positional codes per FanGraphs API
POSITIONS <- list(
  "CS All Players" = list(month = "",  cols = c("playerid", "PlayerNameRoute", "TeamName", "G", "position"),
                           names = c("ID", "Player", "Team", "Games", "Position")),
  "CS C"  = list(month = "35", cols = c("playerid", "PlayerNameRoute", "G"), names = c("ID", "Player", "Games")),
  "CS 1B" = list(month = "36", cols = c("playerid", "PlayerNameRoute", "G"), names = c("ID", "Player", "Games")),
  "CS 2B" = list(month = "37", cols = c("playerid", "PlayerNameRoute", "G"), names = c("ID", "Player", "Games")),
  "CS SS" = list(month = "38", cols = c("playerid", "PlayerNameRoute", "G"), names = c("ID", "Player", "Games")),
  "CS 3B" = list(month = "39", cols = c("playerid", "PlayerNameRoute", "G"), names = c("ID", "Player", "Games")),
  "CS OF" = list(month = "43", cols = c("playerid", "PlayerNameRoute", "G"), names = c("ID", "Player", "Games")),
  "CS DH" = list(month = "44", cols = c("playerid", "PlayerNameRoute", "G"), names = c("ID", "Player", "Games"))
)

# ── Helpers ───────────────────────────────────────────────────────────────────
build_url <- function(month_code) {
  month_param <- if (nchar(month_code) > 0) paste0("&month=", month_code) else "&month="
  paste0(BASE_URL, month_param)
}

fetch_position <- function(sheet_name, cfg) {
  url      <- build_url(cfg$month)
  response <- GET(url, timeout(60))

  if (http_error(response)) {
    warning("HTTP error for sheet '", sheet_name, "': ", status_code(response))
    return(NULL)
  }

  data <- content(response, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE)
  df   <- data$data

  # Subset to available columns only (guards against API changes)
  available <- intersect(cfg$cols, names(df))
  if (length(available) < length(cfg$cols)) {
    warning("Missing columns for '", sheet_name, "': ",
            paste(setdiff(cfg$cols, names(df)), collapse = ", "))
  }

  filtered <- df |> select(all_of(available))
  colnames(filtered) <- cfg$names[seq_along(available)]
  list(sheet = sheet_name, data = filtered)
}

write_to_sheet <- function(result) {
  if (is.null(result)) return(invisible(NULL))
  tryCatch(
    write_sheet(result$data, ss = SHEET_ID, sheet = result$sheet),
    error = function(e) warning("Failed to write '", result$sheet, "': ", e$message)
  )
  cat("✓ Written:", result$sheet, "(", nrow(result$data), "rows )\n")
}

# ── Fetch all positions in parallel ──────────────────────────────────────────
n_cores <- min(length(POSITIONS), detectCores(logical = FALSE))
cat("Fetching", length(POSITIONS), "position endpoints on", n_cores, "cores...\n")

results <- mcmapply(
  fetch_position,
  sheet_name = names(POSITIONS),
  cfg        = POSITIONS,
  SIMPLIFY   = FALSE,
  mc.cores   = n_cores
)

# ── Write results to Google Sheets (sequential to avoid rate limits) ──────────
cat("Writing to Google Sheets...\n")
invisible(lapply(results, write_to_sheet))

cat("\nDone!\n")
