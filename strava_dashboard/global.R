library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(DT)
library(rStrava)
library(googlePolylines)
library(httr)
library(htmltools)
library(tidyr)

STRAVA_ORANGE <- "#FC4C02"

BIKE_TYPES <- c("Ride", "VirtualRide", "EBikeRide", "MountainBikeRide",
                "GravelRide")

MTB_TYPES  <- c("MountainBikeRide")
ROAD_TYPES <- c("Ride", "VirtualRide", "GravelRide")

CACHE_FILE  <- "activities_cache.rds"
CACHE_HOURS <- 6

# ─────────────────────────────────────────────────────────────────────────────
# MAINTENANCE Setup 
# Log for maintenance page
# Update dates below when new service is performed/ parts are replaced
# ─────────────────────────────────────────────────────────────────────────────

MAINTENANCE <- list(

  mtb = data.frame(
    component      = c("Chain", "Cassette", "Front Tire", "Rear Tire",
                       "Fr Brake Pads", "Rr Brake Pads"),
    last_replaced  = as.Date(c(
      "2025-05-21",   # Chain
      "2025-05-21",   # Cassette
      "2025-05-21",   # Front Tire
      "2025-05-21",   # Rear Tire
      "2025-05-21",   # Front Brake Pads
      "2025-05-21"    # Rear Brake Pads
    )),
    recommended_mi = c(2000, 3000, 5000, 3000, 3000, 3000),
    notes          = c("", "", "", "", "", ""),
    stringsAsFactors = FALSE
  ),

  road = data.frame(
    component      = c("Chain", "Cassette", "Front Tire", "Rear Tire",
                       "Bar Tape"),
    last_replaced  = as.Date(c(
      "2026-04-23",   # Chain
      "2026-04-23",   # Cassette
      "2024-07-04",   # Front Tire
      "2024-07-04",   # Rear Tire
      "2026-03-01"    # Bar Tape
    )),
    recommended_mi = c(2000, 3000, 5000, 3000, NA),
    notes          = c("", "", "", "", ""),
    stringsAsFactors = FALSE
  )

)

# ── Authentication ─────────────────────────────────────────────────────────────
get_token <- function() {
  for (path in c(".httr-oauth", "../.httr-oauth")) {
    if (file.exists(path)) {
      return(httr::config(token = readRDS(path)[[1]]))
    }
  }
  stop("No .httr-oauth found. Run strava_dashboard/api.R first.")
}
my_token <- get_token()

# ── Process raw activity list ──────────────────────────────────────────────────
process_activities <- function(act_list) {
  rStrava::compile_activities(act_list) %>%
    
    # Convert data to miles/ ft / hours and round
    mutate(
      distance_miles    = round(distance * 0.621371, 2),
      elevation_gain_ft = round(total_elevation_gain * 3.28084, 2),
      average_speed_mph = round(average_speed * 2.23694, 2),
      max_speed_mph     = round(max_speed * 2.23694, 2),
      moving_time_min   = round(moving_time / 60, 1),
      elapsed_time_min  = round(elapsed_time / 60, 1),
      start_date        = as_datetime(start_date),
      date              = as_date(start_date),
      month             = floor_date(date, "month"),
      year              = year(date),
      sport_type        = dplyr::coalesce(sport_type, type)
    )
}

# ── Decode polylines: strava encodes polylines as GoogleEncodedPolylines
# This section decodes those lines into latitude/ longitude coordiates for the map tab
# ────────────────────────────────────────────────────────────
decode_routes <- function(df) {
  has_poly <- df %>%
    filter(!is.na(map.summary_polyline), nchar(map.summary_polyline) > 0)

  paths <- lapply(seq_len(nrow(has_poly)), function(i) {
    tryCatch({
      pts <- googlePolylines::decode(has_poly$map.summary_polyline[[i]])[[1]]
      if (!is.null(pts) && nrow(pts) > 0) {
        pts$activity_id       <- has_poly$id[i]
        pts$sport_type        <- has_poly$sport_type[i]
        pts$date              <- has_poly$date[i]
        pts$name              <- has_poly$name[i]
        pts$distance_miles    <- has_poly$distance_miles[i]
        pts$elevation_gain_ft <- has_poly$elevation_gain_ft[i]
        pts$moving_time_min   <- has_poly$moving_time_min[i]
        pts
      }
    }, error = function(e) NULL)
  })

  do.call(rbind, Filter(Negate(is.null), paths))
}

# ── Cached data loader: This section fetches the data and caches it so it doesn't overload the API  ─────────────────────────────────────────────────────────
load_data <- function(token, force = FALSE) {
  if (!force && file.exists(CACHE_FILE)) {
    cache <- readRDS(CACHE_FILE)
    age_h <- as.numeric(difftime(Sys.time(), cache$fetched_at, units = "hours"))
    if (age_h < CACHE_HOURS) {
      message("Using cached data (", round(age_h, 1), "h old)")
      return(cache)
    }
  }
  message("Fetching activities from Strava API...")
  acts  <- rStrava::get_activity_list(stoken = token)
  df    <- process_activities(acts)
  paths <- decode_routes(df)
  cache <- list(activities = df, routes = paths, fetched_at = Sys.time())
  saveRDS(cache, CACHE_FILE)
  message("Cached ", nrow(df), " activities, ", nrow(paths), " route points.")
  cache
}

# ── Maintenance helpers  ────────────────────────────────────────────────────────

# Function that calculates distance since last maintenance date
calc_miles_since <- function(activities, since_date, bike_types) {
  if (is.na(since_date) || is.null(since_date)) return(NA_real_)
  since <- suppressWarnings(as.Date(since_date))
  if (is.na(since)) return(NA_real_)
  activities %>%
    filter(sport_type %in% bike_types, !is.na(date), date >= since) %>%
    summarise(m = sum(distance_miles, na.rm = TRUE)) %>%
    pull(m)
}

make_progress_bar <- function(pct) {
  if (is.na(pct)) return("—")
  color       <- if (pct >= 100) "danger" else if (pct >= 80) "warning" else "success"
  display_pct <- min(round(pct), 100)
  sprintf(
    '<div class="progress" style="height:18px;min-width:100px">
       <div class="progress-bar bg-%s" role="progressbar"
            style="width:%d%%;min-width:2.5em">%d%%</div>
     </div>',
    color, display_pct, round(pct)
  )
}

make_status_badge <- function(pct, last_replaced) {
  if (is.na(last_replaced) || is.null(last_replaced)) {
    return('<span class="badge bg-secondary">Not Set</span>')
  }
  if (is.na(pct)) {
    return('<span class="badge bg-info text-dark">Manual</span>')
  }
  if (pct >= 100) return('<span class="badge bg-danger">Overdue</span>')
  if (pct >= 80)  return('<span class="badge bg-warning text-dark">Due Soon</span>')
  return('<span class="badge bg-success">Good</span>')
}

build_maint_table_df <- function(maint_df, acts_data, bike_types) {
  maint_df %>%
    rowwise() %>%
    mutate(
      miles_since = calc_miles_since(acts_data, last_replaced, bike_types),
      pct_life    = if (!is.na(recommended_mi) && !is.na(miles_since))
                      round(miles_since / recommended_mi * 100, 0)
                    else NA_real_
    ) %>%
    ungroup() %>%
    mutate(
      progress_bar      = sapply(pct_life, make_progress_bar),
      status            = mapply(make_status_badge, pct_life, last_replaced,
                                 SIMPLIFY = TRUE),
      last_replaced_fmt = ifelse(is.na(last_replaced), "—",
                                 format(as.Date(last_replaced), "%b %d, %Y")),
      miles_fmt         = ifelse(is.na(miles_since), "—",
                                 paste0(format(round(miles_since),
                                               big.mark = ","), " mi")),
      rec_fmt           = ifelse(is.na(recommended_mi), "—",
                                 paste0(format(as.integer(recommended_mi),
                                               big.mark = ","), " mi"))
    ) %>%
    select(component, last_replaced_fmt, miles_fmt, rec_fmt,
           progress_bar, status, notes)
}

# ── Load at startup ────────────────────────────────────────────────────────────
app_data   <- load_data(my_token)
activities <- app_data$activities
routes     <- app_data$routes
