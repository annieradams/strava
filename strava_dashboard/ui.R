.all_types <- sort(unique(activities$sport_type[!is.na(activities$sport_type)]))
.all_years <- sort(unique(activities$year[!is.na(activities$year)]), decreasing = TRUE)

ui <- page_navbar(
  id    = "main_nav",
  title = tags$span(
    style = "font-weight:800; letter-spacing:-0.5px; font-size:1.1rem;",
    tags$span(style = paste0("color:", STRAVA_ORANGE), "STRAVA"),
    " Dashboard"
  ),
  window_title = "Strava Dashboard",
  theme = bs_theme(
    version      = 5,
    primary      = STRAVA_ORANGE,
    bg           = "#ffffff",
    fg           = "#222222",
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter", wght = 700)
  ),

  header = tags$head(tags$style(HTML("
    body, .bslib-page-fill { background-color: #f4f5f7 !important; }
    .card { border-radius: 10px !important; border: 1px solid rgba(0,0,0,.07) !important; box-shadow: 0 1px 8px rgba(0,0,0,.06) !important; }
    .stat-card { border: none !important; border-radius: 12px !important; border-top: 3px solid #FC4C02 !important; box-shadow: 0 3px 14px rgba(252,76,2,.12) !important; }
    .stat-card .card-body { padding: 1.25rem; }
    .stat-icon { font-size: 1.5rem; margin-bottom: .35rem; }
    .stat-value { font-size: 1.65rem; font-weight: 700; line-height: 1.1; }
    .stat-label { font-size: .72rem; color: #aaa; margin-top: .25rem; letter-spacing: .06em; text-transform: uppercase; }
    .navbar { border-bottom: 3px solid #FC4C02; }
    .card-header { font-weight: 700 !important; font-size: .78rem !important; letter-spacing: .07em; text-transform: uppercase; color: #666 !important; background: rgba(252,76,2,.04) !important; border-bottom: 1px solid rgba(252,76,2,.14) !important; padding: .65rem 1rem !important; }
    .sidebar { background: #fff !important; border-right: 1px solid rgba(0,0,0,.08) !important; }
    .leaflet-tooltip { font-family: 'Inter', sans-serif; font-size: 13px; }
    .selectize-input { font-size: .85rem; border-radius: 6px !important; }
    .btn { border-radius: 6px !important; }
    .form-label, b.small { font-size: .78rem; letter-spacing: .04em; text-transform: uppercase; color: #777; }
    hr.my-2 { border-color: rgba(0,0,0,.07); }
  "))),

  # ── EXPLORE ──────────────────────────────────────────────────────────────────
  nav_panel(
    "Explore", icon = icon("chart-line"),

    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 240,
        bg    = "#f8f9fa",
        open  = TRUE,

        tags$b("Date Range", class = "small"),
        dateRangeInput(
          "explore_dates", NULL,
          start = min(activities$date, na.rm = TRUE),
          end   = Sys.Date()
        ),

        hr(class = "my-2"),

        tags$b("Activity Type", class = "small"),
        selectizeInput(
          "explore_types", NULL,
          choices   = .all_types,
          selected  = NULL,
          multiple  = TRUE,
          options   = list(placeholder = "All types")
        ),

        hr(class = "my-2"),

        tags$b("Heatmap Year", class = "small"),
        selectInput(
          "heatmap_year", NULL,
          choices  = .all_years,
          selected = .all_years[1]
        ),

        hr(class = "my-2"),

        actionButton(
          "refresh_data", "Refresh Strava Data",
          icon  = icon("rotate"),
          class = "btn-sm w-100",
          style = paste0("background:", STRAVA_ORANGE,
                         "; border-color:", STRAVA_ORANGE, "; color:#fff")
        ),
        uiOutput("last_updated_text")
      ),

      div(
        class = "container-fluid py-3",

        uiOutput("stat_cards"),

        card(
          class       = "mt-3",
          full_screen = TRUE,
          card_header("Activity Calendar"),
          card_body(plotlyOutput("heatmap_plot", height = "195px"))
        ),

        layout_columns(
          col_widths = c(6, 6),
          class      = "mt-3",
          card(
            full_screen = TRUE,
            card_header("Monthly Distance by Type"),
            card_body(plotlyOutput("monthly_plot", height = "300px"))
          ),
          card(
            full_screen = TRUE,
            card_header("Distance vs Elevation Gain"),
            card_body(plotlyOutput("dist_elev_plot", height = "300px"))
          )
        ),

        layout_columns(
          col_widths = c(7, 5),
          class      = "mt-3",
          card(
            full_screen = TRUE,
            card_header("Cumulative Miles: Year over Year"),
            card_body(plotlyOutput("yoy_plot", height = "320px"))
          ),
          card(
            full_screen = TRUE,
            card_header("Activity Mix (by miles)"),
            card_body(plotlyOutput("sport_pie_plot", height = "320px"))
          )
        )
      )
    )
  ),

  # ── MAP ──────────────────────────────────────────────────────────────────────
  nav_panel(
    "Map", icon = icon("map"),

    layout_sidebar(
      sidebar = sidebar(
        title = "Map Options",
        width = 250,
        bg    = "#f8f9fa",
        open  = TRUE,

        tags$b("Date Range", class = "small"),
        dateRangeInput(
          "map_dates", NULL,
          start = min(activities$date, na.rm = TRUE),
          end   = Sys.Date()
        ),

        hr(class = "my-2"),

        tags$b("View", class = "small"),
        radioButtons(
          "map_view", NULL,
          choices  = c("Routes" = "routes", "Heatmap" = "heatmap"),
          selected = "routes"
        ),

        conditionalPanel(
          "input.map_view == 'routes'",
          sliderInput("route_opacity", "Route Opacity",
                      min = 0.05, max = 0.9, value = 0.25, step = 0.05)
        ),

        hr(class = "my-2"),

        actionButton(
          "refresh_map", "Refresh Strava Data",
          icon  = icon("rotate"),
          class = "btn-sm w-100",
          style = paste0("background:", STRAVA_ORANGE,
                         "; border-color:", STRAVA_ORANGE, "; color:#fff")
        )
      ),

      leafletOutput("map", height = "calc(100vh - 62px)")
    )
  ),

  # ── MAINTENANCE ──────────────────────────────────────────────────────────────
  nav_panel(
    "Maintenance", icon = icon("wrench"),

    div(
      class = "container-fluid py-3",

      div(
        class = "d-flex justify-content-between align-items-start mb-3 flex-wrap gap-2",
        div(
          h4("Bike Maintenance Log", class = "mb-1"),
          p(class = "text-muted small mb-0",
            icon("circle-info"),
            " To log service, edit the ",
            tags$code("MAINTENANCE"),
            " dates in ",
            tags$code("global.R"),
            " and restart the app. Miles are counted from that date forward by bike type.")
        ),
        actionButton(
          "refresh_data2", "Refresh Activity Data",
          icon  = icon("rotate"),
          class = "btn-sm",
          style = paste0("background:", STRAVA_ORANGE,
                         "; border-color:", STRAVA_ORANGE, "; color:#fff")
        )
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header(
            tags$span(
              tags$i(class = "fas fa-mountain",
                     style = paste0("color:", STRAVA_ORANGE)),
              " Mountain Bike"
            )
          ),
          card_body(
            tags$p(class = "text-muted small mb-2",
                   "Tracks: MountainBikeRide activities"),
            DTOutput("maint_mtb")
          )
        ),

        card(
          card_header(
            tags$span(
              tags$i(class = "fas fa-bicycle",
                     style = paste0("color:", STRAVA_ORANGE)),
              " Road Bike"
            )
          ),
          card_body(
            tags$p(class = "text-muted small mb-2",
                   "Tracks: Ride / VirtualRide / GravelRide activities"),
            DTOutput("maint_road")
          )
        )
      )
    )
  )
)
