server <- function(input, output, session) {

  # ── Reactive state ───────────────────────────────────────────────────────────
  data_rv <- reactiveVal(list(
    activities = activities,
    routes     = routes,
    fetched_at = app_data$fetched_at
  ))

  acts  <- reactive(data_rv()$activities)
  paths <- reactive(data_rv()$routes)

  # ── Filtered activities for Explore page ────────────────────────────────────
  acts_filtered <- reactive({
    req(input$explore_dates[1], input$explore_dates[2])
    a <- acts() %>%
      filter(!is.na(date),
             date >= input$explore_dates[1],
             date <= input$explore_dates[2])
    types <- input$explore_types
    if (!is.null(types) && length(types) > 0) {
      a <- a %>% filter(sport_type %in% types)
    }
    a
  })

  # ── Refresh Strava data ──────────────────────────────────────────────────────
  do_refresh <- function() {
    showNotification("Fetching data from Strava…", id = "fetching",
                     duration = NULL, type = "message")
    tryCatch({
      new_data <- load_data(my_token, force = TRUE)
      data_rv(new_data)
      removeNotification("fetching")
      showNotification("Data refreshed!", type = "message", duration = 3)
    }, error = function(e) {
      removeNotification("fetching")
      showNotification(paste("Error:", e$message), type = "error", duration = 8)
    })
  }

  observeEvent(input$refresh_data,  do_refresh())
  observeEvent(input$refresh_map,   do_refresh())
  observeEvent(input$refresh_data2, do_refresh())

  output$last_updated_text <- renderUI({
    t <- data_rv()$fetched_at
    if (!is.null(t)) {
      tags$p(class = "text-muted mt-2 mb-0", style = "font-size:.72rem",
             icon("clock"), " Updated ", format(t, "%b %d %I:%M %p"))
    }
  })

  # ── Map ──────────────────────────────────────────────────────────────────────
  filtered_paths <- reactive({
    req(input$map_dates[1], input$map_dates[2])
    paths() %>%
      filter(!is.na(date),
             date >= input$map_dates[1],
             date <= input$map_dates[2])
  })

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.5)) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = -117.75, lat = 33.85, zoom = 10)
  })

  observe({
    fp   <- filtered_paths()
    view <- input$map_view
    nav  <- input$main_nav

    req(nav == "Map")

    proxy <- leafletProxy("map")
    proxy %>% clearGroup("routes")
    tryCatch(proxy %>% removeHeatmap("heat"), error = function(e) NULL)

    if (nrow(fp) == 0) return()

    if (view == "heatmap") {
      proxy %>%
        addHeatmap(
          data       = fp,
          lat        = ~lat,
          lng        = ~lon,
          layerId    = "heat",
          minOpacity = 0.4,
          radius     = 8,
          blur       = 12,
          gradient   = c("0" = "#fcd0b8", "0.5" = STRAVA_ORANGE, "1" = "#7a2500")
        )
    } else {
      opacity <- input$route_opacity
      ids <- unique(fp$activity_id)
      for (aid in ids) {
        seg  <- fp[fp$activity_id == aid, ]
        meta <- seg[1, ]

        tip <- htmltools::HTML(sprintf(
          "<b>%s</b><br>%s<br>%.1f mi &nbsp;&bull;&nbsp; %s ft gain<br>%d min moving",
          meta$name,
          format(meta$date, "%b %d, %Y"),
          meta$distance_miles,
          format(round(meta$elevation_gain_ft), big.mark = ","),
          round(meta$moving_time_min)
        ))

        proxy %>%
          addPolylines(
            lat              = seg$lat,
            lng              = seg$lon,
            color            = STRAVA_ORANGE,
            weight           = 2,
            opacity          = opacity,
            label            = tip,
            labelOptions     = labelOptions(
              style    = list("font-size" = "13px", "padding" = "6px 10px"),
              textsize = "13px",
              direction = "auto"
            ),
            highlightOptions = highlightOptions(
              color        = "#ffffff",
              weight       = 3,
              opacity      = 1,
              bringToFront = TRUE
            ),
            group = "routes"
          )
      }
    }
  })

  # ── Stat cards ───────────────────────────────────────────────────────────────
  output$stat_cards <- renderUI({
    a <- acts_filtered()

    make_card <- function(icon_name, value, label) {
      div(
        class = "col-6 col-sm-4 col-lg",
        div(
          class = "card stat-card text-center h-100",
          div(
            class = "card-body",
            div(class = "stat-icon",
                tags$i(class = paste0("fas fa-", icon_name),
                       style = paste0("color:", STRAVA_ORANGE))),
            div(class = "stat-value", value),
            div(class = "stat-label", label)
          )
        )
      )
    }

    div(
      class = "row g-3",
      make_card("person-running",
                format(nrow(a), big.mark = ","),
                "Activities"),
      make_card("road",
                paste0(format(round(sum(a$distance_miles, na.rm = TRUE)),
                              big.mark = ","), " mi"),
                "Total Distance"),
      make_card("mountain",
                paste0(format(round(sum(a$elevation_gain_ft, na.rm = TRUE) / 1000, 1),
                              big.mark = ","), "k ft"),
                "Elevation Gained"),
      make_card("clock",
                paste0(format(round(sum(a$moving_time_min, na.rm = TRUE) / 60),
                              big.mark = ","), " hrs"),
                "Moving Time"),
      make_card("thumbs-up",
                format(sum(a$kudos_count, na.rm = TRUE), big.mark = ","),
                "Kudos Received")
    )
  })

  # ── Activity Calendar Heatmap ────────────────────────────────────────────────
  output$heatmap_plot <- renderPlotly({
    sel_year <- as.integer(req(input$heatmap_year))

    yr_start <- as.Date(paste0(sel_year, "-01-01"))
    yr_end   <- as.Date(paste0(sel_year, "-12-31"))
    all_days <- data.frame(date = seq(yr_start, yr_end, by = "day"))

    # Respect sport type filter but show the full selected year
    a <- acts()
    types <- input$explore_types
    if (!is.null(types) && length(types) > 0) {
      a <- a %>% filter(sport_type %in% types)
    }

    daily <- a %>%
      filter(!is.na(date), year == sel_year) %>%
      group_by(date) %>%
      summarise(
        miles = sum(distance_miles, na.rm = TRUE),
        count = n(),
        .groups = "drop"
      )

    cal <- all_days %>%
      left_join(daily, by = "date") %>%
      replace_na(list(miles = 0, count = 0)) %>%
      mutate(
        week_num = as.integer(format(date, "%U")),
        dow      = wday(date) - 1L,
        tip      = ifelse(
          count > 0,
          paste0(format(date, "%b %d, %Y"), "<br>",
                 round(miles, 1), " mi · ", count,
                 ifelse(count == 1, " activity", " activities")),
          paste0(format(date, "%b %d, %Y"), "<br>No activity")
        )
      )

    max_miles <- max(cal$miles)
    if (max_miles == 0) max_miles <- 1

    # Gray at 0; sharp transition to orange above 0 (normalized threshold ~2%)
    cs <- list(
      list(0,    "#e2e2e2"),
      list(0.02, "#fcd0b8"),
      list(0.4,  STRAVA_ORANGE),
      list(1,    "#7a2500")
    )

    month_starts <- seq(yr_start, as.Date(paste0(sel_year, "-12-01")), by = "month")
    month_ticks  <- as.integer(format(month_starts, "%U"))
    month_labels <- format(month_starts, "%b")

    plot_ly(
      cal,
      x             = ~week_num,
      y             = ~dow,
      z             = ~miles,
      text          = ~tip,
      type          = "heatmap",
      xgap          = 3,
      ygap          = 3,
      hovertemplate = "%{text}<extra></extra>",
      colorscale    = cs,
      zauto         = FALSE,
      zmin          = 0,
      zmax          = max_miles,
      showscale     = TRUE,
      colorbar      = list(title = list(text = "Miles"), len = 0.75, thickness = 10,
                           tickfont = list(size = 10))
    ) %>%
      layout(
        xaxis = list(
          title    = "",
          tickvals = month_ticks,
          ticktext = month_labels,
          tickfont = list(size = 11),
          showgrid = FALSE
        ),
        yaxis = list(
          title     = "",
          tickvals  = c(0, 2, 4, 6),
          ticktext  = c("Sun", "Tue", "Thu", "Sat"),
          autorange = "reversed",
          tickfont  = list(size = 10),
          showgrid  = FALSE
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        margin        = list(t = 5, b = 5, l = 36, r = 10)
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  # ── Monthly distance bar chart ───────────────────────────────────────────────
  output$monthly_plot <- renderPlotly({
    a <- acts_filtered() %>%
      filter(!is.na(month)) %>%
      group_by(month, sport_type) %>%
      summarise(miles = sum(distance_miles, na.rm = TRUE), .groups = "drop")

    plot_ly(a, x = ~month, y = ~miles, color = ~sport_type,
            type = "bar", colors = "Set2") %>%
      layout(
        barmode = "stack",
        xaxis   = list(title = "", tickformat = "%b '%y"),
        yaxis   = list(title = "Miles"),
        legend  = list(orientation = "h", y = -0.3, x = 0,
                       font = list(size = 11)),
        margin  = list(t = 5, b = 5),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  # ── Distance vs elevation scatter ─────────────────────────────────────────────
  output$dist_elev_plot <- renderPlotly({
    a <- acts_filtered() %>%
      filter(!is.na(distance_miles), !is.na(elevation_gain_ft),
             distance_miles > 0.1)

    plot_ly(a,
            x = ~distance_miles, y = ~elevation_gain_ft,
            color = ~sport_type,
            text  = ~paste0("<b>", name, "</b><br>",
                            round(distance_miles, 1), " mi | ",
                            round(elevation_gain_ft), " ft gain"),
            hoverinfo = "text",
            type = "scatter", mode = "markers",
            marker = list(size = 6, opacity = 0.7),
            colors = "Set2") %>%
      layout(
        xaxis  = list(title = "Distance (mi)"),
        yaxis  = list(title = "Elevation Gain (ft)"),
        legend = list(orientation = "h", y = -0.3, font = list(size = 11)),
        margin = list(t = 5),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  # ── Activity mix donut ───────────────────────────────────────────────────────
  output$sport_pie_plot <- renderPlotly({
    a <- acts_filtered() %>%
      filter(!is.na(sport_type)) %>%
      group_by(sport_type) %>%
      summarise(miles = sum(distance_miles, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(miles))

    plot_ly(a, labels = ~sport_type, values = ~miles,
            type = "pie", hole = 0.45,
            textinfo = "label+percent",
            hovertemplate = "%{label}<br>%{value:.0f} mi (%{percent})<extra></extra>") %>%
      layout(
        margin        = list(t = 5, b = 5, l = 10, r = 10),
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      plotly::hide_legend() %>%
      plotly::config(displayModeBar = FALSE)
  })

  # ── Year-over-year cumulative miles ──────────────────────────────────────────
  output$yoy_plot <- renderPlotly({
    a <- acts_filtered() %>%
      filter(!is.na(year), !is.na(date)) %>%
      mutate(month_num = lubridate::month(date)) %>%
      group_by(year, month_num) %>%
      summarise(miles = sum(distance_miles, na.rm = TRUE), .groups = "drop") %>%
      arrange(year, month_num) %>%
      group_by(year) %>%
      mutate(cum_miles = cumsum(miles)) %>%
      ungroup()

    years <- sort(unique(a$year))
    pal   <- colorRampPalette(c("#fcbf8a", STRAVA_ORANGE, "#8b1a00"))(max(length(years), 1))

    p <- plot_ly()
    for (i in seq_along(years)) {
      yr <- filter(a, year == years[i])
      p  <- add_trace(p, data = yr,
                      x = ~month_num, y = ~cum_miles,
                      name = as.character(years[i]),
                      type = "scatter", mode = "lines+markers",
                      line   = list(color = pal[i], width = 2.5),
                      marker = list(color = pal[i], size = 5))
    }
    p %>%
      layout(
        xaxis  = list(title = "", tickvals = 1:12, ticktext = month.abb,
                      tickfont = list(size = 11)),
        yaxis  = list(title = "Cumulative Miles"),
        legend = list(orientation = "h", y = -0.3, font = list(size = 11)),
        margin = list(t = 5),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  # ── Maintenance tables ────────────────────────────────────────────────────────
  render_maint_dt <- function(df) {
    DT::datatable(
      df,
      escape    = FALSE,
      selection = "none",
      rownames  = FALSE,
      colnames  = c("Component", "Last Serviced", "Miles Since",
                    "Recommended", "Life Used", "Status", "Notes"),
      options = list(
        pageLength = 20,
        dom        = "t",
        ordering   = FALSE,
        columnDefs = list(
          list(className = "dt-center", targets = 1:5),
          list(width = "160px", targets = 4)
        )
      ),
      class = "table table-hover table-sm"
    )
  }

  output$maint_mtb <- DT::renderDT({
    render_maint_dt(build_maint_table_df(MAINTENANCE$mtb, acts(), MTB_TYPES))
  }, server = FALSE)

  output$maint_road <- DT::renderDT({
    render_maint_dt(build_maint_table_df(MAINTENANCE$road, acts(), ROAD_TYPES))
  }, server = FALSE)
}
