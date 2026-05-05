library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(lubridate)
library(viridis)
library(sf)
library(tigris)
library(leaflet)
library(htmltools)
library(shinyjs)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# =============================================================================
# DATA LOADING & PREPROCESSING
# =============================================================================

scoped_data <- read_csv("FemaWebDisasterDeclarations.csv") |>
  mutate(
    declarationDate = as_date(declarationDate),
    year            = year(declarationDate)
  ) |>
  filter(year >= 2000, year <= 2024)

sheldus_raw <- tryCatch(readRDS("sheldus_clean.rds"), error = function(e) NULL)

state_mapping <- scoped_data %>%
  distinct(stateCode, stateName) %>%
  mutate(stateName = trimws(stateName))

sheldus_summary <- if (!is.null(sheldus_raw)) {
  sheldus_raw %>%
    group_by(stateCode) %>%
    summarise(
      total_prop_dmg = sum(property_loss, na.rm = TRUE),
      total_fatalities = sum(fatalities, na.rm = TRUE),
      total_injuries = sum(injuries, na.rm = TRUE),
      .groups = "drop"
    )
} else {
  NULL
}

fema_summary_base <- scoped_data %>%
  group_by(stateCode, stateName) %>%
  summarise(total_disasters = n(), years_active = n_distinct(year), .groups = "drop") %>%
  mutate(stateName = trimws(stateName))

state_summary <- fema_summary_base %>%
  left_join(sheldus_summary %||% tibble(stateCode = character()), by = "stateCode") %>%
  rename(state_abbr = stateCode, state_name = stateName) %>%
  mutate(
    total_prop_dmg   = replace_na(total_prop_dmg, 0),
    total_fatalities = replace_na(total_fatalities, 0),
    total_injuries   = replace_na(total_injuries, 0),
    avg_disasters_yr = round(total_disasters / years_active, 1),
    dmg_billions     = round(total_prop_dmg / 1e9, 2),
    total_fema_asst  = total_prop_dmg * 0.15
  )

state_hazard <- scoped_data %>%
  group_by(stateCode, incidentType) %>%
  summarise(count = n(), .groups = "drop") %>%
  rename(state_abbr = stateCode, hazard_type = incidentType)

state_year <- scoped_data %>%
  group_by(stateCode, year) %>%
  summarise(disasters = n(), .groups = "drop") %>%
  rename(state_abbr = stateCode)

national_hazard <- scoped_data %>%
  group_by(incidentType) %>%
  summarise(count = n(), .groups = "drop") %>%
  rename(hazard_type = incidentType) %>%
  arrange(desc(count))

top_disaster_states <- state_summary %>%
  arrange(desc(total_disasters)) %>%
  slice_head(n = 5)
top_damage_states <- state_summary %>%
  arrange(desc(total_prop_dmg)) %>%
  slice_head(n = 5)

state_centroids <- data.frame(
  state_abbr = c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
    "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
    "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
    "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
    "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
  ),
  lat = c(
    32.8, 64.2, 34.3, 34.8, 36.8, 39.0, 41.6, 39.0, 27.8, 32.2,
    20.8, 44.4, 40.0, 39.8, 42.0, 38.5, 37.5, 31.2, 45.4, 39.0,
    42.3, 44.3, 46.4, 32.7, 38.5, 47.0, 41.5, 38.5, 43.5, 40.1,
    34.3, 43.0, 35.5, 47.5, 40.4, 35.6, 44.1, 40.9, 41.7, 33.8,
    44.4, 35.9, 31.1, 39.3, 44.3, 37.8, 47.4, 38.5, 44.4, 43.0
  ),
  lng = c(
    -86.8, -153.4, -111.1, -92.2, -119.4, -105.5, -72.7, -75.5, -81.5, -83.4,
    -157.0, -114.5, -89.2, -86.3, -93.5, -98.4, -85.0, -91.8, -69.2, -76.6,
    -71.8, -85.5, -94.3, -89.7, -92.5, -109.6, -99.9, -117.1, -71.6, -74.4,
    -106.0, -75.5, -79.4, -100.5, -82.8, -96.9, -120.6, -77.2, -71.5, -80.9,
    -100.3, -86.1, -98.4, -111.1, -72.7, -78.2, -120.5, -80.6, -90.2, -107.3
  ),
  stringsAsFactors = FALSE
)

map_data_p3 <- state_summary %>% left_join(state_centroids, by = "state_abbr")

p3_state_year <- scoped_data %>%
  count(stateCode, year, name = "total_disasters")

p3_state_hazard_year <- scoped_data %>%
  count(stateCode, year, incidentType, name = "n")

# V1 / V2 structures
fema_sy <- scoped_data |>
  group_by(stateCode, year) |>
  summarise(n_declarations = n(), .groups = "drop")

full_panel <- expand.grid(
  stateCode = unique(fema_sy$stateCode),
  year      = unique(fema_sy$year)
) |>
  left_join(fema_sy, by = c("stateCode", "year")) |>
  mutate(n_declarations = replace_na(n_declarations, 0))

incident_choices <- c("All", sort(unique(scoped_data$incidentType)))
decl_choices <- c("All", sort(unique(scoped_data$declarationType)))
metric_choices <- c(
  "Total Declarations"    = "n_declarations",
  "Unique Incident Types" = "n_incident_types"
)
p3_metric_choices <- c(
  "Total Disasters" = "total_disasters",
  "Property Damage" = "dmg_billions",
  "FEMA Assistance" = "total_fema_asst",
  "Fatalities" = "total_fatalities"
)
mako_colors <- viridisLite::mako(256)
mako_plotly_scale <- lapply(
  seq_along(mako_colors),
  function(i) list((i - 1) / (length(mako_colors) - 1), mako_colors[[i]])
)
mako_line <- "#3488A6"
mako_line_dark <- "#35264C"
mako_accent <- "#45BDAD"
mako_highlight <- "#B4E4C3"

# Spatial data for V2
us_states_sf <- tryCatch(
  {
    tigris::states(cb = TRUE, resolution = "20m", year = 2022, progress_bar = FALSE) |>
      st_transform(crs = 4326) |>
      filter(!STUSPS %in% c("AS", "GU", "MP", "PR", "VI", "UM")) |>
      select(STUSPS, NAME, geometry)
  },
  error = function(e) NULL
)

dist_matrix_km <- tryCatch(
  {
    if (is.null(us_states_sf)) stop("no sf")
    cents <- suppressWarnings(st_centroid(us_states_sf))
    dm <- st_distance(cents)
    matrix(as.numeric(dm) / 1000,
      nrow     = nrow(cents),
      dimnames = list(cents$STUSPS, cents$STUSPS)
    )
  },
  error = function(e) NULL
)

adj_global <- tryCatch(
  {
    if (!is.null(us_states_sf)) st_touches(us_states_sf) else NULL
  },
  error = function(e) NULL
)

# =============================================================================
# UI
# =============================================================================

ui <- page_fluid(
  useShinyjs(),
  title = "FEMA Disaster Declarations (2000–2024)",
  theme = bs_theme(
    bootswatch   = "flatly",
    base_font    = font_google("IBM Plex Sans"),
    heading_font = font_google("IBM Plex Sans")
  ),
  tags$head(tags$style(HTML("
    body { background-color: #f7f9fb; color: #1b2430; }
    .nav-tabs { border-bottom: 1px solid #d8e2ec; }
    .nav-tabs .nav-link { color: #485464; border-radius: 7px 7px 0 0; }
    .nav-tabs .nav-link.active {
      color: #0b0405; background: #ffffff; border-color: #d8e2ec #d8e2ec #ffffff;
      box-shadow: inset 0 3px 0 #3488A6;
    }
    .card {
      border: 1px solid #d8e2ec; border-radius: 8px;
      box-shadow: 0 8px 24px rgba(30, 44, 64, 0.06); background: #ffffff;
    }
    .card-header { font-weight: 650; font-size: 0.95rem; background: #ffffff;
                   border-bottom: 1px solid #e6edf3; color: #1b2430; }
    .subtitle-text { color: #64748b; font-size: 0.85rem; padding: 4px 12px 8px 12px; }
    .sel-banner {
      background: #eefaf7; border: 1.5px solid #8AD9B1; border-radius: 6px;
      padding: 10px 18px; margin-bottom: 14px;
      font-size: 0.9rem; color: #35264C; font-weight: 500;
    }
    .no-sel-hint {
      color: #7b8794; font-size: 0.9rem; text-align: center; padding: 28px 12px;
      background: #f4f8fb; border-radius: 6px; margin-bottom: 14px;
      border: 1px dashed #cbd8e3;
    }
    .analysis-text {
      font-size: 0.88rem; color: #253042; line-height: 1.7; margin: 0;
    }
    .js-plotly-plot .plotly .cursor-crosshair { cursor: pointer !important; }

    /* PAGE 3 */
    #page3-container {
      position: relative; width: 100%;
      height: calc(100vh - 120px); overflow: hidden;
    }
    #disaster_map { width: 100%; height: 100%; }
    #map-controls {
      position: absolute; top: 16px; left: 50%; transform: translateX(-50%);
      z-index: 1000; display: flex; gap: 10px; align-items: center;
      background: rgba(255,255,255,0.96); border: 1px solid #d8e2ec; border-radius: 8px;
      padding: 8px 18px; box-shadow: 0 8px 24px rgba(30,44,64,0.12);
    }
    #state-panel {
      position: absolute; top: 80px; right: 20px; width: 360px;
      max-height: calc(100vh - 200px); overflow-y: auto;
      background: rgba(255,255,255,0.98); border: 1px solid #d8e2ec; border-radius: 8px;
      box-shadow: 0 18px 42px rgba(30,44,64,0.18); z-index: 1000;
      padding: 20px; transition: all 0.3s ease; display: none;
    }
    #state-panel.visible { display: block; }
    .metric-row { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-bottom: 16px; }
    .metric-card { background: #f4f8fb; border-radius: 8px; padding: 12px; text-align: center; border-left: 3px solid #3488A6; }
    .metric-card.red    { border-left-color: #35264C; }
    .metric-card.green  { border-left-color: #45BDAD; }
    .metric-card.orange { border-left-color: #8AD9B1; }
    .metric-value { font-size: 20px; font-weight: 750; color: #0b0405; display: block; }
    .metric-label { font-size: 10px; color: #64748b; text-transform: uppercase; letter-spacing: 0.5px; }
    .trend-box { background: #edf8f7; border: 1px solid #c7ebe2; border-radius: 8px; padding: 12px; margin-bottom: 16px; font-size: 13px; }
    #map-legend { position: absolute; bottom: 30px; left: 20px; z-index: 1000; background: rgba(255,255,255,0.94); border: 1px solid #d8e2ec; border-radius: 8px; padding: 12px; box-shadow: 0 8px 24px rgba(30,44,64,0.12); font-size: 12px; }
    #map-instruction { position: absolute; bottom: 30px; right: 20px; z-index: 999; background: rgba(255,255,255,0.94); border: 1px solid #d8e2ec; border-radius: 8px; padding: 14px; box-shadow: 0 8px 24px rgba(30,44,64,0.12); font-size: 13px; max-width: 200px; text-align: center; }

    /* ABOUT */
    .about-container { max-width: 1100px; margin: 20px auto; padding: 0 20px 40px; }
    .about-hero { background: linear-gradient(135deg, #0B0405, #35264C 52%, #3488A6); color: white; border-radius: 8px; padding: 30px; margin-bottom: 25px; }
    .about-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 20px; }
    .about-card { background: #fff; border-radius: 8px; padding: 20px; box-shadow: 0 8px 24px rgba(30,44,64,0.08); border-top: 4px solid #3488A6; }
    .stat-badge { display: inline-block; background: #edf8f7; color: #35264C; border-radius: 6px; padding: 2px 8px; font-size: 11px; font-weight: 600; margin: 0 4px 4px 0; }
  "))),
  navset_tab(
    # ------------------------------------------------------------------
    # PAGE 1 — OVERVIEW
    # ------------------------------------------------------------------
    nav_panel(
      "V1 — Overview",
      card(
        class = "mb-3",
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            selectInput("incident_type", "Incident Type", choices = incident_choices, selected = "All"),
            selectInput("decl_type", "Declaration Type", choices = decl_choices, selected = "All"),
            selectInput("metric", "Map Metric", choices = metric_choices, selected = "n_declarations")
          ),
          sliderInput("year_range", "Year Range",
            min = 2000, max = 2024, value = c(2000, 2024), sep = "", width = "100%"
          )
        )
      ),
      # Metric explanation card (restored from appv2)
      card(
        class = "mb-3",
        card_body(
          padding = "12px",
          tags$p(
            style = "margin: 0; font-size: 0.9rem; color: #253042;",
            tags$strong("Metric 1: "),
            "Total federal disaster declarations per U.S. state or territory (2000–2024). ",
            "A declaration means a disaster exceeded local response capacity, triggering federal aid."
          ),
          tags$p(
            style = "margin: 8px 0 0 0; font-size: 0.9rem; color: #253042;",
            tags$strong("Metric 2: "),
            "Annual mean declarations per state with 95% confidence intervals. Tracks whether ",
            "the average disaster burden on U.S. states is growing over time."
          )
        )
      ),
      layout_columns(
        fill = FALSE, col_widths = c(3, 3, 3, 3),
        value_box(title = "Total Declarations", value = textOutput("vb_total"), theme = value_box_theme(bg = "#ffffff", fg = "#1b2430")),
        value_box(title = "Most Common Incident", value = textOutput("vb_incident"), theme = value_box_theme(bg = "#ffffff", fg = "#1b2430")),
        value_box(title = "States / Territories Affected", value = textOutput("vb_states"), theme = value_box_theme(bg = "#ffffff", fg = "#1b2430")),
        value_box(title = "Most Recent Declaration", value = textOutput("vb_recent"), theme = value_box_theme(bg = "#ffffff", fg = "#1b2430"))
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header(textOutput("map_title")),
          div(class = "subtitle-text", textOutput("map_subtitle")),
          plotlyOutput("choropleth", height = "420px")
        ),
        card(
          card_header("Mean Declarations Per State with 95% CI"),
          div(class = "subtitle-text", textOutput("trend_subtitle")),
          plotlyOutput("trend_plot", height = "420px")
        )
      ),
      tags$div(
        style = "text-align:center; color:#aaa; font-size:11px; padding: 12px 0 20px;",
        "Data: FEMA Web Disaster Declarations via OpenFEMA"
      )
    ), # end V1

    # ------------------------------------------------------------------
    # PAGE 2 — SPATIAL ANALYSIS
    # ------------------------------------------------------------------
    nav_panel(
      "V2 — Spatial Analysis",
      layout_sidebar(
        sidebar = sidebar(
          width = 270,
          title = "Spatial Controls",
          tags$p(
            style = "font-size:0.78rem; color:#666; margin-top:-4px;",
            "All controls apply to Page 2 only."
          ),
          hr(style = "margin:6px 0;"),
          selectInput("incident_type2", "Incident Type", choices = incident_choices, selected = "All"),
          selectInput("decl_type2", "Declaration Type", choices = decl_choices, selected = "All"),
          hr(style = "margin:6px 0;"),
          sliderInput("train_years2", "Year Range",
            min = 2000, max = 2024, value = c(2000, 2024), sep = "", width = "100%"
          ),
          hr(style = "margin:6px 0;"),
          radioButtons("map_mode2", "Map Display",
            choices  = c("Historical Totals" = "historical", "Forecast (lm trend)" = "forecast"),
            selected = "historical"
          ),
          conditionalPanel(
            condition = "input.map_mode2 == 'forecast'",
            sliderInput("forecast_year2", "Forecast Year",
              min = 2025, max = 2035, value = 2028, step = 1, sep = "", width = "100%"
            )
          ),
          hr(style = "margin:6px 0;"),
          tags$p(
            style = "font-size:0.75rem; color:#999;",
            tags$strong("How to use: "),
            "Click any state on the map to reveal adjacency and distance analyses. ",
            "All charts and text update reactively."
          ),
          hr(style = "margin:6px 0;"),
          tags$p(
            style = "font-size:0.72rem; color:#bbb; line-height:1.5;",
            "Spatial methods: sf::st_area() · sf::st_centroid() · ",
            "sf::st_distance() · sf::st_touches() · lm() per-state trend"
          )
        ),

        # main panel
        card(
          class = "mb-3",
          card_header(textOutput("v2_map_title")),
          div(class = "subtitle-text", textOutput("v2_map_subtitle")),
          plotlyOutput("v2_main_map", height = "490px")
        ),
        uiOutput("v2_sel_banner"),
        layout_columns(
          fill = FALSE, col_widths = c(4, 4, 4),
          value_box(
            title = "Nearest Above-Average State (sf::st_distance)",
            value = textOutput("vb2_nearest"),
            theme = value_box_theme(bg = "#ffffff", fg = "#1b2430")
          ),
          value_box(
            title = "Neighbor Clustering Ratio (st_touches)",
            value = textOutput("vb2_cluster"),
            theme = value_box_theme(bg = "#ffffff", fg = "#1b2430")
          ),
          value_box(
            title = "Declaration Density (sf::st_area)",
            value = textOutput("vb2_density"),
            theme = value_box_theme(bg = "#ffffff", fg = "#1b2430")
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(textOutput("v2_plot1_title")),
            div(class = "subtitle-text", textOutput("v2_plot1_sub")),
            plotlyOutput("v2_trend_compare", height = "350px")
          ),
          card(
            card_header("Distance Decay: Declarations vs. Distance from Selected State"),
            div(class = "subtitle-text", textOutput("v2_plot2_sub")),
            plotlyOutput("v2_distance_decay", height = "350px")
          )
        ),
        # Restored text-analysis cards from appv2
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Adjacency Analysis  ·  sf::st_touches()"),
            card_body(tags$p(class = "analysis-text", textOutput("v2_text_adjacency")))
          ),
          card(
            card_header("Distance Analysis  ·  sf::st_distance()"),
            card_body(tags$p(class = "analysis-text", textOutput("v2_text_distance")))
          )
        ),
        tags$div(
          style = "text-align:center; color:#aaa; font-size:11px; padding:12px 0 20px;",
          "Spatial: sf::st_area() · sf::st_centroid() · sf::st_distance() · sf::st_touches() · ",
          "lm() per-state trend  |  Data: FEMA / OpenFEMA  |  Boundaries: tigris"
        )
      )
    ), # end V2

    # ------------------------------------------------------------------
    # PAGE 3 — INTERACTIVE LEAFLET MAP
    # ------------------------------------------------------------------
    nav_panel(
      "🗺️ Disaster Map",
      div(
        id = "page3-container",
        leafletOutput("disaster_map", width = "100%", height = "100%"),
        div(
          id = "map-controls",
          tags$label("Metric:"),
          selectInput("p3_map_metric", NULL,
            width = "160px",
            choices = p3_metric_choices
          ),
          tags$label("Years:"),
          sliderInput("p3_year_range", NULL,
            min = 2000, max = 2024,
            value = c(2000, 2024), step = 1, sep = "", width = "200px", ticks = FALSE
          )
        ),
        div(
          id = "state-panel",
          div(
            style = "display:flex; justify-content:space-between; border-bottom:2px solid #e6edf3; margin-bottom:12px; padding-bottom:8px;",
            div(
              h4(textOutput("p3_panel_state"), style = "margin:0;"),
              p("FEMA Disaster Detail", style = "font-size:11px; color:#64748b; margin:0;")
            ),
            tags$button("✕",
              style = "background:none; border:none; color:#64748b; cursor:pointer;",
              onclick = "document.getElementById('state-panel').classList.remove('visible');"
            )
          ),
          div(
            class = "metric-row",
            div(class = "metric-card", span(textOutput("p3_m_disasters"), class = "metric-value"), span("Disasters", class = "metric-label")),
            div(class = "metric-card red", span(textOutput("p3_m_damage"), class = "metric-value"), span("Damage ($B)", class = "metric-label")),
            div(class = "metric-card green", span(textOutput("p3_m_fema"), class = "metric-value"), span("FEMA Asst ($M)", class = "metric-label")),
            div(class = "metric-card orange", span(textOutput("p3_m_fatalities"), class = "metric-value"), span("Fatalities", class = "metric-label"))
          ),
          div(class = "trend-box", htmlOutput("p3_panel_text")),
          p("Hazard Distribution", style = "font-weight:600; font-size:12px; margin-top:10px;"),
          plotlyOutput("p3_chart_hazard", height = "180px"),
          p("Annual Disaster Counts", style = "font-weight:600; font-size:12px; margin-top:10px;"),
          plotlyOutput("p3_chart_ts", height = "150px")
        ),
        div(
          id = "map-legend",
          div(style = "font-weight:700; margin-bottom:5px;", "Mako Metric Scale"),
          div(style = "color:#35264C;", "● Low"), div(style = "color:#8AD9B1;", "● High")
        ),
        div(id = "map-instruction", "👆 Click a marker to explore state details")
      )
    ), # end Page 3

    # ------------------------------------------------------------------
    # PAGE 4 — ABOUT
    # ------------------------------------------------------------------
    nav_panel(
      "ℹ️ About",
      div(
        class = "about-container",
        div(
          class = "about-hero",
          h2("🌪️ FEMA Disaster Dashboard"),
          p("An interactive exploration of federal disaster declarations (2000–2024).")
        ),
        div(
          class = "about-grid",
          div(
            class = "about-card", h5("📊 Dataset Summary"),
            span(class = "stat-badge", paste(nrow(scoped_data), "declarations")),
            span(class = "stat-badge", "50 states"), br(),
            p("Combined FEMA and SHELDUS records provide a unique view into disaster counts and economic impacts.")
          ),
          div(class = "about-card", h5("🏆 Top States (Disasters)"), tableOutput("about_top_disasters")),
          div(class = "about-card", h5("💸 Top States (Damage)"), tableOutput("about_top_damage")),
          div(class = "about-card", h5("🌊 Primary Hazard Types"), plotlyOutput("about_hazard_pie", height = "200px"))
        ),
        hr(),
        h4("National Trend (2000–2024)"),
        plotlyOutput("about_nat_trend", height = "300px")
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  # ---- V1 filter cross-update observers ------------------------------------

  observe({
    d <- scoped_data
    if (input$incident_type != "All") d <- filter(d, incidentType == input$incident_type)
    choices <- c("All", sort(unique(d$declarationType)))
    updateSelectInput(session, "decl_type",
      choices  = choices,
      selected = if (input$decl_type %in% choices) input$decl_type else "All"
    )
  }) |> bindEvent(input$incident_type)

  observe({
    d <- scoped_data
    if (input$decl_type != "All") d <- filter(d, declarationType == input$decl_type)
    choices <- c("All", sort(unique(d$incidentType)))
    updateSelectInput(session, "incident_type",
      choices  = choices,
      selected = if (input$incident_type %in% choices) input$incident_type else "All"
    )
  }) |> bindEvent(input$decl_type)

  # ---- V1 reactives --------------------------------------------------------

  filtered <- reactive({
    d <- scoped_data |> filter(year >= input$year_range[1], year <= input$year_range[2])
    if (input$incident_type != "All") d <- filter(d, incidentType == input$incident_type)
    if (input$decl_type != "All") d <- filter(d, declarationType == input$decl_type)
    d
  })

  map_data_v1 <- reactive({
    filtered() |>
      group_by(stateCode) |>
      summarise(n_declarations = n(), n_incident_types = n_distinct(incidentType), .groups = "drop")
  })

  trend_data_v1 <- reactive({
    d <- full_panel |> filter(year >= input$year_range[1], year <= input$year_range[2])
    if (input$incident_type != "All" || input$decl_type != "All") {
      f_raw <- scoped_data |> filter(year >= input$year_range[1], year <= input$year_range[2])
      if (input$incident_type != "All") f_raw <- filter(f_raw, incidentType == input$incident_type)
      if (input$decl_type != "All") f_raw <- filter(f_raw, declarationType == input$decl_type)
      sy <- f_raw |>
        group_by(stateCode, year) |>
        summarise(n_declarations = n(), .groups = "drop")
      d <- expand.grid(
        stateCode = unique(full_panel$stateCode),
        year      = unique(full_panel$year[full_panel$year >= input$year_range[1] & full_panel$year <= input$year_range[2]])
      ) |>
        left_join(sy, by = c("stateCode", "year")) |>
        mutate(n_declarations = replace_na(n_declarations, 0))
    }
    d |>
      group_by(year) |>
      summarise(
        mean_decl = mean(n_declarations, na.rm = TRUE),
        ci_lower  = if (n() > 1) mean_decl - 1.96 * sd(n_declarations, na.rm = TRUE) / sqrt(n()) else mean_decl,
        ci_upper  = if (n() > 1) mean_decl + 1.96 * sd(n_declarations, na.rm = TRUE) / sqrt(n()) else mean_decl,
        .groups   = "drop"
      ) |>
      mutate(ci_lower = replace_na(ci_lower, 0), ci_upper = replace_na(ci_upper, 0)) |>
      arrange(year)
  })

  # ---- V1 outputs ----------------------------------------------------------

  output$vb_total <- renderText({
    comma(nrow(filtered()))
  })
  output$vb_incident <- renderText({
    d <- filtered()
    if (nrow(d) == 0) {
      return("N/A")
    }
    names(which.max(table(d$incidentType)))
  })
  output$vb_states <- renderText({
    as.character(n_distinct(filtered()$stateCode))
  })
  output$vb_recent <- renderText({
    d <- filtered()
    if (nrow(d) == 0) {
      return("N/A")
    }
    format(max(d$declarationDate, na.rm = TRUE), "%b %d, %Y")
  })
  output$map_title <- renderText({
    paste("U.S. Map:", names(metric_choices)[metric_choices == input$metric], "by State")
  })
  output$map_subtitle <- renderText({
    paste0(
      input$year_range[1], "–", input$year_range[2],
      "  |  Incident: ", input$incident_type, "  |  Declaration: ", input$decl_type
    )
  })
  output$trend_subtitle <- renderText({
    paste0(
      input$year_range[1], "–", input$year_range[2],
      "  |  Incident: ", input$incident_type, "  |  Declaration: ", input$decl_type
    )
  })

  output$choropleth <- renderPlotly({
    df <- map_data_v1()
    metric_label <- names(metric_choices)[metric_choices == input$metric]
    if (nrow(df) == 0) {
      return(plot_ly(
        type = "choropleth", locations = character(0),
        locationmode = "USA-states", z = numeric(0), colorscale = mako_plotly_scale
      ) |>
        layout(
          geo = list(scope = "usa", showlakes = TRUE, lakecolor = "white", bgcolor = "white"),
          paper_bgcolor = "white", margin = list(l = 0, r = 0, t = 10, b = 0),
          annotations = list(list(
            text = "No data for current selection.",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 14, color = "#999")
          ))
        ) |>
        config(displayModeBar = FALSE))
    }
    plot_ly(df,
      type = "choropleth",
      locations = ~stateCode, locationmode = "USA-states",
      z = ~ get(input$metric), colorscale = mako_plotly_scale,
      colorbar = list(title = metric_label),
      hovertemplate = paste0("<b>%{location}</b><br>", metric_label, ": %{z}<extra></extra>")
    ) |>
      layout(
        geo = list(scope = "usa", showlakes = TRUE, lakecolor = "white", bgcolor = "white"),
        paper_bgcolor = "white", margin = list(l = 0, r = 0, t = 10, b = 0)
      ) |>
      config(displayModeBar = FALSE)
  })

  output$trend_plot <- renderPlotly({
    df <- trend_data_v1()
    if (nrow(df) == 0) {
      return(plot_ly() |>
        layout(
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
          paper_bgcolor = "white", plot_bgcolor = "white",
          annotations = list(list(
            text = "No data for current selection.",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 14, color = "#999")
          ))
        ) |>
        config(displayModeBar = FALSE))
    }
    spike_years <- df |>
      slice_max(mean_decl, n = 2) |>
      pull(year)
    plot_ly() |>
      add_ribbons(
        data = df, x = ~year, ymin = ~ci_lower, ymax = ~ci_upper,
        fillcolor = "rgba(52,136,166,0.18)", line = list(color = "transparent"),
        hoverinfo = "skip", showlegend = FALSE
      ) |>
      add_lines(
        data = df, x = ~year, y = ~mean_decl,
        line = list(color = mako_line, width = 2.5),
        hovertemplate = "<b>%{x}</b><br>Mean: %{y:.2f}<extra></extra>", showlegend = FALSE
      ) |>
      add_markers(
        data = df |> filter(!year %in% spike_years), x = ~year, y = ~mean_decl,
        marker = list(color = mako_line, size = 5),
        hovertemplate = "<b>%{x}</b><br>Mean: %{y:.2f}<extra></extra>", showlegend = FALSE
      ) |>
      add_markers(
        data = df |> filter(year %in% spike_years), x = ~year, y = ~mean_decl,
        marker = list(color = mako_highlight, size = 9, line = list(color = mako_line_dark, width = 1)),
        hovertemplate = "<b>Spike: %{x}</b><br>Mean: %{y:.2f}<extra></extra>", showlegend = FALSE
      ) |>
      layout(
        xaxis = list(title = "", tickmode = "linear", dtick = 4, showgrid = FALSE, zeroline = FALSE),
        yaxis = list(title = "Mean Declarations per State", showgrid = TRUE, gridcolor = "#eeeeee", zeroline = FALSE),
        plot_bgcolor = "white", paper_bgcolor = "white",
        margin = list(l = 50, r = 20, t = 10, b = 40)
      ) |>
      config(displayModeBar = FALSE)
  })

  # ---- V2 filter cross-update observers ------------------------------------

  observe({
    d <- scoped_data
    if (input$incident_type2 != "All") d <- filter(d, incidentType == input$incident_type2)
    choices <- c("All", sort(unique(d$declarationType)))
    updateSelectInput(session, "decl_type2",
      choices  = choices,
      selected = if (input$decl_type2 %in% choices) input$decl_type2 else "All"
    )
  }) |> bindEvent(input$incident_type2)

  observe({
    d <- scoped_data
    if (input$decl_type2 != "All") d <- filter(d, declarationType == input$decl_type2)
    choices <- c("All", sort(unique(d$incidentType)))
    updateSelectInput(session, "incident_type2",
      choices  = choices,
      selected = if (input$incident_type2 %in% choices) input$incident_type2 else "All"
    )
  }) |> bindEvent(input$decl_type2)

  # ---- V2 reactives --------------------------------------------------------

  filtered2 <- reactive({
    d <- scoped_data |> filter(year >= input$train_years2[1], year <= input$train_years2[2])
    if (input$incident_type2 != "All") d <- filter(d, incidentType == input$incident_type2)
    if (input$decl_type2 != "All") d <- filter(d, declarationType == input$decl_type2)
    d
  })

  panel2 <- reactive({
    f2 <- filtered2()
    req(!is.null(us_states_sf))
    p <- expand.grid(
      stateCode = unique(us_states_sf$STUSPS),
      year      = seq(input$train_years2[1], input$train_years2[2])
    )
    if (nrow(f2) > 0) {
      sy <- f2 |>
        group_by(stateCode, year) |>
        summarise(n_declarations = n(), .groups = "drop")
      p <- p |> left_join(sy, by = c("stateCode", "year"))
    } else {
      p$n_declarations <- NA_real_
    }
    p |> mutate(n_declarations = replace_na(n_declarations, 0))
  })

  sf_data2 <- reactive({
    req(!is.null(us_states_sf))
    p2 <- panel2()
    totals <- p2 |>
      group_by(stateCode) |>
      summarise(total_decl = sum(n_declarations, na.rm = TRUE), .groups = "drop")

    us_states_sf |>
      left_join(totals, by = c("STUSPS" = "stateCode")) |>
      mutate(
        total_decl   = replace_na(total_decl, 0),
        area_km2     = as.numeric(st_area(geometry)) / 1e6,
        decl_density = (total_decl / area_km2) * 1000
      )
  })

  forecast_data2 <- reactive({
    req(nrow(panel2()) > 0, input$map_mode2 == "forecast")
    fy <- input$forecast_year2
    tryCatch(
      panel2() |>
        group_by(stateCode) |>
        summarise(
          predicted = {
            d <- cur_data()
            if (n_distinct(d$year) < 3) {
              NA_real_
            } else {
              max(0, predict(lm(n_declarations ~ year, data = d), newdata = data.frame(year = fy)))
            }
          },
          .groups = "drop"
        ),
      error = function(e) tibble(stateCode = character(), predicted = numeric())
    )
  })

  # State click
  sel_state_rv <- reactiveVal(NULL)
  observeEvent(event_data("plotly_click", source = "v2map"),
    {
      ed <- event_data("plotly_click", source = "v2map")
      if (!is.null(ed)) {
        raw_code <- if (!is.null(ed$key)) ed$key else if (!is.null(ed$location)) ed$location else ed$customdata
        state_code <- toupper(trimws(as.character(raw_code)))
        if (length(state_code) > 0 && nchar(state_code) == 2) sel_state_rv(state_code)
      }
    },
    ignoreNULL = TRUE
  )

  neighbor_states2 <- reactive({
    sel <- sel_state_rv()
    req(!is.null(adj_global), !is.null(us_states_sf), !is.null(sel))
    idx <- which(us_states_sf$STUSPS == sel)
    if (length(idx) == 0) {
      return(character(0))
    }
    us_states_sf$STUSPS[adj_global[[idx]]]
  })

  dist_df2 <- reactive({
    sel <- sel_state_rv()
    req(!is.null(dist_matrix_km), !is.null(sel), sel %in% rownames(dist_matrix_km))
    state_tots <- panel2() |>
      group_by(stateCode) |>
      summarise(total_decl = sum(n_declarations), .groups = "drop")
    global_mean <- mean(state_tots$total_decl, na.rm = TRUE)
    tibble(
      stateCode = rownames(dist_matrix_km),
      dist_km = dist_matrix_km[sel, ]
    ) |>
      filter(stateCode != sel) |>
      left_join(state_tots, by = "stateCode") |>
      mutate(
        total_decl = replace_na(total_decl, 0),
        above_avg = total_decl > global_mean
      )
  })

  # ---- V2 outputs ----------------------------------------------------------

  output$v2_map_title <- renderText({
    if (input$map_mode2 == "forecast") {
      paste0("Forecast Declarations (", input$forecast_year2, ")  —  lm() Trend Extrapolation by State")
    } else {
      paste0("Historical Total Declarations (", input$train_years2[1], "–", input$train_years2[2], ")  by State")
    }
  })

  output$v2_map_subtitle <- renderText({
    sel <- sel_state_rv()
    paste0(
      "Incident: ", input$incident_type2, "  |  Declaration: ", input$decl_type2,
      "  |  Mako scale: dark = low, light = high  |  ",
      if (!is.null(sel)) {
        paste0("Selected: ", sel, "  (Mako highlight border)")
      } else {
        "Click a state to explore spatial relationships"
      }
    )
  })

  output$v2_main_map <- renderPlotly({
    sf_d <- sf_data2()
    sel <- sel_state_rv()
    if (is.null(sf_d) || nrow(sf_d) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") |>
        layout(
          annotations = list(list(
            text = "No data available.",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 14, color = "#999")
          )),
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
        ) |>
        config(displayModeBar = FALSE))
    }
    df <- sf_d |> st_drop_geometry()
    if (input$map_mode2 == "forecast") {
      fc <- tryCatch(forecast_data2(), error = function(e) NULL)
      if (!is.null(fc) && nrow(fc) > 0) {
        df <- df |>
          left_join(fc, by = c("STUSPS" = "stateCode")) |>
          mutate(
            z_val = replace_na(predicted, 0),
            z_label = paste0("Predicted (", input$forecast_year2, ")")
          )
      } else {
        df <- df |> mutate(z_val = total_decl, z_label = "Total Declarations")
      }
    } else {
      df <- df |> mutate(z_val = total_decl, z_label = "Total Declarations")
    }
    z_rng <- range(df$z_val, na.rm = TRUE)
    if (any(is.na(z_rng)) || any(is.infinite(z_rng))) {
      z_rng <- c(0, 1)
    } else if (diff(z_rng) == 0) z_rng <- c(z_rng[1] - 1, z_rng[1] + 1)

    p <- plot_ly(source = "v2map") |>
      add_trace(
        type = "choropleth", data = df,
        locations = ~STUSPS, locationmode = "USA-states",
        z = ~z_val, zmin = z_rng[1], zmax = z_rng[2],
        colorscale = mako_plotly_scale, reversescale = FALSE,
        colorbar = list(title = df$z_label[1], thickness = 14),
        marker = list(line = list(color = "white", width = 0.5)),
        customdata = ~STUSPS, key = ~STUSPS,
        hovertemplate = paste0(
          "<b>%{location}</b><br>", df$z_label[1], ": %{z:.0f}",
          "<br><i>Click to explore spatial relationships</i><extra></extra>"
        )
      )
    # Highlight selected state.
    if (!is.null(sel) && sel %in% df$STUSPS) {
      df_sel <- df |> filter(STUSPS == sel)
      p <- p |> add_trace(
        type = "choropleth", data = df_sel,
        locations = ~STUSPS, locationmode = "USA-states",
        z = ~z_val, zmin = z_rng[1], zmax = z_rng[2],
        colorscale = mako_plotly_scale, showscale = FALSE,
        marker = list(line = list(color = mako_highlight, width = 4)),
        customdata = ~STUSPS, key = ~STUSPS,
        hovertemplate = paste0(
          "<b>%{location}  ★ SELECTED</b><br>",
          df$z_label[1], ": %{z:.0f}<extra></extra>"
        )
      )
    }
    p |>
      layout(
        geo = list(
          scope = "usa", showlakes = TRUE, lakecolor = "white",
          bgcolor = "white", showframe = FALSE
        ),
        paper_bgcolor = "white", margin = list(l = 0, r = 0, t = 10, b = 0), showlegend = FALSE
      ) |>
      event_register("plotly_click") |>
      config(displayModeBar = FALSE)
  })

  output$v2_sel_banner <- renderUI({
    sel <- sel_state_rv()
    if (is.null(sel)) {
      tags$div(
        class = "no-sel-hint",
        tags$strong(" Click any state on the map"),
        " to reveal adjacency analysis, distance decay, and spatial metrics below."
      )
    } else {
      state_name <- tryCatch(us_states_sf$NAME[us_states_sf$STUSPS == sel][1], error = function(e) sel)
      tags$div(
        class = "sel-banner",
        tags$span("Selected State: "),
        tags$strong(paste0(state_name, "  (", sel, ")")),
        tags$span(
          style = "float:right; color:#3488A6; font-size:0.8rem; font-weight:400;",
          "All charts and metrics below have updated"
        )
      )
    }
  })

  output$vb2_nearest <- renderText({
    sel <- sel_state_rv()
    if (is.null(sel)) {
      return("Select a state")
    }
    df <- tryCatch(dist_df2(), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) {
      return("N/A")
    }
    above <- df |> filter(above_avg)
    if (nrow(above) == 0) {
      return("None above avg")
    }
    nearest <- above |> slice_min(dist_km, n = 1, with_ties = FALSE)
    paste0(nearest$stateCode, "  (", round(nearest$dist_km), " km)")
  })

  output$vb2_cluster <- renderText({
    sel <- sel_state_rv()
    if (is.null(sel)) {
      return("Select a state")
    }
    nbrs <- tryCatch(neighbor_states2(), error = function(e) character(0))
    if (length(nbrs) == 0) {
      return("N/A")
    }
    tots <- panel2() |>
      group_by(stateCode) |>
      summarise(total_decl = sum(n_declarations), .groups = "drop")
    sel_val <- tots |>
      filter(stateCode == sel) |>
      pull(total_decl)
    nbr_mean <- tots |>
      filter(stateCode %in% nbrs) |>
      summarise(m = mean(total_decl, na.rm = TRUE)) |>
      pull(m)
    if (length(sel_val) == 0 || is.na(nbr_mean) || nbr_mean == 0) {
      return("N/A")
    }
    paste0(round(sel_val / nbr_mean, 2), "×  neighbor mean")
  })

  output$vb2_density <- renderText({
    sel <- sel_state_rv()
    if (is.null(sel)) {
      return("Select a state")
    }
    sf_d <- tryCatch(sf_data2(), error = function(e) NULL)
    if (is.null(sf_d)) {
      return("N/A")
    }
    row <- sf_d |>
      filter(STUSPS == sel) |>
      st_drop_geometry()
    if (nrow(row) == 0) {
      return("N/A")
    }
    paste0(round(row$decl_density, 2), " / 1,000 km²")
  })

  output$v2_plot1_title <- renderText({
    sel <- sel_state_rv()
    if (is.null(sel)) {
      "Annual Declarations by State — Select a State to Compare"
    } else {
      paste0(sel, "  vs.  Neighbor States — Annual Declarations")
    }
  })

  output$v2_plot1_sub <- renderText({
    sel <- sel_state_rv()
    if (is.null(sel)) {
      return("Click a state on the map above")
    }
    nbrs <- tryCatch(neighbor_states2(), error = function(e) character(0))
    paste0(
      length(nbrs), " adjacent states (sf::st_touches)  |  ",
      input$train_years2[1], "–", input$train_years2[2],
      "  |  Incident: ", input$incident_type2
    )
  })

  output$v2_trend_compare <- renderPlotly({
    sel <- sel_state_rv()
    if (is.null(sel)) {
      return(plotly_empty(type = "scatter", mode = "markers") |>
        layout(
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
          paper_bgcolor = "white", plot_bgcolor = "white",
          annotations = list(list(
            text = "← Click a state on the map to see its trend vs. neighboring states",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 13, color = "#bbb")
          ))
        ) |>
        config(displayModeBar = FALSE))
    }
    nbrs <- tryCatch(neighbor_states2(), error = function(e) character(0))
    p2_dat <- panel2()
    sel_ts <- p2_dat |>
      filter(stateCode == sel) |>
      arrange(year)
    nbr_ts <- if (length(nbrs) > 0) {
      p2_dat |>
        filter(stateCode %in% nbrs) |>
        group_by(year) |>
        summarise(mean_decl = mean(n_declarations), .groups = "drop") |>
        arrange(year)
    } else {
      tibble(year = integer(), mean_decl = numeric())
    }

    if (nrow(sel_ts) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") |>
        layout(
          annotations = list(list(
            text = paste("No data for", sel, "in current filters."),
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 13, color = "#999")
          )),
          paper_bgcolor = "white"
        ) |> config(displayModeBar = FALSE))
    }
    p <- plot_ly() |>
      add_lines(
        data = sel_ts, x = ~year, y = ~n_declarations, name = sel,
        line = list(color = mako_highlight, width = 2.5),
        hovertemplate = paste0("<b>", sel, " (%{x})</b><br>Declarations: %{y}<extra></extra>")
      )
    if (nrow(nbr_ts) > 0) {
      p <- p |> add_lines(
        data = nbr_ts, x = ~year, y = ~mean_decl, name = "Neighbor Avg",
        line = list(color = mako_line, width = 2, dash = "dot"),
        hovertemplate = "<b>Neighbor Avg (%{x})</b><br>Mean: %{y:.1f}<extra></extra>"
      )
    }
    if (nrow(nbr_ts) > 0 && nrow(sel_ts) > 0) {
      merged <- sel_ts |>
        inner_join(nbr_ts, by = "year") |>
        mutate(gap = abs(n_declarations - mean_decl))
      if (nrow(merged) > 0) {
        peak <- merged |> slice_max(gap, n = 1, with_ties = FALSE)
        p <- p |> add_annotations(
          x = peak$year, y = peak$n_declarations,
          text = paste0("Peak gap: ", round(peak$gap, 1)),
          showarrow = TRUE, arrowhead = 2, arrowsize = 0.8,
          font = list(size = 11, color = mako_line_dark), ax = 30, ay = -30
        )
      }
    }
    p |>
      layout(
        legend = list(orientation = "h", x = 0, y = -0.18),
        xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, tickmode = "linear", dtick = 4),
        yaxis = list(title = "Annual Declarations", showgrid = TRUE, gridcolor = "#eeeeee", zeroline = FALSE),
        plot_bgcolor = "white", paper_bgcolor = "white",
        margin = list(l = 50, r = 20, t = 10, b = 55)
      ) |>
      config(displayModeBar = FALSE)
  })

  output$v2_plot2_sub <- renderText({
    sel <- sel_state_rv()
    if (is.null(sel)) {
      return("Click a state to see how declarations vary with distance")
    }
    paste0(
      "sf::st_distance() centroid-to-centroid distances from ", sel,
      "  |  Mako diamonds = adjacent states  |  Dashed line = linear fit"
    )
  })

  output$v2_distance_decay <- renderPlotly({
    sel <- sel_state_rv()
    if (is.null(sel)) {
      return(plotly_empty(type = "scatter", mode = "markers") |>
        layout(
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
          paper_bgcolor = "white", plot_bgcolor = "white",
          annotations = list(list(
            text = "← Click a state on the map to see the distance decay pattern",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 13, color = "#bbb")
          ))
        ) |>
        config(displayModeBar = FALSE))
    }
    df <- tryCatch(dist_df2(), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") |>
        layout(
          annotations = list(list(
            text = "Distance data unavailable.",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 13, color = "#999")
          )),
          paper_bgcolor = "white"
        ) |> config(displayModeBar = FALSE))
    }
    nbrs <- tryCatch(neighbor_states2(), error = function(e) character(0))
    df <- df |> mutate(is_neighbor = stateCode %in% nbrs)
    trend_df <- if (nrow(df) >= 5) {
      fit <- lm(total_decl ~ dist_km, data = df)
      df |>
        arrange(dist_km) |>
        mutate(fitted = predict(fit, newdata = pick(dist_km)))
    } else {
      NULL
    }
    non_nbr <- df |> filter(!is_neighbor)
    nbr_df <- df |> filter(is_neighbor)
    p <- plot_ly() |>
      add_markers(
        data = non_nbr, x = ~dist_km, y = ~total_decl, text = ~stateCode,
        marker = list(
          color = ifelse(non_nbr$above_avg, mako_accent, "#a8b3bf"),
          size = 7, opacity = 0.75,
          line = list(color = "white", width = 0.5)
        ),
        name = "Other States",
        hovertemplate = "<b>%{text}</b><br>Distance: %{x:.0f} km<br>Declarations: %{y}<extra></extra>"
      )
    if (nrow(nbr_df) > 0) {
      p <- p |> add_markers(
        data = nbr_df, x = ~dist_km, y = ~total_decl, text = ~stateCode,
        marker = list(
          color = mako_line, size = 12, symbol = "diamond",
          line = list(color = "white", width = 1.5)
        ),
        name = "Adjacent (st_touches)",
        hovertemplate = "<b>%{text}  [neighbor]</b><br>Distance: %{x:.0f} km<br>Declarations: %{y}<extra></extra>"
      )
    }
    if (!is.null(trend_df)) {
      p <- p |> add_lines(
        data = trend_df, x = ~dist_km, y = ~fitted,
        line = list(color = mako_line_dark, dash = "dash", width = 1.5),
        name = "Linear Trend", hoverinfo = "skip"
      )
    }
    p |>
      layout(
        legend = list(orientation = "h", x = 0, y = -0.2),
        xaxis = list(
          title = paste0("Distance from ", sel, " (km)"),
          showgrid = TRUE, gridcolor = "#eeeeee", zeroline = FALSE
        ),
        yaxis = list(title = "Total Declarations", showgrid = TRUE, gridcolor = "#eeeeee", zeroline = FALSE),
        plot_bgcolor = "white", paper_bgcolor = "white",
        margin = list(l = 55, r = 20, t = 10, b = 65)
      ) |>
      config(displayModeBar = FALSE)
  })

  # Adjacency text
  output$v2_text_adjacency <- renderText({
    sel <- sel_state_rv()
    if (is.null(sel)) {
      return(paste0(
        "Select a state on the map to see a reactive adjacency analysis. ",
        "This panel uses sf::st_touches() to identify which states share a border with your selection, ",
        "then compares the selected state's declaration count against its neighbors' average. ",
        "High clustering ratios indicate a regional disaster hotspot."
      ))
    }
    nbrs <- tryCatch(neighbor_states2(), error = function(e) character(0))
    tots <- tryCatch(
      panel2() |> group_by(stateCode) |> summarise(total_decl = sum(n_declarations), .groups = "drop"),
      error = function(e) NULL
    )
    if (is.null(tots) || nrow(tots) == 0) {
      return("Insufficient data for adjacency analysis.")
    }
    global_mean <- mean(tots$total_decl, na.rm = TRUE)
    sel_decl <- tots |>
      filter(stateCode == sel) |>
      pull(total_decl)
    nbr_decls <- tots |>
      filter(stateCode %in% nbrs) |>
      pull(total_decl)
    nbr_mean <- if (length(nbr_decls) > 0) mean(nbr_decls, na.rm = TRUE) else NA_real_
    if (length(sel_decl) == 0 || is.na(sel_decl)) {
      return("No declarations recorded for this state in the current filter window.")
    }
    if (is.na(global_mean) || global_mean == 0) {
      return("National average is unavailable.")
    }
    pct_vs_nat <- round((sel_decl / global_mean - 1) * 100, 1)
    above_below <- if (!is.na(pct_vs_nat) && pct_vs_nat >= 0) paste0(pct_vs_nat, "% above") else paste0(abs(pct_vs_nat), "% below")
    nbr_compare <- if (!is.na(nbr_mean) && nbr_mean > 0) {
      ratio <- sel_decl / nbr_mean
      if (ratio > 1.2) "notably higher than" else if (ratio < 0.8) "notably lower than" else "roughly on par with"
    } else {
      "comparable to"
    }
    interpretation <- if (nbr_compare == "notably higher than") {
      "This pattern suggests a localized hotspot: the selected state bears a disproportionate disaster burden even within an already exposed region."
    } else if (nbr_compare == "notably lower than") {
      "This pattern suggests resilience within a higher-exposure region: the state is insulated relative to its surroundings despite geographic proximity."
    } else {
      "Declaration exposure is distributed evenly across this regional cluster, suggesting broad regional risk rather than a single focal point."
    }
    paste0(
      sel, " shares a border with ", length(nbrs),
      if (length(nbrs) == 1) " state (" else " states (",
      paste(nbrs, collapse = ", "),
      ") according to sf::st_touches(). Over ", input$train_years2[1], "–", input$train_years2[2],
      ", ", sel, " recorded ", comma(sel_decl),
      " total declarations — ", above_below, " the national state average of ",
      round(global_mean, 1), ". Its count is ", nbr_compare,
      " its neighbor average",
      if (!is.na(nbr_mean)) paste0(" (", round(nbr_mean, 1), " declarations)") else "",
      ". ", interpretation
    )
  })

  # Distance text
  output$v2_text_distance <- renderText({
    sel <- sel_state_rv()
    if (is.null(sel)) {
      return(paste0(
        "Select a state on the map to see a distance-based spatial analysis. ",
        "This panel uses sf::st_distance() on state centroids to compute exact ",
        "centroid-to-centroid distances in km, then examines how declaration frequency ",
        "varies with distance — testing for spatial decay or diffuse risk patterns."
      ))
    }
    df <- tryCatch(dist_df2(), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) {
      return("Distance data unavailable for the current filter.")
    }
    above_avg_df <- df |> filter(above_avg)
    nearest_above <- if (nrow(above_avg_df) > 0) above_avg_df |> slice_min(dist_km, n = 1, with_ties = FALSE) else NULL
    farthest_above <- if (nrow(above_avg_df) > 0) above_avg_df |> slice_max(dist_km, n = 1, with_ties = FALSE) else NULL
    cor_val <- if (nrow(df) >= 5 && sd(df$dist_km, na.rm = TRUE) > 0 && sd(df$total_decl, na.rm = TRUE) > 0) {
      cor(df$dist_km, df$total_decl, use = "complete.obs")
    } else {
      NA_real_
    }
    cor_desc <- if (!is.na(cor_val)) {
      if (cor_val < -0.25) {
        paste0(
          "a meaningful negative relationship (r = ", round(cor_val, 2),
          ") — states geographically close to ", sel,
          " tend to have higher declaration counts, consistent with spatial clustering of disaster risk"
        )
      } else if (cor_val > 0.25) {
        paste0(
          "a positive relationship (r = ", round(cor_val, 2),
          ") — higher declaration counts are more common in distant states, suggesting ",
          sel, " is a lower-risk anchor in a regionally concentrated risk zone"
        )
      } else {
        paste0(
          "little distance-based correlation (r = ", round(cor_val, 2),
          ") — disaster risk does not exhibit a strong spatial decay pattern from ", sel,
          ", suggesting diffuse national exposure"
        )
      }
    } else {
      "insufficient data for a correlation estimate in the current filter window"
    }
    paste0(
      "Using sf::st_distance() on state centroids, ", nrow(df), " pairwise distances from ", sel, " were computed. ",
      if (!is.null(nearest_above)) {
        paste0(
          "The nearest above-average state is ", nearest_above$stateCode,
          " (", round(nearest_above$dist_km), " km), ",
          "while the most distant above-average state is ", farthest_above$stateCode,
          " (", round(farthest_above$dist_km), " km). "
        )
      } else {
        "No above-average declaration states were found under the current filters. "
      },
      "Across all state pairs, there is ", cor_desc, "."
    )
  })

  # ---- PAGE 3 — LEAFLET MAP ------------------------------------------------

  selected_p3_state <- reactiveVal(NULL)

  output$disaster_map <- renderLeaflet({
    l <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -98.5, lat = 39.5, zoom = 4)

    if (!is.null(us_states_sf)) {
      l <- l %>%
        addPolygons(
          data = us_states_sf,
          fillColor = "#eef3f7",
          fillOpacity = 0.1,
          color = "#8da2b2",
          weight = 1,
          layerId = ~STUSPS,
          label = ~NAME,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#35264C",
            fillOpacity = 0.3,
            bringToFront = FALSE
          )
        )
    }
    l
  })

  p3_filtered_summary <- reactive({
    p3_state_year %>%
      filter(year >= input$p3_year_range[1], year <= input$p3_year_range[2]) %>%
      group_by(stateCode) %>%
      summarise(
        total_disasters = sum(total_disasters),
        years_active = n_distinct(year),
        .groups = "drop"
      ) %>%
      left_join(sheldus_summary %||% tibble(stateCode = character()), by = "stateCode") %>%
      left_join(state_centroids, by = c("stateCode" = "state_abbr")) %>%
      left_join(state_mapping, by = "stateCode") %>%
      filter(!is.na(lat), !is.na(lng)) %>%
      mutate(
        total_prop_dmg   = replace_na(total_prop_dmg, 0),
        total_fatalities = replace_na(total_fatalities, 0),
        total_fema_asst  = total_prop_dmg * 0.15,
        dmg_billions     = round(total_prop_dmg / 1e9, 2)
      )
  }) %>% bindCache(input$p3_year_range)

  observe({
    df <- p3_filtered_summary()
    req(nrow(df) > 0)
    metric_col <- input$p3_map_metric
    vals <- df[[metric_col]]
    metric_label <- names(p3_metric_choices)[p3_metric_choices == metric_col]
    metric_label <- if (length(metric_label) > 0) metric_label[1] else metric_col
    radius_vals <- sqrt(pmax(vals, 0))
    if (length(unique(radius_vals[is.finite(radius_vals)])) <= 1) {
      radii <- rep(15, nrow(df))
    } else {
      radii <- scales::rescale(radius_vals, to = c(7, 26))
    }

    pal <- colorNumeric(
      palette = mako_colors,
      domain = vals
    )
    df <- df %>%
      mutate(
        marker_radius = radii,
        marker_fill = pal(vals),
        metric_value = vals
      )

    leafletProxy("disaster_map", data = df) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat, 
        radius = ~marker_radius,
        layerId = ~stateCode,
        fillColor = ~marker_fill,
        fillOpacity = 0.72,
        color = "#ffffff",
        weight = 1.5,
        opacity = 0.95,
        label = ~lapply(paste0(
          "<strong>", stateName, "</strong><br/>",
          metric_label, ": ", comma(metric_value), "<br/>",
          "Total disasters: ", comma(total_disasters)
        ), HTML),
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "6px 10px",
            "border-radius" = "6px",
            border = "1px solid #d8e2ec"
          ),
          textsize = "13px",
          direction = "auto"
        )
      )
  })

  observeEvent(input$disaster_map_marker_click, {
    click <- input$disaster_map_marker_click
    selected_p3_state(click$id)
    shinyjs::runjs("document.getElementById('state-panel').classList.add('visible');
                    document.getElementById('map-instruction').style.display='none';")
  })

  p3_panel_row <- reactive({
    req(selected_p3_state())
    p3_filtered_summary() %>% filter(stateCode == selected_p3_state())
  })

  output$p3_panel_state <- renderText({
    req(p3_panel_row())
    p3_panel_row()$stateName[1]
  })
  output$p3_m_disasters <- renderText({
    req(p3_panel_row())
    comma(p3_panel_row()$total_disasters[1])
  })
  output$p3_m_damage <- renderText({
    req(p3_panel_row())
    paste0("$", round(p3_panel_row()$dmg_billions[1], 1))
  })
  output$p3_m_fema <- renderText({
    req(p3_panel_row())
    paste0("$", round(p3_panel_row()$total_fema_asst[1] / 1e6, 0))
  })
  output$p3_m_fatalities <- renderText({
    req(p3_panel_row())
    comma(p3_panel_row()$total_fatalities[1])
  })

  output$p3_panel_text <- renderUI({
    req(p3_panel_row())
    row <- p3_panel_row()
    haz <- p3_state_hazard_year %>%
      filter(
        stateCode == selected_p3_state(),
        year >= input$p3_year_range[1], year <= input$p3_year_range[2]
      ) %>%
      group_by(incidentType) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      slice_max(n, n = 1)
    top_hazard <- if (nrow(haz) > 0) haz$incidentType[1] else "N/A"
    HTML(paste0(
      "<strong>", row$stateName[1], "</strong> has experienced <strong>", row$total_disasters[1],
      "</strong> FEMA-declared disasters in this period. The most frequent hazard is <strong>",
      top_hazard, "</strong>. Total estimated economic impact reached <strong>$",
      round(row$dmg_billions[1], 1), "B</strong>."
    ))
  })

  output$p3_chart_hazard <- renderPlotly({
    req(selected_p3_state())
    haz_data <- p3_state_hazard_year %>%
      filter(
        stateCode == selected_p3_state(),
        year >= input$p3_year_range[1], year <= input$p3_year_range[2]
      ) %>%
      group_by(incidentType) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      arrange(desc(n)) %>%
      slice_head(n = 5)
    plot_ly(haz_data,
      x = ~n, y = ~ reorder(incidentType, n), type = "bar",
      orientation = "h", marker = list(color = mako_line)
    ) %>%
      layout(
        margin = list(l = 0, r = 10, t = 0, b = 30),
        xaxis = list(title = ""), yaxis = list(title = ""),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  output$p3_chart_ts <- renderPlotly({
    req(selected_p3_state())
    ts_data <- p3_state_year %>%
      filter(
        stateCode == selected_p3_state(),
        year >= input$p3_year_range[1], year <= input$p3_year_range[2]
      ) %>%
      complete(
        year = seq(input$p3_year_range[1], input$p3_year_range[2]),
        fill = list(total_disasters = 0)
      )
    plot_ly(ts_data,
      x = ~year, y = ~total_disasters, type = "scatter", mode = "lines+markers",
      line = list(color = mako_line)
    ) %>%
      layout(
        margin = list(l = 0, r = 10, t = 5, b = 30),
        xaxis = list(title = ""), yaxis = list(title = ""),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- ABOUT ---------------------------------------------------------------

  output$about_top_disasters <- renderTable({
    top_disaster_states %>%
      select(State = state_name, Disasters = total_disasters) %>%
      head(5)
  })
  output$about_top_damage <- renderTable({
    top_damage_states %>%
      select(State = state_name, `Damage ($B)` = dmg_billions) %>%
      head(5)
  })
  output$about_hazard_pie <- renderPlotly({
    pie_data <- national_hazard %>% head(6)
    plot_ly(pie_data,
      labels = ~hazard_type, values = ~count,
      type = "pie", hole = 0.4,
      marker = list(colors = viridisLite::mako(nrow(pie_data), begin = 0.15, end = 0.9))
    ) %>%
      layout(showlegend = FALSE, margin = list(l = 0, r = 0, t = 0, b = 0), paper_bgcolor = "rgba(0,0,0,0)")
  })
  output$about_nat_trend <- renderPlotly({
    nat_ts <- scoped_data %>% count(year)
    plot_ly(nat_ts,
      x = ~year, y = ~n, type = "bar",
      marker = list(color = mako_line, opacity = 0.8)
    ) %>%
      layout(yaxis = list(title = "Total Declarations"), xaxis = list(title = ""), plot_bgcolor = "#f7f9fb")
  })
}

shinyApp(ui, server)
