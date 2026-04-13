library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(lubridate)
library(sf)
library(tigris)
library(viridis)

# loading data
scoped_data <- read_csv("FemaWebDisasterDeclarations.csv") |>
  mutate(
    declarationDate = as_date(declarationDate),
    year            = year(declarationDate)
  ) |>
  filter(year >= 2000, year <= 2024)

fema_sy <- scoped_data |>
  group_by(stateCode, year) |>
  summarise(n_declarations = n(), .groups = "drop")

full_panel <- expand.grid(
  stateCode = unique(fema_sy$stateCode),
  year      = unique(fema_sy$year)
) |>
  left_join(fema_sy, by = c("stateCode", "year")) |>
  mutate(n_declarations = replace_na(n_declarations, 0))

time_trends <- full_panel |>
  group_by(year) |>
  summarise(
    mean_decl = mean(n_declarations),
    ci_lower  = mean_decl - 1.96 * sd(n_declarations) / sqrt(n()),
    ci_upper  = mean_decl + 1.96 * sd(n_declarations) / sqrt(n()),
    .groups   = "drop"
  ) |>
  arrange(year)

incident_choices <- c("All", sort(unique(scoped_data$incidentType)))
decl_choices     <- c("All", sort(unique(scoped_data$declarationType)))
metric_choices   <- c(
  "Total Declarations"    = "n_declarations",
  "Unique Incident Types" = "n_incident_types"
)

# spatial data 
us_states_sf <- tryCatch({
  tigris::states(cb = TRUE, resolution = "20m", year = 2022, progress_bar = FALSE) |>
    st_transform(crs = 4326) |>
    filter(!STUSPS %in% c("AS", "GU", "MP", "PR", "VI", "UM")) |>
    select(STUSPS, NAME, geometry)
}, error = function(e) NULL)

state_totals <- scoped_data |>
  group_by(stateCode) |>
  summarise(total = n(), .groups = "drop")

dist_matrix_km <- tryCatch({
  req_sf <- us_states_sf
  if (is.null(req_sf)) stop("no sf")
  cents  <- suppressWarnings(st_centroid(req_sf))
  dm     <- st_distance(cents)
  dm_km  <- matrix(as.numeric(dm) / 1000,
                   nrow = nrow(cents),
                   dimnames = list(cents$STUSPS, cents$STUSPS))
  dm_km
}, error = function(e) NULL)

adj_global <- tryCatch({
  if (!is.null(us_states_sf)) st_touches(us_states_sf) else NULL
}, error = function(e) NULL)


# ui
ui <- page_fluid(
  title = "FEMA Disaster Declarations (2000–2024)",
  theme = bs_theme(
    bootswatch   = "flatly",
    base_font    = font_google("IBM Plex Sans"),
    heading_font = font_google("IBM Plex Sans")
  ),
  
  tags$head(tags$style(HTML("
    body { background-color: #ffffff; }
    .card { border: 1px solid #e0e0e0; box shadow: none; }
    .card-header { font-weight: 600; font-size: 0.95rem; background: #ffffff;
                   border-bottom: 1px solid #e0e0e0; color: #222; }
    .subtitle-text { color: #888; font size: 0.85rem; padding: 4px 12px 8px 12px; }
    .sel-banner {
      background: #fff8f0; border: 1.5px solid #ffd6a5; border radius: 6px;
      padding: 10px 18px; margin-bottom: 14px;
      font-size: 0.9rem; color: #b34700; font weight: 500;
    }
    .no-sel-hint {
      color: #b0b8c1; font size: 0.9rem; text align: center; padding: 28px 12px;
      background: #f8f9fa; border radius: 6px; margin bottom: 14px;
      border: 1px dashed #dee2e6;
    }
    .analysis-text {
      font-size: 0.88rem; color: #333; line height: 1.7; margin: 0;
    }
    /* Make map cursor a pointer to signal clickability */
    .js-plotly-plot .plotly .cursor-crosshair { cursor: pointer !important; }
  "))),
  
  navset_tab(
    
    # page 1   overview
    nav_panel(
      "V1 — Overview",
      
      card(
        class = "mb-3",
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            selectInput("incident_type", "Incident Type",
                        choices = incident_choices, selected = "All"),
            selectInput("decl_type", "Declaration Type",
                        choices = decl_choices, selected = "All"),
            selectInput("metric", "Map Metric",
                        choices = metric_choices, selected = "n_declarations")
          ),
          sliderInput("year_range", "Year Range", min = 2000, max = 2024,
                      value = c(2000, 2024), sep = "", width = "100%")
        )
      ),
      
      card(
        class = "mb-3",
        card_body(
          padding = "12px",
          tags$p(style = "margin: 0; font-size: 0.9rem; color: #333;",
                 tags$strong("Metric 1: "),
                 "Total federal disaster declarations per U.S. state or territory (2000–2024). ",
                 "A declaration means a disaster exceeded local response capacity, triggering federal aid."
          ),
          tags$p(style = "margin: 8px 0 0 0; font-size: 0.9rem; color: #333;",
                 tags$strong("Metric 2: "),
                 "Annual mean declarations per state with 95% confidence intervals. Tracks whether ",
                 "the average disaster burden on U.S. states is growing over time."
          )
        )
      ),
      
      layout_columns(
        fill = FALSE, col_widths = c(3, 3, 3, 3),
        value_box(title = "Total Declarations",            value = textOutput("vb_total"),
                  theme = value_box_theme(bg = "#ffffff", fg = "#222222")),
        value_box(title = "Most Common Incident",          value = textOutput("vb_incident"),
                  theme = value_box_theme(bg = "#ffffff", fg = "#222222")),
        value_box(title = "States / Territories Affected", value = textOutput("vb_states"),
                  theme = value_box_theme(bg = "#ffffff", fg = "#222222")),
        value_box(title = "Most Recent Declaration",       value = textOutput("vb_recent"),
                  theme = value_box_theme(bg = "#ffffff", fg = "#222222"))
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
    ), #...end v1
    
    
    #...page 2   spatial analysis
    nav_panel(
      "V2 — Spatial Analysis",
      
      layout_sidebar(
        
        #...sidebar
        sidebar = sidebar(
          width = 270,
          title = "Spatial Controls",
          
          tags$p(style = "font-size:0.78rem; color:#666; margin-top:-4px;",
                 "All controls apply to Page 2 only."),
          
          hr(style = "margin:6px 0;"),
          
          selectInput("incident_type2", "Incident Type",
                      choices = incident_choices, selected = "All"),
          selectInput("decl_type2", "Declaration Type",
                      choices = decl_choices, selected = "All"),
          
          hr(style = "margin:6px 0;"),
          
          sliderInput("train_years2", "Year Range",
                      min = 2000, max = 2024, value = c(2000, 2024),
                      sep = "", width = "100%"),
          
          hr(style = "margin:6px 0;"),
          
          radioButtons("map_mode2", "Map Display",
                       choices = c(
                         "Historical Totals"   = "historical",
                         "Forecast (lm trend)" = "forecast"
                       ),
                       selected = "historical"),
          
          conditionalPanel(
            condition = "input.map_mode2 == 'forecast'",
            sliderInput("forecast_year2", "Forecast Year",
                        min = 2025, max = 2035, value = 2028,
                        step = 1, sep = "", width = "100%")
          ),
          
          hr(style = "margin:6px 0;"),
          
          tags$p(style = "font-size:0.75rem; color:#999;",
                 tags$strong("How to use: "),
                 "Click any state on the map to reveal adjacency and distance analyses. ",
                 "All charts and text update reactively."),
          
          hr(style = "margin:6px 0;"),
          tags$p(style = "font-size:0.72rem; color:#bbb; line-height:1.5;",
                 "Spatial methods: sf::st_area() · sf::st_centroid() · ",
                 "sf::st_distance() · sf::st_touches() · lm() per-state trend")
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
            theme = value_box_theme(bg = "#ffffff", fg = "#222222")
          ),
          value_box(
            title = "Neighbor Clustering Ratio (st_touches)",
            value = textOutput("vb2_cluster"),
            theme = value_box_theme(bg = "#ffffff", fg = "#222222")
          ),
          value_box(
            title = "Declaration Density (sf::st_area)",
            value = textOutput("vb2_density"),
            theme = value_box_theme(bg = "#ffffff", fg = "#222222")
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
        
        layout_columns(
          col_widths = c(6, 6),
          
          card(
            card_header("Adjacency Analysis  ·  sf::st_touches()"),
            card_body(
              tags$p(class = "analysis-text", textOutput("v2_text_adjacency"))
            )
          ),
          
          card(
            card_header("Distance Analysis  ·  sf::st_distance()"),
            card_body(
              tags$p(class = "analysis-text", textOutput("v2_text_distance"))
            )
          )
        ),
        
        tags$div(
          style = "text-align:center; color:#aaa; font-size:11px; padding:12px 0 20px;",
          "Spatial: sf::st_area() · sf::st_centroid() · sf::st_distance() · sf::st_touches() · ",
          "lm() per-state trend  |  Data: FEMA / OpenFEMA  |  Boundaries: tigris"
        )
        
      ) 
    )  
    
  ) 
)   


#server
server <- function(input, output, session) {
  
  # page 1 v1 reactives
  
  observe({
    d <- scoped_data
    if (input$incident_type != "All") d <- filter(d, incidentType == input$incident_type)
    choices <- c("All", sort(unique(d$declarationType)))
    updateSelectInput(session, "decl_type", choices = choices,
                      selected = if (input$decl_type %in% choices) input$decl_type else "All")
  }) |> bindEvent(input$incident_type)
  
  observe({
    d <- scoped_data
    if (input$decl_type != "All") d <- filter(d, declarationType == input$decl_type)
    choices <- c("All", sort(unique(d$incidentType)))
    updateSelectInput(session, "incident_type", choices = choices,
                      selected = if (input$incident_type %in% choices) input$incident_type else "All")
  }) |> bindEvent(input$decl_type)
  
  filtered <- reactive({
    d <- scoped_data |> filter(year >= input$year_range[1], year <= input$year_range[2])
    if (input$incident_type != "All") d <- filter(d, incidentType == input$incident_type)
    if (input$decl_type != "All")    d <- filter(d, declarationType == input$decl_type)
    d
  })
  
  map_data <- reactive({
    filtered() |>
      group_by(stateCode) |>
      summarise(
        n_declarations   = n(),
        n_incident_types = n_distinct(incidentType),
        .groups = "drop"
      )
  })
  
  trend_data <- reactive({
    d <- full_panel |>
      filter(year >= input$year_range[1], year <= input$year_range[2])
    
    if (input$incident_type != "All" || input$decl_type != "All") {
      filtered_raw <- scoped_data |>
        filter(year >= input$year_range[1], year <= input$year_range[2])
      if (input$incident_type != "All")
        filtered_raw <- filter(filtered_raw, incidentType == input$incident_type)
      if (input$decl_type != "All")
        filtered_raw <- filter(filtered_raw, declarationType == input$decl_type)
      
      sy <- filtered_raw |>
        group_by(stateCode, year) |>
        summarise(n_declarations = n(), .groups = "drop")
      
      d <- expand.grid(
        stateCode = unique(full_panel$stateCode),
        year      = unique(full_panel$year[
          full_panel$year >= input$year_range[1] & full_panel$year <= input$year_range[2]
        ])
      ) |>
        left_join(sy, by = c("stateCode", "year")) |>
        mutate(n_declarations = replace_na(n_declarations, 0))
    }
    
    d |>
      group_by(year) |>
      summarise(
        mean_decl = mean(n_declarations, na.rm = TRUE),
        #...handle cases where sd is na (e.g. n=1) or mean is nan
        ci_lower  = if(n() > 1) mean_decl - 1.96 * sd(n_declarations, na.rm = TRUE) / sqrt(n()) else mean_decl,
        ci_upper  = if(n() > 1) mean_decl + 1.96 * sd(n_declarations, na.rm = TRUE) / sqrt(n()) else mean_decl,
        .groups   = "drop"
      ) |>
      mutate(
        ci_lower = replace_na(ci_lower, 0),
        ci_upper = replace_na(ci_upper, 0)
      ) |>
      arrange(year)
  })
  
  output$vb_total    <- renderText({ comma(nrow(filtered())) })
  output$vb_incident <- renderText({
    d <- filtered(); if (nrow(d) == 0) return("N/A")
    names(which.max(table(d$incidentType)))
  })
  output$vb_states   <- renderText({ as.character(n_distinct(filtered()$stateCode)) })
  output$vb_recent   <- renderText({
    d <- filtered(); if (nrow(d) == 0) return("N/A")
    format(max(d$declarationDate, na.rm = TRUE), "%b %d, %Y")
  })
  
  output$map_title <- renderText({
    paste("U.S. Map:", names(metric_choices)[metric_choices == input$metric], "by State")
  })
  output$map_subtitle <- renderText({
    paste0(input$year_range[1], "–", input$year_range[2],
           "  |  Incident: ", input$incident_type, "  |  Declaration: ", input$decl_type)
  })
  output$trend_subtitle <- renderText({
    paste0(input$year_range[1], "–", input$year_range[2],
           "  |  Incident: ", input$incident_type, "  |  Declaration: ", input$decl_type)
  })
  
  output$choropleth <- renderPlotly({
    df <- map_data()
    metric_label <- names(metric_choices)[metric_choices == input$metric]
    if (nrow(df) == 0) {
      return(plot_ly(type = "choropleth", locations = character(0),
                     locationmode = "USA-states", z = numeric(0), colorscale = "Blues") |>
               layout(geo = list(scope="usa", showlakes=TRUE, lakecolor="white", bgcolor="white"),
                      paper_bgcolor="white", margin=list(l=0,r=0,t=10,b=0),
                      annotations = list(list(text="No data for current selection.",
                                              x=0.5, y=0.5, xref="paper", yref="paper",
                                              showarrow=FALSE, font=list(size=14, color="#999")))) |>
               config(displayModeBar=FALSE))
    }
    plot_ly(df, type="choropleth", locations=~stateCode, locationmode="USA-states",
            z=~get(input$metric), colorscale="Blues",
            colorbar=list(title=metric_label),
            hovertemplate=paste0("<b>%{location}</b><br>", metric_label, ": %{z}<extra></extra>")) |>
      layout(geo=list(scope="usa",showlakes=TRUE,lakecolor="white",bgcolor="white"),
             paper_bgcolor="white", margin=list(l=0,r=0,t=10,b=0)) |>
      config(displayModeBar=FALSE)
  })
  
  output$trend_plot <- renderPlotly({
    df <- trend_data()
    if (nrow(df) == 0) {
      return(plot_ly() |>
               layout(xaxis=list(visible=FALSE), yaxis=list(visible=FALSE),
                      paper_bgcolor="white", plot_bgcolor="white",
                      annotations=list(list(text="No data for current selection.",
                                            x=0.5, y=0.5, xref="paper", yref="paper",
                                            showarrow=FALSE, font=list(size=14, color="#999")))) |>
               config(displayModeBar=FALSE))
    }
    spike_years <- df |> slice_max(mean_decl, n=2) |> pull(year)
    plot_ly() |>
      add_ribbons(data=df, x=~year, ymin=~ci_lower, ymax=~ci_upper,
                  fillcolor="rgba(44,127,184,0.15)", line=list(color="transparent"),
                  hoverinfo="skip", showlegend=FALSE) |>
      add_lines(data=df, x=~year, y=~mean_decl,
                line=list(color="#2c7fb8", width=2),
                hovertemplate="<b>%{x}</b><br>Mean: %{y:.2f}<extra></extra>",
                showlegend=FALSE) |>
      add_markers(data=df |> filter(!year %in% spike_years), x=~year, y=~mean_decl,
                  marker=list(color="#2c7fb8", size=5),
                  hovertemplate="<b>%{x}</b><br>Mean: %{y:.2f}<extra></extra>",
                  showlegend=FALSE) |>
      add_markers(data=df |> filter(year %in% spike_years), x=~year, y=~mean_decl,
                  marker=list(color="#cc0000", size=9),
                  hovertemplate="<b>Spike: %{x}</b><br>Mean: %{y:.2f}<extra></extra>",
                  showlegend=FALSE) |>
      layout(
        xaxis=list(title="", tickmode="linear", dtick=4, showgrid=FALSE, zeroline=FALSE),
        yaxis=list(title="Mean Declarations per State", showgrid=TRUE,
                   gridcolor="#eeeeee", zeroline=FALSE),
        plot_bgcolor="white", paper_bgcolor="white",
        margin=list(l=50,r=20,t=10,b=40)
      ) |>
      config(displayModeBar=FALSE)
  })
  
  
  # page 2 v2 reactives
  
  # filter observers
  observe({
    d <- scoped_data
    if (input$incident_type2 != "All") d <- filter(d, incidentType == input$incident_type2)
    choices <- c("All", sort(unique(d$declarationType)))
    updateSelectInput(session, "decl_type2", choices=choices,
                      selected=if (input$decl_type2 %in% choices) input$decl_type2 else "All")
  }) |> bindEvent(input$incident_type2)
  
  observe({
    d <- scoped_data
    if (input$decl_type2 != "All") d <- filter(d, declarationType == input$decl_type2)
    choices <- c("All", sort(unique(d$incidentType)))
    updateSelectInput(session, "incident_type2", choices=choices,
                      selected=if (input$incident_type2 %in% choices) input$incident_type2 else "All")
  }) |> bindEvent(input$decl_type2)
  
  #...filtered data for page 2
  filtered2 <- reactive({
    d <- scoped_data |> filter(year >= input$train_years2[1], year <= input$train_years2[2])
    if (input$incident_type2 != "All") d <- filter(d, incidentType == input$incident_type2)
    if (input$decl_type2 != "All")    d <- filter(d, declarationType == input$decl_type2)
    d
  })
  
  panel2 <- reactive({
    f2 <- filtered2()
    
    # full state year panel for the selected range
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
    tryCatch({
      panel2() |>
        group_by(stateCode) |>
        summarise(
          predicted = {
            d <- cur_data()
            if (n_distinct(d$year) < 3) NA_real_
            else max(0, predict(lm(n_declarations ~ year, data = d),
                                newdata = data.frame(year = fy)))
          },
          .groups = "drop"
        )
    }, error = function(e) tibble(stateCode = character(), predicted = numeric()))
  })
  
  # Capture state click using native Plotly events (no JS required)
  sel_state_rv <- reactiveVal(NULL)
  
  observeEvent(event_data("plotly_click", source = "v2map"), {
    ed <- event_data("plotly_click", source = "v2map")
    if (!is.null(ed)) {
      # Try key first (mapped below), then location, then customdata
      raw_code <- if (!is.null(ed$key)) ed$key 
      else if (!is.null(ed$location)) ed$location
      else if (!is.null(ed$customdata)) ed$customdata
      else NULL
      
      state_code <- toupper(trimws(as.character(raw_code)))
      if (length(state_code) > 0 && nchar(state_code) == 2) {
        sel_state_rv(state_code)
      }
    }
  }, ignoreNULL = TRUE)
  
  # neighbor states via sf::st_touches()
  neighbor_states2 <- reactive({
    sel <- sel_state_rv()
    req(!is.null(adj_global), !is.null(us_states_sf), !is.null(sel))
    idx <- which(us_states_sf$STUSPS == sel)
    if (length(idx) == 0) return(character(0))
    us_states_sf$STUSPS[adj_global[[idx]]]
  })
  
  # distance data via sf::st_distance() centroid matrix
  dist_df2 <- reactive({
    sel <- sel_state_rv()
    req(!is.null(dist_matrix_km), !is.null(sel), sel %in% rownames(dist_matrix_km))
    
    state_tots <- panel2() |>
      group_by(stateCode) |>
      summarise(total_decl = sum(n_declarations), .groups = "drop")
    
    global_mean <- mean(state_tots$total_decl, na.rm = TRUE)
    
    tibble(
      stateCode = rownames(dist_matrix_km),
      dist_km   = dist_matrix_km[sel, ]
    ) |>
      filter(stateCode != sel) |>
      left_join(state_tots, by = "stateCode") |>
      mutate(
        total_decl = replace_na(total_decl, 0),
        above_avg  = total_decl > global_mean
      )
  })
  
  
  #...big map
  output$v2_main_map <- renderPlotly({
    sf_d <- sf_data2()
    sel  <- sel_state_rv()
    
    if (is.null(sf_d) || nrow(sf_d) == 0) {
      return(
        plotly_empty(type = "scatter", mode = "markers") |>
          layout(
            annotations = list(list(
              text = "No data available. Try adjusting the filters.",
              x=0.5, y=0.5, xref="paper", yref="paper",
              showarrow=FALSE, font=list(size=14, color="#999")
            )),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          ) |>
          config(displayModeBar = FALSE)
      )
    }
    
    df <- sf_d |> st_drop_geometry()
    if (input$map_mode2 == "forecast") {
      fc <- tryCatch(forecast_data2(), error = function(e) NULL)
      if (!is.null(fc) && nrow(fc) > 0) {
        df <- df |> left_join(fc, by = c("STUSPS" = "stateCode")) |>
          mutate(z_val   = replace_na(predicted, 0),
                 z_label = paste0("Predicted (", input$forecast_year2, ")"))
      } else {
        df <- df |> mutate(z_val = total_decl, z_label = "Total Declarations")
      }
    } else {
      df <- df |> mutate(z_val = total_decl, z_label = "Total Declarations")
    }
    
    z_rng <- range(df$z_val, na.rm = TRUE)
    # Safety: ensure z_rng is valid and not NA
    if (any(is.na(z_rng)) || any(is.infinite(z_rng))) {
      z_rng <- c(0, 1)
    } else if (diff(z_rng) == 0) {
      z_rng <- c(z_rng[1] - 1, z_rng[1] + 1)
    }
    
    p <- plot_ly(source = "v2map") |>
      add_trace(
        type         = "choropleth",
        data         = df,
        locations    = ~STUSPS,
        locationmode = "USA-states",
        z            = ~z_val,
        zmin         = z_rng[1], zmax = z_rng[2],
        colorscale   = "Viridis",
        reversescale = FALSE,
        colorbar     = list(title = df$z_label[1], thickness = 14),
        marker       = list(line = list(color = "white", width = 0.5)),
        customdata   = ~STUSPS,
        key          = ~STUSPS,
        hovertemplate = paste0(
          "<b>%{location}</b><br>",
          df$z_label[1], ": %{z:.0f}",
          "<br><i>Click to explore spatial relationships</i>",
          "<extra></extra>"
        )
      )
    
    # orange border highlight overlay for selected state
    if (!is.null(sel) && sel %in% df$STUSPS) {
      df_sel <- df |> filter(STUSPS == sel)
      p <- p |> add_trace(
        type         = "choropleth",
        data         = df_sel,
        locations    = ~STUSPS,
        locationmode = "USA-states",
        z            = ~z_val,
        zmin         = z_rng[1], zmax = z_rng[2],
        colorscale   = "Viridis",
        showscale    = FALSE,
        marker       = list(line = list(color = "#FF4500", width = 4)),
        customdata   = ~STUSPS,
        key          = ~STUSPS,
        hovertemplate = paste0(
          "<b>%{location}  ★ SELECTED</b><br>",
          df$z_label[1], ": %{z:.0f}<extra></extra>"
        )
      )
    }
    
    p |>
      layout(
        geo = list(scope="usa", showlakes=TRUE, lakecolor="white",
                   bgcolor="white", showframe=FALSE),
        paper_bgcolor = "white",
        margin        = list(l=0, r=0, t=10, b=0),
        showlegend    = FALSE
      ) |>
      event_register("plotly_click") |>
      config(displayModeBar=FALSE)
  })
  
  # selected state banner
  output$v2_sel_banner <- renderUI({
    sel <- sel_state_rv()
    if (is.null(sel)) {
      tags$div(
        class = "no-sel-hint",
        tags$span(style="font-size:1.1rem;"),
        tags$strong(" Click any state on the map"),
        " to reveal adjacency analysis, distance decay, and spatial metrics below."
      )
    } else {
      state_name <- tryCatch(
        us_states_sf$NAME[us_states_sf$STUSPS == sel][1], error = function(e) sel
      )
      tags$div(
        class = "sel-banner",
        tags$span("Selected State: "),
        tags$strong(paste0(state_name, "  (", sel, ")")),
        tags$span(
          style = "float:right; color:#c07030; font-size:0.8rem; font-weight:400;",
          "All charts and metrics below have updated"
        )
      )
    }
  })
  
  output$v2_map_title <- renderText({
    if (input$map_mode2 == "forecast") {
      paste0("Forecast Declarations (", input$forecast_year2,
             ")  —  lm() Trend Extrapolation by State")
    } else {
      paste0("Historical Total Declarations (",
             input$train_years2[1], "–", input$train_years2[2], ")  by State")
    }
  })
  output$v2_map_subtitle <- renderText({
    sel <- sel_state_rv()
    paste0(
      "Incident: ", input$incident_type2,
      "  |  Declaration: ", input$decl_type2,
      "  |  Viridis scale: purple = low, yellow = high  |  ",
      if (!is.null(sel)) paste0("Selected: ", sel, "  (orange border)")
      else "Click a state to explore spatial relationships"
    )
  })
  
  
  # value boxes
  
  output$vb2_nearest <- renderText({
    sel <- sel_state_rv()
    if (is.null(sel)) return("Select a state")
    df <- tryCatch(dist_df2(), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return("N/A")
    above <- df |> filter(above_avg)
    if (nrow(above) == 0) return("None above avg")
    nearest <- above |> slice_min(dist_km, n=1, with_ties=FALSE)
    paste0(nearest$stateCode, "  (", round(nearest$dist_km), " km)")
  })
  
  output$vb2_cluster <- renderText({
    sel <- sel_state_rv()
    if (is.null(sel)) return("Select a state")
    nbrs <- tryCatch(neighbor_states2(), error = function(e) character(0))
    if (length(nbrs) == 0) return("N/A")
    tots <- panel2() |>
      group_by(stateCode) |>
      summarise(total_decl = sum(n_declarations), .groups="drop")
    sel_val  <- tots |> filter(stateCode == sel) |> pull(total_decl)
    nbr_mean <- tots |> filter(stateCode %in% nbrs) |>
      summarise(m = mean(total_decl, na.rm=TRUE)) |> pull(m)
    if (length(sel_val)==0 || is.na(nbr_mean) || nbr_mean==0) return("N/A")
    paste0(round(sel_val / nbr_mean, 2), "×  neighbor mean")
  })
  
  output$vb2_density <- renderText({
    sel <- sel_state_rv()
    if (is.null(sel)) return("Select a state")
    sf_d <- tryCatch(sf_data2(), error=function(e) NULL)
    if (is.null(sf_d)) return("N/A")
    row <- sf_d |> filter(STUSPS == sel) |> st_drop_geometry()
    if (nrow(row)==0) return("N/A")
    paste0(round(row$decl_density, 2), " / 1,000 km²")
  })
  
  
  # plot 1: selected state vs. neighbor mean trend
  output$v2_plot1_title <- renderText({
    sel <- sel_state_rv()
    if (is.null(sel)) "Annual Declarations by State — Select a State to Compare"
    else paste0(sel, "  vs.  Neighbor States — Annual Declarations")
  })
  output$v2_plot1_sub <- renderText({
    sel <- sel_state_rv()
    if (is.null(sel)) return("Click a state on the map above")
    nbrs <- tryCatch(neighbor_states2(), error=function(e) character(0))
    paste0(length(nbrs), " adjacent states (sf::st_touches)  |  ",
           input$train_years2[1], "–", input$train_years2[2],
           "  |  Incident: ", input$incident_type2)
  })
  
  output$v2_trend_compare <- renderPlotly({
    sel <- sel_state_rv()
    
    if (is.null(sel)) {
      return(
        plotly_empty(type = "scatter", mode = "markers") |>
          layout(xaxis=list(visible=FALSE), yaxis=list(visible=FALSE),
                 paper_bgcolor="white", plot_bgcolor="white",
                 annotations=list(list(
                   text = "← Click a state on the map to see its trend vs. neighboring states",
                   x=0.5, y=0.5, xref="paper", yref="paper",
                   showarrow=FALSE, font=list(size=13, color="#bbb")
                 ))) |>
          config(displayModeBar=FALSE)
      )
    }
    
    nbrs <- tryCatch(neighbor_states2(), error=function(e) character(0))
    p2   <- panel2()
    
    sel_ts <- p2 |> filter(stateCode == sel) |> arrange(year)
    nbr_ts <- if (length(nbrs) > 0) {
      p2 |> filter(stateCode %in% nbrs) |>
        group_by(year) |>
        summarise(mean_decl = mean(n_declarations), .groups="drop") |>
        arrange(year)
    } else tibble(year=integer(), mean_decl=numeric())
    
    if (nrow(sel_ts) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") |>
               layout(annotations=list(list(
                 text=paste("No data for", sel, "in current filters."),
                 x=0.5, y=0.5, xref="paper", yref="paper",
                 showarrow=FALSE, font=list(size=13, color="#999")
               )), paper_bgcolor="white") |> config(displayModeBar=FALSE))
    }
    
    p <- plot_ly() |>
      add_lines(
        data = sel_ts, x = ~year, y = ~n_declarations,
        name = sel,
        line = list(color="#FF4500", width=2.5),
        hovertemplate = paste0("<b>", sel, " (%{x})</b><br>Declarations: %{y}<extra></extra>")
      )
    
    if (nrow(nbr_ts) > 0) {
      p <- p |> add_lines(
        data = nbr_ts, x = ~year, y = ~mean_decl,
        name = "Neighbor Avg",
        line = list(color="#2c7fb8", width=2, dash="dot"),
        hovertemplate = "<b>Neighbor Avg (%{x})</b><br>Mean: %{y:.1f}<extra></extra>"
      )
    }
    
    if (nrow(nbr_ts) > 0 && nrow(sel_ts) > 0) {
      merged <- sel_ts |>
        inner_join(nbr_ts, by="year") |>
        mutate(gap = abs(n_declarations - mean_decl))
      if (nrow(merged) > 0) {
        peak <- merged |> slice_max(gap, n=1, with_ties=FALSE)
        p <- p |> add_annotations(
          x = peak$year, y = peak$n_declarations,
          text = paste0("Peak gap: ", round(peak$gap, 1)),
          showarrow = TRUE, arrowhead = 2, arrowsize = 0.8,
          font = list(size=11, color="#555"), ax=30, ay=-30
        )
      }
    }
    
    p |>
      layout(
        legend = list(orientation="h", x=0, y=-0.18),
        xaxis  = list(title="", showgrid=FALSE, zeroline=FALSE,
                      tickmode="linear", dtick=4),
        yaxis  = list(title="Annual Declarations", showgrid=TRUE,
                      gridcolor="#eeeeee", zeroline=FALSE),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin = list(l=50, r=20, t=10, b=55)
      ) |>
      config(displayModeBar=FALSE)
  })
  
  
  # plot 2: distance decay
  output$v2_plot2_sub <- renderText({
    sel <- sel_state_rv()
    if (is.null(sel)) return("Click a state to see how declarations vary with distance")
    paste0("sf::st_distance() centroid-to-centroid distances from ", sel,
           "  |  Blue diamonds = adjacent states  |  Dashed line = linear fit")
  })
  
  output$v2_distance_decay <- renderPlotly({
    sel <- sel_state_rv()
    
    if (is.null(sel)) {
      return(
        plotly_empty(type = "scatter", mode = "markers") |>
          layout(xaxis=list(visible=FALSE), yaxis=list(visible=FALSE),
                 paper_bgcolor="white", plot_bgcolor="white",
                 annotations=list(list(
                   text="← Click a state on the map to see the distance decay pattern",
                   x=0.5, y=0.5, xref="paper", yref="paper",
                   showarrow=FALSE, font=list(size=13, color="#bbb")
                 ))) |>
          config(displayModeBar=FALSE)
      )
    }
    
    df <- tryCatch(dist_df2(), error=function(e) NULL)
    if (is.null(df) || nrow(df) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") |> layout(
        annotations=list(list(text="Distance data unavailable.",
                              x=0.5,y=0.5,xref="paper",yref="paper",
                              showarrow=FALSE, font=list(size=13,color="#999"))),
        paper_bgcolor="white") |> config(displayModeBar=FALSE))
    }
    
    nbrs <- tryCatch(neighbor_states2(), error=function(e) character(0))
    df   <- df |> mutate(is_neighbor = stateCode %in% nbrs)
    
    trend_df <- if (nrow(df) >= 5) {
      fit   <- lm(total_decl ~ dist_km, data=df)
      df |> arrange(dist_km) |> (\(d) mutate(d, fitted = predict(fit, newdata=d)))()
    } else NULL
    
    non_nbr <- df |> filter(!is_neighbor)
    nbr_df  <- df |> filter(is_neighbor)
    
    p <- plot_ly() |>
      add_markers(
        data = non_nbr,
        x=~dist_km, y=~total_decl, text=~stateCode,
        marker=list(
          color = ifelse(non_nbr$above_avg, "#d73027", "#aaaaaa"),
          size=7, opacity=0.75,
          line=list(color="white", width=0.5)
        ),
        name="Other States",
        hovertemplate="<b>%{text}</b><br>Distance: %{x:.0f} km<br>Declarations: %{y}<extra></extra>"
      )
    
    if (nrow(nbr_df) > 0) {
      p <- p |> add_markers(
        data = nbr_df,
        x=~dist_km, y=~total_decl, text=~stateCode,
        marker=list(color="#2c7fb8", size=12, symbol="diamond",
                    line=list(color="white", width=1.5)),
        name="Adjacent (st_touches)",
        hovertemplate="<b>%{text}  [neighbor]</b><br>Distance: %{x:.0f} km<br>Declarations: %{y}<extra></extra>"
      )
    }
    
    if (!is.null(trend_df)) {
      p <- p |> add_lines(
        data=trend_df, x=~dist_km, y=~fitted,
        line=list(color="#888", dash="dash", width=1.5),
        name="Linear Trend",
        hoverinfo="skip"
      )
    }
    
    p |>
      layout(
        legend = list(orientation="h", x=0, y=-0.2),
        xaxis  = list(title=paste0("Distance from ", sel, " (km)"),
                      showgrid=TRUE, gridcolor="#eeeeee", zeroline=FALSE),
        yaxis  = list(title="Total Declarations", showgrid=TRUE,
                      gridcolor="#eeeeee", zeroline=FALSE),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin = list(l=55, r=20, t=10, b=65)
      ) |>
      config(displayModeBar=FALSE)
  })
  
  
  # text chunk 1: adjacency analysis
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
    
    nbrs <- tryCatch(neighbor_states2(), error=function(e) character(0))
    tots <- tryCatch({
      panel2() |>
        group_by(stateCode) |>
        summarise(total_decl = sum(n_declarations), .groups="drop")
    }, error=function(e) NULL)
    
    if (is.null(tots) || nrow(tots)==0) return("Insufficient data for adjacency analysis.")
    
    global_mean <- mean(tots$total_decl, na.rm=TRUE)
    sel_decl    <- tots |> filter(stateCode==sel) |> pull(total_decl)
    nbr_decls   <- tots |> filter(stateCode %in% nbrs) |> pull(total_decl)
    nbr_mean    <- if (length(nbr_decls)>0) mean(nbr_decls, na.rm=TRUE) else NA_real_
    
    if (length(sel_decl) == 0 || is.na(sel_decl))
      return("No declarations recorded for this state in the current filter window.")
    
    # Check for valid math before if()
    if (is.na(global_mean) || global_mean == 0) return("National average is unavailable.")
    
    pct_vs_nat  <- round((sel_decl / global_mean - 1) * 100, 1)
    
    above_below <- if (!is.na(pct_vs_nat) && pct_vs_nat >= 0)
      paste0(pct_vs_nat, "% above") else paste0(abs(pct_vs_nat), "% below")
    
    nbr_compare <- if (!is.na(nbr_mean) && nbr_mean > 0) {
      ratio <- sel_decl / nbr_mean
      if (ratio > 1.2) "notably higher than"
      else if (ratio < 0.8) "notably lower than"
      else "roughly on par with"
    } else "comparable to"
    
    interpretation <- if (nbr_compare == "notably higher than")
      "This pattern suggests a localized hotspot: the selected state bears a disproportionate disaster burden even within an already exposed region."
    else if (nbr_compare == "notably lower than")
      "This pattern suggests resilience within a higher-exposure region: the state is insulated relative to its surroundings despite geographic proximity."
    else
      "Declaration exposure is distributed evenly across this regional cluster, suggesting broad regional risk rather than a single focal point."
    
    paste0(
      sel, " shares a border with ", length(nbrs),
      if (length(nbrs)==1) " state (" else " states (",
      paste(nbrs, collapse=", "),
      ") according to sf::st_touches(). Over ", input$train_years2[1], "–",
      input$train_years2[2], ", ", sel, " recorded ", comma(sel_decl),
      " total declarations : ", above_below, " the national state average of ",
      round(global_mean, 1), ". Its count is ", nbr_compare,
      " its neighbor average",
      if (!is.na(nbr_mean)) paste0(" (", round(nbr_mean, 1), " declarations)") else "",
      ". ", interpretation
    )
  })
  
  
  # text chunk 2: distance analysis
  output$v2_text_distance <- renderText({
    sel <- sel_state_rv()
    
    if (is.null(sel)) {
      return(paste0(
        "Select a state on the map to see a distance-based spatial analysis. ",
        "This panel uses sf::st_distance() on state centroids to compute exact ",
        "centroid-to-centroid distances in km, then examines how declaration frequency ",
        "varies with distance : testing for spatial decay or diffuse risk patterns."
      ))
    }
    
    df <- tryCatch(dist_df2(), error=function(e) NULL)
    if (is.null(df) || nrow(df)==0)
      return("Distance data unavailable for the current filter.")
    
    above_avg_df  <- df |> filter(above_avg)
    nearest_above <- if (nrow(above_avg_df)>0)
      above_avg_df |> slice_min(dist_km, n=1, with_ties=FALSE) else NULL
    farthest_above <- if (nrow(above_avg_df)>0)
      above_avg_df |> slice_max(dist_km, n=1, with_ties=FALSE) else NULL
    
    cor_val <- if (nrow(df) >= 5 && sd(df$dist_km, na.rm=TRUE) > 0 &&
                   sd(df$total_decl, na.rm=TRUE) > 0) {
      cor(df$dist_km, df$total_decl, use="complete.obs")
    } else NA_real_
    
    cor_desc <- if (!is.na(cor_val)) {
      if (cor_val < -0.25)
        paste0("a meaningful negative relationship (r = ", round(cor_val,2),
               ") : states geographically close to ", sel,
               " tend to have higher declaration counts, consistent with spatial clustering of disaster risk")
      else if (cor_val > 0.25)
        paste0("a positive relationship (r = ", round(cor_val,2),
               ") : higher declaration counts are more common in distant states, suggesting ",
               sel, " is a lower-risk anchor in a regionally concentrated risk zone")
      else
        paste0("little distance-based correlation (r = ", round(cor_val,2),
               ") : disaster risk does not exhibit a strong spatial decay pattern from ", sel,
               ", suggesting diffuse national exposure")
    } else "insufficient data for a correlation estimate in the current filter window"
    
    paste0(
      "Using sf::st_distance() on state centroids, ",
      nrow(df), " pairwise distances from ", sel, " were computed. ",
      if (!is.null(nearest_above))
        paste0("The nearest above-average state is ", nearest_above$stateCode,
               " (", round(nearest_above$dist_km), " km), ",
               "while the most distant above-average state is ", farthest_above$stateCode,
               " (", round(farthest_above$dist_km), " km). ")
      else
        "No above-average declaration states were found under the current filters. ",
      "Across all state pairs, there is ", cor_desc, "."
    )
  })
  
}


# launch
shinyApp(ui, server)
