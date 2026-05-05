library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(scales)
library(lubridate)

# LOADING DATA, note was using a api but easier to read directly from the CSV
scoped_data <- read_csv("FemaWebDisasterDeclarations.csv") %>%
  mutate(
    declarationDate = as_date(declarationDate),
    year            = year(declarationDate)
  ) %>%
  filter(year >= 2000, year <= 2024)

# handling implicit zeros
fema_sy <- scoped_data %>%
  group_by(stateCode, year) %>%
  summarise(n_declarations = n(), .groups = "drop")

full_panel <- expand.grid(
  stateCode = unique(fema_sy$stateCode),
  year      = unique(fema_sy$year)
) %>%
  left_join(fema_sy, by = c("stateCode", "year")) %>%
  mutate(n_declarations = replace_na(n_declarations, 0))

# timetrends with CI --> got from hw1
time_trends <- full_panel %>%
  group_by(year) %>%
  summarise(
    mean_decl = mean(n_declarations),
    se        = sd(n_declarations) / sqrt(n()),
    ci_lower  = mean(n_declarations) - 1.96 * (sd(n_declarations) / sqrt(n())),
    ci_upper  = mean(n_declarations) + 1.96 * (sd(n_declarations) / sqrt(n())),
    .groups   = "drop"
  ) %>%
  arrange(year)

incident_choices <- c("All", sort(unique(scoped_data$incidentType)))
decl_choices     <- c("All", sort(unique(scoped_data$declarationType)))
metric_choices   <- c(
  "Total Declarations"    = "n_declarations",
  "Unique Incident Types" = "n_incident_types"
)

#NOTE - UI
ui <- page_fluid(
  title = "FEMA Disaster Declarations (2000-2024)",
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("IBM Plex Sans")),
  
  tags$head(tags$style(HTML("
    body { background-color: #ffffff; }
    .card { border: 1px solid #e0e0e0; box-shadow: none; }
    .card-header { font-weight: 600; font-size: 0.95rem; background: #ffffff; border-bottom: 1px solid #e0e0e0; color: #222; }
    .subtitle-text { color: #888; font-size: 0.85rem; padding: 4px 12px 8px 12px; }
    @media (max-width: 768px) { .bslib-column { width: 100% !important; } }
  "))),
  
  # filters -> dropdowns
  card(
    class = "mb-3",
    card_body(
      layout_columns(
        col_widths = c(4, 4, 4),
        selectInput("incident_type", "Incident Type", choices = incident_choices, selected = "All"),
        selectInput("decl_type", "Declaration Type", choices = decl_choices, selected = "All"),
        selectInput("metric", "Map Metric", choices = metric_choices, selected = "n_declarations")
      ),
      sliderInput("year_range", "Year Range", min = 2000, max = 2024,
                  value = c(2000, 2024), sep = "", width = "100%")
    )
  ),
  
  # text --> metric description
  card(
    class = "mb-3",
    card_body(
      padding = "12px",
      tags$p(style = "margin: 0; font-size: 0.9rem; color: #333;",
             tags$strong("Metric 1: "),
             "Total federal disaster declarations per U.S. state or territory (2000-2024). A declaration means that a disaster exceeded local and state response capacity, triggering federal aid."
      ),
      tags$p(style = "margin: 8px 0 0 0; font-size: 0.9rem; color: #333;",
             tags$strong("Metric 2: "),
             "Annual mean declarations per state with 95% confidence intervals. This metric tracks whether the average disaster burden on U.S. states is growing over time."
      )
    )
  ),
  
  # Value boxes
  layout_columns(
    fill = FALSE,
    col_widths = c(3, 3, 3, 3),
    value_box(title = "Total Declarations", value = textOutput("vb_total"),
              theme = value_box_theme(bg = "#ffffff", fg = "#222222")),
    value_box(title = "Most Common Incident", value = textOutput("vb_incident"),
              theme = value_box_theme(bg = "#ffffff", fg = "#222222")),
    value_box(title = "States / Territories Affected", value = textOutput("vb_states"),
              theme = value_box_theme(bg = "#ffffff", fg = "#222222")),
    value_box(title = "Most Recent Declaration", value = textOutput("vb_recent"),
              theme = value_box_theme(bg = "#ffffff", fg = "#222222"))
  ),
  
  # Map + CI trend chart side by side
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
)

# NOTE - SERVER
server <- function(input, output, session) {
  
  filtered <- reactive({
    d <- scoped_data %>%
      filter(year >= input$year_range[1], year <= input$year_range[2])
    if (input$incident_type != "All") d <- filter(d, incidentType == input$incident_type)
    if (input$decl_type != "All")    d <- filter(d, declarationType == input$decl_type)
    d
  })
  
  map_data <- reactive({
    filtered() %>%
      group_by(stateCode) %>%
      summarise(
        n_declarations   = n(),
        n_incident_types = n_distinct(incidentType),
        .groups = "drop"
      )
  })
  
  # reactive CI trend
  trend_data <- reactive({
    # filter by year range
    d <- full_panel %>%
      filter(year >= input$year_range[1], year <= input$year_range[2])
    
    # if incident or decl type filtered = reaggregate from scoped_data
    if (input$incident_type != "All" || input$decl_type != "All") {
      filtered_raw <- scoped_data %>%
        filter(year >= input$year_range[1], year <= input$year_range[2])
      if (input$incident_type != "All")
        filtered_raw <- filter(filtered_raw, incidentType == input$incident_type)
      if (input$decl_type != "All")
        filtered_raw <- filter(filtered_raw, declarationType == input$decl_type)
      
      sy <- filtered_raw %>%
        group_by(stateCode, year) %>%
        summarise(n_declarations = n(), .groups = "drop")
      
      d <- expand.grid(
        stateCode = unique(full_panel$stateCode),
        year      = unique(full_panel$year[full_panel$year >= input$year_range[1] &
                                             full_panel$year <= input$year_range[2]])
      ) %>%
        left_join(sy, by = c("stateCode", "year")) %>%
        mutate(n_declarations = replace_na(n_declarations, 0))
    }
    
    d %>%
      group_by(year) %>%
      summarise(
        mean_decl = mean(n_declarations),
        ci_lower  = mean(n_declarations) - 1.96 * (sd(n_declarations) / sqrt(n())),
        ci_upper  = mean(n_declarations) + 1.96 * (sd(n_declarations) / sqrt(n())),
        .groups   = "drop"
      ) %>%
      arrange(year)
  })
  
  output$vb_total    <- renderText({ comma(nrow(filtered())) })
  output$vb_incident <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("N/A")
    names(which.max(table(d$incidentType)))
  })
  output$vb_states   <- renderText({ as.character(n_distinct(filtered()$stateCode)) })
  output$vb_recent   <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("N/A")
    format(max(d$declarationDate, na.rm = TRUE), "%b %d, %Y")
  })
  
  output$map_title <- renderText({
    metric_label <- names(metric_choices)[metric_choices == input$metric]
    paste("U.S. Map:", metric_label, "by State")
  })
  output$map_subtitle <- renderText({
    paste0("Showing ", input$year_range[1], "-", input$year_range[2],
           "  |  Incident: ", input$incident_type,
           "  |  Declaration: ", input$decl_type)
  })
  output$trend_subtitle <- renderText({
    paste0(input$year_range[1], "-", input$year_range[2],
           "  |  Incident: ", input$incident_type,
           "  |  Declaration: ", input$decl_type)
  })
  
  output$choropleth <- renderPlotly({
    df <- map_data()
    metric_label <- names(metric_choices)[metric_choices == input$metric]
    
    if (nrow(df) == 0) {
      plot_ly(type = "choropleth", locations = character(0),
              locationmode = "USA-states", z = numeric(0), colorscale = "Blues") %>%
        layout(geo = list(scope = "usa", showlakes = TRUE, lakecolor = "white", bgcolor = "white"),
               paper_bgcolor = "white", margin = list(l=0,r=0,t=10,b=0),
               annotations = list(list(text = "No data for current filter selection",
                                       x=0.5, y=0.5, xref="paper", yref="paper",
                                       showarrow=FALSE, font=list(size=14, color="#999")))) %>%
        config(displayModeBar = FALSE)
    } else {
      plot_ly(df, type = "choropleth", locations = ~stateCode,
              locationmode = "USA-states", z = ~get(input$metric),
              colorscale = "Blues", colorbar = list(title = metric_label),
              hovertemplate = paste0("<b>%{location}</b><br>", metric_label, ": %{z}<extra></extra>")) %>%
        layout(geo = list(scope = "usa", showlakes = TRUE, lakecolor = "white", bgcolor = "white"),
               paper_bgcolor = "white", margin = list(l=0,r=0,t=10,b=0)) %>%
        config(displayModeBar = FALSE)
    }
  })
  
  output$trend_plot <- renderPlotly({
    df <- trend_data()
    
    if (nrow(df) == 0) {
      return(plot_ly() %>%
               layout(xaxis = list(visible=FALSE), yaxis = list(visible=FALSE),
                      paper_bgcolor = "white", plot_bgcolor = "white",
                      annotations = list(list(text = "No data for current filter selection",
                                              x=0.5, y=0.5, xref="paper", yref="paper",
                                              showarrow=FALSE, font=list(size=14, color="#999")))) %>%
               config(displayModeBar = FALSE))
    }
    
    spike_years <- df %>% slice_max(mean_decl, n = 2) %>% pull(year)
    
    plot_ly() %>%
      # 95% CI ribbon
      add_ribbons(data = df, x = ~year, ymin = ~ci_lower, ymax = ~ci_upper,
                  fillcolor = "rgba(44, 127, 184, 0.15)",
                  line = list(color = "transparent"),
                  hoverinfo = "skip", showlegend = FALSE) %>%
      # mean
      add_lines(data = df, x = ~year, y = ~mean_decl,
                line = list(color = "#2c7fb8", width = 2),
                hovertemplate = "<b>%{x}</b><br>Mean: %{y:.2f}<extra></extra>",
                showlegend = FALSE) %>%
      # points
      add_markers(data = df %>% filter(!year %in% spike_years),
                  x = ~year, y = ~mean_decl,
                  marker = list(color = "#2c7fb8", size = 5),
                  hovertemplate = "<b>%{x}</b><br>Mean: %{y:.2f}<extra></extra>",
                  showlegend = FALSE) %>%
      # spike = red
      add_markers(data = df %>% filter(year %in% spike_years),
                  x = ~year, y = ~mean_decl,
                  marker = list(color = "#cc0000", size = 9),
                  hovertemplate = "<b>Spike: %{x}</b><br>Mean: %{y:.2f}<extra></extra>",
                  showlegend = FALSE) %>%
      layout(
        xaxis = list(title = "", tickmode = "linear", dtick = 4,
                     showgrid = FALSE, zeroline = FALSE),
        yaxis = list(title = "Mean Declarations per State", showgrid = TRUE,
                     gridcolor = "#eeeeee", zeroline = FALSE),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin = list(l=50, r=20, t=10, b=40)
      ) %>%
      config(displayModeBar = FALSE)
  })
}

# NOTE - RUNAPP
shinyApp(ui, server)
