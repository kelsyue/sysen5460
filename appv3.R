library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(scales)
library(sf)
library(tigris)
library(viridis)
library(lubridate)

# --- Data Loading ---

# 1. FEMA Declarations
# Assumes running from within 'finalproject' directory
fema_data <- read_csv("FemaWebDisasterDeclarations.csv", show_col_types = FALSE) |>
  mutate(
    declarationDate = as_date(declarationDate),
    year = year(declarationDate)
  ) |>
  filter(year >= 2000, year <= 2024)

# 2. Direct Loss Data (Economic Loss)
loss_data_raw <- read_csv("direct_loss_aggregated_output_30125.csv", show_col_types = FALSE)

# State mapping for loss data (it has StateName, we need stateCode)
state_map <- tibble(
  StateName = toupper(state.name),
  stateCode = state.abb
) |> 
  add_row(StateName = "DISTRICT OF COLUMBIA", stateCode = "DC") |>
  add_row(StateName = "PUERTO RICO", stateCode = "PR")

loss_data <- loss_data_raw |>
  left_join(state_map, by = "StateName") |>
  mutate(
    County_FIPS = sprintf("%05d", as.numeric(County_FIPS))
  )

# 3. Spatial Data
us_states_sf <- tryCatch(
  {
    tigris::states(cb = TRUE, resolution = "20m", year = 2022, progress_bar = FALSE) |>
      st_transform(crs = 4326) |>
      filter(!STUSPS %in% c("AS", "GU", "MP", "PR", "VI", "UM")) |>
      select(STUSPS, NAME, geometry)
  },
  error = function(e) NULL
)

# Fixed Path: removed 'finalproject/' prefix
us_counties_sf <- readRDS("us_counties_sf.rds") |>
  st_transform(crs = 4326)

# Pre-calculate distances and adjacency for spatial analysis
dist_matrix_km <- tryCatch(
  {
    cents <- suppressWarnings(st_centroid(us_states_sf))
    dm <- st_distance(cents)
    dm_mat <- matrix(as.numeric(dm) / 1000, nrow = nrow(cents), dimnames = list(cents$STUSPS, cents$STUSPS))
    dm_mat
  },
  error = function(e) NULL
)

adj_global <- tryCatch(
  {
    st_touches(us_states_sf)
  },
  error = function(e) NULL
)

# --- Constants & Choices ---
fema_incident_choices <- c("All", sort(unique(fema_data$incidentType)))
loss_hazard_choices <- c("All", sort(unique(loss_data$Hazard)))

metric_choices_fema <- c(
  "Total Declarations" = "n_declarations",
  "Unique Incident Types" = "n_incident_types"
)

metric_choices_loss <- c(
  "Total Fatalities" = "Fatalities",
  "Total Injuries" = "Injuries",
  "Property Damage ($)" = "PropertyDmg",
  "Crop Damage ($)" = "CropDmg",
  "Duration (Days)" = "Duration_Days"
)

# --- UI ---
ui <- page_navbar(
  title = "Hazard & Loss Analytics Dashboard",
  theme = bs_theme(
    version = 5,
    bootswatch = "yeti",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  header = tags$head(tags$style(HTML("
    :root { --bs-primary: #2563eb; }
    body { background-color: #f8fafc; font-family: 'Inter', sans-serif; }
    .card { border: none; border-radius: 12px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); margin-bottom: 20px; }
    .card-header { background-color: #ffffff; border-bottom: 1px solid #f1f5f9; font-weight: 600; color: #1e293b; border-radius: 12px 12px 0 0 !important; }
    .value-box { border-radius: 12px !important; }
    .leaflet-container { background: #f8fafc; border-radius: 8px; }
    .sel-banner { background: #eff6ff; color: #1e40af; padding: 12px; border-radius: 8px; margin-bottom: 15px; border-left: 4px solid #2563eb; }
    .no-sel-hint { color: #64748b; font-style: italic; text-align: center; padding: 20px; background: #f1f5f9; border-radius: 8px; }
    .analysis-text { font-size: 0.92rem; line-height: 1.6; color: #334155; }
  "))),
  
  navset_tab(
    nav_panel(
      "Overview",
      card(
        card_body(
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            selectInput("data_source", "Data Source", 
                        choices = c("FEMA Declarations" = "fema", "Economic Losses" = "loss")),
            uiOutput("conditional_hazard_input"),
            uiOutput("conditional_metric_input"),
            conditionalPanel(
              condition = "input.data_source == 'fema'",
              sliderInput("year_range", "Year Range", min = 2000, max = 2024, value = c(2000, 2024), sep = "")
            )
          )
        )
      ),
      layout_columns(
        fill = FALSE, col_widths = c(3, 3, 3, 3),
        value_box(title = textOutput("vb_title_1"), value = textOutput("vb_val_1"), theme = "primary"),
        value_box(title = textOutput("vb_title_2"), value = textOutput("vb_val_2"), theme = "secondary"),
        value_box(title = textOutput("vb_title_3"), value = textOutput("vb_val_3"), theme = "info"),
        value_box(title = textOutput("vb_title_4"), value = textOutput("vb_val_4"), theme = "dark")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header(textOutput("map_title")),
          leafletOutput("map_main", height = "550px")
        ),
        card(
          card_header("Temporal Trends (FEMA)"),
          plotlyOutput("trend_plot", height = "550px")
        )
      )
    ),
    
    nav_panel(
      "Spatial Analysis",
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          title = "Spatial Controls",
          selectInput("data_source2", "Data Source", 
                      choices = c("FEMA Declarations" = "fema", "Economic Losses" = "loss")),
          uiOutput("conditional_hazard_input2"),
          sliderInput("year_range2", "Year Range", min = 2000, max = 2024, value = c(2000, 2024), sep = ""),
          hr(),
          p("Click on the map to select a state for detailed spatial analysis."),
          p(style="font-size: 0.8rem; color: #64748b;", 
            "Spatial tools: sf::st_touches for adjacency, sf::st_distance for decay logic.")
        ),
        uiOutput("v2_sel_banner"),
        card(
          card_header("Interactive Spatial Map"),
          leafletOutput("v2_main_map", height = "500px")
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Regional Comparison"),
            plotlyOutput("v2_trend_compare", height = "350px")
          ),
          card(
            card_header("Distance Decay Analysis"),
            plotlyOutput("v2_distance_decay", height = "350px")
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Adjacency Analysis"),
            card_body(p(class="analysis-text", textOutput("v2_text_adjacency")))
          ),
          card(
            card_header("Distance Insights"),
            card_body(p(class="analysis-text", textOutput("v2_text_distance")))
          )
        )
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  sel_state_rv <- reactiveVal(NULL)
  
  # --- Dynamic Inputs ---
  
  output$conditional_hazard_input <- renderUI({
    if (input$data_source == "fema") {
      selectInput("incident_type", "Incident Type", choices = fema_incident_choices, selected = "All")
    } else {
      selectInput("hazard_type", "Hazard Type", choices = loss_hazard_choices, selected = "All")
    }
  })
  
  output$conditional_metric_input <- renderUI({
    if (input$data_source == "fema") {
      selectInput("metric", "Map Metric", choices = metric_choices_fema, selected = "n_declarations")
    } else {
      selectInput("metric_loss", "Map Metric", choices = metric_choices_loss, selected = "PropertyDmg")
    }
  })
  
  output$conditional_hazard_input2 <- renderUI({
    if (input$data_source2 == "fema") {
      selectInput("incident_type2", "Incident Type", choices = fema_incident_choices, selected = "All")
    } else {
      selectInput("hazard_type2", "Hazard Type", choices = loss_hazard_choices, selected = "All")
    }
  })
  
  # --- Reactive Data ---
  
  # FEMA processed for map
  fema_map_data <- reactive({
    d <- fema_data |> filter(year >= input$year_range[1], year <= input$year_range[2])
    if (!is.null(input$incident_type) && input$incident_type != "All") {
      d <- d |> filter(incidentType == input$incident_type)
    }
    
    d |>
      group_by(stateCode) |>
      summarise(
        n_declarations = n(),
        n_incident_types = n_distinct(incidentType),
        .groups = "drop"
      )
  })
  
  # Loss processed for map
  loss_map_data <- reactive({
    d <- loss_data
    if (!is.null(input$hazard_type) && input$hazard_type != "All") {
      d <- d |> filter(Hazard == input$hazard_type)
    }
    
    d |>
      group_by(County_FIPS) |>
      summarise(
        Fatalities = sum(Fatalities, na.rm = TRUE),
        Injuries = sum(Injuries, na.rm = TRUE),
        PropertyDmg = sum(`PropertyDmg(ADJ 2024)`, na.rm = TRUE),
        CropDmg = sum(`CropDmg(ADJ 2024)`, na.rm = TRUE),
        Duration_Days = sum(Duration_Days, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # --- Value Boxes ---
  
  output$vb_title_1 <- renderText({
    if (input$data_source == "fema") "Total Declarations" else "Total Property Loss"
  })
  output$vb_val_1 <- renderText({
    if (input$data_source == "fema") {
      comma(sum(fema_map_data()$n_declarations))
    } else {
      paste0("$", comma(sum(loss_map_data()$PropertyDmg) / 1e6), "M")
    }
  })
  
  output$vb_title_2 <- renderText({
    if (input$data_source == "fema") "Most Common Incident" else "Total Fatalities"
  })
  output$vb_val_2 <- renderText({
    if (input$data_source == "fema") {
      d <- fema_data |> filter(year >= input$year_range[1], year <= input$year_range[2])
      if (nrow(d) == 0) return("N/A")
      names(which.max(table(d$incidentType)))
    } else {
      comma(sum(loss_map_data()$Fatalities))
    }
  })
  
  output$vb_title_3 <- renderText({
    "States Affected"
  })
  output$vb_val_3 <- renderText({
    if (input$data_source == "fema") {
      nrow(fema_map_data())
    } else {
      n_distinct(loss_data$stateCode)
    }
  })
  
  output$vb_title_4 <- renderText({
    if (input$data_source == "fema") "Recent Declaration" else "Most Impacted State"
  })
  output$vb_val_4 <- renderText({
    if (input$data_source == "fema") {
      d <- fema_data |> filter(year >= input$year_range[1], year <= input$year_range[2])
      if (nrow(d) == 0) return("N/A")
      format(max(d$declarationDate), "%b %Y")
    } else {
      st_sums <- loss_data |> group_by(stateCode) |> summarise(v = sum(`PropertyDmg(ADJ 2024)`, na.rm=T))
      st_sums$stateCode[which.max(st_sums$v)]
    }
  })
  
  # --- Main Map (Overview) ---
  
  output$map_main <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(-98.5, 39.8, 4)
  })
  
  output$map_title <- renderText({
    if (input$data_source == "fema") "FEMA Disaster Declarations by State" else "County-Level Economic Loss"
  })
  
  observe({
    proxy <- leafletProxy("map_main")
    proxy |> clearShapes() |> clearControls()
    
    if (input$data_source == "fema") {
      df <- us_states_sf |> left_join(fema_map_data(), by = c("STUSPS" = "stateCode"))
      metric <- if(is.null(input$metric)) "n_declarations" else input$metric
      df$val <- df[[metric]]
      df$val[is.na(df$val)] <- 0
      
      pal <- colorNumeric("YlOrRd", domain = df$val)
      
      proxy |>
        addPolygons(
          data = df,
          fillColor = ~pal(val),
          weight = 1,
          opacity = 1,
          color = "white",
          fillOpacity = 0.7,
          layerId = ~STUSPS,
          highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
          label = ~paste0(NAME, ": ", comma(val))
        ) |>
        addLegend(pal = pal, values = df$val, title = "Declarations", position = "bottomright")
        
    } else {
      df <- us_counties_sf |> left_join(loss_map_data(), by = c("GEOID" = "County_FIPS"))
      metric <- if(is.null(input$metric_loss)) "PropertyDmg" else input$metric_loss
      df$val <- df[[metric]]
      df$val[is.na(df$val)] <- 0
      
      pal <- colorNumeric("Viridis", domain = df$val)
      
      proxy |>
        addPolygons(
          data = df,
          fillColor = ~pal(val),
          weight = 0.5,
          opacity = 1,
          color = "white",
          fillOpacity = 0.7,
          layerId = ~GEOID,
          highlightOptions = highlightOptions(weight = 1, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
          label = ~paste0(NAME, " County: ", comma(val))
        ) |>
        addLegend(pal = pal, values = df$val, title = "Loss Metric", position = "bottomright")
    }
  })
  
  # --- Trend Plot ---
  
  output$trend_plot <- renderPlotly({
    if (input$data_source != "fema") {
      return(plot_ly() |> layout(annotations = list(text = "Trends available for FEMA data only", showarrow = F)))
    }
    
    d <- fema_data |> filter(year >= input$year_range[1], year <= input$year_range[2])
    if (!is.null(input$incident_type) && input$incident_type != "All") {
      d <- d |> filter(incidentType == input$incident_type)
    }
    
    ts <- d |> group_by(year) |> summarise(n = n())
    
    plot_ly(ts, x = ~year, y = ~n, type = "scatter", mode = "lines+markers", line = list(color = "#2563eb")) |>
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Total Declarations"),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  # --- Spatial Analysis Page ---
  
  output$v2_main_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(-98.5, 39.8, 4)
  })
  
  # Capture click on v2 map
  observeEvent(input$v2_main_map_shape_click, {
    id <- input$v2_main_map_shape_click$id
    # Ensure it's a state code (2 chars)
    if (nchar(id) == 2) {
      sel_state_rv(id)
    }
  })
  
  output$v2_sel_banner <- renderUI({
    sel <- sel_state_rv()
    if (is.null(sel)) {
      tags$div(class = "no-sel-hint", "Click a state on the map below to begin spatial analysis.")
    } else {
      name <- us_states_sf$NAME[us_states_sf$STUSPS == sel]
      tags$div(class = "sel-banner", 
               tags$strong("Selected State: "), name, paste0(" (", sel, ")"),
               span(style="float:right; font-size:0.8rem;", "Comparison & Distance decay updated."))
    }
  })
  
  # Reactive for spatial data (always state level for simplicity in V2)
  spatial_sf_data <- reactive({
    if (input$data_source2 == "fema") {
      d <- fema_data |> filter(year >= input$year_range2[1], year <= input$year_range2[2])
      if (!is.null(input$incident_type2) && input$incident_type2 != "All") {
        d <- d |> filter(incidentType == input$incident_type2)
      }
      totals <- d |> group_by(stateCode) |> summarise(val = n())
    } else {
      d <- loss_data
      if (!is.null(input$hazard_type2) && input$hazard_type2 != "All") {
        d <- d |> filter(Hazard == input$hazard_type2)
      }
      totals <- d |> group_by(stateCode) |> summarise(val = sum(`PropertyDmg(ADJ 2024)`, na.rm=T))
    }
    us_states_sf |> left_join(totals, by = c("STUSPS" = "stateCode")) |> mutate(val = replace_na(val, 0))
  })
  
  observe({
    df <- spatial_sf_data()
    pal <- colorNumeric("Blues", domain = df$val)
    proxy <- leafletProxy("v2_main_map")
    proxy |> clearShapes()
    
    proxy |>
      addPolygons(
        data = df,
        fillColor = ~pal(val),
        weight = 1, color = "white", fillOpacity = 0.7,
        layerId = ~STUSPS,
        label = ~paste0(NAME, ": ", comma(val))
      )
    
    sel <- sel_state_rv()
    if (!is.null(sel)) {
      sel_poly <- df |> filter(STUSPS == sel)
      proxy |> addPolygons(data = sel_poly, fillColor = "transparent", weight = 3, color = "#FF4500", opacity = 1, layerId = "selection")
    }
  })
  
  # Plot 1: Trend Compare (Selected vs Neighbors)
  output$v2_trend_compare <- renderPlotly({
    sel <- sel_state_rv()
    req(sel)
    
    # Neighbors
    idx <- which(us_states_sf$STUSPS == sel)
    nbr_codes <- us_states_sf$STUSPS[adj_global[[idx]]]
    
    if (input$data_source2 == "fema") {
      sel_ts <- fema_data |> filter(stateCode == sel, year >= input$year_range2[1], year <= input$year_range2[2]) |>
        group_by(year) |> summarise(n = n())
      nbr_ts <- fema_data |> filter(stateCode %in% nbr_codes, year >= input$year_range2[1], year <= input$year_range2[2]) |>
        group_by(year) |> summarise(n = n() / length(nbr_codes))
      
      plot_ly() |>
        add_lines(data = sel_ts, x = ~year, y = ~n, name = sel, line = list(color = "#FF4500")) |>
        add_lines(data = nbr_ts, x = ~year, y = ~n, name = "Neighbors (Avg)", line = list(color = "#2563eb", dash = "dot")) |>
        layout(title = "Annual FEMA Declarations", xaxis = list(title = ""), yaxis = list(title = "Count"))
    } else {
      # Loss data is not time-series in this CSV, so show a bar chart of Hazards for State vs Neighbors
      sel_h <- loss_data |> filter(stateCode == sel) |> group_by(Hazard) |> summarise(v = sum(`PropertyDmg(ADJ 2024)`, na.rm=T))
      nbr_h <- loss_data |> filter(stateCode %in% nbr_codes) |> group_by(Hazard) |> summarise(v = sum(`PropertyDmg(ADJ 2024)`, na.rm=T) / length(nbr_codes))
      
      plot_ly() |>
        add_bars(data = sel_h, x = ~Hazard, y = ~v, name = sel, marker = list(color = "#FF4500")) |>
        add_bars(data = nbr_h, x = ~Hazard, y = ~v, name = "Neighbors (Avg)", marker = list(color = "#2563eb")) |>
        layout(title = "Property Loss by Hazard", barmode = "group")
    }
  })
  
  # Plot 2: Distance Decay
  output$v2_distance_decay <- renderPlotly({
    sel <- sel_state_rv()
    req(sel, dist_matrix_km)
    
    dists <- dist_matrix_km[sel, ]
    sf_d <- spatial_sf_data() |> st_drop_geometry()
    
    df <- tibble(stateCode = names(dists), dist = dists) |>
      left_join(sf_d, by = c("stateCode" = "STUSPS")) |>
      filter(stateCode != sel)
    
    plot_ly(df, x = ~dist, y = ~val, type = "scatter", mode = "markers", text = ~NAME,
            marker = list(size = 10, opacity = 0.6, color = "#2563eb")) |>
      layout(
        xaxis = list(title = "Distance from Selected State (km)"),
        yaxis = list(title = "Total Value (Declarations or Loss)"),
        title = "Spatial Distance Decay"
      )
  })
  
  # Text Analysis
  output$v2_text_adjacency <- renderText({
    sel <- sel_state_rv()
    req(sel)
    idx <- which(us_states_sf$STUSPS == sel)
    nbrs <- us_states_sf$STUSPS[adj_global[[idx]]]
    name <- us_states_sf$NAME[idx]
    
    paste0(name, " shares a border with ", length(nbrs), " states: ", paste(nbrs, collapse = ", "), ". ",
           "This analysis uses sf::st_touches to identify direct physical adjacency, which often correlates with shared regional hazard exposure.")
  })
  
  output$v2_text_distance <- renderText({
    sel <- sel_state_rv()
    req(sel, dist_matrix_km)
    dists <- dist_matrix_km[sel, ]
    nearest <- names(sort(dists[dists > 0])[1])
    farthest <- names(sort(dists, decreasing = T)[1])
    
    paste0("Based on sf::st_distance (centroid-to-centroid), the state nearest to your selection is ", nearest, 
           ", while the farthest is ", farthest, ". The distance decay plot visualizes how the intensity of hazards changes as we move away from the focal state.")
  })
}

# --- Launch ---
shinyApp(ui, server)
