#
# CO2 Visualization App v0.22
# Interactive dashboard for exploring global CO2 emissions data

# Libraries ----
#Shiny
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(bslib)
library(shinyBS)
library(thematic)
#Data wrangling
library(tidyverse)
library(readxl)
#Data visualization
library(ggplot2)
library(plotly)
library(viridis)

color_scheme = "viridis"
# Data Import ----
co2 <- read_excel("Downloads/EDGAR_2024_GHG_booklet_2024_fossilCO2only.xlsx",
                  sheet = "fossil_CO2_by_sector_country_su")

co2_per_capita <- read_excel("Downloads/EDGAR_2024_GHG_booklet_2024_fossilCO2only.xlsx", 
                             sheet = "fossil_CO2_per_capita_by_countr")

# Data Transformation ----
# Convert wide format to long format
melted_co2 <- co2 %>%
  pivot_longer(cols = -c(Substance, Sector, `EDGAR Country Code`, Country),
               names_to = "Year",
               values_to = "CO2_Mt")

melted_capita <- co2_per_capita %>%
  pivot_longer(cols = -c(Substance, `EDGAR Country Code`, Country),
               names_to = "Year",
               values_to = "CO2_Mt")

# Sector Data ----
# Aggregate emissions by sector and year
co2_sector <- melted_co2 %>%
  group_by(Sector, Year, Country) %>%
  summarise(CO2_Mt = sum(CO2_Mt, na.rm = TRUE), .groups = "drop") %>%
  na.omit() %>%
  group_by(Year) %>%
  mutate(
    CO2_cumsum = rev(cumsum(rev(CO2_Mt))),
    pos = CO2_Mt/2 + lead(CO2_cumsum, 1),
    pos = if_else(is.na(pos), CO2_Mt/2, pos),
    percent = round(CO2_Mt/sum(CO2_Mt) * 100, digits = 1)
  ) %>%
  ungroup()

# Sector color mapping
sector_order_2023 <- co2_sector %>%
  filter(Year == 2023) %>%
  group_by(Sector) %>%
  summarise(CO2_Mt = sum(CO2_Mt, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(CO2_Mt)) %>%
  pull(Sector)

sector_colors <- setNames(
  viridis(n = n_distinct(co2_sector$Sector), option = color_scheme, direction = 1, alpha = 0.85), sector_order_2023
)

# Country Data - Total Emissions ----
co2_country <- melted_co2 %>%
  filter(!str_detect(Country, 'GLOBAL TOTAL')) %>%
  group_by(Country, Year) %>%
  summarise(CO2_Mt = sum(CO2_Mt, na.rm = TRUE), .groups = "drop") %>%
  mutate(Year = as.numeric(Year)) %>%
  arrange(desc(Year), desc(CO2_Mt))

# Country Data - Per Capita ----
co2_country_capita <- melted_capita %>%
  filter(!str_detect(Country, 'GLOBAL TOTAL')) %>%
  group_by(Country, Year) %>%
  summarise(CO2_Mt = sum(CO2_Mt, na.rm = TRUE), .groups = "drop") %>%
  mutate(Year = as.numeric(Year)) %>%
  arrange(desc(Year), desc(CO2_Mt))

welcome_text <- "Welcome to my CO₂ Emissions Explorer! In this interactive dashboard you can explore global carbon dioxide emissions from 1970 to 2023. Use the year slider to see different years through time and learn how emissions have evolved broken down by sector. Toggle between total emissions and per-capita data to understand both absolute contributions and individual footprints. Select specific countries you are interested in, or view all nations at once."

# Get all unique countries for selection
all_countries <- sort(unique(co2_country$Country))

# UI ----
ui <- page_fluid(
  theme = bs_theme(preset = "minty"),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      #bsthemerContainer {
        top: auto !important;
        bottom: 0 !important;
        left: 0 !important;
      }
    "))
  ),
  
  # Title
  titlePanel("Digging Deeper into CO₂ Emissions"),
  
  # Layout
  sidebarLayout(
    # Sidebar controls
    sidebarPanel(
      width = 3,
      wellPanel(
        sliderInput(
          "animation",
          "Which year are you interested in:",
          min = 1970,
          max = 2023,
          value = 2020,
          step = 1,
          sep = ""
          #animate = animationOptions(interval = 100, loop = TRUE)
        ),
        checkboxInput(
          "capita_selection",
          "Show CO2 emissions per person",
          value = FALSE
        ),
        
        pickerInput(
          "country_selection",
          "Which region(s) are you interested in:",
          choices = all_countries,
          selected = all_countries,
          options = list(`actions-box` = TRUE),
          multiple = TRUE
        )
      )
    ),
    
    # Main panel outputs
    mainPanel(
      width = 9,
      bsCollapse(
        id = "welcome_panel",
        open = "welcome",
        bsCollapsePanel(
          "About my CO₂ dashboard",
          welcome_text,
          style = "info"
        )
      ),
      fluidRow(
        column(12, withSpinner(plotOutput("linegraph", height = "600px")))
      ),
      fluidRow(
        column(6, withSpinner(plotlyOutput("graph", height = "400px"))),
        column(6, withSpinner(plotOutput("stackedgraph", height = "400px")))
      ),
      fluidRow(
        column(12, withSpinner(plotOutput("propgraph", height = "400px")))
      ),
      fluidRow(downloadButton("download_data", "Download Data"))
    )
  )
)

# Server ----
server <- function(input, output, session) {
  thematic_shiny(font = "Lato", )
  if (interactive()) bs_themer()
  
  theme_set(theme_minimal(base_size = 14))
  
  selected_year <- reactive(input$animation)
  selected_countries <- reactive({
    req(length(input$country_selection) > 0)
    input$country_selection
  })
  n_top_countries <- reactive({
    min(20, length(selected_countries()))  # Highlight top 20 or all if fewer
  })

# Top countries based on original data
  top_countries <- reactive({ co2_country %>%
    filter(Country %in% selected_countries()) %>%
    filter(Year == max(Year)) %>%
      slice_max(CO2_Mt, n = n_top_countries()) %>%
      pull(Country)
  })
  
  top_countries_capita <- reactive({ co2_country_capita %>%
    filter(Country %in% selected_countries()) %>%
    filter(Year == max(Year)) %>%
    slice_max(CO2_Mt, n = n_top_countries()) %>%
    pull(Country)
  })
  
  # Add country highlighting for total emissions
  co2_country_highlighted <- reactive({
    # First filter by selected countries
    filtered_data <- co2_country %>%
      filter(Country %in% selected_countries())
    
    # Get country order from filtered data
    country_order <- filtered_data %>%
      filter(Year == max(Year)) %>%
      arrange(desc(CO2_Mt)) %>%
      pull(Country)
    
    # Apply highlighting only to filtered countries
    filtered_data %>%
      mutate(
        Country = factor(Country, levels = country_order),
        Country_highlight = factor(
          if_else(Country %in% top_countries(), as.character(Country), "Other"),
          levels = c(intersect(top_countries(), country_order), "Other")
        )
      )
  })
  
  # Add country highlighting for per capita
  co2_country_capita_highlighted <- reactive({
    # First filter by selected countries
    filtered_data <- co2_country_capita %>%
    filter(Country %in% selected_countries())
    
    # Get country order from filtered data
    country_order_capita <- filtered_data %>%
      filter(Year == max(Year)) %>%
      arrange(desc(CO2_Mt)) %>%
      pull(Country)
    
    # Apply highlighting only to filtered countries
    filtered_data %>%
      mutate(
        Country = factor(Country, levels = country_order_capita),
        Country_highlight = factor(
          if_else(Country %in% top_countries_capita(), as.character(Country), "Other"),
          levels = c(intersect(top_countries_capita(), country_order_capita), "Other")
        )
      )
  })
  
  # Select data based on capita toggle
  data_selected <- reactive({
    if (input$capita_selection) {
      co2_country_capita_highlighted()
    } else {
      co2_country_highlighted()
    }
  })
  
  # Stacked area data
  co2_stacked <- reactive({
    data_selected() %>%
      group_by(Year) %>%
      arrange(Year, Country) %>%
      mutate(
        CO2_cumsum = cumsum(CO2_Mt),
        CO2_start = lag(CO2_cumsum, default = 0),
        CO2_mid = (CO2_start + CO2_cumsum) / 2
      ) %>%
      ungroup()
  })
  
  # Proportional stacked data
  co2_prop <- reactive({
    co2_stacked() %>%
      group_by(Year) %>%
      arrange(Year, Country) %>%
      mutate(
        total_CO2 = sum(CO2_Mt),
        percentage = CO2_Mt / total_CO2,
        pct_cumsum = cumsum(percentage),
        pct_start = lag(pct_cumsum, default = 0),
        pct_mid = (pct_start + pct_cumsum) / 2
      ) %>%
      ungroup()
  })
  
  # Filtered data
  final_data_prop <- reactive({
    co2_prop()
  })
  
  final_data_stacked <- reactive({
    co2_stacked()
  })
  
  final_data_line <- reactive({
    data_selected()
  })
  
  # Download Function
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("co2_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(final_data_line(), file, row.names = FALSE)
    }
  )
  # Pie Chart - Emissions by Sector ----
  output$graph <- renderPlotly({
    
    data_filtered <- co2_sector %>%
      filter(Year == selected_year()) %>%
      filter(Country %in% selected_countries()) %>%
      group_by(Sector) %>%
      summarise(CO2_Mt = sum(CO2_Mt, na.rm = TRUE), .groups = "drop") %>%
      mutate(Sector = factor(Sector, levels = sector_order_2023)) %>%
      arrange(Sector) %>%
      mutate(Sector = as.character(Sector))
    
    plot_ly(
      data_filtered,
      labels = ~Sector,
      values = ~CO2_Mt,
      type = 'pie',
      hole=0.3,
      marker = list(colors = sector_colors[data_filtered$Sector]),
      textposition = 'outside',
      sort = FALSE,
      textinfo = 'label+percent',
      hoverinfo = 'label+value+percent'
    ) %>%
      layout(
        title = paste("Emissions by Sector -", as.character(selected_year())),
        showlegend = FALSE,
        margin = list(l = 50, r = 50, t = 80, b = 80)  # Add margins
      )
  })
  
  # Line Chart - Country Trends ----
  output$linegraph <- renderPlot({
    ggplot(final_data_line(), aes(x = Year, y = CO2_Mt, 
                                  group = Country, 
                                  color = Country_highlight)) +
      geom_line(alpha = 0.5, linewidth = 1) +
      geom_vline(xintercept = as.numeric(selected_year()), 
                 #color = "red",
                 linewidth = 0.7) +
      scale_color_viridis(discrete = TRUE, option = color_scheme, direction = 1) +
      labs(
        title = "CO₂ Emissions Trends Since 1970",
        x = "Year",
        y = ifelse(input$capita_selection==FALSE, "CO₂ Emissions (Megatons)",  "CO₂ Emissions (tons CO2eq per capita per year)"),
        color = "Country"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
        #,
        #legend.position = "bottom"
      )
  })
  
  # Stacked Area - Absolute Emissions ----
  output$stackedgraph <- renderPlot({
    ggplot(final_data_stacked(), aes(x = Year, y = CO2_Mt, 
                                     group = Country, 
                                     fill = Country_highlight)) +
      geom_area(alpha = 0.6, linewidth = 0.5, 
                colour = "grey99", position = "stack") +
      geom_vline(xintercept = as.numeric(selected_year()), 
                 #color = "red",
                 linewidth = 0.7) +
      scale_fill_manual(values = c(viridis(n_top_countries(), option = color_scheme), "grey95")) +
      labs(
        title = "Total CO₂ Emissions stacked",
        x = "Year",
        y = "CO₂ Emissions (Mt)"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
  })
  
  # Stacked Area - Proportional ----
  output$propgraph <- renderPlot({
    ggplot(final_data_prop(), aes(x = Year, y = percentage, 
                                  group = Country, 
                                  fill = Country_highlight)) +
      geom_area(alpha = 0.6, linewidth = 0.5, 
                colour = "white", position = "stack") +
      geom_vline(xintercept = as.numeric(selected_year()), 
                 #color = "red",
                 linewidth = 0.7) +
      scale_fill_manual(values = c(viridis(n_top_countries(), option = color_scheme), "grey95")) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = "Share of Total CO₂ Emissions",
        x = "Year",
        y = "Percentage of Total Emissions"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
  })
}

# Run App ----
shinyApp(ui = ui, server = server)