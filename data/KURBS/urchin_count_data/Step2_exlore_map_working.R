# app.R  (stable bubble-map version)

rm(list=ls())

library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(janitor)
library(leaflet)
library(googlesheets4)
library(stringr)

################################################################################
# Google Sheets

# gs4_auth()

counts_sheet_id <- "1IQsGCYl4hXibI9KnnpSjxPLYulQjXGDP_MgT4lIOtsg"
sites_sheet_id  <- "1JQLUORvIinNcRq1tyhS__6ytuDjekWPqQCqNbirYcDk"

ss_counts <- as_sheets_id(counts_sheet_id)
ss_sites  <- as_sheets_id(sites_sheet_id)

################################################################################
# Read data

dat_raw <- read_sheet(ss_counts, sheet = 1) |>
  janitor::clean_names()

sites_raw <- read_sheet(ss_sites, sheet = 1) |>
  janitor::clean_names()

################################################################################
# Convert pretty location names → site_id (KF1_deep etc)

to_site_id <- function(x) {
  x <- str_to_lower(x)
  
  prefix <- case_when(
    str_detect(x, "kelp\\s*forest") ~ "KF",
    str_detect(x, "bar+en|barren") ~ "BR",
    TRUE ~ NA_character_
  )
  
  num <- str_extract(x, "\\b\\d+\\b")
  
  depth <- case_when(
    str_detect(x, "shallow") ~ "shallow",
    str_detect(x, "deep") ~ "deep",
    TRUE ~ NA_character_
  )
  
  ifelse(is.na(prefix) | is.na(num) | is.na(depth),
         NA_character_,
         paste0(prefix, num, "_", depth))
}

sites <- sites_raw |>
  transmute(
    site_id  = site_name,
    location = site_name,
    lat      = as.numeric(latitude),
    lon      = as.numeric(longitude),
    mpa,
    depth_m
  )

################################################################################
# Shiny UI

ui <- fluidPage(
  titlePanel("Recruitment Counts by Site"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_rng", "Collection date"),
      
      selectInput("locations", "Site",
                  choices = NULL, multiple = TRUE),
      
      selectInput("substrates", "Substrate type",
                  choices = NULL, multiple = TRUE),
      
      checkboxGroupInput(
        "taxa", "Taxa (summed together)",
        choices = c(
          "Total urchins" = "total_urchins",
          "Purple urchins" = "number_of_purple_urchins",
          "Red urchins" = "number_of_red_urchins",
          "Unidentified urchins" = "number_of_unidentified_urchins",
          "Crabs" = "number_of_crabs",
          "Gastropods" = "number_of_gastropods",
          "Bivalves" = "number_of_bivalves",
          "Plastic" = "number_of_plastic"
        ),
        selected = "total_urchins"
      ),
      
      radioButtons(
        "agg", "Aggregate",
        choices = c("Sum" = "sum",
                    "Mean per sample" = "mean"),
        selected = "sum"
      ),
      
      checkboxInput("log_scale",
                    "Log-scale bubble size",
                    value = TRUE)
    ),
    
    mainPanel(
      leafletOutput("m", height = 650),
      hr(),
      tableOutput("tbl")
    )
  )
)

################################################################################
# Server

server <- function(input, output, session) {
  
  dat0 <- reactive({
    dat_raw |>
      mutate(
        collection_date = as.Date(collection_date),
        total_urchins =
          coalesce(number_of_purple_urchins, 0) +
          coalesce(number_of_red_urchins, 0) +
          coalesce(number_of_unidentified_urchins, 0),
        site_id = to_site_id(location)
      )
  })
  
  observeEvent(dat0(), {
    d <- dat0()
    
    updateDateRangeInput(
      session, "date_rng",
      start = min(d$collection_date, na.rm = TRUE),
      end   = max(d$collection_date, na.rm = TRUE)
    )
    
    updateSelectInput(
      session, "locations",
      choices = sort(unique(sites$location)),
      selected = sort(unique(sites$location))
    )
    
    updateSelectInput(
      session, "substrates",
      choices = sort(unique(d$substrate_type)),
      selected = sort(unique(d$substrate_type))
    )
  }, once = TRUE)
  
  map_dat <- reactive({
    req(input$taxa)
    
    d <- dat0() |>
      filter(
        collection_date >= input$date_rng[1],
        collection_date <= input$date_rng[2]
      )
    
    if (length(input$substrates) > 0)
      d <- d |> filter(substrate_type %in% input$substrates)
    
    if (length(input$locations) > 0) {
      keep_ids <- sites$site_id[sites$location %in% input$locations]
      d <- d |> filter(site_id %in% keep_ids)
    }
    
    d <- d |>
      select(site_id, substrate_type, all_of(input$taxa)) |>
      mutate(across(all_of(input$taxa), ~coalesce(.x, 0))) |>
      mutate(value = rowSums(across(all_of(input$taxa)))) |>
      select(site_id, substrate_type, value)
    
    if (input$agg == "mean") {
      d <- d |>
        group_by(site_id, substrate_type) |>
        summarise(value = mean(value), .groups = "drop")
    } else {
      d <- d |>
        group_by(site_id, substrate_type) |>
        summarise(value = sum(value), .groups = "drop")
    }
    
    d |>
      left_join(sites, by = "site_id") |>
      filter(!is.na(lat), !is.na(lon))
  })
  
  output$m <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron")
  })
  
  observe({
    d <- map_dat()
    if (nrow(d) == 0) return()
    
    leafletProxy("m") |>
      clearMarkers() |>
      clearControls() |>
      fitBounds(
        lng1 = min(d$lon),
        lat1 = min(d$lat),
        lng2 = max(d$lon),
        lat2 = max(d$lat)
      )
    
    pal <- colorFactor("Dark2", domain = d$substrate_type)
    
    # bubble scaling
    size_vals <- if (input$log_scale)
      log1p(d$value)
    else
      d$value
    
    leafletProxy("m") |>
      addCircleMarkers(
        lng = d$lon,
        lat = d$lat,
        radius = scales::rescale(size_vals, to = c(5, 25)),
        fillColor = pal(d$substrate_type),
        fillOpacity = 0.7,
        stroke = FALSE,
        label = paste0(
          d$location,
          " | ", d$substrate_type,
          " | value: ", d$value
        )
      ) |>
      addLegend("bottomright",
                pal = pal,
                values = d$substrate_type,
                title = "Substrate")
  })
  
  output$tbl <- renderTable({
    map_dat() |>
      select(location, substrate_type, value, depth_m, mpa) |>
      arrange(desc(value))
  })
}

shinyApp(ui, server)
