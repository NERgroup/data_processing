


rm(list=ls())

################################################################################
#Prep workspace and load packages

librarian::shelf("googlesheets4", "writexl", "fs", "janitor", "shiny","lubridate",
                 "scales","ggplot2")

#gs4_auth()

server_root <- "/Volumes/enhydra/data"
dir.exists(server_root)

backup_root <- "/Volumes/enhydra/data/KURBS/GDrive_backups/urchin_count_data"
dir_create(backup_root, recurse = TRUE)
dir.exists(backup_root)

################################################################################
#Define sheet

sheet_id <- "1IQsGCYl4hXibI9KnnpSjxPLYulQjXGDP_MgT4lIOtsg"
ss <- as_sheets_id(sheet_id)

################################################################################
#Read data (only one sheet, so use 1)

dat_raw <- read_sheet(
  ss,
  sheet = 1
) |>
  janitor::clean_names()


str(dat_raw)







# app.R


# app.R

library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(janitor)

ui <- fluidPage(
  titlePanel("Urchin + sorting counts (by site / habitat & substrate)"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "date_rng", "Collection date",
        start = NULL, end = NULL
      ),
      
      selectInput(
        "locations", "Site (location)",
        choices = NULL, selected = NULL, multiple = TRUE
      ),
      
      selectInput(
        "substrates", "Substrate type",
        choices = NULL, selected = NULL, multiple = TRUE
      ),
      
      radioButtons(
        "group_by", "Group by",
        choices = c("Site (location)" = "location", "Habitat (Forest vs Barren)" = "habitat"),
        selected = "location",
        inline = TRUE
      ),
      
      checkboxGroupInput(
        "taxa", "Taxa to plot",
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
        choices = c("Sum" = "sum", "Mean per sample" = "mean"),
        selected = "sum",
        inline = TRUE
      )
    ),
    
    mainPanel(
      plotOutput("p", height = 520),
      hr(),
      tableOutput("tbl")
    )
  )
)

server <- function(input, output, session) {
  
  dat0 <- reactive({
    dat_raw |>
      mutate(
        collection_date = as.Date(collection_date),
        total_urchins = coalesce(number_of_purple_urchins, 0) +
          coalesce(number_of_red_urchins, 0) +
          coalesce(number_of_unidentified_urchins, 0),
        habitat = case_when(
          grepl("kelp\\s*forest", location, ignore.case = TRUE) ~ "Forest",
          grepl("bar+en|barren", location, ignore.case = TRUE) ~ "Barren",
          TRUE ~ "Other"
        )
      ) |>
      janitor::clean_names()
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
      choices = sort(unique(d$location)),
      selected = sort(unique(d$location))
    )
    
    updateSelectInput(
      session, "substrates",
      choices = sort(unique(d$substrate_type)),
      selected = sort(unique(d$substrate_type))
    )
  }, once = TRUE)
  
  dat_long <- reactive({
    req(input$date_rng)
    req(input$taxa)
    
    d <- dat0() |>
      filter(
        collection_date >= input$date_rng[1],
        collection_date <= input$date_rng[2]
      )
    
    if (!is.null(input$locations) && length(input$locations) > 0) {
      d <- d |> filter(location %in% input$locations)
    }
    
    if (!is.null(input$substrates) && length(input$substrates) > 0) {
      d <- d |> filter(substrate_type %in% input$substrates)
    }
    
    d |>
      select(collection_date, location, habitat, substrate_type, all_of(input$taxa)) |>
      pivot_longer(cols = all_of(input$taxa), names_to = "taxon", values_to = "count") |>
      mutate(count = coalesce(count, 0))
  })
  
  dat_sum <- reactive({
    d <- dat_long()
    grp <- input$group_by
    
    if (input$agg == "mean") {
      d |>
        group_by(.data[[grp]], substrate_type, taxon) |>
        summarise(value = mean(count, na.rm = TRUE), .groups = "drop")
    } else {
      d |>
        group_by(.data[[grp]], substrate_type, taxon) |>
        summarise(value = sum(count, na.rm = TRUE), .groups = "drop")
    }
  })
  
  output$p <- renderPlot({
    d <- dat_sum()
    grp <- input$group_by
    
    taxon_labels <- c(
      total_urchins = "Total urchins",
      number_of_purple_urchins = "Purple urchins",
      number_of_red_urchins = "Red urchins",
      number_of_unidentified_urchins = "Unidentified urchins",
      number_of_crabs = "Crabs",
      number_of_gastropods = "Gastropods",
      number_of_bivalves = "Bivalves",
      number_of_plastic = "Plastic"
    )
    
    d <- d |>
      mutate(
        taxon = factor(taxon, levels = names(taxon_labels), labels = taxon_labels[names(taxon_labels)])
      )
    
    ggplot(d, aes(x = .data[[grp]], y = value, fill = substrate_type)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      facet_wrap(~ taxon, scales = "free_y") +
      scale_y_continuous(labels = label_number()) +
      labs(
        x = if (grp == "location") NULL else "Habitat",
        y = ifelse(input$agg == "mean", "Mean per sample", "Total count"),
        fill = "Substrate"
      ) +
      theme_bw() +
      theme(
        axis.text.x = if (grp == "location") element_text(angle = 35, hjust = 1) else element_text(angle = 0),
        panel.grid.minor = element_blank()
      )
  })
  
  output$tbl <- renderTable({
    dat_sum() |>
      arrange(taxon, .data[[input$group_by]], substrate_type)
  })
}

shinyApp(ui, server)
