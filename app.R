#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#suppressPackageStartupMessages() # eliminate package startup messages

### Needed Packages ---------------------------------------------------------
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(ggplot2)
library(tidyverse)

# SET LIMITS --------------------------------------------------------------

# slider input for year selection
INTERVAL = 29
value = c(as.numeric(format(Sys.Date()-32, "%Y"))-INTERVAL, as.numeric(format(Sys.Date()-32, "%Y")))

### READ IN DATA ------------------------------------------------------------

# Read in Rain Gauge Data
gauges <- sf::st_read('data/active/Active_Rain_Gauges.shp') |>
  sf::st_transform('+proj=longlat +datum=WGS84')

# Read in Pasture Bounds
pastures <- sf::st_read("data/pastures/pastures.shp") |>
  sf::st_transform('+proj=longlat +datum=WGS84')

# Read in SRER Bounds
bounds <- sf::st_read("data/srerboundary/bounds.shp") |>
  sf::st_transform('+proj=longlat +datum=WGS84')

# Read in Road Bounds
roads <- sf::st_read("data/roads/roads.shp") |>
  sf::st_transform('+proj=longlat +datum=WGS84')

# Precipitation data
precipitation <- read_csv("data/precipitation/active_gauges_precip.csv")


### HOME PAGE MODULAIZATION -------------------------------------------------------

# Module UI function
pageWelcomeUi <- function(id) {
  ns <- NS(id)
    tagList(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      fluidPage(
        div(
          class = "main-container",
          div(
            class = "first-section",
            h1(id = "title-home",
               strong('Welcome to the Santa Rita Precipitation Website!')),
            p(id = "description-home", "This tool serves the purpose of downloading and visualizing
             monthly-updated precipitation data from the Santa Rita Experimental
             Range. The Santa Rita Experimental Range currently has 24 active rain
             gauges that are monitored monthly. Here, you can interact with, download,
             and visualize important historical and current data!")),
          hr(
            style = "border-top: 1.5px solid grey;"
          ),
          div(
            class = "second-section",
            h1(id = "second-home",
               strong("What Can You Do?")),
            div(
              class = "icon-section",
              icon("check", id = "home-icons"),
              icon("download", id = "home-icons"),
              icon("bar-chart", id = "home-icons")
            ),
            div(
              class = "display-home-section",
              h2(id = "display-home-text", "Select"),
              h2(id = "display-home-text", "Download"),
              h2(id = "display-home-text", "Visualize")
            )
          ),
          hr(
            style = "border-top: 1.5px solid grey;"
          ),
          div(
            h2(id = "goals",
               "Goals:"),
            p(id = "home-goals", "1) "),
            p(id = "home-goals", "2) "),
            p(id = "home-goals", "3) "),
            actionButton(ns("get_started_button"), label = "Get Started!", icon("paper-plane"),
                         style = "font-size: 20px; margin-left: 65em; color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
          )
      )
    )
} # End of UI

# Module Server function
pageWelcomeServer <- function(id, parentSession){
  moduleServer(id, function(input, output, session){
    #server for "get started" button
      observeEvent(input$get_started_button, {
        updateNavbarPage(session = parentSession,
                         inputId = "navbar",
                         selected = "Select & Download")
      })
  })
} #End of Server


### SELECT & DOWNLOAD PAGE -------------------------------------------------------------
# Module UI function
pageSelectUi <- function(id, selectedGauge) {
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Select rain gauges and set year and month"),
          p("1. Select rain guage (can hover on icons to view location of gauge)"),
          p("2. Use sliders to choose time period of interest (this includes year and month)"),
          p("3. Click download or visualize (message will appear to show progress)"),

          # select gauges
          selectInput(ns("selectGauges"),
                      label = "Select Rain Gauges:",
                      choices = unique(precipitation$station)),

          # select months
          sliderInput(ns("selectMonths"),
                      "Month Selection",
                      min = 1, max = 12, value = c(3, 9)),

          # slider input for years
          sliderInput(ns("selectYears"),
                      "Year Selection",
                      min = 1922, max = max(precipitation$year), value = value, sep=''),


          downloadButton(ns("downloadData"),"Download data"),
          actionButton(ns("visualizeData"), "Visualize data"),
          hr(),
          p("Re-download the dataset if you make any changes to location, time period or selected drought index"),
          p("All statistics and figures on other pages are calculated based on the location and time period specified here. The time period selection forces a minimum length of 30 years to ensure enough observations to calculate meaningful drought indices and climate statistics.")

        ),
        mainPanel(
          leafletOutput(ns("srerMap"),
                        height = "800px",
                        width = "1000px")
        )
      )
    )
  )
} # End of UI

# Module Server function
pageSelectServer <- function(id, selectedGauge, selectedYear) {
  moduleServer(id, function(input, output, session) {
    #Interactive map where you can select rain gauges
    output$srerMap = leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addMarkers(data = gauges, label = gauges$STATION) |>
        leaflet::addPolygons(data = bounds,
                             color = "black",
                             weight = 2) |>
        leaflet::addPolylines(
          data = roads,
          color = "black",
          weight = 0.5,
          opacity = 1
        ) |>
        #addPolylines(data = pastures, color="lightgreen", weight = 0.5, opacity = 0.5) %>%
        leaflet::setView(lng = -110.8529,
                         lat = 31.8331,
                         zoom = 12)
    })

    filtered_data <- reactive ({
      precipitation |>
        filter(station == input$selectGauges,
               (month_id >= input$selectMonths[1] & month_id <= input$selectMonths[2]),
               (year >= input$selectYears[1] & year <= input$selectYears[2])
        )
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("precipitaiton_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filtered_data(), file)
      }
    )
    # Log reactive values
    observeEvent(input$selectGauges, {
      selectedGauge(input$selectGauges)
    })
    # observeEvent(input$selectMonths, {
    #   selectedYear(input$selectMonths)
    # })
    observeEvent(input$selectYears, {
      selectedYear(input$selectYears)
    })
  })
} # End of Server


### GENERAL VISUALIZATION ---------------------------------------------------
# Module UI function
pageVisualizationUi <- function(id, selectedGauge, selectedYear){
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          leafletOutput(ns("selectedMap"))
        ),
        mainPanel(
          plotOutput(outputId = ns("graph"))
        )
      )
    )
  )
}

pageVisualizationServer <- function(id, selectedGauge, selectedYear) {
  moduleServer(id, function(input, output, session) {
    output$selectedMap = leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        #leaflet::addMarkers(data = selectedGauge(), label = selectedGauge()) |>
        leaflet::addPolygons(data = bounds,
                             color = "black",
                             weight = 2) |>
        leaflet::addPolylines(
          data = roads,
          color = "black",
          weight = 0.5,
          opacity = 1
        ) |>
        #addPolylines(data = pastures, color="lightgreen", weight = 0.5, opacity = 0.5) %>%
        leaflet::setView(lng = -110.8529,
                         lat = 31.8331,
                         zoom = 11)
    })

    output$graph <- renderPlot({
      filtered <- precipitation %>%
        filter(station %in% selectedGauge(),
               year >= selectedYear()[1] & year <= selectedYear()[2]) %>%
        group_by(year) %>%
        summarise(avg_precip = mean(precipitation))

      ggplot(filtered, aes(x=year, y=avg_precip)) +
        geom_bar(stat = "identity", fill = "skyblue", color = 'grey') +
        labs(x = "Year",
             y = "Average Precipitation",
             title = paste("Average Annual Precipitation for", selectedGauge())) +
        theme_bw()
    })
  })
} # End of Server

### SPI VISUALIZATION ---------------------------------------------------
# Module UI function
spiUI <- function(id) {
  tagList(
  )
}

spiServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}

### DROUGHT CATEGORY VISUALIZATION ---------------------------------------------------
# Module UI function
pageDroughtUi <- function(id){
  tagList()
} # End of UI

pageDroughtServer <- function(id){
  moduleServer(id, function(input, output, session){

  })
} # End of Server

### APP CALLS ------------------------------------------------------------------

# Main App UI
ui <- navbarPage(
  title = strong("Santa Rita Experimental Range Precipitation Explorer Tool"),
  id = "navbar",
  #theme = shinythemes::shinytheme("readable"),

  # Main UI: Home Page Tab
  tabPanel(title = "Home Page",
           icon = icon('house'),
           pageWelcomeUi("welcome")
  ),

  # Main UI: Select & Download Tab
  tabPanel(title = "Select & Download",
           icon = icon('map-location-dot'),
           pageSelectUi("select", selectedGauge)
  ),

  # Main UI: General Visualization Tab
  tabPanel(title = "General Visualiztion",
           icon = icon('chart-simple'),
           pageVisualizationUi("visualization"),
           plotOutput("annualPlot")
  ),

  # Main UI: SPI Tab
  tabPanel(title = "SPI",
           icon = icon('cloud-rain'),
           spiUI("spi")
  ),

  # Main UI: Drought Tab
  tabPanel(title = "Drought Cateogry",
           icon = icon('sun-plant-wilt'),
           pageDroughtUi("drought"))
) # End of UI


  # Main App Server
  server <- function(input, output, session) {
    selectedGauge <- reactiveVal(NULL)
    selectedYear <- reactiveVal (NULL)
    pageWelcomeServer("welcome", parentSession = session)
    pageSelectServer("select", selectedGauge, selectedYear)
    pageVisualizationServer("visualization", selectedGauge, selectedYear)
    spiServer("spi")
    pageDroughtServer("drought")
  } # Server definition ends


# Run the application
shinyApp(ui = ui, server = server)
