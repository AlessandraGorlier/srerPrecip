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
            actionButton(NS(id, "get_started_button"), label = "Get Started!", icon("paper-plane"),
                         style = "font-size: 20px; margin-left: 65em; color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
          )
      )
    )
} # End of UI

# Module Server function
pageWelcomeServer <- function(id, parentSession){
  moduleServer( id, function(input, output, session){
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
pageSelectUi <- function(id) {
  fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4("Select rain gauges and set year and month"),
      p("1. Click map to select location (use +/- buttons to zoom, use cursor to pan)"),
      p("2. Use slider to choose time period of interest (limited to at least 30 yr period)"),
      p("3. Choose drought index (SPI or SPEI)"),
      p("4. Click download or visualize (message will appear to show progress"),

      # select gauges
      pickerInput(NS(id,"select-gauges"),
        label = "Select Rain Gauges:",
        choices = c('GRARI', 'AMADO', 'PAST3','SW','41','ROAD',
                    'WHITE', 'RODEN', 'DESGR', 'FORES', '45',
                    'BOX','IBP','PARKE','RUELA', 'ERIOP', 'MUHLE',
                    'NW','DESST','164','DESRI','HUERF','LIMST',
                    'NE'),
        multiple = TRUE,
        options = list("max-options" = 5)
      ),

      # select months
      sliderTextInput(NS(id, "select-months"),
        label = "Select Specificed Month Range:",
        choices = month.abb,
        grid = TRUE,
        selected = month.abb[c(4, 8)]
      ),

      # slider input for years
      sliderInput("years", "Year Selection",
                  min = 1895, max = as.numeric(format(Sys.Date()-32, "%Y")), value = value, sep=''),


      actionButton("refresh","Download data"),
      hr(),
      p("Re-download the dataset if you make any changes to location, time period or selected drought index"),
      p("All statistics and figures on other pages are calculated based on the location and time period specified here. The time period selection forces a minimum length of 30 years to ensure enough observations to calculate meaningful drought indices and climate statistics.")

    ),
    mainPanel(
      leafletOutput(NS(id, "srerMap"),
                    height = "800px",
                    width = "1000px")
      )
    )
  )
} # End of UI

# Module Server function
pageSelectServer <- function(id, session) {
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
  })
} # End of Server


### GENERAL VISUALIZATION ---------------------------------------------------
# Module UI function
pageVisualizationUi <- function(id){
  tagList()
} # End of UI

pageVisualizationServer <- function(id){
  moduleServer(id, function(input, output, session){

  })
} # End of Server

### SPI VISUALIZATION ---------------------------------------------------
# Module UI function
pageSpiUi <- function(id){
  tagList()
} # End of UI

pageSpiServer <- function(id){
  moduleServer(id, function(input, output, session){

  })
} # End of Server

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
           pageSelectUi("select")
  ),

  # Main UI: General Visualization Tab
  tabPanel(title = "General Visualiztion",
           icon = icon('chart-simple'),
           pageVisualizationUi("visualization")
  ),

  # Main UI: SPI Tab
  tabPanel(title = "SPI",
           icon = icon('cloud-rain'),
           pageSpiUi("spi")
  ),

  # Main UI: Drough Tab
  tabPanel(title = "Drought Cateogry",
           icon = icon('sun-plant-wilt'),
           pageDroughtUi("drought"))
) # End of UI


  # Main App Server
  server <- function(input, output, session) {
    pageWelcomeServer("welcome", parentSession = session)
    pageSelectServer("select")
    pageVisualizationServer("visualization")
    pageSpiServer("spi")
    pageDroughtServer("drought")
  } # Server definition ends


# Run the application
shinyApp(ui = ui, server = server)
