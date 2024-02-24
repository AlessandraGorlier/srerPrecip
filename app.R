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
library(plotly)
library(RColorBrewer)
library(tidyverse)
library(DT)
library(SPEI)

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
             and visualize important historical and current data!"),
          actionButton(ns("get_started_button"), label = "Get Started!", icon("paper-plane"),
                       style = "font-size: 20px; align-item: center;
                                color: #fff; background-color: #337ab7;
                                border-color: #2e6da4")
          ),
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
          p(id = "home-goals", "3) ")
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
          h3("Select rain gauges and set year"),
          p("1. Select rain guage (can hover on icons to view location of gauge)"),
          p("2. Use sliders to choose time period of interest"),
          p("3. Click download or visualize (message will appear to show progress)"),
          p(HTML("<i>If DOWNLOADING data, please select <b> months, years, and rain gauge(s)</b></i>")),
          p(HTML("<i>If VISUALIZING data, please select <b> years, and rain gauge</b></i>")),
          hr(style = "border-top: 1.5px solid grey;"),
          h3("Visualize Data"),
          # select gauges - VISUALIZATION
          selectInput(ns("selectGauges"),
                      label = "Select Rain Gauge:",
                      choices = unique(precipitation$station)),

          # slider input for years - VISUALIZATION
          sliderInput(ns("selectYears"),
                      "Year Selection:",
                      min = 1922, max = max(precipitation$year), value = value, sep=''),
          actionButton(ns("visualizeData"), "Visualize Data"),
          hr(style = "border-top: 1.5px solid grey;"),
          h3("Downoad Data"),
          # select gauges - DOWNLOAD
          selectInput(ns("downloadSelectGauges"),
                      label = "Select Rain Gauge:",
                      choices = unique(precipitation$station),
                      multiple = TRUE),

          # slider input for years - DOWNLOAD
          sliderInput(ns("downloadSelectYears"),
                      "Year Selection:",
                      min = 1922, max = max(precipitation$year), value = value, sep=''),
          # select months - DOWNLOAD
          sliderInput(ns("selectMonths"),
                      "Month Selection",
                      min = 1, max = 12, value = c(3, 9)),
          downloadButton(ns("downloadData"),"Download Data"),
          hr(),
          p("*Please revisualize the data if you make any changes to the selected rain gauge or years."),
          p("*Figures and calculations are determined from the selected rain gauge and time period (year) chosen above.")

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
pageSelectServer <- function(id, selectedGauge, selectedYear, downloadSelectGauge, selectMonths, downloadSelectYear) {
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
        filter(station %in% input$downloadSelectGauges,
               (month_id >= input$selectMonths[1] & month_id <= input$selectMonths[2]),
               (year >= input$downloadSelectYears[1] & year <= input$downloadSelectYears[2])
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
    # Log reactive values - VISUALIZATION
    observeEvent(input$selectGauges, {
      selectedGauge(input$selectGauges)
    })
    observeEvent(input$selectYears, {
      selectedYear(input$selectYears)
    })
    # Log reactive values - DOWNLOAD
    observeEvent(input$downloadSelectGauges, {
      downloadSelectGauge(input$downloadSelectGauges)
    })
    observeEvent(input$downloadSelectYears, {
      downloadSelectYear(input$downloadSelectYears)
    })
    observeEvent(input$selectMonths, {
      selectMonths(input$selectMonths)
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
          h3("Average Annual Precipitation"),
          leafletOutput(ns("selectedMap")),
          p(HTML("<b>Selected Rain Gauge:</b>")),
          textOutput(ns("textSelectedGauge")),
          br(),
          p(HTML("<b>Average Annual Precipitation:</b>")),
          textOutput(ns("textFiltered")),
          br(),
          br(),
          br(),
          downloadButton(ns("downloadDT"),"Download Data Table"),
          downloadButton(ns("downloadAnnPlot"), "Download Plot"),
        ),
        mainPanel(
          plotOutput(outputId = ns("AnnGraph")),
          br(),
          hr(style = "border-top: 1.5px solid grey;"),
          br(),
          DT::dataTableOutput(outputId = ns("annualDT"))
        )
      )
    )
  )
} #End of UI

pageVisualizationServer <- function(id, selectedGauge, selectedYear) {
  moduleServer(id, function(input, output, session) {
    #filter to selected gauge
    gauge_selected <- reactive ({
      gauges |>
        filter(STATION %in% selectedGauge())
    })

    #map selected gauge
    output$selectedMap = leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addMarkers(data = gauge_selected(), label = gauge_selected()$STATION) |>
        leaflet::addPolygons(data = bounds,
                             color = "black",
                             weight = 2) |>
        leaflet::addPolylines(
          data = roads,
          color = "black",
          weight = 0.5,
          opacity = 1
        ) |>
        leaflet::setView(lng = -110.8529,
                         lat = 31.8331,
                         zoom = 11)
    })

    #Filter data for graph and data table
    filtered_means <- reactive ({
      precipitation |>
        filter(station %in% selectedGauge(),
               year >= selectedYear()[1] & year <= selectedYear()[2]) |>
        group_by(year) |>
        summarise(avg_precip = mean(precipitation))

    })

    #Filter to find mean
    overall_mean <- reactive ({
      precipitation |>
        filter(station %in% selectedGauge(),
               year >= selectedYear()[1] & year <= selectedYear()[2])

    })

    #Average Temperature plot per selected yrs + gauge
    annual_graph <- reactive ({
      ggplot(filtered_means(), aes(x = year, y = avg_precip)) +
        geom_bar(stat = "identity", fill = "skyblue", color = 'grey') +
        geom_hline(yintercept = mean(overall_mean()$precipitation, na.rm=TRUE)) +
        #geom_text(aes(0, mean(filtered()$avg_precip, na.rm=TRUE), label = 'mean avg. precipitation', vjust = -1)) +
        labs(x = "Year",
             y = "Average Precipitation",
             title = paste("Average Annual Precipitation for", selectedGauge(),
                           "(", selectedYear()[1], "-", selectedYear()[2], ")")) +
        theme_light(base_size = 15)
    })

    output$AnnGraph <- renderPlot({
      annual_graph()
    })

    #Load data table()
    output$annualDT <- DT::renderDataTable({
      filtered_means()
    })

    #selected gauge as text output
    output$textSelectedGauge <- renderText({
      selectedGauge()
    })

    # mean as text output
    output$textFiltered <- renderText({
      mean(overall_mean()$precipitation)
    })

    # download data table
    output$downloadDT <- downloadHandler(
      filename = function() {
        paste0("annual_precip_", selectedGauge(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filtered_means(), file, row.names = FALSE)
      }
    )
    #download plot
    output$downloadAnnPlot <- downloadHandler(
      filename = function(){
        paste0("annual_plot_", selectedGauge(),".png", sep = "")
        },
      content = function(file){
        ggsave(file, plot = annual_graph(), width = 15, height = 7)
      }
    )
  })
} # End of Server

### SPI VISUALIZATION ---------------------------------------------------
# Module UI function
spiUI <- function(id, selectedGauge, selectedYear) {
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h3("Standard Precipitation Index"),
          leafletOutput(ns("selectedMap")),
          br(),
          downloadButton(ns("downloadSPIPlot"), "Download Plot")
        ),
        mainPanel(
          plotOutput(outputId = ns("spiGraph")),
          plotlyOutput(outputId = ns("multiSPIgraph"))
        )
      )
    )
  )
} # End of UI

spiServer <- function(id, selectedGauge, selectedYear) {
  moduleServer(id, function(input, output, session) {
    #filter to selected gauge
    gauge_selected <- reactive ({
      gauges |>
        filter(STATION %in% selectedGauge())
    })
    #map selected gauge
    output$selectedMap = leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addMarkers(data = gauge_selected(), label = gauge_selected()$STATION) |>
        leaflet::addPolygons(data = bounds,
                             color = "black",
                             weight = 2) |>
        leaflet::addPolylines(
          data = roads,
          color = "black",
          weight = 0.5,
          opacity = 1
        ) |>
        leaflet::setView(lng = -110.8529,
                         lat = 31.8331,
                         zoom = 11)
    })
    #data calculation for 1,3,and 12

    #filter
    processed_spi <- reactive ({
      filtered_spi <- precipitation |>
        filter(station %in% selectedGauge(),
               year >= selectedYear()[1] & year <= selectedYear()[2]) |>
        select(year, month_id, precipitation) |>
        mutate(precipitation = (precipitation/ 100) * 25.4)

      #find spi
      spi1 <- spi(filtered_spi$precipitation, 1)
      filtered_spi$spi1<-spi1$fitted
      spi3 <- spi(filtered_spi$precipitation, 3)
      filtered_spi$spi3<-spi3$fitted
      spi12 <- spi(filtered_spi$precipitation, 12)
      filtered_spi$spi12<-spi12$fitted

      #data cleaning
      filtered_spi <- filtered_spi |>
        mutate(spi1 = ifelse(spi1 == -Inf, NA, spi1)) |> #replace -Inf with NA
        mutate(date = make_date(year, month_id)) |> #combine year and month into date column
        pivot_longer(cols = c('spi1', 'spi3', 'spi12'),
                     names_to = 'timescale',
                     values_to = 'spi_vals') |> #pivot dataframe
        mutate(timescale = ifelse(timescale == 'spi1', 'SPI (1-month)', timescale),
               timescale = ifelse(timescale == 'spi3', 'SPI (3-month)', timescale),
               timescale = ifelse(timescale == 'spi12', 'SPI (12-month)', timescale)) |> #change naming of spi1, spi3, and spi12
        mutate(timescale = factor(timescale, levels = c('SPI (1-month)', 'SPI (3-month)', 'SPI (12-month)'))) |>  #set levels
        mutate(pos = spi_vals >=0) |> #create TRUE/FALSE column for positives
        select(-c(year, month_id, precipitation)) |>#deselect unneeded columns
        mutate(timescale = as.character(timescale),  # Convert to character type if it's a factor
               timescale2 = as.numeric(gsub("\\D", "", timescale)))  # Perform conversion
    })


    #data viz (facet by month (1,3,12))
    output$spiGraph <-renderPlot({
      ggplot(processed_spi(), aes(x = date, y = spi_vals, fill = pos))+
        geom_bar(stat = "identity", position = "identity")+
        scale_fill_manual(values = c("#8c510a","#01665e"), guide = FALSE)+
        facet_wrap(~ timescale, ncol = 1)+
        labs(x = 'month/year',y = 'SPI', title = paste0('Standard Precipitation Index ', selectedGauge(),' - 1/3/12 month'))+
        theme_bw()
    })

    processed_spi_multi <- reactive ({
      filtered_spi_multi <- precipitation |>
        filter(station %in% selectedGauge(),
               year >= selectedYear()[1] & year <= selectedYear()[2]) |>
        select(year, month, precipitation) |>
        mutate(precipitation = (precipitation/ 100) * 25.4)

      filtered_spi_multi$date <- paste(filtered_spi_multi$month, "-", filtered_spi_multi$year, sep = "")

      for (i in 1:50) {
        multi_SPI <- spi(filtered_spi_multi$precipitation, i, na.rm = TRUE)
        filtered_spi_multi[[paste('spi', i, sep = '')]] <- multi_SPI$fitted

      }

      filtered_spi_multi <- filtered_spi_multi |>
        mutate(across(starts_with("spi"), ~ifelse(. == -Inf, NA, .)))

      spi_multi_all <- filtered_spi_multi |>
        pivot_longer(cols = starts_with("spi"), names_to = "variable", values_to = "spi_value") |>
        mutate(date = paste(month, year, sep = "-"),
               variable = as.numeric(gsub("\\D", "", variable)))

    })


    # #data viz (multi-month)
    output$multiSPIgraph <- renderPlotly({
      plot_ly(processed_spi_multi(), x = ~year, y = ~variable, z = ~spi_value,
              colors = brewer.pal(11,'BrBG'), type = "heatmap", zmin = -3, zmax = 3) |>
        layout(title = paste0("Multi-scale ", selectedGauge(), " Plot"),
               xaxis = list(title = "Month-Year"),
               yaxis = list(title = "Scale(mos)")
        )
    })

    #download SPI plot
    output$downloadSPIPlot <- downloadHandler(
      filename = function(){
        paste0("spi_timescale_", selectedGauge(),".png", sep = "")
      },
      content = function(file){
        ggsave(file, plot = spiGraph(), width = 15, height = 7)
      }
    )

  })
} # End of Server

### DROUGHT CATEGORY VISUALIZATION ---------------------------------------------------
# Module UI function
pageDroughtUi <- function(id, selectedGauge){
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h3("Period Selection"),
          leafletOutput(ns("selectedMap")),
          # select period 1 months
          sliderInput(ns("periodt1"),
                      "Period 1 selection (1-12 months):",
                      min = 1, max = 12, value = c(1, 3)),
          sliderInput(ns("periode1"),
                      "Period 1 End Month:",
                      min = 1, max = 12, value = c(1, 7)),
          sliderInput(ns("periodt2"),
                      "Period 2 selection (1-12 months):",
                      min = 1, max = 12, value = c(1, 3)),
          sliderInput(ns("periode2"),
                      "Period 2 End Month):",
                      min = 1, max = 12, value = c(1, 9)),
          downloadButton(ns("downloadDroughtDT"),"Download Data Table"),
          downloadButton(ns("downloadDroughPlot"), "Download Plot"),
        ),
        mainPanel(
          plotOutput(outputId = ns("droughtProbs")),
          br(),
          plotOutput(outputId = ns("droughtPeriods")),
          hr(style = "border-top: 1.5px solid grey;"),
          br(),
          DT::dataTableOutput(outputId = ns("downloadDroughtPlot"))
        )
      )
    )
  )
} # End of UI

pageDroughtServer <- function(id, selectedGauge){
  moduleServer(id, function(input, output, session){
    #filter to selected gauge
    gauge_selected <- reactive ({
      gauges |>
        filter(STATION %in% selectedGauge())
    })
    #map selected gauge
    output$selectedMap = leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addMarkers(data = gauge_selected(), label = gauge_selected()$STATION) |>
        leaflet::addPolygons(data = bounds,
                             color = "black",
                             weight = 2) |>
        leaflet::addPolylines(
          data = roads,
          color = "black",
          weight = 0.5,
          opacity = 1
        ) |>
        leaflet::setView(lng = -110.8529,
                         lat = 31.8331,
                         zoom = 11)
    })

    #drought category transitions
    # output$droughtProbs <-renderPlot({
    #   ggplot(data=plotStates)+
    #     geom_tile(aes(x=state2,y=state1,fill=anom),color = "black")+
    #     geom_text(aes(x=state2,y=state1,fill=anom, label = textlabel), size=4)+
    #     scale_fill_gradient2(low="lightyellow", mid="white",high="lightblue", midpoint = 0,
    #                          guide = guide_legend(title="prob anom(%)"))+
    #     scale_x_discrete(labels=lblText, limits=lims)+
    #     scale_y_discrete(labels=lblText, limits=lims)+
    #     labs(x = xlabText, y=ylabText, title=(droughtTitle))+
    #     theme(axis.text = element_text(size =14))+
    #     theme(axis.title = element_text(size=14))+
    #     theme(axis.text.x = element_text(size=14))+
    #     theme(axis.text.y = element_text(size=14))+
    #     theme(legend.position = "none") # can change to left or right
    # })

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
  tabPanel(title = "General Visualization",
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
  selectMonths <- reactiveVal (NULL)
  downloadSelectGauge <- reactiveVal(NULL)
  downloadSelectYear <- reactiveVal (NULL)
  pageWelcomeServer("welcome", parentSession = session)
  pageSelectServer("select", selectedGauge, selectedYear, selectMonths, downloadSelectYear, downloadSelectGauge)
  pageVisualizationServer("visualization", selectedGauge, selectedYear)
  spiServer("spi", selectedGauge, selectedYear)
  pageDroughtServer("drought", selectedGauge)
} # Server definition ends


# Run the application
shinyApp(ui = ui, server = server)
