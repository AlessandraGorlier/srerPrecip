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
library(naniar)
library(DT)
library(SPEI)
library(scico)

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
precipitation <- read_csv("data/precipitation/estimated_precip.csv")

# Get images
imgs <- list.files("images/", pattern=".png", full.names = TRUE)

# Create image text
imgTexts <- c(
  "Interact with specified rain gauges to analyze long-term annual trends.",
  "Select specific months to analyze changes in long-term trends.",
  "View high-quality vizualizations representing wet periods and droughts."
)




### HOME PAGE MODULAIZATION -------------------------------------------------------

# Module UI function
pageWelcomeUi <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(

      titlePanel(
        h1("Welcome to the Santa Rita Precipitation Website!", align = "center",
           style = "padding: 50px;")),

      sidebarLayout(
        sidebarPanel(
          h3("What can you do?",
             style = "font-weight: bold; text-align: center;"),
          p("This tool serves the purpose of downloading and visualizing
             monthly-updated precipitation data from the Santa Rita Experimental
             Range. The Santa Rita Experimental Range currently has 24 active rain
             gauges that are monitored monthly. Here, you can interact with, download,
             and visualize important historical (starting in 1923) and current data!",
             style = "font-size: 20px;"),
          br(),
          h3("Goals:",
             style = "font-weight: bold; text-align: center;"),
          p("1) View and interact with high-quality visualizations that show wet
            periods, droughts, and longer term trends.",
            style = "font-size: 20px;"),
          p("2) Download calculated SPI (Standard Precipitation Index) values across multiple time periods.",
            style = "font-size: 20px;"),
          p("3) Gain a broader understanding of trends with the assistance of descriptive statistics.",
            style = "font-size: 20px;")
        ),

        mainPanel(
          h2("Three examples of visualizations you can create with this tool:"),
          actionButton(ns("get_started_button"), label = "Get Started!", icon("paper-plane")),
          br(),
          br(),
          imageOutput(ns("image")) |>
            tagAppendAttributes(style = 'align-items: right'),
          textOutput(ns("text")) |>
            tagAppendAttributes(style= 'font-size: 20px; padding: 20px; margin-left: 120px;'),
          fluidRow(
            column(3, offset=2, actionButton(ns("previous"), "Previous")),
            column(3, offset=2, actionButton(ns("next"), "Next"))
          ),
        )
      )
    )
  )
}
# Module Server function
pageWelcomeServer <- function(id, parentSession){
  moduleServer(id, function(input, output, session){
    observeEvent(input$get_started_button, {
      updateNavbarPage(session = parentSession,
                       inputId = "navbar",
                       selected = "Annual Averages")
    })
    index <- reactiveVal(1)

    observeEvent(input[["previous"]], {
      index(max(index()-1, 1))
    })
    observeEvent(input[["next"]], {
      index(min(index()+1, length(imgs)))
    })

    output$image <- renderImage({
      x <- imgs[index()]
      list(src = x, alt = "alternate text")
    }, deleteFile = FALSE)

    output$text <- renderText({
      text <- imgTexts[index()]
      text
    })

  })
} #End of Server


### DOWNLOAD PAGE -------------------------------------------------------------
# Module UI function
pageSelectUi <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h3("Select rain gauges and year"),
          p("1. Select rain guage(s) (can hover on icons to view location of gauge)"),
          p("2. Use sliders to choose time period of interest"),
          p("3. Click download"),
          p(HTML("<i>If DOWNLOADING data, please select <b> months, years, and rain gauge(s)</b></i>")),
          hr(style = "border-top: 1.5px solid grey;"),
          h3("Download Data"),
          # select gauges - DOWNLOAD
          selectInput(ns("downloadSelectGauges"),
                      label = "Select Rain Gauge(s):",
                      choices = unique(precipitation$station),
                      multiple = TRUE),

          # slider input for years - DOWNLOAD
          sliderInput(ns("downloadSelectYears"),
                      "Year Selection:",
                      min = 1922, max = max(precipitation$year), value = value, sep=''),
          # select months - DOWNLOAD
          sliderInput(ns("selectMonths"),
                      "Month Selection",
                      min = 1, max = 12, value = c(1, 12)),
          downloadButton(ns("downloadData"),"Download Data"),
          downloadButton(ns("downloadAllData"), "Download All Data"),
          hr()
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
pageSelectServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    #Interactive map where you can select rain gauges
    output$srerMap = leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles('Esri.WorldTopoMap') |>
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

    all_data <- reactive ({
      precipitation <- precipitation
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("precipitaiton_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filtered_data(), file)
      }
    )
    # download all table
    output$downloadAllData <- downloadHandler(
      filename = function() {
        paste0("all_data", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(all_data(), file, row.names = FALSE)
      }
    )
  })
} # End of Server


### ANNUAL VISUALIZATION ---------------------------------------------------
# Module UI function
pageVisualizationUi <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h3("Average Annual Precipitation"),
          p("1. Select a rain guage (can hover on icons to view location of gauge)"),
          p("2. Use drop-down to pick a singular gauge"),
          p("3. Use sliders to select a time period of interest"),
          leafletOutput(ns("srerMap"),
                        height = "450px",
                        width = "550px"),
          #select input for gauges
          selectInput(ns("selectGauges"),
                      label = "Select Rain Gauge:",
                      choices = unique(precipitation$station)),
          # slider input for years - VISUALIZATION
          sliderInput(ns("selectYears"),
                      "Year Selection:",
                      min = 1922, max = max(precipitation$year), value = value, sep=''),
          p(HTML("<b>Average:</b>")),
          textOutput(ns("textFiltered")),
          p(HTML("<b>Standard Deviation:</b>")),
          textOutput(ns("textFiltered2")),
          p(HTML("<b>Maximum:</b>")),
          textOutput(ns("textFiltered3")),
          p(HTML("<b>Minimum:</b>")),
          textOutput(ns("textFiltered4")),
          br(),
          downloadButton(ns("downloadDT"),"Download Data Table"),
          br(),
          br(),
          br(),
          p("To download the plot hover over the image and select the camera icon"),
          width = 5
        ),
        mainPanel(
          plotlyOutput(outputId = ns("AnnGraph")),
          br(),
          hr(style = "border-top: 1.5px solid grey;"),
          br(),
          DT::dataTableOutput(outputId = ns("annualDT")),
          width = 7
        )
      )
    )
  )
} #End of UI

pageVisualizationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    #map of gauges
    output$srerMap = leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles('Esri.WorldTopoMap') |>
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
                         zoom = 11.4)
    })

    #Filter data for graph and data table (add 12-month period spi - will be used in dt)
    filtered_means <- reactive ({
      filtered <- precipitation |>
        filter(station %in% input$selectGauges,
               year >= input$selectYears[1] & year <= input$selectYears[2]) |>
          mutate(precipitation = (precipitation/ 100) * 25.4) |>
          select(year, month_id, precipitation)


        spi12 <- spi(filtered$precipitation, 12)
        spi12 <- spi12$fitted

        filtered <- filtered |>
          mutate(spiyear = spi12) |>
          group_by(year) |>
          summarise(avg_precip_mm = mean(precipitation),
                    spiyear = mean(spiyear, na.rm = TRUE)) |>
          mutate(across(c('avg_precip_mm', 'spiyear'), round, 2))

    })

    #find overall stats
    overall_mean <- reactive({
      filtered_data <- filtered_means()
      filtered_data <- filtered_data |>
        summarise(all_mean = mean(filtered_data$avg_precip_mm, na.rm = TRUE),
                  all_sd = sd(filtered_data$avg_precip_mm, na.rm = TRUE),
                  all_max = max(filtered_data$avg_precip_mm, na.rm = TRUE),
                  all_min = min(filtered_data$avg_precip_mm, na.rm = TRUE))|>
        mutate(across(c('all_mean', 'all_sd', 'all_max', 'all_min'), round, 2))
    })


    #Average Temperature plot per selected yrs + gauge
    annual_graph <- reactive ({
      ann <- ggplot(filtered_means(), aes(x = year, y = avg_precip_mm)) +
        geom_bar(stat = "identity", fill = "skyblue", color = 'grey') +
        geom_hline(yintercept = mean(overall_mean()$all_mean, na.rm=TRUE)) +
        #geom_text(aes(0, mean(filtered()$avg_precip, na.rm=TRUE), label = 'mean avg. precipitation', vjust = -1)) +
        labs(x = "Year",
             y = "Average Precipitation",
             title = paste("Average Annual Precipitation (mm) for", input$selectGauges,
                           "(", input$selectYears[1], "-", input$selectYears[2], ")")) +
        theme_light(base_size = 15)

      ggplotly(ann)
    })


    output$AnnGraph <- renderPlotly({
      annual_graph()
    })

    #Load data table()
    output$annualDT <- DT::renderDataTable({
      filtered_means()
    })

    # mean as text output
    output$textFiltered <- renderText({
      overall_mean()$all_mean
    })

    output$textFiltered2 <- renderText({
      overall_mean()$all_sd
    })

    output$textFiltered3 <- renderText({
      overall_mean()$all_max
    })

    output$textFiltered4 <- renderText({
      overall_mean()$all_min
    })


    # download information table
    output$downloadDT <- downloadHandler(
      filename = function() {
        paste0("avg_annual", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filtered_means(), file, row.names = FALSE)
      }
    )
  })
} # End of Server

### SPI VISUALIZATION ---------------------------------------------------
# Module UI function
spiUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h3("Standard Precipitation Index"),
          p("1. Select a rain guage (can hover on icons to view location of gauge)"),
          p("2. Use drop-down to pick a singular gauge"),
          p("3. Use sliders to select a time period of interest"),
          leafletOutput(ns("srerMap")),
          #select input for gauges
          selectInput(ns("selectGauges"),
                      label = "Select Rain Gauge:",
                      choices = unique(precipitation$station)),
          # slider input for years - VISUALIZATION
          sliderInput(ns("selectYears"),
                      "Year Selection:",
                      min = 1922, max = max(precipitation$year), value = value, sep=''),
          br(),
          downloadButton(ns("downloadSPITable"), "Download SPI Data"),
          downloadButton(ns("downloadSPIPlot"), "Download SPI (1,3,12) Plot"),
          br(),
          br(),
          br(),
          p("To download the plot hover over the image and select the camera icon"),
          p("Shorter time periods (smaller sample size) might result in irregularities in
            SPI calculations."),
          width = 5
        ),
        mainPanel(
          plotOutput(outputId = ns("spiGraph")),
          br(),
          hr(style = "border-top: 1.5px solid grey;"),
          br(),
          plotlyOutput(outputId = ns("multiSPIgraph")),
          width = 7
        )
      )
    )
  )
} # End of UI

spiServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    #map of gauges
    output$srerMap = leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles('Esri.WorldTopoMap') |>
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
                         zoom = 11.4)
    })
    #data calculation for 1,3,and 12
    processed_spi_month <- reactive ({
      filtered_spi_multi <- precipitation |>
        filter(station %in% input$selectGauges,
               year >= input$selectYears[1] & year <= input$selectYears[2]) |>
        select(year, month_id, precipitation) |>
        mutate(precipitation = (precipitation/ 100) * 25.4) |>
        mutate(date = as.Date(paste(year, sprintf("%02d", month_id), "01", sep = "-")))


      for (i in 1:48) {
        multi_SPI <- spi(filtered_spi_multi$precipitation, i, na.rm = TRUE)
        filtered_spi_multi[[paste('spi', i, sep = '')]] <- multi_SPI$fitted

      }

      spi_month <- filtered_spi_multi |>
        naniar::replace_with_na_all(condition = ~.x == -Inf) |>
        select(date, year, spi1, spi3, spi12) |>
        rename("SPI (1-Month)" = spi1,
               "SPI (3-Month)" = spi3,
               "SPI (12-Month)" = spi12) |>
        pivot_longer(cols = starts_with("SPI"), names_to = "variable", values_to = "spi_value") |>
        mutate(pos = spi_value >=0) |>
        mutate(variable = factor(variable, levels = c("SPI (1-Month)", "SPI (3-Month)", "SPI (12-Month)")))

    })

    #calc for multiple
    processed_spi_multi <- reactive ({
      filtered_spi_multi <- precipitation |>
        filter(station %in% input$selectGauges,
               year >= input$selectYears[1] & year <= input$selectYears[2]) |>
        select(year, month_id, precipitation) |>
        mutate(precipitation = (precipitation/ 100) * 25.4) |>
        mutate(date = as.Date(paste(year, sprintf("%02d", month_id), "01", sep = "-")))


      for (i in 1:48) {
        multi_SPI <- spi(filtered_spi_multi$precipitation, i, na.rm = TRUE)
        filtered_spi_multi[[paste('spi', i, sep = '')]] <- multi_SPI$fitted

      }

      spi_multi_all <- filtered_spi_multi |>
        naniar::replace_with_na_all(condition = ~.x == -Inf) |>
        pivot_longer(cols = starts_with("spi"), names_to = "variable", values_to = "spi_value") |>
        mutate(variable = as.numeric(gsub("\\D", "", variable)))

    })

    #data viz (1,3,and 12 month)
    spi_graph <- reactive ({
      ggplot(processed_spi_month(), aes(x = date,y = spi_value, fill = pos))+
        geom_bar(stat = "identity", position = "identity")+
        scale_fill_manual(values = c("#8c510a","#01665e"), guide = FALSE)+
        facet_wrap(~ variable, ncol = 1)+
        labs(x = 'month/year',y = 'SPI', title = paste0(input$selectGauges, ' - 1/3/12 month'))+
        theme_bw()

    })

    output$spiGraph <- renderPlot({
      spi_graph()
    })

    #data viz (multi-month)
    output$multiSPIgraph <- renderPlotly({
      plot_ly(processed_spi_multi(), x = ~date, y = ~variable, z = ~spi_value,
              colors=brewer.pal(11,'BrBG'), type = "heatmap", zmin=-3, zmax=3) %>%
        layout(title = paste0("1-48 months SPI calculations for ", input$selectGauges, " (mm)"),
               xaxis=list(title="Month-Year"),
               yaxis=list(title="Scale(months)"))
    })

    # download information table
    output$downloadSPITable <- downloadHandler(
      filename = function() {
        paste0("spi_data", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(processed_spi_month(), file, row.names = FALSE)
      }
    )

    #download SPI plot
    output$downloadSPIPlot <- downloadHandler(
      filename = function(){
        paste0("spi_timescale_", input$selectGauges,".png", sep = "")
      },
      content = function(file){
        ggsave(file, plot = spi_graph(), width = 15, height = 7)
      }
    )

  })
} # End of Server

### MONTHLY VISUALIZATION ---------------------------------------------------
# Module UI function
pageDroughtUi <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h3("Period Selection"),
          p("1. Select a rain guage (can hover on icons to view location of gauge)"),
          p("2. Use drop-down to pick a singular gauge"),
          p("3. Use sliders to select a time period of interest"),
          leafletOutput(ns("srerMap")),
          #select input for gauges
          selectInput(ns("selectGauges"),
                      label = "Select Rain Gauge:",
                      choices = unique(precipitation$station)),
          # slider input for years - VISUALIZATION
          sliderInput(ns("selectYears"),
                      "Year Selection:",
                      min = 1922, max = max(precipitation$year), value = value, sep=''),
          # select period
          sliderInput(ns("selectMonths"),
                      "Month Selection:",
                      min = 1, max = 12, value = c(1, 12)),
          p(HTML("<b>Average:</b>")),
          textOutput(ns("Filtered")),
          p(HTML("<b>Standard Deviation:</b>")),
          textOutput(ns("Filtered2")),
          p(HTML("<b>Maximum:</b>")),
          textOutput(ns("Filtered3")),
          p(HTML("<b>Minimum:</b>")),
          textOutput(ns("Filtered4")),
          br(),
          downloadButton(ns("DownloadMonthlyDT"),"Download Table"),
          br(),
          br(),
          br(),
          p("To download the plot hover over the image and select the camera icon"),
          width = 5
        ),
        mainPanel(
          plotlyOutput(outputId = ns("heatmap")),
          DT::dataTableOutput(outputId = ns("monthlyDT")),
          width = 7
        )
      )
    )
  )
} # End of UI

pageDroughtServer <- function(id){
  moduleServer(id, function(input, output, session){

    #map of gauges
    output$srerMap = leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles('Esri.WorldTopoMap') |>
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
                         zoom = 11.4)
    })

    #calc for multiple
    processed_heat_map <- reactive ({
      rain_data <- precipitation |>
        filter(station %in% input$selectGauges,
               year >= input$selectYears[1] & year <= input$selectYears[2],
               month_id >= input$selectMonths[1] & month_id <= input$selectMonths[2])|>
        mutate(precipitation = (precipitation/ 100) * 25.4) |>
        mutate(month = fct_inorder(month))
    })

    #calc for sum
    sum_heat_map <- reactive ({
      sum_rain_data <- precipitation |>
        filter(station %in% input$selectGauges,
               year >= input$selectYears[1] & year <= input$selectYears[2],
               month_id >= input$selectMonths[1] & month_id <= input$selectMonths[2])|>
        mutate(precipitation = (precipitation/ 100) * 25.4) |>
        summarise(mean_precip = mean(precipitation), na.rm = TRUE,
                  sd_precip = sd(precipitation), na.rm = TRUE,
                  min_precip = min(precipitation), na.rm = TRUE,
                  max_precip = max(precipitation), na.rm = TRUE) |>
        mutate(across(c('mean_precip', 'sd_precip', 'min_precip', 'max_precip'), round, 2))
    })

    #calc for the data table
    monthly_dt <- reactive ({
      filtered <- precipitation |>
        filter(station %in% input$selectGauges,
               year >= input$selectYears[1] & year <= input$selectYears[2],
               month_id >= input$selectMonths[1] & month_id <= input$selectMonths[2]) |>
        mutate(precipitation = (precipitation/ 100) * 25.4) |>
        select(year, month_id, precipitation)


      spi1 <- spi(filtered$precipitation, 1)
      spi1 <- spi1$fitted


      filtered <- filtered |>
        mutate(spi1_avg = spi1) |>
        naniar::replace_with_na_all(condition = ~.x == -Inf) |>
        group_by(year) |>
        summarise(spi1_avg = mean(spi1_avg, na.rm = TRUE)) |>
        mutate(across(c('spi1_avg'), round, 2)) |>
        mutate(months = paste(input$selectMonths[1], "-", input$selectMonths[2])) |>
        select(year, months, spi1_avg)
    })

    heatMap <- reactive({
      gg <- ggplot(processed_heat_map(), aes(x = year, y = month, fill = precipitation)) +
        geom_tile(colour = "gray20", linewidth = 1, stat = "identity") +
        scale_fill_viridis_c(option = "mako",
                              limits = c(0, 225),
                              oob = scales::squish,
                              breaks = c(0, 60, 120, 180),
                              labels = c("0", "60", "120", ">180"),
                              direction = -1) +

        theme(
          plot.title = element_text(hjust = 0, vjust = 1, size = rel(2)),
          axis.text = element_text(size = rel(1)),
          axis.text.y = element_text(hjust = 1),
          legend.text = element_text(size = rel(1.3))
        ) +
        labs(x = 'Year',y = 'Month', title = paste0('Total Monthly Rainfall for ', input$selectGauges, ' (mm)'),
             fill = 'precipitation (mm)')

      ggplotly(gg)
    })

    output$heatmap <- renderPlotly({
      heatMap()
    })

    output$AnnGraph <- renderPlotly({
      annual_graph()
    })

    #Load data table()
    output$monthlyDT <- DT::renderDataTable({
      monthly_dt()
    })

    # mean as text output
    output$Filtered <- renderText({
      sum_heat_map()$mean_precip
    })

    output$Filtered2 <- renderText({
      sum_heat_map()$sd_precip
    })

    output$Filtered3 <- renderText({
      sum_heat_map()$max_precip
    })

    output$Filtered4 <- renderText({
      sum_heat_map()$min_precip
    })


    # download information table
    output$DownloadMonthlyDT <- downloadHandler(
      filename = function() {
        paste0("spi_monthly", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(monthly_dt(), file, row.names = FALSE)
      }
    )

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

  # Main UI: General Visualization Tab
  tabPanel(title = "Annual Averages",
           icon = icon('chart-simple'),
           pageVisualizationUi("visualization"),
           plotOutput("annualPlot")
  ),

  # Main UI: Drought Tab
  tabPanel(title = "Monthly Comparisons",
           icon = icon('sun-plant-wilt'),
           pageDroughtUi("drought")
  ),

  # Main UI: SPI Tab
  tabPanel(title = "SPI",
           icon = icon('cloud-rain'),
           spiUI("spi")
  ),
  # Main UI: Download Tab
  tabPanel(title = "Download Data",
           icon = icon('download'),
           pageSelectUi("select")
  ),
) # End of UI


# Main App Server
server <- function(input, output, session) {
  pageWelcomeServer("welcome", parentSession = session)
  pageSelectServer("select")
  pageVisualizationServer("visualization")
  spiServer("spi")
  pageDroughtServer("drought")
} # Server definition ends


# Run the application
shinyApp(ui = ui, server = server)
