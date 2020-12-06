library(shinydashboard)
library(shiny)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(plotly)
library(data.table)
library(lubridate)
library(RCurl)
require(plotly)
library(tidyr)
library(ggplot2)
library(forecast)
library(tseries)
library(sjmisc)
source("helperMethods.R")
library(shinycssloaders)

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)




df = getForecastVsActual()
gendf = getGenMix()


addDashboardMenu <- function(id,logo = "dashboard"){
  menuItem(id,tabName = id,icon = icon(logo))
}

addDateRangeSelector <- function(id, start=Sys.Date() - 1, end=Sys.Date() + 1){
  dateRangeInput(inputId = id,label = id,start = start,end   = end)
}

addHourSlider <- function(id="Hour"){
  sliderInput(
    id,
    label = id,
    value = c(0, 23),
    min = 0,
    max = 23
  )
}


addCategory <- function(id = "Category"){
  
  selectInput(id,label = id,choices = unique(as.character(getForecastVsActual()$variable)),
    selected = c("wind_pen_actual", "wind_pen_STF", "wind_pen_MTF"),
    multiple = TRUE)
}

addPlot <- function(id,height = 400){
  
  plotlyOutput(outputId = id, height =
                 height) %>% withSpinner(type = 4, color="#0dc5c1")
}

addLoadFilesFromDirectory <- function(id){
  fileInput(
    id,
    'Choose CSV File',
    accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
  )
}

addNumericInput <- function(id,caption="Enter Numeric Input"){
  numericInput(
    id,
    caption,
    value = 5,
    min = 1,
    step = 1
  )
}

addChartOptions <- function(){
  selectInput(
    "charttype",
    "Plot Type",
    choices = c("Line", "Bar", "Table"),
    multiple = FALSE
  )
}

addPredictorOptions <- function(){
  selectInput(
    "algo",
    "Algorithm",
    choices = c("ARIMA", "Linear Regression"),
    multiple = FALSE
  )
}

addDashboardTabContent <- function(){
  tabItem(tabName = "Dashboard",
          fluidRow(
            box(
              width = 3,
              addDateRangeSelector("date"),
              addHourSlider(),
              addCategory()
            ),
            box(
              width = 9,
              addPlot("line")
            )
          ),
          fluidRow(
            box(
              width = 12, 
              addPlot("bar", height = 800)
            )))
  
}

addWidgetTabContent <- function(){
  
  tabItem(
    tabName = "Widgets",
    addLoadFilesFromDirectory("file1"),
    addNumericInput("n","Enter Number of Rows to be displayed"),
    tableOutput("head")
    
  )
  
}

addVisuTabContent <- function(){
  tabItem(tabName = "Visualization",
          fluidRow(
            box(
              width = 3,
              addChartOptions(),
              uiOutput("lineplotVar1"), # Reactive Input
              uiOutput("lineplotVar2"), # Reactive Input
              uiOutput("lineplotVar3"), # Reactive Input
              uiOutput("tablecol1"),    # Reactive Input
              uiOutput("tablecol2"),
              uiOutput("tablecol3")
              
            ),
            box(
              width = 9,
              uiOutput("LinePlot"),
              uiOutput("tableplot")
              )
          )
          )
  
}

addPredictTabContent <- function(){
  tabItem(tabName = "Prediction",
          fluidRow(
            box(
              width = 3,
              addPredictorOptions(),
              uiOutput("P"),
              uiOutput("Q"),
              uiOutput("D"),
              uiOutput("y_variable"),
              uiOutput("x")
            ),
            box(
              width = 9,
              uiOutput("table1plot"))
          ))
  
}

ui <- dashboardPage(
  dashboardHeader(
    title = "Basic dashboard"),
  dashboardSidebar(sidebarMenu(
    addDashboardMenu(id = "Dashboard"),
    addDashboardMenu(id ="Widgets",logo = "th"),
    addDashboardMenu(id = "Visualization",logo = "th"),
    addDashboardMenu(id = "Prediction",logo = "th")
  )),
  dashboardBody(# Boxes need to be put in a row (or column)
    tabItems(
      # 1st tab content
      addDashboardTabContent(),
      # 2nd tab content
      addWidgetTabContent(),
      # 3rd tab
      addVisuTabContent(),
      # 4th tab
      addPredictTabContent()
    ))
)
