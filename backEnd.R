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
library('ggplot2')
library('forecast')
library('tseries')
source("helperMethods.R")
library(shinycssloaders)

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)





df = getForecastVsActual()
gendf = getGenMix()





options(shiny.maxRequestSize = 30 * 1024 ^ 2)

server <- function(input, output, session) {
  ########### RT Update of SPP operational Data ####################
  
  addWindPenetrationPlot <- function(){
    
    ForecastVsActual <- reactive({
      invalidateLater(60000, session)
      getForecastVsActual() %>%
        filter(between(as_date(df$Interval), input$date[1], input$date[2]) &
                 df$variable %in% input$Category) %>%
        filter(between(Hour, input$Hour[1], input$Hour[2]))
      
    })
    
    output$line <- renderPlotly({
      validate(need(nrow(ForecastVsActual()) > 0, "Data insufficient for plot"))
      p1 <- ForecastVsActual() %>%
        ggplot(aes(
          x = Interval,
          y = value,
          col = variable,
          text = paste(
            "</br>Date: ",
            Interval,
            "</br>Value: ",
            value,
            "</br>Category: ",
            variable
          )
        )) + geom_point() + geom_line() + theme_minimal() + labs(y = "Wind Penetration (%)", x = "Datetime", col = "Category")
      
      ggplotly(p1, tooltip = c("text"))
      
      
      
    })
  }
  
  addGenMixBarPlot <- function(){
    genMixData <- reactive({
      invalidateLater(60000, session)
      
      # functionThatGetsData()
      getGenMix()
      
    })
    output$bar <- renderPlotly({
      validate(need(nrow(genMixData()) > 0, "Data insufficient for plot"))
      p1 <-
        ggplot(genMixData(),
               aes(
                 x = variable,
                 y = value,
                 fill = variable,
                 text = paste("</br>Resource: ", variable, "</br>Energy (MWh): ", value)
               )) +
        geom_bar(position = "dodge", stat = "identity")+theme_classic() + labs(y = "Energy (MWh)", x = "Resources", fill = "Resources") +
        theme_minimal()
      ggplotly(p1, tooltip = c("text"))
      
    })
  }
  readUploadedCSVData <- function(){
    
    data <- reactive({
      req(input$file1)
      inFile <- input$file1
      print(inFile$datapath)
      read.csv(inFile$datapath, header = TRUE)
    })
    
    ########## Displays top 5 rows of the imported csv #############
    output$head <- renderTable({
      head(data(), input$n)
    })
    
    return(data)
    
  }
  addLinePlot <- function(){
    output$dline <- renderPlotly({
  
        
        p1 <-
          data() %>% 
          rename(
            col1 = input$xaxis,
            col2 = input$yaxis,
            category = input$category
          ) %>%
          ggplot(aes(
            x = col1,
            y = col2,
            fill = category,
            text = paste(
              "</br>X-axis: ",
              col1,
              "</br>Y-Axis: ",
              col2,
              "</br>Category: ",
              category
            )
          )) + geom_col() + theme_minimal() + labs(y = input$yaxis,
                                                   x = input$xaxis,
                                                   col = input$category)
        ggplotly(p1, tooltip = c("text"))
        
        
    })
    
  }
  
  displayDynamicUI <- function(){
    
    output$lineplotVar1 <- renderUI({
      if (input$charttype == "Line")
      {
        choices <- unique(colnames(data()))
        selectInput("xaxis",
                    "X_Axis",
                    choices = choices,
                    multiple = TRUE)
        
      }
    })
    
    output$lineplotVar2 <- renderUI({
      if (input$charttype == "Line")
      {
        choices <- unique(colnames(data()))
        
        selectInput("yaxis",
                    "Y_Axis",
                    choices = choices,
                    multiple = TRUE)
        
        
        
      }
    })
    
    output$lineplotVar3 <- renderUI({
      if (input$charttype == "Line")
      {
        choices <- unique(colnames(data()))
        selectInput("category",
                    "Category",
                    choices = choices,
                    multiple = TRUE)
      }
      
      
    })
    
    output$LinePlot <- renderUI({
      
      if (input$charttype == "Line")
      {
        validate(
          need(input$xaxis!='','Choose X Axis'),
          need(input$yaxis!='','Choose Y Axis'),
          need(input$category !='','Choose Category'))
        plotlyOutput(outputId = "dline", height = 400) %>% withSpinner(type = 4, color="#0dc5c1")
       # addLinePlot()
        
      }
      
      
    })
    
    ####################### Table Display ###################
    
    output$tablecol1 <- renderUI({
      if (input$charttype == "Table")
      {
        choices <- unique(colnames(data()))
        selectInput("VALUE",
                    "ENTER THE AMOUNT OR VALUE",
                    choices = choices,
                    multiple = TRUE)
        
      }
      
      
    })
    
    output$tablecol2 <- renderUI({
      if (input$charttype == "Table")
      {
        choices <- unique(colnames(data()))
        
        selectInput("Row",
                    "List of Row",
                    choices = choices,
                    multiple = TRUE)
        
        
      }
      
      
    })
    
    output$tablecol3 <- renderUI({
      if (input$charttype == "Table")
      {
        choices <- unique(colnames(data()))
        
        selectInput("Column",
                    "List of Columns",
                    choices = choices,
                    multiple = TRUE)
        
      }
      
      
    })
    
    
    
    output$tableplot <- renderUI({
      if (input$charttype == "Table")
      {
        validate(
          need(input$Row !='','Choose Amount'),
          need(input$VALUE !='','Choose Rows'),
          need(input$Column !='','Choose Column'))
        DT::dataTableOutput(outputId = "table")
        
      }
      
      
    })
    
    
  }
  
  addTable <- function(){
    data <- readUploadedCSVData()
    
    
    new_data <- reactive({
      print(input$Column[2])
      rowname <- input$Row
      if (is.na(input$Column[2]))
      {
        newdata1 <- data() %>% rename(
          Col1 = input$Column[1],
          Vue = input$VALUE,
        )
        
        
        newdata1 %>% 
          select(Col1, Vue, input$Row) %>%
          pivot_wider(names_from = Col1, values_from = Vue,values_fn = list(Vue = sum))

      }
      
      else if (!is.na(input$Column[2]))
      {
        newdata1 <- data1() %>% rename(
          Col1 = input$Column[1],
          Col2 = input$Column[2],
          Vue = input$VALUE,
          Rws = input$Row
        )
        newdata1 %>% group_by(Col1, Col2) %>% summarise(Value = sum(as.numeric(Vue)))
        # aggregate(newdata1$input$VALUE, by=list(newdata1$input$Column), FUN=sum)
      }
      
    })
    
    
    output$table <- DT::renderDataTable({
      new_data()
      
      #,options = list(pageLength = 5,initComplete = I("function(settings, json) {alert('Done.');}"))
    })
    
    
  }
  

  
  ############# Static Line Chart #################################
  
  addWindPenetrationPlot()
  
  ############# Static Bar Chart #################################
  
  addGenMixBarPlot()
  
  ################ Visualization Tab #########################
  
  ############ Read the imported csv file ######################
  
  data <- readUploadedCSVData()
  
  ########### Display Line Plot #################
  
  displayDynamicUI()
  addLinePlot()
  addTable()
  
  
  
  
  
  
  ##################### PREDICTION #########################
  
  output$y_variable <- renderUI({
    if (input$algo == "ARIMA")
    {
      choices <- unique(colnames(data()))
      selectInput("y_variable1",
                  "What to Predict",
                  choices = choices,
                  multiple = TRUE)
      
    }
  })
  
  output$y_variable <- renderUI({
    if (input$algo == "ARIMA")
    {
      choices <- unique(colnames(data()))
      selectInput("y_variable1",
                  "What to Predict",
                  choices = choices,
                  multiple = TRUE)
      
    }
  })
  
  output$P <- renderUI({
    if (input$algo == "ARIMA")
    {
      numericInput("P1",
                   h3("Auto Regressive Coeffcienct (P)"),
                   value = 1)
      
      
    }
  })
  
  output$Q <- renderUI({
    if (input$algo == "ARIMA")
    {
      numericInput("Q1",
                   h3("Moving Average Coeffcienct (P)"),
                   value = 1)
      
      
    }
  })
  
  output$D <- renderUI({
    if (input$algo == "ARIMA")
    {
      numericInput("D1",
                   h3("Differencing (D)"),
                   value = 1)
      
      
    }
  })
  
  #  ARIMA(data(),input$y_variable1,input$P1,input$Q1,input$D1)
  
}
