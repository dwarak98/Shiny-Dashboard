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

################################# Downloading VER data #########################################

get_data <- function() {
  myfile <-
    getURL(
      'https://marketplace.spp.org/chart-api/load-forecast/asFile',
      ssl.verifyhost = FALSE,
      ssl.verifypeer = FALSE
    )
  df <- read.csv(textConnection(myfile), header = T)
  setDT(df)
  df$Interval = str_replace_all(df$Interval, "/", "-")
  df$Interval <- mdy_hms(df$Interval, tz = Sys.timezone())
  df$Hour <- hour(df$Interval)
  df$wind_pen_actual <- df$Actual.Wind * 100 / df$Actual.Load
  df$wind_pen_STF <- df$STWF * 100 / df$STLF
  df$wind_pen_MTF <- df$MTWF * 100 / df$MTLF
  df <- melt(df, id = c("Interval", "Hour", "GMTIntervalEnd"))
  return(df)
}

################################# Downloading Generation Data from spp Portal #########################################

get_gen_data <- function() {
  myfile     <-
    getURL(
      'https://marketplace.spp.org/chart-api/gen-mix-dl/asFile',
      ssl.verifyhost = FALSE,
      ssl.verifypeer = FALSE
    )
  
  gendf      <- read.csv(textConnection(myfile), header = T)
  
  gendf$GMT.MKT.Interval <-
    ymd_hms(gendf$GMT.MKT.Interval, tz = Sys.timezone())
  
  gendf$Coal <- gendf$Coal.Market + gendf$Coal.Self
  
  gendf$Nuclear <- gendf$Nuclear.Market + gendf$Nuclear.Self
  
  gendf$oil <-
    gendf$Diesel.Fuel.Oil.Market + gendf$Diesel.Fuel.Oil.Self
  
  gendf$Hydro   <- gendf$Hydro.Market + gendf$Hydro.Self
  
  gendf$Natural_Gas <-
    gendf$Natural.Gas.Market + gendf$Natural.Gas.Self
  
  gendf$Solar   <- gendf$Solar.Market + gendf$Solar.Self
  
  gendf$Waste_Disposal   <-
    gendf$Waste.Disposal.Services.Market + gendf$Waste.Disposal.Services.Self
  
  gendf$Wind <- gendf$Wind.Market + gendf$Wind.Self
  
  gendf$Others <- gendf$Other.Market + gendf$Other.Self
  
  gendf <- gendf[-c(2:22)]
  
  
  gendf$Hour <- hour(gendf$GMT.MKT.Interval)
  setDT(gendf)
  
  
  gendf = melt(gendf, id = c("GMT.MKT.Interval", "Hour"))
  
  
  newgendf <-
    gendf[order(gendf$GMT.MKT.Interval, decreasing = TRUE),]
  
  return(head(newgendf, 9))
}



df = get_data()
gendf = get_gen_data()


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem("Widgets", tabName = "widgets", icon = icon("th")),
    menuItem("Visualization", tabName = "Visualization", icon = icon("th")),
    menuItem("Prediction", tabName = "Prediction", icon = icon("th"))
  )),
  dashboardBody(# Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  width = 3,
                  dateRangeInput(
                    "date",
                    "Date range:",
                    start = Sys.Date() - 1,
                    end   = Sys.Date() + 1
                  ),
                  
                  sliderInput(
                    "slider_input",
                    label = "Hour",
                    value = c(0, 23),
                    min = 0,
                    max = 23
                  ),
                  selectInput(
                    "select_input",
                    label = "Category",
                    choices = unique(as.character(df$variable)),
                    selected = c("wind_pen_actual", "wind_pen_STF", "wind_pen_MTF"),
                    multiple = TRUE
                  ),
                ),
                
                
                box(width = 9, plotlyOutput(outputId = "line", height =
                                              400))
                
                
              ),
              fluidRow(box(
                width = 12, plotlyOutput(outputId = "bar", height = 800)
              ))),
      
      # Second tab content
      tabItem(
        tabName = "widgets",
        fileInput(
          'file1',
          'Choose CSV File',
          accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
        ),
        numericInput(
          "n",
          "Rows",
          value = 5,
          min = 1,
          step = 1
        ),
        tableOutput("head")
        
      ),
      # 3rd tab
      tabItem(tabName = "Visualization",
              fluidRow(
                box(
                  width = 3,
                  selectInput(
                    "charttype",
                    "Plot Type",
                    choices = c("Line", "Bar", "Table"),
                    multiple = FALSE
                  ),
                  uiOutput("lineplotVar1"),
                  uiOutput("lineplotVar2"),
                  uiOutput("lineplotVar3"),
                  uiOutput("tablecol1"),
                  uiOutput("tablecol2"),
                  uiOutput("tablecol3")
                  
                ),
                box(width = 9, uiOutput("LinePlot"), uiOutput("tableplot"), )
              )),
      # 4th tab
      tabItem(tabName = "Prediction",
              fluidRow(
                box(
                  width = 3,
                  selectInput(
                    "algo",
                    "Algorithm",
                    choices = c("ARIMA", "Linear Regression"),
                    multiple = FALSE
                  ),
                  uiOutput("P"),
                  uiOutput("Q"),
                  uiOutput("D"),
                  uiOutput("y_variable"),
                  uiOutput("x"),

                ),
                box(width = 9, uiOutput("table1plot"), )
              ))
    ))
)

server <- function(input, output, session) {
  
  ########### RT Update of SPP operational Data ####################
  sourceData <- reactive({
    invalidateLater(60000, session)
    
    # functionThatGetsData()
    get_data()
  })
  
  
  gensourceData <- reactive({
    invalidateLater(60000, session)
    
    # functionThatGetsData()
    get_gen_data()
    
  })
  
  VER_data_r1 <- reactive({
    dplyr::filter(
      sourceData(),
      between(as_date(df$Interval), input$date[1], input$date[2]) &
        df$variable %in% input$select_input
    )
  })
  
  VER_data_r2 <- reactive({
    dplyr::filter(VER_data_r1(),
                  between(Hour, input$slider_input[1], input$slider_input[2]))
  })
  
  
  ############# Static Line Chart #################################
  
  
  
  output$line <- renderPlotly({
    p1 <-
      ggplot(VER_data_r2(),
             aes(
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
  ############# Static Bar Chart #################################
  
  
  output$bar <- renderPlotly({
    p1 <-
      ggplot(gensourceData(),
             aes(
               x = variable,
               y = value,
               fill = variable,
               text = paste("</br>Resource: ", variable, "</br>Energy (MWh): ", value)
             )) +
      geom_bar(position = "dodge", stat = "identity")
    p2 <-
      p1 + theme_classic() + labs(y = "Energy (MWh)", x = "Resources", fill = "Resources") +
      theme_minimal()
    ggplotly(p2, tooltip = c("text"))
    
  })
  
  ################ Visualization #########################
  
  ############ Read the imported csv file ######################
  
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
  
  
  
  ########### Display Line Plot #################
  
  
  data1 <- reactive({
    req(input$file1)
    inFile <- input$file1
    print(inFile$datapath)
    read.csv(inFile$datapath, header = TRUE)
  })
  
  
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
      plotlyOutput(outputId = "dline", height = 400)
      
    }
    
    
  })
  
  
  
  
  output$dline <- renderPlotly({
  
    {
      data2 <- data1()
      print(data2)
      
      data2 <- data2 %>% rename(
        col1 = input$xaxis,
        col2 = input$yaxis,
        category = input$category
      )
      
      p1 <-
        ggplot(data2, aes(
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
      DT::dataTableOutput(outputId = "table")
      
    }
    
    
  })
  
  
  
  new_data <- reactive({
    
    print(input$Column[2])
    if (is.na(input$Column[2]))
    {
      newdata1 <- data1() %>% rename(
        Col1 = input$Column[1],
        Vue = input$VALUE,
        Rws = input$Row
      )
     
      newdata1 %>% group_by(Col1) %>% summarise(Value = sum(as.numeric(Vue)))
      # aggregate(newdata1$input$VALUE, by=list(newdata1$input$Column), FUN=sum)
    }
    
    else if (!is.na(input$Column[2]))
    {
      newdata1 <- data1() %>% rename(
        Col1 = input$Column[1],
        Col2 = input$Column[2],
        Vue = input$VALUE,
        Rws = input$Row
      )
      newdata1 %>% group_by(Col1,Col2) %>% summarise(Value = sum(as.numeric(Vue)))
      # aggregate(newdata1$input$VALUE, by=list(newdata1$input$Column), FUN=sum)
    }
    
  })
  
  
    output$table <- DT::renderDataTable({
      new_data()
      
      #,options = list(pageLength = 5,initComplete = I("function(settings, json) {alert('Done.');}"))
    })
    
  
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


ARIMA <- function(df,y,p,q,d)
{
  ts <- data.frame(df$Interval,df$value)
  x <- auto.arima()
  
}




shinyApp(ui, server)