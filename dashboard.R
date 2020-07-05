# install.packages("shinydashboard")

# pushing the code to master
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

# df <- read.csv("VER-Curtailments-20200401.csv", header=TRUE)

get_data <- function(){
  
  
  myfile <- getURL('https://marketplace.spp.org/chart-api/load-forecast/asFile', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  mydat <- read.csv(textConnection(myfile), header=T)
  df <- mydat
  # df$date <- as_date(df$Interval, tz=Sys.timezone())
  # df <- aggregate(MTWF ~ date, df, sum)
  df$Interval=str_replace_all(df$Interval, "/", "-")
  
  
  df$Interval=mdy_hms(df$Interval,tz=Sys.timezone())
  df$Hour <- hour(df$Interval)
  df$wind_pen_actual <- df$Actual.Wind*100/df$Actual.Load
  df$wind_pen_STF <- df$STWF*100/df$STLF
  df$wind_pen_MTF <- df$MTWF*100/df$MTLF
  df= melt(df,id=c("Interval","Hour","GMTIntervalEnd"))
  #  dat1 <- df %>% pivot_wider(names_from = variable, values_from = value)  
  
  
  
  return(df)
}

################################# Downloading Generation Data from spp Portal #########################################

get_gen_data <- function(){
  
  myfile <- getURL('https://marketplace.spp.org/chart-api/gen-mix-dl/asFile', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  class(myfile)
  mygendat <- read.csv(textConnection(myfile), header=T)
  gendf <- mygendat
  
  gendf$GMT.MKT.Interval=ymd_hms(gendf$GMT.MKT.Interval,tz=Sys.timezone())
  
  gendf$Coal <- gendf$Coal.Market+gendf$Coal.Self
  gendf$Nuclear <- gendf$Nuclear.Market+gendf$Nuclear.Self
  gendf$oil <- gendf$Diesel.Fuel.Oil.Market+gendf$Diesel.Fuel.Oil.Self
  gendf$Hydro <- gendf$Hydro.Market+gendf$Hydro.Self
  gendf$Natural_Gas <- gendf$Natural.Gas.Market+gendf$Natural.Gas.Self
  gendf$Solar <- gendf$Solar.Market+gendf$Solar.Self
  gendf$Waste_Disposal <- gendf$Waste.Disposal.Services.Market+gendf$Waste.Disposal.Services.Self
  gendf$Wind <- gendf$Wind.Market+gendf$Wind.Self
  gendf$Others <- gendf$Other.Market+gendf$Other.Self
  gendf <- gendf[-c(2:22) ]
  
  gendf$Hour <- hour(gendf$GMT.MKT.Interval)
  
  gendf= melt(gendf,id=c("GMT.MKT.Interval","Hour"))
  #names(gendf)[names(gendf) == "variable"] <- "Resources"
  #names(gendf)[names(gendf) == "value"] <- "Energy"
  
  
  newgendf <- gendf[order(gendf$GMT.MKT.Interval,decreasing =TRUE),]
  return(head(newgendf,9))
}



df = get_data()
gendf = get_gen_data()


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar( sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th")),
    menuItem("Dynammic UI", tabName = "DynammicUI", icon = icon("th"))
  )),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                              box(width=3,dateRangeInput("date", "Date range:",
                                             start = Sys.Date()-1,
                                             end   = Sys.Date()+1),
                              
                              sliderInput("slider_input",label="Hour",value=c(0,23),min=0,max=23),
                              selectInput("select_input",label="Category",choices=unique(as.character(df$variable)),selected = c("wind_pen_actual","wind_pen_STF","wind_pen_MTF"),multiple = TRUE),
              ),
              
                     
                     box(width=9,plotlyOutput(outputId = "line", height=400))
                     
                     
              ),
              fluidRow(
                              
                              
                              box(width=12,plotlyOutput(outputId = "bar",height=800)
              )
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              fileInput('file1', 'Choose CSV File',
                        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
              numericInput("n", "Rows", value = 5, min = 1, step = 1),
              tableOutput("head")
              
      ),
      
      tabItem(tabName = "DynammicUI",
              fluidRow(box(width=3,selectInput("xaxis", "X_Axis", choices = NULL, multiple = TRUE),
              selectInput("yaxis", "Y_Axis", choices = NULL, multiple = TRUE),
              selectInput("category", "Category", choices = NULL, multiple = TRUE)),
              box(width=9,plotlyOutput(outputId = "dline", height=400)))
              
      )
    )
  )
)

server <- function(input, output,session) {
  
  sourceData <- reactive({
    invalidateLater(60000,session)
    
    # functionThatGetsData()
    get_data()
  })
  
  
  gensourceData <- reactive({
    invalidateLater(60000,session)
    
    # functionThatGetsData()
    get_gen_data()
    
  })
  
  newtweets <- reactive({
    dplyr::filter(sourceData(), between(as_date(df$Interval),input$date[1],input$date[2]) & df$variable %in% input$select_input)
  })
  
  newtweets1 <- reactive({
    dplyr::filter(newtweets(), between(Hour,input$slider_input[1],input$slider_input[2]))
  })
  
 
  
  output$line <- renderPlotly({
    
    p1 <- ggplot(newtweets1(),aes(x=Interval,y=value,col=variable, text = paste("</br>Date: ", Interval, "</br>Value: ", value,"</br>Category: ", variable)))+geom_point()+geom_line()+theme_minimal()+labs(y="Energy (MWh)", x = "Datetime",col = "Category")                                                                                                                                                                                                   
    ggplotly(p1, tooltip = c("text"))
    
    
    
    
    
  })
  
  output$bar <- renderPlotly({
    
    
    ############# Bar Chart #################################
    
    
    p1 <- ggplot(gensourceData(),aes(x=variable,y=value,fill=variable,text = paste("</br>Resource: ", variable, "</br>Energy (MWh): ", value)))+
      geom_bar(position="dodge",stat="identity")
    p2 <- p1+theme_classic()+labs(y="Energy (MWh)", x = "Resources",fill = "Resources")+theme_minimal()
    ggplotly(p2, tooltip = c("text"))
    
  })
  
  data <- reactive({
    req(input$file1)
    inFile <- input$file1
    print(inFile$datapath)
    read.csv(inFile$datapath, header=TRUE)
  })
  #     read.csv(inFile$datapath, header=TRUE, sep=',', quote='')

  output$head <- renderTable({
    head(data(),input$n)
  })
  
  observeEvent(data(), {
    choices <- unique(colnames(data()))
    updateSelectInput(session, "xaxis", choices = choices,selected = NULL) 
  })
  
  observeEvent(data(), {
    choices <- unique(colnames(data()))
    updateSelectInput(session, "yaxis", choices = choices,selected = NULL) 
  })
  
  observeEvent(data(), {
    choices <- unique(colnames(data()))
    updateSelectInput(session, "category", choices = choices,selected = NULL) 
  })
  
  data1 <- reactive({
    req(input$file1)
    inFile <- input$file1
    print(inFile$datapath)
    read.csv(inFile$datapath, header=TRUE)
  })  
  
  output$dline <- renderPlotly({
   data2 <- data1()
   print(data2)
   
   data2 <- data2 %>% rename(col1 = input$xaxis,
                             col2 = input$yaxis,
                             category = input$category)
  
    p1 <- ggplot(data2,aes(x=col1,y=col2,fill=category,text = paste("</br>X-axis: ", col1, "</br>Y-Axis: ", col2,"</br>Category: ", category )))+geom_col()+theme_minimal()+labs(y=input$yaxis, x = input$xaxis,col = input$category)                                                                                                                                                                                                   
    ggplotly(p1, tooltip = c("text"))
    
    
    
    
    
  })
  
  
}

shinyApp(ui, server)