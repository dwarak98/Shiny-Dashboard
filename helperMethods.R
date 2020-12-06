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


###################### Date Time Formatting #########################################
format_DateTime <- function(df, dateColName, format) {
  df[[dateColName]] = str_replace_all(df[[dateColName]], "/", "-")
  if (format == "ymd_hms") {
    df[[dateColName]] <-
      ymd_hms(df[[dateColName]], tz = Sys.timezone())
  }
  else if (format == "mdy_hms") {
    df[[dateColName]] <- mdy_hms(df[[dateColName]], tz = Sys.timezone())
  }
  
  df$Hour <- hour(df[[dateColName]])
  return(df)
}

################################# Downloading VER data #########################################


getForecastVsActual <- function() {
  myfile <-
    getURL(
      'https://marketplace.spp.org/chart-api/load-forecast/asFile',
      ssl.verifyhost = FALSE,
      ssl.verifypeer = FALSE
    )
  df <- read.csv(textConnection(myfile), header = T)
  df <- format_DateTime(df, "Interval", "mdy_hms")
  df$wind_pen_actual <- df$Actual.Wind * 100 / df$Actual.Load
  df$wind_pen_STF <- df$STWF * 100 / df$STLF
  df$wind_pen_MTF <- df$MTWF * 100 / df$MTLF
  setDT(df)
  df <- melt(df, id = c("Interval", "Hour", "GMTIntervalEnd"))
  return(df)
}

combine_df_Columns <- function(df, resultColName, colNames) {
  i = 0
  for (colName in colNames) {
    if (i == 0) {
      df[[resultColName]] <- df[[colName]]
    }
    else{
      df[[resultColName]] <- df[[resultColName]] + df[[colName]]
    }
    i = i + 1
    
  }
  
  return(df)
  
  
}


################################# Downloading Generation Data from spp Portal #########################################

getGenMix <- function() {
  myfile     <-
    getURL(
      'https://marketplace.spp.org/chart-api/gen-mix-dl/asFile',
      ssl.verifyhost = FALSE,
      ssl.verifypeer = FALSE
    )
  
  gendf      <- read.csv(textConnection(myfile), header = T)
  gendf <- format_DateTime(gendf, "GMT.MKT.Interval", "ymd_hms")
  
  gendf <- gendf %>%
    combine_df_Columns("Coal", c("Coal.Market", "Coal.Self")) %>%
    combine_df_Columns("Nuclear", c("Nuclear.Market", "Nuclear.Self")) %>%
    combine_df_Columns("oil", c("Diesel.Fuel.Oil.Market", "Diesel.Fuel.Oil.Self")) %>%
    combine_df_Columns("Hydro", c("Hydro.Market", "Hydro.Self")) %>%
    combine_df_Columns("Natural_Gas", c("Natural.Gas.Market", "Natural.Gas.Self")) %>%
    combine_df_Columns("Solar", c("Solar.Market", "Solar.Self")) %>%
    combine_df_Columns(
      "Waste_Disposal",
      c(
        "Waste.Disposal.Services.Market",
        "Waste.Disposal.Services.Self"
      )
    ) %>%
    combine_df_Columns("Wind", c("Wind.Market", "Wind.Self")) %>%
    combine_df_Columns("Others", c("Other.Market", "Other.Self")) %>%
    select(-ends_with(".Market")) %>%
    select(-ends_with(".Self")) %>%
    setDT() %>%
    melt(id = c("GMT.MKT.Interval", "Hour")) %>%
    arrange(desc(GMT.MKT.Interval))
  
  # print(head(gendf,length(unique(gendf$variable))))
  
  return(head(gendf, length(unique(gendf$variable))))
}
