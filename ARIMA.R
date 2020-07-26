library(forecast)

df <- read.csv("dalmp.csv",header = TRUE)

df = subset(df, select = c(OPERATION_DATE,PRICE))

df <- aggregate(df$PRICE,by=list(OPERATION_DATE = df$OPERATION_DATE),FUN = mean)

length(df$x)

x <- auto.arima(df$x)

future <- forecast(x,h=10)

length(future$fitted)

future$upper

ARIMA <- function(df,y,p,q,d)
{
 
  
  
}