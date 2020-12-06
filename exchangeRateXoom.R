library(rvest)
library(htm2txt)
library(XML)
library(RCurl)
library(stringr)

# library(RTidyHTML)
url <- "https://www.xoom.com/india/send-money"

simple <- read_html("https://www.xoom.com/india/send-money")
text <- gettxt(url)

res <- str_match(text, "1 USD = \\s*(.*?)\\s*INR")
res[,2]
