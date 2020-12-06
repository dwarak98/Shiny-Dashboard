library(blastula)
library(keyring)
library('rmarkdown')
library("blastula")
library("glue")
setwd("/Users/dvaradharajan/Git/Shiny-Dashboard")

source("helperMethods.R")
library(ggplot2)



df <- getForecastVsActual()

#img_file_path <- "saved_image.jpeg"



#jpeg(file=img_file_path, width = 1080, height = 920)

p1 <- df %>%
  filter(variable %in% c("wind_pen_actual", "wind_pen_STF", "wind_pen_MTF")) %>%
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

plot_image <- ggplotly(p1, tooltip = c("text"))



date_time <- add_readable_time()
plot_email <- add_ggplot(plot_image, height = 12.266666666666667, width = 14.4)

email <- compose_email(
  body = md(c("Hi Team, This important forecast needs to go out today.", plot_email)),
  footer = md(c("Email sent on ", date_time,"."))
)


Sys.setenv(SMTP_PASSWORD="Ooct248014")


# email <- render_email('email.Rmd')
# create_smtp_creds_key(
#   id = "gmail",
#   user = "sppim.newsletter@gmail.com",
#   provider = "gmail",
#  # overwrite = TRUE
# )

email %>%
  smtp_send(
    from = "sppim.newsletter@gmail.com",
    to = "dwarakvaradharajan@gmail.com",
    subject = "Testing the `smtp_send()` function",
    credentials = creds_envvar(
      user = "sppim.newsletter@gmail.com",
      pass_envvar = "SMTP_PASSWORD",
      provider = "gmail",
      host = NULL,
      port = NULL,
      use_ssl = TRUE
    )
    )
