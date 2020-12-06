library(taskscheduleR)

setwd("/Users/dvaradharajan/Git/Shiny-Dashboard")

myscript <- "C:/Users/dvaradharajan/Git/Shiny-Dashboard/email_newsletter.R"

taskscheduler_create(taskname = "myfancyscript4", rscript = myscript, 
                     schedule = "ONCE", starttime = format(Sys.time() + 62, "%H:%M"))
