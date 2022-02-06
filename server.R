source('./src.R')
library(shiny)
library(shinydashboard)
library(knitr)
library(dplyr)
library(sparkline)
library(jsonlite)
library(DT)
library(lazyeval)
library(memoise)
library(rstudioapi)


# library(shinyWidgets)
# library(shinyBS)
# library(tidyr)
# library(shinyjs)
# library(DBI)
# library(opSelection)
# library(magrittr)
# library(data.table)
# library(stringr)
# library(lubridate)
# library(readxl)
# library(shinycssloaders)


data<-Medical_Drugs_Feedback()
table_output<-data$table_output()
stat_condition<-data$stat_condition()

callModule(Topic_model_server,"Topic_model_id")

shinyApp(ui = shinyUI, server = shinyServer)