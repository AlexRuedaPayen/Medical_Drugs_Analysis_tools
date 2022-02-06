library(shiny)
library(shinydashboard)

source("./ui_shiny/Topic_model_UI.R")
source("./server_shiny/Topic_model_server.R")

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
        sidebarMenu(
            id = "tabs",
            menuItem("Topic model", tabName = "topicmodel", icon = icon("tools")),
            menuItem("Neural Net", tabName = "neuralnet", icon = icon("file-medical-alt"))
        ),
        tabItems(
            tabItem(tabName = "topicmodel"
                    ,Topic_model_UI("Topic_model_id")
            )
        )
    ),
    dashboardBody()
)
