library(shiny)

source("./ui_shiny/Topic_model_UI.R")
source("./server_shiny/Topic_model_server.R")

sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        menuItem("Topic model", tabName = "topicmodel", icon = icon("tools")),
        menuItem("Neural Net", tabName = "neuralnet", icon = icon("file-medical-alt")
    )),
    tabItems(
        tabItem(tabName = "topicmodel"
                ,Topic_model_UI("Topic_model_UI_id")
        )
    )
)

