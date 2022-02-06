library(shiny)

shinyUI(fluidPage(

    titlePanel("Medical Drugs"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("n_topics",
                        "Number of topics:",
                        min = 1,
                        max = 25,
                        value = 10)
        ),
        mainPanel(
            dataTableOutput('table_output'),
            dataTableOutput('stat_condition')
            # column(width=12,
            #     box(width=12,{dataTableOutput('table_output')}),
            #     box(width=12,{dataTableOutput('stat_condition')})
            # )
        )
    )
    )
)
