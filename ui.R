library(shiny)

shinyUI(fluidPage(

    titlePanel("Medical Drugs"),

    sidebarLayout(
        sidebarPanel(
        ),
        mainPanel(
            column(width=12,
                 box(width=6,
                     sliderInput("n_topics",
                        "Number of topics:",
                        min = 1,
                        max = 25,
                        value = 10)
                     ),
                box(width=6,
                    selectInput(
                        inputId="condition",
                        label="Select a condition",
                        choices=stat_condition$condition[1:10]
                    )
                )
            ),
            column(width=12,
                   box(width=6,
                       dataTableOutput('table_output'),
                       dataTableOutput('stat_condition')
                    ),
                    box(width=6,
                        dataTableOutput('topic_model_on_condition_beta'),
                        plotOutput('topic_model_on_condition_gamma')
                    )
                )
            )
        )
    )
)
