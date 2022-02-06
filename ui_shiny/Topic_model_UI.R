
Topic_model_UI<-function(id,label="Topic_model_UI") {
  ns<- NS(id)
  fluidPage(
    mainPanel(
      column(width=12,
             box(width=6,
                 sliderInput(ns("n_topics"),
                             "Number of topics:",
                             min = 1,
                             max = 25,
                             value = 10)
             ),
             box(width=6,
                 selectInput(
                   inputId=ns("condition"),
                   label="Select a condition",
                   choices=stat_condition$condition[1:10],
                   selected="Anxiety"
                 )
             )
      ),
      column(width=12,
             box(width=6,
                 uiOutput(ns("Title_descriptive_stat")),
                 dataTableOutput(ns('table_output')),
                 dataTableOutput(ns('stat_condition'))
             ),
             box(width=6,
                 uiOutput(ns("Title_topic_model")),
                 dataTableOutput(ns('topic_model_on_condition_beta')),
                 plotOutput(ns('topic_model_on_condition_gamma'))
             )
        )
      )
    )
}