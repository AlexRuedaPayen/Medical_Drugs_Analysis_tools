library(shiny)

Topic_model_server <- function(input, output, session){
  # 
  # shiny::moduleServer(id, function(input, output,session){
  #   
  #   
    output$Title_descriptive_stat<-renderUI({
      HTML(paste0("Descriptive stat on data"))
    })
    
    output$Title_topic_model<-renderUI({
      HTML(paste0("Topic model on condition ",input$condition))
    })
    
    output$table_output <- renderDataTable({
      return(DT::datatable(table_output,options = list(lengthMenu = c(5, 30, 50), pageLength = 5)))
    })
    output$stat_condition <-renderDataTable({
      return(DT::datatable(stat_condition,options = list(lengthMenu = c(5, 30, 50), pageLength = 5)))
    })
    output$topic_model_on_condition_gamma<-renderDataTable({
      data_preprocessed=read.csv2(file=paste0("./class/Medical_Drugs_Feedback/data/test_data.csv"))
      display=read.csv2(file=paste0("./class/Medical_Drugs_Feedback/data/Anxiety/topic_model_on_condition_gamma.csv"))
      display=display%>%left_join(data_preprocessed,by=c("document"="uniqueID"))
      return(DT::datatable(display,options = list(lengthMenu = c(5, 30, 50), pageLength = 5)))
    })
    output$topic_model_on_condition_beta<-renderPlot({
      library(ggplot2)
      display=read.csv2(file=paste0("./class/Medical_Drugs_Feedback/data/",input$condition,"/topic_model_on_condition_beta.csv"))
      display %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(beta, term, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        scale_y_reordered()
      
      return(DT::datatable(display,options = list(lengthMenu = c(5, 30, 50), pageLength = 5)))
    })

}