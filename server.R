source('./src.R')

data<-Medical_Drugs_Feedback()
table_output<-data$table_output()
stat_condition<-data$stat_condition()

shinyServer(function(input, output) {

    output$table_output <- renderDataTable({
        return(DT::datatable(table_output,options = list(lengthMenu = c(5, 30, 50), pageLength = 5)))
    })
    output$stat_condition <-renderDataTable({
        return(DT::datatable(stat_condition,options = list(lengthMenu = c(5, 30, 50), pageLength = 5)))
    })
    output$topic_model_on_condition<-renderDataTable({
        data$topic_model_on_condition(condition_name=input$condition,n_topics=input$n_topics,object_name='data',on_cloud=FALSE)
        display=read.csv2(file=paste0("./class/Medical_Drugs_Feedback/data/",input$condition,"/topic_model_on_condition_",input$n_topics,"_topics.csv"))
        return(DT::datatable(display,options = list(lengthMenu = c(5, 30, 50), pageLength = 5)))
    })
})

shinyApp(ui = shinyUI, server = shinyServer)