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

})

shinyApp(ui = shinyUI, server = shinyServer)