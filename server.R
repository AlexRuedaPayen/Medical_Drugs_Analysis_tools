library(shiny)
library(dplyr)
library(devtools)
library(NLP)
library(slam)
library(tm)
library(topicmodels)
library(corpusdatr)
library(corpuslingr)

library("spacyr")
spacy_initialize(model = "en_core_web_sm")

source('./src.R')

data<-Medical_Drugs_Feedback()
table_output<-data$table_output()
stat_condition<-data$stat_condition()

shinyServer(function(input, output) {

    output$table_output <- renderDataTable({
        return(table_output)
    })
    output$stat_condition <-renderDataTable({
        return(stat_condition)
    })

})

spacy_finalize()
