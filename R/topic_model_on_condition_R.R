library(shiny)
library(dplyr)
library(devtools)
library(NLP)
library(slam)
library(tm)
library(topicmodels)
# library(corpusdatr)
# library(corpuslingr)
library(spacyr)
spacy_initialize(model = "en_core_web_sm")

devtools::install_github("jaytimm/corpuslingr")
devtools::install_github("juliasilge/tidytext")
install.packages("slam", type = "binary")
devtools::install_github("cran/tm")
devtools::install_github("cran/topicmodels")

topic_model_on_condition<-function(filename,n_topics=8,condition_name='Anxiety') {
   train_data=read.csv2(file=filename)
  
    
    stopifnot(c("condition","review","uniqueID") %in% colnames(train_data))
    
    data_condition<-(train_data)%>%filter(condition==condition_name)
    data_condition_parsable<-setNames(data_condition$review,data_condition$uniqueID)
    
    parsed_data<-spacy_parse(data_condition_parsable)
    parsed_data_NVAA<-parsed_data%>%filter(pos %in% c("NOUN","VERB","ADJ","ADV"))
    
    DTM_NVAA<-parsed_data_NVAA%>%
      corpuslingr::clr_get_freq(agg_var=c('doc_id','lemma'),
                                toupper=FALSE)%>%
      arrange(doc_id)
    
    static_DTM_NVAA <- DTM_NVAA%>%
      filter(docf < 500 & docf > 5)%>%
      tidytext::cast_sparse(row=doc_id,column=lemma,value=txtf)
    
    static_topic_NVAA <- topicmodels::LDA(static_DTM_NVAA, 
                                          k = n_topics, 
                                          method='Gibbs',
                                          control=list(iter = 500, verbose = 25))
    
    static_topic_NVAA_results<-posterior(static_topic_NVAA)
    
    vocabulary <- static_topic_NVAA_results$terms  
    vocbulary_distribution <- static_topic_NVAA_results$topics 
    dim(theta)  
    
    return(static_topic_NVAA_results)
    
}

library("optparse")
param<-list()

library(pracma)
library(stringr)
run.arguments <- commandArgs(TRUE)
valid.run.parameters <- c( "filename", "n_topics", "condition_name" )
for ( i in 1:length( run.arguments ) ) {
  if ( strcmpi( substr( run.arguments[i], 1, 2 ), "--" ) & grepl( "=", run.arguments[i], fixed = TRUE) ) {
    key.pair <- str_split( run.arguments[i], "=", simplify=TRUE )
    run.parameter <- gsub( "--", "", key.pair[1] )
    run.argument <- key.pair[2]
    if ( run.parameter %in% valid.run.parameters ) {
      
      # DO YOUR MAGIC HERE! Here is an example...
      cat( run.parameter, "\n" )
      cat( run.argument,  "\n\n" )
      
      param[[run.parameter]]<-run.argument
    }
  }
}

print(param)



topic_model_on_condition(filename=param[['filename']],
                         n_topics=param[['n_topics']],
                         condition_name=param[['condition_name']])



