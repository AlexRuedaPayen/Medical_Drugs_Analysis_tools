library(shiny)
library(dplyr)
library(devtools)
library(NLP)
library(slam)
library(tm)
library(topicmodels)
library(corpuslingr)
library(spacyr)
spacy_initialize(model = "en_core_web_sm")
# load("./env/test_env.Rdata") <-test by loading env

topic_model_on_condition<-function(filename,n_topics=8,condition_name='Anxiety',object_name='data') {
   train_data=read.csv2(file=filename,stringsAsFactors = FALSE)
 
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
    topic_distribution <- static_topic_NVAA_results$topics 
    
    system(paste0("mkdir ./class/Medical_Drugs_Feedback/",object_name,"/",condition_name))
    write.csv2(x=topic_distribution,file=paste0("./class/Medical_Drugs_Feedback/",object_name,"/",condition_name,"/topic_model_on_condition.csv"))
    
    return(static_topic_NVAA_results)
    
}

param<-list()

library(pracma)
library(stringr)
run.arguments <- commandArgs(TRUE)
valid.run.parameters <- c( "filename", "n_topics", "condition_name","object_name")
for ( i in 1:length( run.arguments ) ) {
  if ( strcmpi( substr( run.arguments[i], 1, 2 ), "--" ) & grepl( "=", run.arguments[i], fixed = TRUE) ) {
    key.pair <- str_split( run.arguments[i], "=", simplify=TRUE )
    run.parameter <- gsub( "--", "", key.pair[1] )
    run.argument <- key.pair[2]
    if ( run.parameter %in% valid.run.parameters ) {

      cat( run.parameter, "\n" )
      cat( run.argument,  "\n\n" )

      param[[run.parameter]]<-run.argument
    }
  }
}

print(param)

# 
# param=list(filename="./class/Medical_Drugs_Feedback/data/test_data.csv",
#            n_topics=8,
#            condition_name="Anxiety",
#            object_name="data")

topic_model_on_condition(filename=as.character(param[['filename']]),
                         n_topics=as.integer(param[['n_topics']]),
                         condition_name=as.character(param[['condition_name']]),
                         object_name=as.character(param[['object_name']]))



