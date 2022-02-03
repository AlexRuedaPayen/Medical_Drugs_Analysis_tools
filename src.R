Object <- setRefClass("Object",
                         fields=list(),
                         methods=list(
                          
                          save=function(name) {
                             
                             inst_name<-name
                             class_name<-class(.self)[1]
                             
                             dir_path<-paste0("./class/",class_name,"/",inst_name)
                             dir.create(dir_path)
                             
                             for (inst in names(get(class_name)$fields())[sapply(names(get(class_name)$fields()),function(x){is.data.frame(get(x))})]) {
                               file_name<-(paste0(dir_path,"/",inst,".csv"))
                               
                               if (file.exists(file_name)) file.remove(file_name)
                               write.csv2(inst,file=file_name,sep=";",col.names=T)
                             }
                           },
                           
                           load=function(name){
                             
                             inst_name<-name
                             class_name<-class(.self)[1]
                             
                             dir_path<-paste0("./class/",class_name,"/",inst_name)
                             stopifnot(file.exists(dir_path))
                             
                             for (inst in names(get(class_name)$fields())[sapply(names(get(class_name)$fields()),function(x){is.data.frame(get(x))})]) {
                               file_name<-(paste0(dir_path,"/",inst,".csv"))
                               
                               if (file.exists(file_name)) assign(paste0(".self[[",inst,"]]"),read.csv2(file=file_name,sep=";",header=T))
                             }
                           }
                         ))

Medical_Drugs_Feedback <- setRefClass("Medical_Drugs_Feedback",
                                      contains="Object",
                                      fields = list(train_data ="data.frame",
                                                    test_data ="data.frame"
                                                    ),
                                      methods=list(
                                        
                                        initialize=function(train_data_path="./data/drugsComTrain_raw.csv",
                                                            test_data_path="./data/drugsComTest_raw.csv") {
                                          
                                         stopifnot(file.exists(train_data_path))
                                         stopifnot(file.exists(test_data_path))
                                          
                                        .self$train_data<-read.csv(file=train_data_path,stringsAsFactors = FALSE)
                                        .self$test_data<-read.csv(file=test_data_path,stringsAsFactors = FALSE)
                                        },
                                        
                                        table_output=function(n=10) {
                                          stopifnot("review" %in% colnames(.self$train_data))
                                          stopifnot(n>0)
                                          
                                          redable_data<-(.self$train_data%>%filter(nchar(review)<1500))
                                          
                                          stopifnot(n>nrow(redable_data))
                                          
                                          return(redable_data[1:n,])
                                        },
                                        stat_condition=function(){
                                          stopifnot("condition" %in% colnames(.self$train_data))
                                          
                                          return((.self$train_data)%>%group_by(condition)%>%summarise(Number_of_messages=n())%>%arrange(desc(Number_of_messages)))
                                        },
                                        topic_model_on_condition=function(condition_name="Anxiety",n_topics=8){
                                          stopifnot(c("condition","review","uniqueID") %in% colnames(.self$train_data))
                                          
                                          data_condition<-(.self$train_data)%>%filter(condition==condition_name)
                                          data_condition_parsable<-setNames(data_condition$uniqueID,data_condition$review)
                                          
                                          parsed_data<-spacy_parse(data_condition_parsable)
                                          parsed_data_NVAA<-parsed_data%>%filter(pos %in% c("NOUN","VERB","ADJ","ADV"))
                                          
                                          DTM_NVAA<-parsed_data_NVAA%>%
                                                            corpuslingr::clr_get_freq(agg_var=c('doc_id','lemma'),
                                                                                    toupper=FALSE)%>%
                                                            arrange(doc_id)
                                          
                                          static_DTM_NVAA <- dtm%>%
                                            filter(docf < 500 & docf > 5)%>%
                                            tidytext::cast_sparse(row=doc_id,column=lemma,value=txtf)
                                          
                                          static_topic_NVAA <- topicmodels::LDA(static_DTM_NVAA, 
                                                                                 k = n_topics, 
                                                                                 control=list(seed=12))
                                          return(static_topic_NVAA)
                                        }
                                        
                                      )
)