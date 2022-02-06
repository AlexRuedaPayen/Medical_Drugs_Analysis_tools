library(dplyr)
library(reticulate)
library(spacyr)
# use_condaenv("Medical_Drugs_Analysis_tools")
# use_python("~/anaconda3/envs/Medical_Drugs_Analysis_tools/bin/python3", required = NULL)



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
                               write.csv2(.self[[inst]],file=file_name,sep=";",col.names=T)
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
                         ,
                         
                      initialize_cloud=function(host="MacAlexandre@34.125.182.253",
                                                keyfile="~/.ssh/VM-1-GCP-Instance1/key"){
                        
                        class_name<-class(.self)[1]
                        
                        # session=ssh_connect(host=host,
                        #                      keyfile=keyfile)
                        # 
                        # 
                        # print(paste0('creating virtual environnement ',class_name))
                        # 
                        # ssh_exec_wait(session, command = c(
                        #  'sudo apt-get install wget',
                        #  'wget wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh',
                        #  'bash Miniconda3-latest-Linux-x86_64.sh'
                        #  ))###need to read and validate a licence so it will not work
                        # 
                        # ssh_disconnect(session) ##restarting to intialize conda
                        session=ssh_connect(host=host,
                                            keyfile=keyfile)
                        tryCatch({
                          ssh_exec_wait(session, command = c(
                           paste0('conda create -n ',class_name, ' python=3.9 anaconda'),
                           paste0('conda activate ',class_name),
                           'conda install numpy=1.19.5', #conda is a package manager, it verifies that you don't have apckages overlapping .. if too slow install through pip
                           'conda install sklearn',
                           'conda install pandas',
                           'conda install tensorflow',
                           paste0('conda desactivate')
                           )
                          )
                          print('environnement ready to work')
                        },
                        error=function(cond) {
                          print("Didn't work as expected")
                        },
                        finally={
                          ssh_disconnect(session)
                        })
                        
                       
                        
                      },
                      connect_to_cloud=function(host="MacAlexandre@34.125.182.253",
                                                keyfile="~/.ssh/VM-1-GCP-Instance1/key"){
                        
                        class_name<-class(.self)[1]
                        
                        session=(ssh_connect(host=host,
                                           keyfile=keyfile))
                        ssh_exec_wait(session,c(paste0('conda activate ',class_name)))
                        return(session)
                      },
                      disconnect_from_cloud=function(session) {
                        ssh_exec_wait(session,c(paste0('conda deactivate')))
                        ssh_disconnect(session)
                      },
                      cloud_compute=function(session,object_name,method_name='disorder_learner_LSTM_neural_net',language="python") {
                            browser()
                            class_name<-class(.self)[1]
                            tryCatch({  
                              scp_upload(session,
                                         files=paste0('./class/',class_name,'/',object_name,'/train_data.csv'),
                                         to=paste0('./Projects/Medical_Drugs_Analysis_tools/class/',class_name,'/',object_name,'/train_data.csv')
                              )
                            
                              if (language=="python") {
                                  ssh_exec_wait(session, command = c(
                                    paste0('python3 ./Projects/Medical_Drugs_Analysis_tools/python/',method_name,'_py.py ./Projects/Medical_Drugs_Analysis_tools/class/',class_name,'/',object_name,'/test_data.csv')
                                  ))
                              }
                              if (language=="R") {
                                ssh_exec_wait(session, command = c(
                                  paste0('Rscript ./Projects/Medical_Drugs_Analysis_tools/R/',method_name,'_R.R ./Projects/Medical_Drugs_Analysis_tools/class/',class_name,'/',object_name,'/test_data.csv')
                                ))
                              }
                              scp_download(session,
                                           files=paste0('~/Projects/Medical_Drugs_Analysis_tools/class/',class_name,'/',object_name,'/',method_name,'.csv'),
                                           to=paste0('./class/',class_name,'/',object_name,'/',method_name,'.csv')
                              )
                             },
                            error=function(cond) {
                              print("Didn't work as expected")
                            },
                            finally={
                              ssh_disconnect(session)
                            }
                         )
                      }
                      
        )
)

Medical_Drugs_Feedback <- setRefClass("Medical_Drugs_Feedback",
                                      contains="Object",
                                      fields = list(train_data ="data.frame",
                                                    test_data ="data.frame",
                                                    topic_model="data.frame"
                                                    
                                                    ),
                                      methods=list(
                                        
                                        initialize=function(train_data_path="./data/drugsComTrain_raw.csv",
                                                            test_data_path="./data/drugsComTest_raw.csv") {
                                          
                                         stopifnot(file.exists(train_data_path))
                                         stopifnot(file.exists(test_data_path))
                                          
                                        .self$train_data<-read.csv(file=train_data_path,stringsAsFactors = FALSE)
                                        .self$test_data<-read.csv(file=test_data_path,stringsAsFactors = FALSE)
                                        },
                                        
                                        table_output=function() {
                                          stopifnot("review" %in% colnames(.self$train_data))
                                          
                                          redable_data<-(.self$train_data)%>%filter(nchar(review)>15)%>%arrange(nchar(review))
                                          
                                          return(redable_data)
                                        },
                                        stat_condition=function(){
                                          stopifnot("condition" %in% colnames(.self$train_data))     
                                          return(((.self$train_data)%>%group_by(condition)%>%summarise(Number_of_messages=n())%>%arrange(desc(Number_of_messages))))
                                        },
                                        topic_model_on_condition=function(condition_name="Anxiety",n_topics=8,object_name,on_cloud=FALSE){
                                          method_name="topic_model_on_condition"
                                          if (on_cloud) {
                                            session=.self$connect_to_cloud()
                                            cloud_compute(session=session,object_name=object_name,method_name=method_name,language='R')
                                            .self$result<-read.csv2(file=paste0('~/class/Medical_Drugs_analysis/',name,'/topic_model_on_condition.csv'))
                                          }
                                          else {
                                            ###lunch like this in order to use Nohup + mail when programm is done... can run some in parallel
                                            system(paste0('Rscript ./R/',method_name,'_R.R --filename=./class/',class_name,'/',object_name,'/test_data.csv --n_topics=',n_topics,' --condition_name=',condition_name,' --object_name=',object_name))
                                          }
                                         },
                                        disorder_learner_topic_model=function(){
                                          
                                        },
                                        disorder_learner_LSTM_neural_net=function(object_name,on_cloud=TRUE) {
                                          if (on_cloud) {
                                            session=.self$connect_to_cloud()
                                            cloud_compute(session=session,object_name=object_name,method_name='disorder_learner_LSTM_neural_net',language='python')
                                            .self$result<-read.csv2(file=paste0('~/class/Medical_Drugs_analysis/',name,'/disorder_learner_LSTM_neural_net.csv'))
                                          }
                                          else {
                                            system(paste0('python3 ./Projects/Medical_Drugs_Analysis_tools/python/',method_name,'_py.py ./Projects/Medical_Drugs_Analysis_tools/class/',class_name,'/',object_name,'/test_data.csv'))
                                          }
                                        },
                                        disorder_learner_BERT=function() {
                                        }
                                      )
)

if (!file.exists(paste0("./class/",as.character(classname)))) {
  for (classname in class(get(ls()))) {
    system(paste0("mkdir ./class/",as.character(classname)))
  }
}