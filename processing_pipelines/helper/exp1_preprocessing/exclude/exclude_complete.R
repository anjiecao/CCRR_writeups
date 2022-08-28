



# take task dictionary 
# check if task is complete 

LIMIT = 0.25 
SHORTEST_CHAR = 19 

check_complete <- function (id, task_dict, task_list, raw_df){
  COMPLETE = TRUE 
  sub_d <- raw_df %>% filter(subject == id)
  for (task in task_list){
    task_num <- as.numeric(task_dict[task])
    task_d <- sub_d %>% filter(trial_type == task)
    task_count <- count(task_d) %>% pull()
    if (task == "free-description-response" | task == "attribution-Q1"){
      task_d <- sub_d %>% 
        filter(trial_type == task) %>% 
        filter((str_length(as.character(responses))) > SHORTEST_CHAR) ## filter out the < 10 character resposnes 
      
      task_count <- count(task_d) %>% pull()
    }
    
    if (task_count < LIMIT * task_num) {
      COMPLETE = FALSE
      return(COMPLETE)
    }
  }
  return (COMPLETE)
}

###### main processing function #####

complete_exclusion <- function(raw_data){
  
  
  ##### Set up iterating materials ##### 
  
  task_dict <- dict::dict()
  task_dict["ebbinghaus-keyboard"] <- 34
  #task_dict["draw-circles"] <- 1
  #task_dict["circle-label"] <- 1
  task_dict["RMTStrain"] <- 4
  task_dict["RMTStest"] <- 4
  task_dict["horizon-sticker"] <- 1
  task_dict["free-description-response"] <- 7 # can be greater than
  task_dict["attribution-Q1"] <- 2 # can be greater than 2
  task_dict["pen-choice"] <- 1 
  
  
  
  task_list <- 
    c("ebbinghaus-keyboard", # ebbinghaus 10 +24 = 34 trials
      #"draw-circles", #syms 1 
      #"circle-label", #syms 1
      "RMTStrain", #RMTS train 4
      "RMTStest", #RMTS test 4
      "horizon-sticker",
      "free-description-response", # 14
      "attribution-Q1", #2 trials
      "pen-choice") # pen choice 
  #)# syms 1)
  
  
  
  ##### Loop through all the subject ID ##### 
  num_id <- unique(raw_data$subject)
  id_to_exclude <- c()
  
  for (id in num_id){
    complete_check <- check_complete(id,task_dict,task_list,raw_data)
    if (complete_check == FALSE){
      id_to_exclude = c(id_to_exclude, id)
    }
  }
  
  
  # drop duplicate
  id_to_exclude = id_to_exclude[!duplicated(id_to_exclude)]
  
  return (id_to_exclude)
  
  
}


complete_exclusion_by_task <- function(raw_data){
  
  
  ##### Set up iterating materials ##### 
  
  task_dict <- Dict$new()
  task_dict["ebbinghaus-keyboard"] <- 34
  #task_dict["draw-circles"] <- 1
  #task_dict["circle-label"] <- 1
  task_dict["RMTStrain"] <- 4
  task_dict["RMTStest"] <- 4
  task_dict["horizon-sticker"] <- 1
  task_dict["free-description-response"] <- 7 # can be greater than
  task_dict["attribution-Q1"] <- 2 # can be greater than 2
  task_dict["pen-choice"] <- 1 
  
  
  
  task_list <- 
    c("ebbinghaus-keyboard", # ebbinghaus 10 +24 = 34 trials
      #"draw-circles", #syms 1 
      #"circle-label", #syms 1
      "RMTStrain", #RMTS train 4
      "RMTStest", #RMTS test 4
      "horizon-sticker",
      "free-description-response", # 14
      "attribution-Q1", #2 trials
      "pen-choice") # pen choice 
  #)# syms 1)
  
  
  
  num_id <- unique(raw_data$subject)
  id_to_exclude <- c()
  
  
  EBB_complete_exclude <- c()
  RMTS_complete_exclude <- c()
  HZ_complete_exclude <- c()
  FD_complete_exclude <- c()
  CA_complete_exclude <- c() 
  PC_compelte_exclude <- c()
  for (id in num_id){
    id_data <- raw_data %>% filter(subject == id)
    # check EBB completeness 
    if ((filter(id_data, trial_type == "ebbinghaus-keyboard") %>% 
        count()) < task_dict["ebbinghaus-keyboard"]){
      EBB_complete_exclude <- c(ebb_complete_exclude, id)
    }
    
    # check RMTS completeness 
    if ((filter(id_data, trial_type == "RMTStest") %>% 
         count()) < task_dict["RMTStest"]){
      RMTS_complete_exclude <- c(RMTS_complete_exclude, id)
    }
    # check HZ completeness
    if ((filter(id_data, trial_type == "horizon-sticker") %>% 
         count()) < task_dict["horizon-sticker"]){
      HZ_complete_exclude <- c(HZ_complete_exclude, id)
    }
    # check FD completeness 
    if ((filter(id_data, trial_type == "free-description-response") %>% 
         filter((str_length(as.character(responses))) > SHORTEST_CHAR) %>% 
         count()) < task_dict["free-description-response"]){
      FD_complete_exclude <- c(FD_complete_exclude, id)
    }
    # check CA completeness 
    if ((filter(id_data, trial_type == "attribution-Q1") %>% 
         filter((str_length(as.character(responses))) > SHORTEST_CHAR) %>% 
         count()) < task_dict["attribution-Q1"]){
      
      CA_complete_exclude <- c(CA_complete_exclude, id)
    }
    
    # check PC completeness 
    if ((filter(id_data, trial_type == "pen-choice") %>% 
         count()) < task_dict["pen-choice"]){
      PC_compelte_exclude <- c(PC_compelte_exclude, id)
    }
    
  }
  
  
  summary_complete_df <- num_id %>% 
    enframe() %>% 
    rename(
      subject = value
    ) %>% 
    mutate("reason_EBB_complete" = subject %in% EBB_complete_exclude, 
           "reason_RMTS_complete" = subject %in% RMTS_complete_exclude, 
           "reason_HZ_complete" = subject %in% HZ_complete_exclude,
           "reason_FD_complete" = subject %in% FD_complete_exclude,
           "reason_CA_complete" = subject %in% CA_complete_exclude)
  
  return(summary_complete_df)
  
}