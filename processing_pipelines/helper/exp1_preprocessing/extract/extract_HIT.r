library(here)
library(tidyverse)

RAW_FILE_PATH <- here("data/raw_data/anonymized_All.csv")
OUT_FILE_PATH <- here("data/raw_data/HIT_Raw_All.csv")


extract_HIT_table <- function(in_path, out_path, culture_string, task, 
                              coded_subject = c()){
  
MIN_RESPONSE_LENGTH = 19
  
d <- read.csv(in_path) %>% 
  filter(culture == culture_string) %>% 
  filter(!(subject%in%coded_subject))


##### Free Description ##### 

fd_response <- d %>% filter(
  trial_type == "free-description-response") %>% 
  filter((str_length(as.character(responses))) >= MIN_RESPONSE_LENGTH) %>% # get rid of shorter response
  select(subject, responses, rt) 

fd_stimulus <- d %>% filter(
  trial_type == "image-keyboard-response" & str_detect(stimulus, "free_description")
) %>% 
  mutate(subject = as.character(subject)) %>% 
  select(subject, stimulus) 


fd_stimulus$trial_num <- sequence(rle(fd_stimulus$subject)$lengths) #add trial num for each participant

fd_table <- bind_cols(fd_stimulus, fd_response) %>% 
  mutate(task_name = "FD", 
         trial_RT = rt) %>%  
  rowwise() %>% 
  mutate(trial_raw = 
           unlist(fromJSON(as.character(responses))),
         subject = subject...1) %>% 
  select(subject, task_name, stimulus,trial_raw, trial_num) %>% 
  mutate(
    codeable = 1, 
    first_mention_focal = 1, 
    bckgrd_description = 0, 
    focal_description = 0, 
    imada_bckgrd_description = 0,
    imada_focal_description = 0,
    full_bckgrd_description = 0,
    full_focal_description = 0,
    coder = ""
  )




##### Causal Attribution ##### 

CA_table <- d %>% 
  filter(trial_type == "attribution-Q1") %>% 
  filter((str_length(as.character(responses))) >= MIN_RESPONSE_LENGTH)  %>% 
  rowwise() %>% 
  mutate(
    task_name = "CA",
    stimulus = "CA",
    trial_raw = unlist(fromJSON(as.character(responses))), 
    subject = as.character(subject)
  ) %>% 
  select(subject, task_name, trial_raw)

CA_table$trial_num <- sequence(rle(CA_table$subject)$lengths)

CA_table <- CA_table %>% 
  mutate(
    stimulus = case_when(
      trial_num == 1 ~ "Kelly", 
      trial_num == 2 ~ "Lucy"
    ), 
    codeable = 1, 
    person_attribution = 0,
    situation_attribution = 0,
    coder = ""
  )

if (task == "FD"){
  write_csv(fd_table, out_path)
  return(fd_table)
}else if (task == "CA"){
  write_csv(CA_table, out_path)
  return(CA_table)
}

}


extract_funnel_feedback <- function(in_path, out_path, culture_string){
  
  MIN_RESPONSE_LENGTH = 19
  
  d <- read.csv(in_path) %>% 
    filter(culture == culture_string)
  
  
  ##### Funnel Debriefing familiarity  ##### 
  
  funnel_familiarity_table <- d %>% 
    filter(variable_type == "funnel_familiarity") %>% 
    select(subject, variable_type, responses) %>% 
    toJSON() %>% 
    fromJSON() %>% 
    mutate(
      trial_raw = map(responses, ~ fromJSON(.) %>% as.data.frame())) %>% 
    unnest(trial_raw) %>% 
    mutate(question_type = "familiarity", 
           question_answer = familiarity) %>% 
    select(-c(responses, variable_type, familiarity)) 
  
  ##### Funnel Debriefing say more  ##### 
  funnel_tell_more_table <- d %>% 
    filter(variable_type == "funnel_tell_more") %>% 
    select(subject, variable_type, responses) %>% 
    toJSON() %>% 
    fromJSON() %>% 
    mutate(
      trial_raw = map(responses, ~ fromJSON(.) %>% as.data.frame())) %>%
    unnest(trial_raw) %>%
    mutate(question_type = "saymore", 
           question_answer = tellmore) %>% 
    select(-c(responses, variable_type, tellmore)) 
  
  ##### General Feedback  ##### 
  
  funnel_final_feedback <- d %>% 
    #FIXME: Currently traditional way not working bc forgot to put data variable type in general
    filter(grepl("finalfeedback", responses)) %>% 
    select(subject, responses) %>% 
    toJSON() %>% 
    fromJSON() %>% 
    mutate(
      trial_raw = map(responses, ~ fromJSON(.) %>% as.data.frame())) %>%
    unnest(trial_raw) %>% 
    mutate(question_type = "final_feedback", 
           question_answer = finalfeedback) %>% 
    select(-c(responses, finalfeedback))



funnel_feedback_table <- bind_rows(funnel_familiarity_table,
                                   funnel_tell_more_table, 
                                   funnel_final_feedback)


write.csv(funnel_feedback_table, out_path)
return(funnel_feedback_table)
}


extract_SymS_Label_table <- function(INPUT_PATH, OUT_FILE_PATH, culture_string){
  
  check_basic_label <- function(str, culture_string){
    count = 0
    if (culture_string == "US"){
    kin_term <- c("mom","dada", "mommy", "daddy", "mother", "father", "papa", "mama", "mum","da", "ma", "pa")
    } else{
    kin_term <- c("妈","爸", "妈妈", "爸爸", "爹", "娘", "老爸", "老妈")
    }
    #str <- strsplit(str, " ")
    for (k in kin_term){
      if (str_detect(str, regex(k, ignore_case = TRUE))){
        count = count + 1
      }
    }
    
    if (count == 0) {
      return (TRUE) 
    }else{
      return(FALSE)
    }
  }
  
  create_SymS_label_df <- function(raw_d){
    
    label_table <- raw_d %>% 
      filter(trial_type == "circle-label") %>% 
      select(subject, responses) %>%
      toJSON() %>% 
      fromJSON() %>%
      mutate(
        circ_label = map(responses, ~ fromJSON(.) %>% as.data.frame())) %>% 
      unnest(circ_label) %>% 
      mutate(
        Q_Me = "Me"
      ) %>% 
      relocate(Q_Me,.before = Q0) %>% 
      pivot_longer(Q_Me:Q6, names_to = "label_names", values_to = "circ_label") %>% 
      filter(circ_label != "")  %>% 
      select(subject, circ_label) %>% 
      group_by(subject) %>% 
      mutate(label_all = paste(circ_label, collapse = " ")) %>% 
      distinct(subject, label_all)
    
    return(label_table)
    
    
  }
  
  
  create_SymS_circle_df <- function(raw_d){
    circ_table <- raw_d %>% 
      filter(trial_type == "draw-circles") %>% 
      select(subject, locations) %>% 
      toJSON() %>% 
      fromJSON() %>% 
      mutate(
        circle_drawn = map(locations, ~ fromJSON(.) %>% as.data.frame())) %>% 
      unnest(circle_drawn) %>% 
      select(-locations)
    
    return(circ_table)
    
  }
  
  raw_data <- read.csv(INPUT_PATH) %>% filter(culture == culture_string)
  
  horizon_df <- create_horizon_df(raw_data)
  syms_label_df <- create_SymS_label_df(raw_data)
  
  syms_label_to_check <- syms_label_df %>% 
    filter(check_basic_label(label_all,culture_string)) %>% 
    mutate(
      task_name = "symsLabel",
      task_resp = label_all, 
      human_rate = ""
    ) %>% 
    select(subject, task_name,task_resp, human_rate)
  
  write_csv(syms_label_to_check, OUT_FILE_PATH)
  
  
  
  
  
} 