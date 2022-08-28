library(tidyverse)

get_EBB_raw <- function(raw_d){
  EB_table <- raw_d %>%
    filter(trial_type == "ebbinghaus-keyboard") %>%      
    separate(stimulus, into = c("path_a","path_b","stim_name"),sep = "/") %>%
    separate(stim_name, into = c("stim_id", "trial_stim_left", "trial_stim_right"), sep = "_") %>% 
    mutate(
      trial_stim_left = gsub("-.*$", "", trial_stim_left),
      trial_stim_right = gsub("-.*$","",trial_stim_right)
    )%>% 
    select(subject, culture, key_press, rt, answer_correct, stim_id, trial_stim_left, trial_stim_right) %>% 
    mutate(
      task_name = "EB",
      block_type = case_when(
        stim_id == "N" ~ "NC", 
        stim_id == "H" ~ "HELPFUL",
        stim_id == "I" ~ "IL"
      ), 
      trial_stim_right = gsub(".jpg","", trial_stim_right), 
      trial_key_pressed = case_when(
        key_press == 77 ~ "m", 
        TRUE ~ "z"
      ), 
      trial_correct = answer_correct,
      temp_id = paste(subject, block_type), 
      trial_RT = rt
    )
  
  EB_table$trial_num <- sequence(rle(EB_table$temp_id)$lengths)
  EB_table_summarize <- EB_table %>% 
    group_by(subject,block_type) %>% 
    summarise(
      block_RT = mean(as.numeric(trial_RT)),
      block_correct = mean(as.numeric(trial_correct))
    ) %>% 
    mutate(temp_id = paste(subject, block_type))
  
  EB_table <- left_join(EB_table, EB_table_summarize, by = "temp_id") %>% 
    mutate(subject = subject.x,
           block_type = block_type.x) %>% 
    select(subject, culture, task_name, block_type, block_RT, block_correct, trial_num, trial_stim_left, trial_stim_right, trial_key_pressed, trial_RT, trial_correct)
  
  return(EB_table)
  
}


get_EBB_main <- function(raw_d){
  EB_table <- get_EBB_raw(raw_d)
  EBB_main <- EB_table %>% 
    mutate(
      trial_stim_left = as.numeric(trial_stim_left), 
      trial_stim_right = as.numeric(trial_stim_right), 
      trial_correct = as.numeric(trial_correct),
      trial_info = as.character(abs(trial_stim_left - trial_stim_right)),
      task_info = block_type, 
      task_name = "EBB", 
      resp_type = "acc", 
      resp = trial_correct,
    ) %>% 
    select(
      subject, culture,task_name, task_info, trial_info, 
      resp_type, resp
    ) 
  
  return(EBB_main)
  
}