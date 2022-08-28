get_RMTS_for_exclusion <- function(raw_df){
  raw_df %>% filter(trial_type == "rmts-test") %>% 
    select(subject, culture, choice, sameSide, samePair, diffPair) %>% 
    mutate(
      task_name = "RMTS", 
      choice = as.character(choice)) %>% 
    select(subject, culture, choice)
  
}


get_RMTS_raw <- function(raw_d){
  
  
  
  RMTS_table <- raw_d %>% filter(trial_type == "rmts-test") %>% 
    select(subject, culture, choice, sameSide, samePair, diffPair) %>% 
    mutate(
      task_name = "RMTS", 
      choice = as.character(choice), 
      task_info = "RMTS", 
      trial_info = "RMTS", 
      resp_type = "choice_match", 
      sameSide = as.character(sameSide), 
      trial_choice = paste0(choice, sameSide), 
      trial_choice = case_when(
        (trial_choice == "rightTestButtonR") ~ "obj",
        (trial_choice == "rightTestButtonL") ~ "rel", 
        (trial_choice == "leftTestButtonR") ~ "rel", 
        (trial_choice == "leftTestButtonL") ~ "obj", 
      )
    ) %>% 
    select(subject, culture, task_name, task_info, trial_info, resp_type, trial_choice)
  
  RMTS_table$trial_num <- sequence(rle(RMTS_table$subject)$lengths)
  
  return(RMTS_table)

}

get_RMTS_main <- function(raw_d){
  RMTS_table <- get_RMTS_raw(raw_d)
  
  RMTS_main <- RMTS_table %>% 
    mutate(resp = case_when(
      trial_choice == "obj" ~ 0,
      trial_choice == "rel" ~ 1 
    )) %>% 
    select(subject, culture, task_name, task_info, trial_info, resp_type, resp)
  
  return(RMTS_main)
}

