get_change_detection_RT_raw <- function(raw_df){
  raw_df <- raw_df %>% 
    filter(trial_type == "change-detection") %>% 
    select(subject, culture, stimuli, trial_type, rt)
  
  return(raw_df)
}

score_stimulus_condition <- function(cd_res, stimulus){
  
  for (i in 1:nrow(cd_res)){
    if(grepl(cd_res$stimulus[[i]], stimulus)){
      return (cd_res$change_type[[i]])
    }
  }
  
}

score_stimulus_trial_info <- function(cd_res, stimulus){
  # this is so that it would correspond to the coded written response 
  for (i in 1:nrow(cd_res)){
    if(grepl(cd_res$stimulus[[i]], stimulus)){
      return (cd_res$prompt[[i]])
    }
  }
}


get_change_detection_RT_main <- function(raw_df){
  
  raw_cd_df <- get_change_detection_RT_raw(raw_df)
  
  cd_res <- read_csv(here("processing_pipelines/helper/exp2_preprocessing/task/cd_answer.csv")) %>% 
    janitor::clean_names()
  
  coded_df <- get_change_detection_coded() %>% 
    rename(trial_info = prompt)
  
  main_cd_df <- raw_cd_df %>% 
    mutate(task_name = "CD", 
           #task_info = "CD", 
           trial_info = stimuli, 
           resp_type = "RT", 
           resp = rt) %>% 
    rowwise() %>% 
    mutate(task_info = score_stimulus_condition(cd_res, trial_info), 
           trial_info = score_stimulus_trial_info(cd_res, trial_info))%>% 
    select(subject, culture, task_name, task_info, trial_info, resp_type, resp) %>% 
    ungroup()
  
  
  main_cd_df <- main_cd_df %>% 
    left_join(coded_df %>% select(subject, trial_info, correct), 
              by = c("subject", "trial_info")) %>% 
    filter(correct == 1) %>% 
    select(subject, culture, task_name, task_info, trial_info, resp_type, resp)
  
  
  return(main_cd_df)
  
}


get_change_detection_wr_coding_sheet <- function(raw_df){
  
  
  cleaned_df <- raw_df %>% 
    filter(variable_type == "change_detection_report") %>% 
    select(subject, culture, responses) %>% 
    mutate(responses = map(responses, ~ fromJSON(.) %>% as.data.frame)) %>% 
    unnest() %>% 
    pivot_longer(cols = starts_with("Description"), 
                 names_to = "prompt", 
                 values_to = "response", 
                 values_drop_na = TRUE) %>% 
    arrange(prompt) %>% 
    mutate(correct = 1) %>% 
    mutate(coder = "")
  
  cleaned_us_sheet <- cleaned_df %>% filter(culture == "US")
  cleaned_cn_sheet <- cleaned_df %>% filter(culture == "CN")
  
  write_csv(cleaned_us_sheet, here("data/to_be_annotated/US/CD_wr.csv"))
  write_csv(cleaned_cn_sheet, here("data/to_be_annotated/CN/CD_wr.csv"))
  
}


get_change_detection_coded <- function(){
  
  cn_coded <- read_csv(here("data/01b_annotated_data/exp2/CN/CD_wr.csv"))
  us_coded <- read_csv(here("data/01b_annotated_data/exp2/US/CD_wr.csv"))
  coded_df <- bind_rows(cn_coded, us_coded)
  return(coded_df)
  
}

get_change_detection_excluded <- function(){
  
  coded_df <- get_change_detection_coded()
  
  exclude_cd_df <- coded_df %>% 
    group_by(subject) %>% 
    summarise(mean_correct = mean(correct)) %>% 
    filter(mean_correct < .75) %>% 
    select(subject) %>% 
    mutate(exclude_reason = "CD_<.75codeable")
  
  return (exclude_cd_df)
}


get_change_detection_mc_coding_sheet <- function(raw_df){
  
 
 cleaned_df <- raw_df %>% 
    filter(variable_type == "change_detection_manipulation_check") %>% 
    select(subject, culture, prompt, variable_type, responses) %>% 
    rename(stimulus_displayed = prompt, 
           task_name = variable_type) %>% 
    mutate(focal_object_correct = 1, coder = NA_character_)
 
  cleaned_us_sheet <- cleaned_df %>% filter(culture == "US")
  cleaned_cn_sheet <- cleaned_df %>% filter(culture == "CN")
  
  write_csv(cleaned_us_sheet, here("data/to_be_annotated/US/CD_mc.csv"))
  write_csv(cleaned_cn_sheet, here("data/to_be_annotated/CN/CD_mc.csv"))
   
}

get_change_detection_mc_coded <- function(){
  us_coded_sheet <- read_csv(here("data/to_be_annotated/US/CD_mc.csv"))
  cn_coded_sheet <- read_csv(here("data/to_be_annotated/CN/CD_mc.csv"))
  
  cd_coded_mc <- bind_rows(us_coded_sheet, cn_coded_sheet) %>% 
    mutate(task_name = "CD", 
           task_info = "CD_MC", 
           trial_info = stimulus_displayed, 
           resp_type = "fo_correct", 
           resp = focal_object_correct) %>% 
    select(subject, subject, culture, task_name, task_info, trial_info, resp_type, resp)
  
}

