get_semantic_intuition_answer_raw <- function(raw_df){
  raw_si_df <- raw_df %>% 
    filter(grepl("semantic_intuition", variable_type)) %>% 
    select(subject, culture, responses) %>% 
    mutate(trial_info = case_when(
      grepl("allheal1", responses) ~ "allheal1", 
      grepl("allheal2", responses) ~ "allheal2",
      grepl("monkpic1", responses) ~ "monkpic1", 
      grepl("monkpic2", responses) ~ "monkpic2", 
      grepl("claymen1", responses) ~ "claymen1", 
      grepl("claymen2", responses) ~ "claymen2", 
      grepl("superdog1", responses) ~ "superdog1", 
      grepl("superdog2", responses) ~ "superdog2", 
      grepl("walter1", responses) ~ "walter1",
      grepl("walter2", responses) ~ "walter2"
      
    ), 
    raw_response = case_when(
      grepl("Yes", responses) ~ "Yes", 
      grepl("No", responses) ~ "No", 
      grepl("错误", responses) ~ "错误", 
      grepl("正确", responses) ~ "正确"
    )) %>% 
    select(subject, culture, trial_info, raw_response)
    
  return(raw_si_df)
}


get_semantic_intuition_answer_main <- function(raw_df){
  raw_si_df <- get_semantic_intuition_answer_raw(raw_df)
  
  main_si_df <- raw_si_df %>% 
    mutate(task_info = case_when(
      trial_info %in% c("allheal1", "allheal2","walter1","walter2", "monkpic1","monkpic2", 
                        "claymen1", "superdog2") ~ "control", 
      trial_info %in% c("superdog1", "claymen2") ~ "critical"
    )) %>% 
    mutate(
      task_resp = case_when(
        # allheal1: owen, no-control; allheal2: greg, yes-control 
        trial_info == "allheal1" & raw_response %in% c("错误", "No") ~ "correct",
        trial_info == "allheal1" & raw_response %in% c("正确", "Yes") ~ "incorrect",
        trial_info == "allheal2" & raw_response %in% c("错误", "No") ~ "incorrect",
        trial_info == "allheal2" & raw_response %in% c("正确", "Yes") ~ "correct",
        
        # walter1: walter, yes-control; walter2: james: yes-control
        trial_info %in% c("walter1", "walter2")  & raw_response %in% c("正确", "Yes") ~ "correct",
        trial_info %in% c("walter1", "walter2")  & raw_response %in% c("错误", "No") ~ "incorrect",
        
        # monkpic1: leo, no-control; monkpic2: leo, yes-control 
        trial_info == "monkpic1" & raw_response %in% c("错误", "No") ~ "correct",
        trial_info == "monkpic1" & raw_response %in% c("正确", "Yes") ~ "incorrect",
        trial_info == "monkpic2" & raw_response %in% c("错误", "No") ~ "incorrect",
        trial_info == "monkpic2" & raw_response %in% c("正确", "Yes") ~ "correct",
        
        #superdog1: pickles, critical; superdog2:blaze, no-control 
        trial_info == "superdog1" & raw_response %in% c("错误", "No") ~ "causal_historical",
        trial_info == "superdog1" & raw_response %in% c("正确", "Yes") ~ "descriptivist",
        trial_info == "superdog2" & raw_response %in% c("错误", "No") ~ "correct",
        trial_info == "superdog2" & raw_response %in% c("正确", "Yes") ~ "incorrect",
        
        #claymen1: alvin, no-control; claymen2:peter, critical 
        trial_info == "claymen1" & raw_response %in% c("错误", "No") ~ "correct",
        trial_info == "claymen1" & raw_response %in% c("正确", "Yes") ~ "incorrect",
        trial_info == "claymen2" & raw_response %in% c("错误", "No") ~ "causal_historical",
        trial_info == "claymen2" & raw_response %in% c("正确", "Yes") ~ "descriptivist"
      )
    ) %>% 
    mutate(task_name = "SeI", 
           resp_type = "answer") %>% 
    rename(resp = task_resp) %>% 
    select(subject, culture, task_name, task_info, trial_info, resp_type, resp) 
  
  return(main_si_df)
  
}


get_semantic_intuition_mc_coding_sheet <- function(raw_df){
  
  coding_sheet <- raw_df %>% 
    select(subject, culture, responses) %>% 
    filter(grepl("si_li", responses)) %>% 
    mutate(trial_info = case_when(
      grepl("allheal1", responses) ~ "allheal1", 
      grepl("allheal2", responses) ~ "allheal2",
      grepl("monkpic1", responses) ~ "monkpic1", 
      grepl("monkpic2", responses) ~ "monkpic2", 
      grepl("claymen_cc_1", responses) ~ "claymen1", 
      grepl("claymen_cc_2", responses) ~ "claymen2", 
      grepl("superdog1", responses) ~ "superdog1", 
      grepl("superdog2", responses) ~ "superdog2", 
      grepl("walter1", responses) ~ "walter1",
      grepl("walter2", responses) ~ "walter2"
    ), 
    raw_response = responses) %>% 
    select(subject, culture, trial_info, raw_response) %>% 
    mutate(response = 1, 
           coder = NA_character_) %>% 
    arrange(trial_info)
  
  cleaned_us_sheet <- coding_sheet %>% filter(culture == "US")
  cleaned_cn_sheet <- coding_sheet %>% filter(culture == "CN")
  
  write_csv(cleaned_us_sheet, here("data/to_be_annotated/US/SeI.csv"))
  write_csv(cleaned_cn_sheet, here("data/to_be_annotated/CN/SeI.csv"))
    
  
}

get_semantic_Intuition_mc_coded_sheet <- function(){
  
  us_coded_sheet <- read_csv(here("data/to_be_annotated/US/SeI.csv"))
  cn_coded_sheet <- read_csv(here("data/to_be_annotated/CN/SeI.csv"))
  
  cd_coded_mc <- bind_rows(us_coded_sheet, cn_coded_sheet) %>% 
    mutate(task_name = "SeI", 
           task_info = "SeI_MC", 
           resp_type = "correct", 
           resp = response) %>% 
    select(subject, culture, task_name, task_info, trial_info, resp_type, resp)
  
  
}

get_sei_exclude <- function(raw_df){
  
  raw_df %>% 
    filter(task_name == "SeI") %>% 
    filter(task_info == "control") %>% 
    mutate(resp = as.numeric((resp=="correct"))) %>% 
    group_by(subject) %>% 
    summarise(control_score = sum(resp)) %>% 
    filter(control_score <= 5) %>% 
    select(subject) %>% 
    mutate(exclude_reason = "SI_controlq")
  
}

