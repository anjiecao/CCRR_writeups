
get_FD_coding_sheet <- function(raw_d){
  
  MIN_RESPONSE_LENGTH = 19
  
  
  fd_response <- raw_d %>% filter(
    trial_type == "free-description-response") %>% 
    group_by(subject) %>% 
    mutate(trial_num = row_number()) %>% 
    filter((str_length(as.character(responses))) >= 10) %>% # get rid of shorter response
    select(subject, culture, responses, rt, trial_num) 
  
  fd_stimulus <- raw_d %>% filter(
    trial_type == "image-keyboard-response" & str_detect(stimulus, "free_description")
  ) %>% 
    mutate(subject = as.character(subject)) %>% 
    select(subject,culture,  stimulus) 
  
  
  fd_stimulus$trial_num <- sequence(rle(fd_stimulus$subject)$lengths) #add trial num for each participant

  fd_table <- fd_stimulus %>% 
    left_join(fd_response, by = c("subject", "trial_num", "culture")) %>% 
    filter(!is.na(responses)) %>% 
    mutate(task_name = "FD", 
           trial_RT = rt) %>%  
    rowwise() %>% 
    mutate(trial_raw = 
             unlist(fromJSON(as.character(responses))),
           ) %>% 
    select(subject, culture, task_name, stimulus,trial_raw, trial_num) %>% 
    mutate(
      codeable = 1, 
      first_mention_focal = 1, 
      bckgrd_description = 0, 
      focal_description = 0, 
      coder = ""
    ) %>% 
    arrange(stimulus)
  
  fd_table_cn <- fd_table %>% filter(culture == "CN")
  fd_table_us <- fd_table %>% filter(culture == "US")
  
  write_csv(fd_table_cn, here("data/to_be_annotated/CN/FD.csv"))
  write_csv(fd_table_us, here("data/to_be_annotated/US/FD.csv"))
  
}


get_FD_main <- function(){
  
  CN_FD_ANNOTATED_PATH <- here("data/01b_annotated_data/exp2/CN/FD.csv")
 
  US_FD_ANNOTATED_PATH <- here("data/01b_annotated_data/exp2/US/FD.csv")
 
  ##### Extract the FD ###########
  
  cn_fd <- read_csv(CN_FD_ANNOTATED_PATH) %>% 
    mutate(culture = "CN")
  us_fd <- read_csv(US_FD_ANNOTATED_PATH) %>% 
    mutate(culture = "US")
  fd <-  bind_rows(us_fd, cn_fd) %>% 
    filter(codeable == 1) %>% 
    mutate(
      task_name = "FD", 
      task_info = "FD", 
      trial_info = stimulus
    ) %>% 
    pivot_longer(cols = first_mention_focal, 
                 names_to = "resp_type", 
                 values_to = "resp") %>% 
    select(
      subject, culture, task_name, task_info, 
      trial_info, resp_type, resp
    )
    
  return(fd)
  
}

get_FD_exclusion <- function(){
  
  CN_FD_ANNOTATED_PATH <- here("data/01b_annotated_data/exp2/CN/FD.csv")
  
  US_FD_ANNOTATED_PATH <- here("data/01b_annotated_data/exp2/US/FD.csv")
  
  ##### Extract the participants who have more than 25% uncodeable ###########
  
  cn_fd <- read_csv(CN_FD_ANNOTATED_PATH) %>% 
    mutate(culture = "CN")
  us_fd <- read_csv(US_FD_ANNOTATED_PATH) %>% 
    mutate(culture = "US")
  fd_exclusion_df <-  bind_rows(us_fd, cn_fd) %>% 
    group_by(subject) %>% 
    summarise(mean_codeable = mean(codeable)) %>% 
    filter(mean_codeable < 0.75) %>% 
    select(subject) %>% 
    mutate(exclude_reason = "FD_>.25uncodeable")

  
  return(fd_exclusion_df)
  
}

