get_CA_raw <- function(raw_df){
  # the unequal number of rows from response is due to 
  # for situational factor in one story, we eliminated one option 
  # "The ruthless and brutal behavior of Chinese Communists set an example for him."
  
  raw_CA_df <- raw_df %>% 
    filter(grepl("personal", responses)) %>% 
    select(subject, culture, responses) %>% 
    mutate(responses = map(responses, ~ fromJSON(.) %>% as.data.frame())) %>% 
    unnest(responses) %>% 
    select(subject, culture, starts_with("personal") | starts_with("situational")) %>% 
    pivot_longer(cols = -c("subject", "culture"), 
                names_to = "question_type", 
                values_to = "response")
  
  return(raw_CA_df)

}


get_CA_main <- function(raw_df){
  
 raw_CA_df <- get_CA_raw(raw_df)
  main_CA_df <- raw_CA_df %>% 
    mutate(
      task_name = "CA",
      task_info = case_when(
      grepl("personal", question_type) ~ "personal", 
      grepl("situational", question_type) ~ "situational"
    )) %>% 
    rename(trial_info = question_type, 
           resp = response) %>% 
    mutate(resp_type = "likert_rating") %>% 
    select(subject, culture, task_name, task_info, trial_info, resp_type, resp) 
  
  return(main_CA_df)
  
}