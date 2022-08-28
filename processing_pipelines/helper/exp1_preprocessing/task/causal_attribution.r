get_CA_main <- function(raw_d){
  
  CN_CA_ANNOTATED_PATH <- here("data/01b_annotated_data/exp1/CN/cn_ca_merged_coded.csv")
  US_CA_ANNOTATED_PATH <- here("data/01b_annotated_data/exp1/US/us_ca_coded.csv")
  included_subject <- raw_d %>% 
    select(subject) %>% 
    distinct() %>% 
    pull()
  
  ##### Extract the FD ###########
  
  cn_ca <- read_csv(CN_CA_ANNOTATED_PATH) %>% 
    mutate(culture = "CN")
  us_ca <- read_csv(US_CA_ANNOTATED_PATH) %>% 
    mutate(culture = "US")
  ca <- bind_rows(us_ca, cn_ca) %>% 
    filter(subject %in% included_subject) %>% 
    mutate(
      task_name = "CA", 
      task_info = "CA", 
      trial_info = stimulus) %>% 
    pivot_longer(cols = person_attribution:situation_attribution, 
                 names_to = "resp_type", 
                 values_to = "resp") %>% 
    select(
      subject, culture, task_name, task_info, 
      trial_info, resp_type, resp
    ) 
  
  return(ca)
  
  
  
  
  
  
  
  
}