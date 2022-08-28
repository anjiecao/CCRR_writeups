

get_triads_for_exclusion <- function(raw_df){
  options_df <- raw_df %>% filter(trial_type == "triads") %>% 
    group_by(subject) %>% 
    mutate(page_num = row_number()) %>% 
    select(subject, culture, options, page_num) %>% 
    mutate(options = strsplit(options, split = ","),
           upper_option = map(options, ~ split(., 1:2)[[1]]),
           lower_option = map(options, ~ split(., 1:2)[[2]])
           ) %>% 
    select(subject, page_num, upper_option, lower_option)
  
  
  triads <- raw_df %>% 
    filter(trial_type == "triads") %>% 
    group_by(subject) %>% 
    mutate(page_num = row_number()) %>% 
    select(subject, culture, response, page_num) 
  
  # the two page responses are actually duplicates 
  # some data logging oddities 
  triads_response_cn <- triads %>% 
    filter(page_num == 1) %>% 
    filter(culture == "CN") %>% 
    mutate(response = map(response, 
                          ~ fromJSON(.) %>%  
                            as.data.frame())) %>% 
    unnest(response) %>% 
    pivot_longer(cols = -c("subject", "culture", "page_num"), 
                 names_to = "item", 
                 values_to = "item_response") 
  
  triads_response_us <- triads %>% 
    filter(page_num == 1) %>% 
    filter(culture == "US") %>% 
    mutate(response = map(response, 
                          ~ fromJSON(.) %>%  
                            as.data.frame())) %>% 
    unnest(response) %>% 
    pivot_longer(cols = -c("subject", "culture", "page_num"), 
                 names_to = "item", 
                 values_to = "item_response") 
  
  triads_response <- bind_rows(triads_response_cn, triads_response_us)
  
  triads_with_options <- triads_response%>% 
    left_join(options_df, by = c("subject", "page_num")) %>% 
    rowwise() %>% 
    mutate(item_response_position = ifelse(item_response %in% upper_option, "upper", "lower"))
    
  return (triads_with_options)
}





get_triads_raw <- function(raw_df){
  triads <- raw_df %>% 
    filter(trial_type == "triads") %>% 
    group_by(subject) %>% 
    mutate(page_num = row_number()) %>% 
    select(subject, culture, response, page_num) 
  
  # the two page responses are actually duplicates 
  # some data logging oddities 
  triads_page_1 <- triads %>% 
    filter(page_num == 1) %>% 
    mutate(response = map(response, 
                          ~ fromJSON(.) %>%  
                            as.data.frame())) %>% 
    unnest(response) %>% 
    pivot_longer(cols = -c("subject", "culture", "page_num"), 
                 names_to = "item", 
                 values_to = "item_response", 
                 values_drop_na = TRUE) %>% 
    select(-page_num)
  
  triads_page_2 <- triads %>% 
    filter(page_num == 2) %>% 
    mutate(response = map(response, 
                          ~ fromJSON(.) %>%  
                            as.data.frame())) %>% 
    unnest(response) %>% 
    pivot_longer(cols = -c("subject", "culture", "page_num"), 
                 names_to = "item", 
                 values_to = "item_response",
                 values_drop_na = TRUE) %>% 
    select(-page_num)
  
  triads_raw <- bind_rows(triads_page_1, triads_page_2)
  
  
  return(triads_raw)
  
}

score_triad_item <- function(raw_item, raw_item_response, triads_key){

  for(i in 1:nrow(triads_key)){
    if (grepl(raw_item, triads_key$cue[i])){
      tax_match = (triads_key$tax_match[i] == raw_item_response)
      return (tax_match)
    }
  }
}

get_triads_main <- function(raw_df){
  
  triads_key <- read_csv(here("processing_pipelines/helper/exp2_preprocessing/task/triads_answer.csv"))
  
  triads_raw <- get_triads_raw(raw_df)
  
  triads_raw <- triads_raw %>% 
    separate(item, into = c("item", "item_type"), sep = "_") 
  
  triads_only <- triads_raw %>% 
    filter(item_type == "triads") %>% 
    filter(!is.na(item_response))
  
  triads_raw$tax_match <- 
    mapply(score_triad_item, 
           triads_raw$item, 
           triads_raw$item_response, 
           MoreArgs=list(triads_key), 
           SIMPLIFY = FALSE)
  
  triads_triad_df <- triads_raw %>% 
    unnest(tax_match) %>% 
    mutate(
      task_name = "TD",
      task_info= "triads", 
      resp_type = "tax_match") %>% 
    rename(trial_info = item, 
           resp = tax_match) %>% 
    select(subject, culture, task_name, task_info, trial_info, resp_type, resp) 
    
  
  triads_catch_df <- triads_raw %>% 
    filter(item_type == "catch") %>% 
    mutate(resp = case_when(
      grepl("春天", item) & item_response == "春天" ~ TRUE, 
      grepl("猫", item) & item_response == "猫" ~ TRUE,
      grepl("spring", item) & item_response == "spring" ~ TRUE, 
      grepl("cat", item) & item_response == "cat" ~ TRUE,
      TRUE ~ FALSE
    )) %>% 
    mutate(task_name = "TD", 
           task_info = "catch", 
           trial_info = item, 
           resp_type = "catch_resp", 
           resp = resp) %>% 
    select(subject, culture, task_name, task_info, trial_info, resp_type, resp) 
  
  triads_main <- bind_rows(triads_triad_df, triads_catch_df)
  return(triads_main)


}

get_td_exclude <- function(raw_df){
  
  raw_df %>% 
    filter(task_name == "TD") %>% 
    filter(task_info == "catch") %>% 
    filter(resp == FALSE) %>% 
    select(subject) %>% 
    mutate(exclude_reason = "TD_failcatch")
  
}

