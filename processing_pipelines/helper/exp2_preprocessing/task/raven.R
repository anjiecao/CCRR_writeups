
get_RV_response_bias <- function(raw_df){
  raw_df %>% 
    filter(variable_type == "raven") %>% 
    select(subject, culture, responses) %>% 
    mutate(responses = map(responses, ~ fromJSON(.) %>% as.data.frame())) %>% 
    unnest(responses) %>% 
    pivot_longer(cols = a:l, names_to = "trial_info", values_to = "responses") %>% 
    filter(!is.na(responses)) %>% 
    rowwise() %>% 
    # the last part is the option order
    mutate(response_option = (sub(paste0('.+/',trial_info, '(.+)'), '\\1', responses))) %>% 
    select(subject, culture, response_option)
}


get_RV_main <- function(raw_df){
  
  rv_df <- raw_df %>% 
    filter(variable_type == "raven")
  
  correct_answer <- c("p4", "a7", "b6", "c8", 
                      "d2", "e1", "f5", "g1", 
                      "h6", "i3", "j2", "k4", "l5")
  
  check_answer <- function(str){
    match = 0 
    for (key in correct_answer){
        if (grepl(key, str)){
          match = match + 1
        }
    }
    
    return (match)
    
  }
  
  scored_rv_df <- rv_df %>% 
    rowwise() %>% 
    mutate(
      raven_correct = check_answer(responses)
    ) %>% 
    group_by(subject) %>% 
    mutate(
      trial_info = as.character(row_number()), 
      task_name = "RV", 
      task_info = "RV", 
      resp_type = "acc", 
      resp = raven_correct
    ) %>% 
    select(
      subject, culture,task_name, task_info, trial_info, 
      resp_type, resp
    )
  
  return(scored_rv_df)
  
  
}