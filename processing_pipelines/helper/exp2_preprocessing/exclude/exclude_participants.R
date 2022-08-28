

get_exclude_CA_response_bias <- function(raw_df){
  get_CA_main(raw_df) %>% 
    group_by(subject) %>% 
    summarise(sd_resp = sd(resp, na.rm = TRUE)) %>% 
    filter(sd_resp == 0) %>% 
    mutate(exclude_reason = "CA_responsebias") %>% 
    select(subject, exclude_reason)
}

get_exclude_RMTS_response_bias <- function(raw_df){
  get_RMTS_for_exclusion(raw_df) %>% 
    group_by(subject, choice) %>% 
    summarise(n = n()) %>% 
    filter(n == 4) %>% 
    ungroup() %>% 
    mutate(exclude_reason = "RMTS_responsebias") %>% 
    select(subject, exclude_reason)
}

get_exclude_triads_response_bias <- function(raw_df){
  
  get_triads_for_exclusion(raw_df) %>% 
    group_by(subject, item_response_position) %>% 
    summarise(n = n()) %>% 
    filter(n > 29) %>% 
    mutate(exclude_reason = "triads_responsebias")
}

get_exclude_ravens_response_bias <- function(raw_df){
  get_RV_response_bias(raw_df) %>% 
    group_by(subject, response_option) %>% 
    summarise(n = n()) %>% 
    filter(n == 13) %>% 
    select(subject) %>% 
    mutate(exclude_reason = "ravens_responsebias")
  
}

get_exclude_demog_response_bias <- function(raw_df){
  extract_demog_questionaire(raw_df) %>% 
    group_by(subject, demog_response) %>% 
    summarise(n = n()) %>% 
    filter(n == 34) %>% 
    select(subject) %>% 
    mutate(exclude_reason = "demog_responsebias")
}

exclude_participants_main <- function(raw_df){
  
  # Causal attribution: same response on likert scale 
  exclude_CA_response_bias_df <- get_exclude_CA_response_bias(raw_df)
  # RMTS: same response on all trials 
  exclude_RMTS_response_bias_df <- get_exclude_RMTS_response_bias(raw_df)
  # Triads: same response order on all trials 
  exclude_triads_response_bias_df <- get_exclude_triads_response_bias(raw_df)
  # Ravens: same response on all trials 
  exclude_raven_response_bias_df <- get_exclude_ravens_response_bias(raw_df)
  # Demographic: same response on all scales  
  exclude_demog_response_bias_df <- get_exclude_demog_response_bias(raw_df)
  
  exclude_participants_df <- bind_rows(exclude_CA_response_bias_df, 
                                       exclude_RMTS_response_bias_df, 
                                       exclude_triads_response_bias_df, 
                                       exclude_raven_response_bias_df, 
                                       exclude_demog_response_bias_df) %>% 
    select(subject, exclude_reason)
  
  return (exclude_participants_df)
  
}