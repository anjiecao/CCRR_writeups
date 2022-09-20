

get_d_ci_df <- function(d, main_df, task_name_str){
  
  subject_count_df <- main_df %>% 
    filter(task_name == task_name_str) %>% 
    distinct(subject, culture) %>% 
    group_by(culture) %>% 
    count()
  
  var_d <- (sum(subject_count_df$n)/prod(subject_count_df$n)) + 
    (d^2) / (2 * (sum(subject_count_df$n)))
  
  ci_lower <- d - 1.96 * sqrt(var_d)
  ci_upper <- d + 1.96 * sqrt(var_d)
  
  d_ci_df <- tibble(task_name = task_name_str, 
                    d = d, 
                    d_ci_lower = ci_lower, 
                    d_ci_upper = ci_upper)
  
  return (d_ci_df)
}

get_d_partial_pooling <- function(main_df, model, task_name_str){
  
  # task_name_str 
  interaction_effect_task <- c("CD", "CA", "EBB")
  
  # get variance 
  varcor_df <- summary(model)$varcor %>% 
    as.data.frame() %>% 
    # not entirely clear what this means 
    # but the doc says: second variable (NA for variance parameters)
    filter(is.na(var2)) 
  
  # calculate the pooled variance 
  pooled_var <- sqrt(sum(varcor_df$vcov))
  
  # get estimate for fixed effect
  fixed_effect_df <- summary(model)$coefficients %>% 
    as.data.frame() %>% rownames_to_column("term_name") 
  
  # branching because some cultural effects are main and some are interactions
  if(!(task_name_str %in% interaction_effect_task)){
    estimate <- fixed_effect_df %>% 
      filter(term_name == "cultureUS") %>% 
      pull(Estimate)
  }else{
    if(task_name_str == "EBB"){
      estimate <- fixed_effect_df %>% 
        filter(grepl("cultureUS:contextNC:size_diff", term_name)) %>% 
        pull(Estimate)
    }else{
      estimate <- fixed_effect_df %>% 
        filter(grepl("cultureUS:", term_name)) %>% 
        pull(Estimate)
    }
    
    
  }
  
  d <- estimate / pooled_var
  d_ci_df <- get_d_ci_df(d, main_df, task_name_str)
  
  return (d_ci_df)
  
}

get_d_no_pooling <- function(main_df, task_name_str){
  # needs to make sure it's us - cn to keep it consistent 
  # in complete yet 
  
  if(task_name_str == "SSI"){
    
    task_df <- main_df %>% 
      filter(resp_type == "task_score_ratio") %>% 
      filter(task_name == task_name_str) %>% 
      mutate(resp = as.numeric(resp)) 
    
  }else if(task_name_str == "CP"){
    task_df <- main_df %>% 
      filter(task_name == task_name_str)
    
  }else if(task_name_str == "SI"){
    task_df <- main_df %>% 
      filter(task_name == task_name_str) %>% 
      filter(resp_type == "inflation_score_ratio")
  }else if(task_name_str == "CA"){
    # for study 1
    task_df <- main_df %>% 
      # first is for s1, then for s2
      filter(grepl("situation", resp_type) | 
               grepl("situation", trial_info))
  }else if(task_name_str == "HZ"){
    task_df <- main_df %>% 
      filter(task_name == "HZ", resp_type == "hz_height")
  }
  
  
  mean_df <- task_df %>% 
    group_by(culture) %>% 
    summarise(mean_resp = mean(resp, na.rm = TRUE))
  sd <- sd(task_df$resp, na.rm = TRUE)
  
  d = (filter(mean_df, culture == "US")$mean_resp - filter(mean_df, culture == "CN")$mean_resp) / sd
  
  d_var = get_d_ci_df(d, main_df, task_name_str)
  return (d_var)
  
}