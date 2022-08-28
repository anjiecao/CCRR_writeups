get_familiarity_coding <- function(raw_df){
  clean_familiarity_coding <- raw_df %>% 
    filter(trial_type == "funnel-debriefing-general") %>% 
    select(subject, culture, responses) %>% 
    mutate(responses = map(responses, ~ fromJSON(.) %>% as.data.frame)) %>% 
    unnest(responses) %>% 
    pivot_longer(cols = c("familiarity", "tellmore"), 
                 names_to = "prompt", 
                 values_to = "response", 
                 values_drop_na = TRUE) %>% 
    mutate(excluded_task = "", coder = "")

  
  cleaned_us_sheet <- clean_familiarity_coding %>% filter(culture == "US")
  cleaned_cn_sheet <- clean_familiarity_coding %>% filter(culture == "CN")
  
  write_csv(cleaned_us_sheet, here("data/to_be_annotated/US/familiarity.csv"))
  write_csv(cleaned_cn_sheet, here("data/to_be_annotated/CN/familiarity.csv"))  
}

