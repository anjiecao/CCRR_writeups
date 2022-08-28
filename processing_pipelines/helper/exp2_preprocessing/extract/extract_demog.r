

library(tidyverse)
library(here)
library(jsonlite)

#INPUT_PATH <- here("data/raw_data/anonymized_All.csv")
#OUTPUT_PATH <- here("data/processed_data/demog_All.csv")

extract_demog_questionaire <- function(raw_df){
  demog_questionaires_all <- raw_df %>% 
    filter(trial_type == "survey-likert" & 
             !grepl("personal", responses)) %>% # this is to get rid of the personal / situational ratings
    select(subject, culture, responses) %>% 
    mutate(scale_name = case_when(
      grepl("identity_local", responses) ~ "identity_local", 
      grepl("identity_global", responses) ~ "identity_global",
      grepl("consumption_local", responses) ~ "consumption_local", 
      grepl("consumption_global", responses) ~ "consumption_global", 
      grepl("cosmopolitanism", responses) ~ "cosmopolitanism"
    )) 
  
  all_scale_names <- c("identity_local", "identity_global", "consumption_local", 
                       "consumption_global", "cosmopolitanism")
  
  demog_questionaires_all_clean <- NULL
  
  for (name in all_scale_names){
    # bad practice to expanding dataframes within for loops but since 
    # we are not going to do this multipe times it is ok 
    demo_scale <- demog_questionaires_all %>% 
      filter(scale_name == name) %>% 
      mutate(responses = map(responses, ~ fromJSON(.) %>% as.data.frame())) %>% 
      unnest(responses) %>% 
      select(-c("scale_name")) %>% 
      pivot_longer(cols = -c("subject", "culture"), 
                   names_to = "demog_question", 
                   values_to = "demog_response") %>% 
      mutate(demog_response = as.character(demog_response)) # this is to make sure we can combine all dataframes together 
    
    demog_questionaires_all_clean <- bind_rows(demog_questionaires_all_clean, demo_scale)
    
  }
  return (demog_questionaires_all_clean)
}

# take raw_df 
# take culture string "US" or "CN"

extract_demog <- function(raw_df, culture_string){
  
  
  # need to double check if this is exhaustive
  # also there's weird Q0 and Q1 involved in demog question 
  # also weird which_regions issue 
  
  
  demog_fields <- c("demog_country_born_current_grew_up", 
                    "demog_county_zip_places_lived",
                    "demog_oversea_experience",
                    "demog_conditional_year_abroad_regions",
                    "demog_language",
                    "demog_conditional_language_target_fluency",
                    "demog_age_gender_ethnic",
                    "demo-objses",
                    "demog_current_in_college", 
                    "demog-education")
  
  
  # demog questionaires 
  demog_questionaires_all_clean <- extract_demog_questionaire(raw_df) %>% 
    filter(culture == culture_string)

  # non dropdown demog 
  demog <- raw_df %>%
    filter(culture == culture_string) %>% 
  filter(variable_type %in% demog_fields) %>% 
    select(subject, culture, trial_type, variable_type, responses) %>%
    filter(trial_type != "demog-dropdown-audio") %>% 
    group_by(subject) %>%
    mutate(responses = map(responses, ~ fromJSON(.) %>% as.data.frame())) %>%
    unnest(responses) %>%
    select(-variable_type) %>%
    group_by(subject) %>%
    mutate_at(vars(-group_cols()), function(x) {
      lapply((list(as.character(x))), function(x) unique(x[!is.na(x)]))
    }) %>%
    distinct() %>% 
    mutate_at(vars(-group_cols()), as.character) %>% 
    pivot_longer(
      cols = -c("subject", "culture", "trial_type"), 
      names_to = "demog_question", 
      values_to = "demog_response"
    ) %>% 
    mutate(
      demog_response = as.character(demog_response)
    ) %>% 
    select(-trial_type)
  
   demog_dropdown <- raw_df %>%
     filter(culture == culture_string) %>% 
     filter(variable_type %in% demog_fields) %>% 
     select(subject, trial_type, variable_type, responses) %>%
     filter(trial_type == "demog-dropdown-audio") %>% 
     group_by(subject) %>%
     mutate(responses = map(responses, ~ fromJSON(.))) %>%
     unnest(responses) %>% 
     group_by(subject, variable_type) %>% 
     #add row number to question type to seperate question from answer
     mutate(q_id = row_number()) %>% 
     # since answer often occurs second, it must be an even number 
     filter(q_id %% 2 == 0) %>% 
     mutate(q_id = q_id / 2)
   
   demog_dropdown_country_bcg <- demog_dropdown %>% 
     filter(variable_type == "demog_country_born_current_grew_up") %>% 
     mutate(
       demog_question = case_when(
         q_id == 1 ~ "country_born", 
         q_id == 2 ~ "current_in", 
         q_id == 3 ~ "state_grewup"
       )
     )
   
   demog_dropdown_age_gender_ethnic <- demog_dropdown %>% 
     filter(variable_type == "demog_age_gender_ethnic") %>% 
     mutate(
       demog_question = case_when(
         q_id == 1 ~ "age", 
         q_id == 2 ~ "gender", 
         q_id == 3 ~ "ethnic"
       )
     )
   
   demog_dropdown_langfluency <- demog_dropdown %>% 
     filter(variable_type == "demog_conditional_language_target_fluency") %>% 
     mutate(
       demog_question = case_when(
         q_id == 1 ~ "lang_sp", 
         q_id == 2 ~ "lang_ud"
       )
     )
   
   demog_dropdown_all <- bind_rows(demog_dropdown_country_bcg, 
                                   demog_dropdown_age_gender_ethnic, 
                                   demog_dropdown_langfluency) %>% 
     mutate(demog_response = as.character(responses)) %>% 
     select(subject, demog_question, demog_response)
   
   demog_all <- bind_rows(demog, demog_dropdown_all, demog_questionaires_all_clean) %>% 
     rowwise() %>% 
     mutate(culture = culture_string, 
            demog_response = if_else(demog_response == "character(0)", 
                                     NA_character_, 
                                     demog_response)) %>% 
     mutate(demog_question = case_when(
       demog_question == "Q0" ~ "lang_sp", 
       demog_question == "Q1" ~ "lang_ud",
       TRUE ~ demog_question
     )) %>% 
     
     select(-variable_type)
   
   
   
  
  
  return (demog_all)
}