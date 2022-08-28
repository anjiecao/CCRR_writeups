library(tidyverse)
library(here)

#INPUT_PATH <- here("data/processed_data/demog_All.csv")

#d <- read.csv(INPUT_PATH)




demog_exclusion <- function(raw_data, culture, 
                            check_exclusion = FALSE, 
                            detail_table = TRUE){
  
  
  #[CN/US] participants who report living abroad for more than 2 years in regions 
  # with predominantly [European/Asian] populations (respectively).
 
  
  
  contain_region <- function(str, culture){
    
    if (culture == "US"){

      no_abroad_regions <- c("Asia")
      
    }else if (culture == "CN"){
      no_abroad_regions <- c("北美洲",
                             "南美洲", 
                             "欧洲", 
                             "非洲",
                             "澳洲")
    }
    
    
    match = 0
    for (r in no_abroad_regions){
      if (grepl(r, str)){
        match = match + 1
      }
    }
    return (match == 0)
  }
  
  
    abroad_exp_sbj <- raw_data %>% 
      filter(culture == culture) %>% 
      filter(demog_question == "which_regions") %>% 
      filter(!is.na(demog_response))  
  
  
  
  # make sure that they actually have abroad experience
  if (length(abroad_exp_sbj %>% 
             distinct(subject) %>% 
             pull(subject)) != 0){
  
    abroad_exp_sbj <- abroad_exp_sbj %>% 
    rowwise() %>% 
    filter(!(contain_region(demog_response, culture))) %>% 
    filter(!is.na(demog_response)) %>% 
    distinct(subject) %>% 
    pull(subject)
    
  }
  
  
  lang_flu_sbj <- raw_data %>% 
    filter(culture == culture) %>% 
    
    filter(demog_question == "lang_sp" | demog_question == "lang_ud") %>% 
    mutate(demog_response = as.numeric(demog_response)) %>% 
    filter(demog_response >= 3) %>% 
    distinct(subject) %>% 
    pull(subject)

  
   total_sbj <- raw_data %>% 
     filter(culture == culture) %>% 
     distinct(subject) %>% 
     pull()
  
  id_to_exclude <- c(abroad_exp_sbj,lang_flu_sbj)
  id_to_exclude = id_to_exclude[!duplicated(id_to_exclude)]
  
  
  # create a dataframe to report on exclusion reason 
  
  summary_df <- total_sbj %>% 
    enframe() %>% 
    rename(
      subject = value
    ) %>% 
    mutate("reason_abroad" = subject %in% abroad_exp_sbj, 
           "reason_language" = subject %in% lang_flu_sbj, 
           culture = culture)
  
  if (detail_table == TRUE){
    return (summary_df)
  }
  
  
  
  if (check_exclusion == TRUE){
    
    # if exclude due to abroad experience, then reason abroad has "true"
    
    
    summary_df %>% 
      group_by(reason_abroad, 
               reason_language) %>% 
      mutate(num = n()) %>%
      mutate(
        exclude_reason = case_when(
          reason_abroad == TRUE && reason_language == TRUE ~ "abroad and language",
          reason_abroad == FALSE && reason_language == TRUE ~ "language", 
          reason_abroad == TRUE && reason_language == FALSE ~ "abroad", 
          reason_abroad == FALSE && reason_language == FALSE ~ "included"
        )
      ) %>% 
      ggplot(aes(x = exclude_reason, y = num)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      labs(title = culture)
    
    
    
    
  }else{
  return (id_to_exclude)
  }
}
