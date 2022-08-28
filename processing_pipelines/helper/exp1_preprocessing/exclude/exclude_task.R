#------------ exclude task.R ------------#
#This script contains function to exclude participants based on following criteria:
# Task-based: 
#   - Ebbinghaus : Side-bias
#   - RMTS: Side-bias
#   - Horizon-sicker: Did not put horizon sticker 
#   - Self-inflation: Check if the number of circles correspond with number of labels + 1


library(here)
library(tidyverse)



##### Create Task-specific Tidy Dataframe #####
create_Ebb_df <- function (raw_d){
  ebb_df <- raw_d %>% 
              filter(trial_type == "ebbinghaus-keyboard") %>% 
              mutate(
                block = if_else(grepl("N_", stimulus, fixed = TRUE), "first", "second"),
                key_char = if_else(key_press == 77, "m", "z")) %>% 
              group_by(subject, block, key_char) %>% 
              count() %>% 
              pivot_wider(names_from = key_char, values_from = n) %>% 
              mutate(
                percentage = m / (m + z),
                bias = case_when(
                  percentage < 0.1 ~ TRUE, 
                  percentage > 0.9 ~ TRUE, 
                  TRUE ~ FALSE
                )
              )
  return (ebb_df)
  
}

create_RMTS_df <- function(raw_d){
  RMTS_df <- raw_d %>% 
            filter(trial_type == "RMTStest") %>% 
            group_by(subject, choice) %>% 
            count() %>% 
            mutate(
              side_bias = if_else(n == 0 || n == 4, TRUE, FALSE)
            ) %>% 
            select(subject, choice, side_bias) %>% 
            pivot_wider(names_from = choice, values_from = side_bias) %>% 
            mutate(
              side_bias = leftTestButton | rightTestButton
            ) %>% 
            select(subject, side_bias) 
  return (RMTS_df)
  
}

create_horizon_df <- function(raw_d){
  horizon_df <- raw_d %>% 
                filter(trial_type == "horizon-sticker")
  return(horizon_df)
  
}

create_SymS_label_df <- function(raw_d){
  circ_table <- raw_d %>% 
    filter(trial_type == "draw-circles") %>% 
    select(subject, locations) %>% 
    toJSON() %>% 
    fromJSON() %>% 
    mutate(
      circle_drawn = map(locations, ~ fromJSON(.) %>% as.data.frame())) %>% 
    unnest(circle_drawn) %>% 
    filter(radius != 0) 
  
  label_table <- raw_d %>% 
    filter(trial_type == "circle-label") %>% 
    select(subject, responses) %>%
    toJSON() %>% 
    fromJSON() %>%
    mutate(
      circ_label = map(responses, ~ fromJSON(.) %>% as.data.frame())) %>% 
    unnest(circ_label) %>% 
    mutate(
      Q_Me = "Me"
    ) %>% 
    relocate(Q_Me,.before = Q0) %>% 
    pivot_longer(Q_Me:Q6, names_to = "label_names", values_to = "circ_label") %>% 
    filter(circ_label != "")  %>% 
    select(subject, circ_label) %>% 
    group_by(subject) %>% 
    mutate(label_all = paste(circ_label, collapse = " ")) %>% 
    distinct(subject, label_all)
  
  return(label_table)
  
  
}

##### Check Exclusion Criteria #####

# This function takes in a pre-processed Ebbinghaus dataframe and an subject id
# If that participant has side bias, return True 
# If no bias, return False
check_Ebb_SideBias <- function (id, df){
  
  sub_d <- df %>% 
          filter(subject == id) %>% 
          select(subject, block, bias) %>% 
          pivot_wider(names_from = block, values_from = bias) %>% 
          mutate(
            bias_exist = first | second # if one block has bias, the participant has bias
          )
  
  # count if biased block > 0 
  count <- sub_d %>% 
            filter(bias_exist == TRUE) %>% 
            count() %>% 
            select(n) %>% 
            pull()
  
  
  if (identical(count, integer(0))){
    return (FALSE) #no bias
  }else{
    return (TRUE) #has bias
  }
  
}

# This function takes in a pre-processed RMTS dataframe and an subject id
# If that participant has side bias, return True 
# If no bias, return False

check_RMTS_SideBias <- function(id, df){
  sub_d <- df %>% 
    filter(subject == id) 
  
  count <- sub_d %>% 
           filter(side_bias == TRUE) %>% 
           count() %>% 
           select(n) %>% 
           pull()
  
  if (identical(count, integer(0))){
    return (FALSE) #no bias
  }else{
    return (TRUE) #has bias
  }
  
  
}

# This function takes in a pre-processed horizon dataframe and an subject id
# If that participant moved horizon sticker, return True 
# If not, return False

check_horizon <- function(id, df){
  sub_d <- df %>% filter(subject == id)
  
  count <- sub_d %>% 
    filter(grepl("horizon", moves, fixed = TRUE)) %>% 
    count() %>% 
    pull()
  # if did **not** move horizon, return false, else true 
  value = if_else(count == 0, FALSE, TRUE)
  return (value)
  
}

# This function takes in a raw dataframe
# Creates two dataframes in the middle steps, one label df one cirlce df 
# Return True if length(label_df) = length(circle_df)
# False otherwise, indicates that the numbers do not match

check_SymS <- function(id, raw_df){
  sub_d <- raw_df %>% filter(subject == id)
  
  circ_table <- sub_d %>% 
    filter(trial_type == "draw-circles") %>% 
    select(subject, locations) %>% 
    toJSON() %>% 
    fromJSON() %>% 
    mutate(
      circle_drawn = map(locations, ~ fromJSON(.) %>% as.data.frame())) %>% 
    unnest(circle_drawn) %>% 
    filter(radius != 0, radius != 1)
  
  label_table <- sub_d %>% 
    filter(trial_type == "circle-label") %>% 
    select(subject, responses) %>%
    toJSON() %>% 
    fromJSON() %>%
    mutate(
      circ_label = map(responses, ~ fromJSON(.) %>% as.data.frame())) %>% 
    unnest(circ_label) %>% 
    mutate(
      Q_Me = "Me"
    ) %>% 
    relocate(Q_Me,.before = Q0) %>% 
    pivot_longer(Q_Me:Q6, names_to = "label_names", values_to = "circ_label") %>% 
    filter(circ_label != "")  %>% 
    select(subject, circ_label)
  
  count_circ <- circ_table %>% count()
  count_label <- label_table %>% count()
  
  # if the circle number is less than the label number, throw out, otherwise waits for human check
  if (count_circ == count_label) {
    return (TRUE)
  }else{
    return (FALSE)
  }
  
}

##### extract human annotated results #####


summarize_codeable_rate <- function(df){
  codeable_df <- df %>% 
    mutate(codeable = as.numeric(codeable)) %>% 
    group_by(subject) %>% 
    summarise(
      codeable_rate = (sum(codeable) / n()), # 1 = codeable; 0 = uncodeable
      include = if_else(codeable_rate > 0.75, "yes", "no")
    )
  
  return (codeable_df)
}

get_excluded_subjects <- function(summarized_df){
  excluded_subject <- summarized_df %>% 
    filter(include == "no") %>% 
    select(subject) %>% 
    pull()
}





##### Run Functions And Generate Exclusion List #####

#this function takes in the raw_data 
#return all the subject id that needs to be excluded based on task performance
# if include_annotation == TRUE, will consider the exclude based on human annotation  
# if check_exclusion == TRUE, will automatically print out ID got excluded



task_exclusion <- function(raw_data, 
                           culture, 
                           include_annotation = FALSE, 
                           check_exclusion = TRUE, 
                           detail_table = FALSE){
  
  

  
  CN_CA_ANNOTATED_PATH <- here("data/01b_annotated_data/exp1/CN/cn_ca_merged_coded.csv")
  US_CA_ANNOTATED_PATH <- here("data/01b_annotated_data/exp1/US/us_ca_coded.csv")
  CN_SI_ANNOTATED_PATH <- here("data/01b_annotated_data/exp1/CN/cn_si_merged_coded.csv")
  US_SI_ANNOTATED_PATH <- here("data/01b_annotated_data/exp1/US/us_si_coded.csv")
  
  
  CN_FD_ANNOTATED_PATH <- here("data/01b_annotated_data/exp1/CN/cn_fd_merged_coded.csv")
  US_FD_ANNOTATED_PATH <- here("data/01b_annotated_data/exp1/US/us_fd_coded.csv")
  
  ##### Create all the dataframe to be used ##### 
  ebb_df <- create_Ebb_df(raw_data)
  RMTS_df <- create_RMTS_df(raw_data)
  horizon_df <- create_horizon_df(raw_data)
  
  if (include_annotation){
  
    if(culture == "US"){
       FD_df <- read_csv(US_FD_ANNOTATED_PATH)
       CA_df <- read_csv(US_CA_ANNOTATED_PATH)
       SI_df <- read_csv(US_SI_ANNOTATED_PATH)
       
     }else if (culture == "CN"){
       FD_df <- read_csv(CN_FD_ANNOTATED_PATH)
       CA_df <- read_csv(CN_CA_ANNOTATED_PATH)
       SI_df <- read_csv(CN_SI_ANNOTATED_PATH)
    }
    
    
    ##### Process human annotated df ##### 
    
    FD_summarized <- summarize_codeable_rate(FD_df)
    CA_summarized <- summarize_codeable_rate(CA_df)
    SI_summarized <- summarize_codeable_rate(SI_df)
    
    FD_to_exclude <- get_excluded_subjects(FD_summarized)
    CA_to_exclude <- get_excluded_subjects(CA_summarized)
    SI_to_exclude <- get_excluded_subjects(SI_summarized)
  
  }else{
    
    FD_to_exclude <- c()
    CA_to_exclude <- c()
    SI_to_exclude <- c()
  }
  
  
  ##### Process automatic processing df, Loop through all the subject ID ##### 
  num_id <- unique(raw_data$subject)
  id_to_exclude <- c()
  
  
  ebb_exclude <- c()
  RMTS_exclude <- c()
  HZ_exclude <- c()
  for (id in num_id){
    ebb_check <- check_Ebb_SideBias(id, ebb_df)
    RMTS_check <- check_RMTS_SideBias(id, RMTS_df)
    horizon_check <- check_horizon(id, horizon_df)
    if (ebb_check == TRUE){
      id_to_exclude <- c(id_to_exclude, id)
      ebb_exclude <- c(ebb_exclude, id)
    } else if (RMTS_check == TRUE){
      id_to_exclude <- c(id_to_exclude, id)
      RMTS_exclude <- c(RMTS_exclude, id)
    } else if (horizon_check == FALSE){
      id_to_exclude <- c(id_to_exclude, id)
      HZ_exclude <- c(HZ_exclude, id)
    } 

  }
  
 
  summary_df <- num_id %>% 
        enframe() %>% 
        rename(
          subject = value
        ) %>% 
        mutate("reason_EBB" = subject %in% ebb_exclude, 
               "reason_RMTS" = subject %in% RMTS_exclude, 
               "reason_HZ" = subject %in% HZ_exclude,
               "reason_FD" = subject %in% FD_to_exclude,
               "reason_CA" = subject %in% CA_to_exclude,
               "reason_SI" = subject %in% SI_to_exclude,
               culture = culture)
  

  
  if (detail_table == TRUE){
    return (summary_df)
  }
  
  

  if (check_exclusion == TRUE){
    
    summary_df %>% 
      summarise(
        EBB_exclude = sum(reason_EBB), 
        RMTS_exclude = sum(reason_RMTS), 
        HZ_exclude = sum(reason_HZ), 
        SI_exclude = sum(reason_SI),
        FD_exclude = sum(reason_FD), 
        CA_exclude = sum(reason_CA)
      ) %>% 
      pivot_longer(cols = EBB_exclude:CA_exclude, 
                   names_to = "exclude_reason", 
                   values_to = "num") %>% 
      ggplot(aes(x = exclude_reason, y = num)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      labs(title = culture)
    
    
  }else{
    
  

  
  id_to_exclude <- c(id_to_exclude, 
                     #FD_to_exclude, 
                     CA_to_exclude, 
                     SI_to_exclude)

  # drop duplicate
  id_to_exclude = id_to_exclude[!duplicated(id_to_exclude)]
  
  return (id_to_exclude)
  
  }
}

# this will create a dataframe that shows the breakdown of excluded participants across tasks 
task_exclusion_details <- function(raw_data){
  
  
  ##### Create all the dataframe to be used ##### 
  ebb_df <- create_Ebb_df(raw_data)
  RMTS_df <- create_RMTS_df(raw_data)
  horizon_df <- create_horizon_df(raw_data)
  
  
  ##### Loop through all the subject ID ##### 
  num_id <- unique(raw_data$subject)
  ebb_exclude_id <- c()
  RMTS_exlcude_id <- c()
  HZ_exclude_id <- c()
  
  
  for (id in num_id){
    ebb_check <- check_Ebb_SideBias(id, ebb_df)
    RMTS_check <- check_RMTS_SideBias(id, RMTS_df)
    horizon_check <- check_horizon(id, horizon_df)
    SymS_check <- check_SymS(id, raw_data)
    if (ebb_check == TRUE){
      ebb_exclude_id <- c(ebb_exclude_id, id)
    } else if (RMTS_check == TRUE){
      RMTS_exlcude_id <- c(RMTS_exlcude_id, id)
    } else if (horizon_check == FALSE){
      HZ_exclude_id <- c(HZ_exclude_id, id)
    } else if (SymS_check == FALSE){
      id_to_exclude <- c(id_to_exclude, id)
      #print("id:")
      #print(id)
      #print("exclude because of SymS")
    }
  }
  
  
  # drop duplicate
  id_to_exclude = id_to_exclude[!duplicated(id_to_exclude)]
  
  return (id_to_exclude)
  
  
}
  









