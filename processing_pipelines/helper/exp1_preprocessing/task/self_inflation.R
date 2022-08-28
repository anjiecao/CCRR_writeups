


label_position_parser <- function(string){
  
  # get rid of the \\
  string <- gsub("\\\\", "", string)
  # get rid of the "
  string <- gsub("\"", "", string)
  
  # parse to a list of strings containing relevant info 
  j_list <- regmatches(string, gregexpr("\\{\\K[^{}]+(?=\\})", string, perl=TRUE))[[1]]
  
  # convert the list to a reasonable dataframe 
  # by first spliting the list into a list of string
  # then transpose
  # then converting to dataframe
  j_df <- as.data.frame(t(as.list(strsplit(j_list, ","))))
  
  # further processing the json df into tidy format   
  tidy_j_df <- t(j_df) %>% 
    as.data.frame() %>% 
    separate(col = V1, into = c("label", "x", "y", "fontSize", "fill"),sep = ",") %>% 
    mutate(
      label = gsub(".*:(.+)\"", "\\1", label),
      label_x = as.numeric(gsub(".*:(.+)\"", "\\1", x)), 
      label_y = as.numeric(gsub(".*:(.+)\"", "\\1", y)),
    ) %>% 
    select(-c(fontSize, fill)) %>% 
    # this is for filtering out deleted label (does not have label associated with the component, only positions that did not get deleted)
    filter(
      is.na(as.numeric(label))
    )
  
  return(tidy_j_df)
  
}


generate_coding_sheet <- function(si_df){
  
  coding_sheet <- si_df %>% 
    mutate(
      row_number = row_number(),
      circle_id = paste("circle", row_number, sep = "")
    ) %>% 
    toJSON() %>% 
    fromJSON() %>% 
    mutate(
      #num_labels = count_label(labels),
      circle_drawn = map(locations, ~ fromJSON(.) %>% as.data.frame()))%>%
    unnest(circle_drawn) %>% 
    select(-locations) %>% 
    group_by(subject) %>% 
    mutate(circle_number = row_number())%>% 
    select(subject, circle_id, circle_number, x, y, radius) %>% 
    mutate(
      codeable = "1",
      me_circle = "0"
    ) %>% 
    # reorder to reduce possible errors
    select(subject, circle_id, circle_number, codeable, me_circle, x, y, radius)
  
  return(coding_sheet)
  
  
}



save_image_new <- function(si_df, directory_to_save){
  
  si_df <- si_df %>% 
    mutate(
      row_number = row_number(),
      circle_id = paste("circle", row_number, sep = "")
    )
  
  
  label_unnest <- si_df %>% 
    mutate(
      labels_drawn = map(labels_locations, ~label_position_parser(.)))%>%
    unnest(labels_drawn) 
  
  circle_unnest <- si_df %>% 
    toJSON() %>% 
    fromJSON() %>% 
    mutate(
      #num_labels = count_label(labels),
      circle_drawn = map(locations, ~ fromJSON(.) %>% as.data.frame()))%>%
    unnest(circle_drawn) %>% 
    select(-locations) %>% 
    group_by(subject) %>% 
    mutate(circle_number = row_number())
  
  
  
  num_id <- unique(circle_unnest$circle_id)
  
  for (id in num_id){
    
    
    d_circle <- circle_unnest %>% 
      filter(circle_id == id) 
    
    d_label <- label_unnest %>% 
      filter(circle_id == id)
    
    
    id <- as.character(id)
    f_name <- paste(directory_to_save, id, ".png", sep="")
    print("f_name")
    showtext_auto()
    plot <-  
      ggplot() + 
      geom_circle(data = d_circle, 
                  aes(x0 = x, y0 = y, r = radius)) + 
      geom_text(data = d_circle, 
                aes(x = x, y = y, label = circle_number))+
      geom_text(data = d_label, 
                aes(x = label_x, y = label_y, label = label)) + 
      coord_fixed(xlim = c(0,1024), ylim = c(0, 800)) + 
      scale_y_reverse()+
      labs(title = id)
    
    ggsave(f_name, plot)
    
    #cairo_pdf(f_name)
    #print(plot)
    #dev.off()
    
  }
  
}








get_SS_raw <- function(raw_d){
  circle_table <- raw_d %>% 
    filter(trial_type == "draw-circles") %>% 
    select(subject, culture, locations, rt) %>% 
    toJSON() %>% 
    fromJSON() %>% 
    mutate(
      circle_drawn = map(locations, ~ fromJSON(.) %>% as.data.frame())) %>% 
    unnest(circle_drawn) %>% 
    mutate(
      circ_num = label, 
      circ_r = radius, 
      task_RT = rt, 
      task_name = "SymS",
    ) %>% 
    select(subject, culture, task_name, circ_num, circ_r, task_RT, task_name) %>% 
    filter(circ_r != 0)
  
  
  # get the label of the first drawn circle 
  first_circle_df <- aggregate(circle_table, list(circle_table$subject), FUN=head, 1)
  
  first_circle_df <- first_circle_df %>% 
    mutate(first_circle = circ_num) %>% 
    select(subject, first_circle)
  
  circle_table <- left_join(circle_table, first_circle_df, by = "subject")
  
  # calculate the average non-self circle 
  average_other_circle <- circle_table %>% 
    filter(circ_num != first_circle) %>% 
    group_by(subject) %>% 
    summarise(
      mean = mean(circ_r*2)
    )
  
  calculate_inflation_score <- function(id, other_df, summarize_df){
    other_average <- other_df %>% 
      filter(subject == id) %>% 
      select(mean) %>% 
      pull()
    
    self_circle <- summarize_df %>% 
      filter(subject == id) %>% 
      filter(circ_num == first_circle) %>% 
      select(circ_r) %>% 
      pull() 
    
    return (self_circle * 2 - other_average)
  }
  
  
  circle_table$task_score <- unlist(map(circle_table$subject,calculate_inflation_score, average_other_circle, circle_table))
  
  return(circle_table)
  
}

get_SI_main <- function(raw_data, exclusion_subject){
  
  CN_SI_ANNOTATED_PATH <- here("data/01b_annotated_data/exp1/CN/cn_si_merged_coded.csv")
  US_SI_ANNOTATED_PATH <- here("data/01b_annotated_data/exp1/US/us_si_coded.csv")
  
  included_subject <- raw_data %>% 
    filter(!subject %in% exclusion_subject) %>% 
    select(subject) %>% 
    distinct() %>% 
    pull() 
  
  cn_si <- read_csv(CN_SI_ANNOTATED_PATH) %>% 
    mutate(culture = "CN")
  us_si <- read_csv(US_SI_ANNOTATED_PATH) %>% 
    mutate(culture = "US")
  si_all <- bind_rows(cn_si, us_si) %>% 
    filter(codeable == 1)
  
  
  ##### Calculate score ###########
  
  calculate_inflation_score_ratio <- function(id, coded_circle_df){
    
    
    
    me_radius <-  coded_circle_df %>% 
      filter(subject == id) %>% 
      filter(me_circle == "1") %>% 
      select(radius) %>% 
      pull()
    
    other_radius <- coded_circle_df %>% 
      filter(subject == id) %>% 
      filter(me_circle != "1") %>% 
      summarise(
        mean = mean(radius)
      ) %>% 
      select(mean) %>% 
      pull()
    
    
    #return (me_radius - other_radius)
    return ((me_radius * 2) / (other_radius * 2))
  }
  
  calculate_inflation_score_diff <- function(id, coded_circle_df){
    
    
    
    me_radius <-  coded_circle_df %>% 
      filter(subject == id) %>% 
      filter(me_circle == "1") %>% 
      select(radius) %>% 
      pull()
    
    other_radius <- coded_circle_df %>% 
      filter(subject == id) %>% 
      filter(me_circle != "1") %>% 
      summarise(
        mean = mean(radius)
      ) %>% 
      select(mean) %>% 
      pull()
    
    
    return (me_radius - other_radius)
  }
  
  
  si_all$task_score_ratio <- unlist(map(si_all$subject,
                                  calculate_inflation_score_ratio, 
                                  si_all))
  
  si_all$task_score_diff <- unlist(map(si_all$subject,
                                       calculate_inflation_score_diff, 
                                        si_all))
  
  
  
  
  
  
  

  SI_main <- si_all %>% 
    filter(subject %in% included_subject) %>% 
    select(subject, culture, task_score_ratio, task_score_diff) %>% 
    rename(inflation_score_ratio = task_score_ratio, 
           inflation_score_diff = task_score_diff) %>% 
    pivot_longer(inflation_score_ratio:inflation_score_diff, 
                 names_to = "resp_type", 
                 values_to = "resp") %>% 
    mutate(
      task_name = "SI", 
      task_info = "SI", 
      trial_info = "SI", 
    ) %>% 
    select(subject, culture, task_name, task_info, trial_info, resp_type , resp) %>% 
    distinct(subject, culture, task_name, task_info, trial_info, resp_type , resp ) 
  
  return(SI_main)
  
}


count_circle_number <- function(raw_data){
  num_circ <- raw_data %>% 
    filter(trial_type == "draw-circles") %>% 
    select(subject, locations) %>% 
    toJSON() %>% 
    fromJSON() %>% 
    mutate(
      circle_drawn = map(locations, ~ fromJSON(.) %>% as.data.frame())) %>% 
    unnest(circle_drawn) %>% 
    select(-locations) %>% 
    filter(radius != 0, radius != 1) %>% 
    group_by(subject) %>% 
    count() %>% 
    mutate(circle_n = n ) %>% 
    select(-n)
  
  return (num_circ)
  
}

count_label_numbers <- function(raw_data){
  num_lab <- raw_data %>% 
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
    count() %>% 
    mutate(label_n = n) %>% 
    select(-n)
  
  return(num_lab)
  
  
}


extract_label <- function(raw_data){
  
  label_extracted <- raw_data %>% 
    filter(trial_type == "circle-label") %>% 
    select(subject, responses, culture) %>%
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
    select(subject, circ_label, culture) %>% 
    group_by(subject) %>% 
    mutate(
      label_all = paste(circ_label, collapse = " ")) %>% 
    distinct(subject, label_all, .keep_all = TRUE)
  
  return(label_extracted)
}

extract_circle <- function(raw_data){
  
  circle_extracted <- raw_data %>% 
    filter(trial_type == "draw-circles") %>% 
    select(subject, locations) %>% 
    toJSON() %>% 
    fromJSON() %>% 
    mutate(
      circle_drawn = map(locations, ~ fromJSON(.) %>% as.data.frame())) %>% 
    unnest(circle_drawn) %>% 
    filter(radius != 0, radius != 1)
  
  return(circle_extracted)
  
}





check_basic_label <- function(str, culture_string){
  count = 0
  if (culture_string == "US"){
    kin_term <- c("mom","dada", "mommy", "daddy", "mother", "father", "papa", "mama", "mum","da", "ma", "pa")
  } else{
    kin_term <- c("妈","爸", "妈妈", "爸爸", "爹", "娘", "老爸", "老妈")
  }
  #str <- strsplit(str, " ")
  for (k in kin_term){
    if (str_detect(str, regex(k, ignore_case = TRUE))){
      count = count + 1
    }
  }
  
  if (count == 0) {
    return (FALSE) 
  }else{
    return(TRUE)
  }
}







