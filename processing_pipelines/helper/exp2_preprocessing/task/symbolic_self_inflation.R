create_SSI_to_be_annotated <- function(raw_df){
  
  # add ssi id so it's easier to code 
  circle_task_df <- raw_df %>% 
    filter(grepl("konva", trial_type)) %>% 
    group_by(culture) %>% 
    mutate(idx = row_number(), 
           ssi_id = paste0(culture, idx)) %>% 
    select(subject, culture, ssi_id, labels_processed, circles_location) %>% 
    ungroup()

  
  label_df <- circle_task_df %>% 
    select(subject, culture, ssi_id, labels_processed) %>% 
    mutate(labels_processed = map(labels_processed, ~ fromJSON(.) %>%  as.data.frame())) %>% 
    unnest(labels_processed)
  
  circle_df <- circle_task_df %>% 
    select(subject, culture, ssi_id, circles_location) %>% 
    mutate(circles_location = map(circles_location, ~ fromJSON(.) %>%  as.data.frame())) %>% 
    unnest(circles_location) %>% 
    group_by(subject, culture, ssi_id) %>% 
    mutate(circle_number = row_number())
  
  # here create annotating sheet 
  circle_df <- circle_df %>% 
    mutate(me_circle = 0, 
           codeable = 1, 
           coder = "") %>% 
    select(subject, culture, ssi_id, circle_number, codeable, me_circle, coder, x, y, r)
  
  CN_circle_df <- circle_df %>% filter(culture == "CN")
  US_circle_df <- circle_df %>% filter(culture == "US")
    
  write_csv(CN_circle_df, here("data/to_be_annotated/CN/SSI.csv"))
  write_csv(US_circle_df, here("data/to_be_annotated/US/SSI.csv"))
  
  
  # below creating images to be coded 
  
  all_circle_id <- unique(circle_task_df$ssi_id)
  
  for (id in all_circle_id){
    this_subject_id <- circle_task_df[circle_task_df$ssi_id == id, ]$subject
    this_subject_culture <- circle_task_df[circle_task_df$ssi_id == id, ]$culture
    this_label_df <- label_df %>% filter(ssi_id == id)
    this_circle_df <- circle_df %>% filter(ssi_id == id)
    # if doesn't have label 
    if(nrow(this_label_df) == 0){
      this_label_df <- tibble("subject" = this_subject_id, 
                              "culture" = this_subject_culture, 
                              "ssi_id" = id, 
                              "x" = 500, 
                              "y" = 500, 
                              "text" = "no label")
    }
    
    # if doesn't draw any circle 
    if(nrow(this_circle_df) == 0){
      this_circle_df <- tibble("subject" = this_subject_id, 
                              "culture" = this_subject_culture, 
                              "ssi_id" = id, 
                              "x" = 500, 
                              "y" = 400, 
                              "r" = 0,
                              "circle_number" = 'no circle',
                              "text" = "no circle")
    }
     
    if(grepl("CN", id)){
      directory_to_save = here("data/to_be_annotated/CN/SSI/")
    }else{
      directory_to_save = here("data/to_be_annotated/US/SSI/")
    }
    
    f_name <- paste(directory_to_save, id, ".png", sep="")
    print("f_name")
    print(id)
    showtext_auto()
    plot <-  
      ggplot() + 
      geom_circle(data = this_circle_df, 
                  aes(x0 = x, y0 = y, r = r)) + 
      geom_text(data = this_circle_df, 
                aes(x = x, y = y, label = circle_number))+
      geom_text(data = this_label_df, 
                aes(x = x, y = y, label = text)) + 
      coord_fixed(xlim = c(0,1024), ylim = c(0, 800)) + 
      scale_y_reverse()+
      labs(title = id)
    
    ggsave(f_name, plot)
    
    
    
  }
    
  
    

  
}

get_SSI_main <- function(){
 CN_df <- read_csv(here("data/01b_annotated_data/exp2/CN/SSI.csv"))
 US_df <- read_csv(here("data/01b_annotated_data/exp2/US/SSI.csv"))
 
 SSI_raw <- bind_rows(CN_df, US_df)

 # first take out all the participants who are not codeable 
 to_be_excluded <- SSI_raw %>% 
   group_by(subject,) %>% 
   summarise(mean_codeable = mean(codeable)) %>% 
   filter(mean_codeable < 1) %>% 
   pull(subject)
 
 SSI_filtered <- SSI_raw %>% filter(!subject %in% to_be_excluded)
 
 ##### Calculate score ###########
 
 calculate_inflation_score_ratio <- function(id, coded_circle_df){
  
   me_radius <-  coded_circle_df %>% 
     filter(subject == id) %>% 
     filter(me_circle == "1") %>% 
     select(r) %>% 
     pull()
   
   other_radius <- coded_circle_df %>% 
     filter(subject == id) %>% 
     filter(me_circle != "1") %>% 
     summarise(
       mean = mean(r)
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
     select(r) %>% 
     pull()
   
   other_radius <- coded_circle_df %>% 
     filter(subject == id) %>% 
     filter(me_circle != "1") %>% 
     summarise(
       mean = mean(r)
     ) %>% 
     select(mean) %>% 
     pull()
   
   
   return (me_radius - other_radius)
 }
 
 
 SSI_filtered$task_score_ratio <- unlist(map(SSI_filtered$subject,
                                       calculate_inflation_score_ratio, 
                                       SSI_filtered))
 
 SSI_filtered$task_score_diff <- unlist(map(SSI_filtered$subject,
                                      calculate_inflation_score_diff, 
                                      SSI_filtered))
  
 SSI_main <- SSI_filtered %>% 
   distinct(subject, culture,task_score_ratio, task_score_diff ) %>% 
   select(subject, culture, task_score_ratio, task_score_diff) %>% 
   pivot_longer(cols = c("task_score_ratio", "task_score_diff"), 
                names_to = "resp_type", 
                values_to = "resp") %>% 
   mutate(task_name = "SSI", task_info = "SSI", trial_info = "SSI") %>% 
   select(subject, culture,task_name, task_info, trial_info, 
           resp_type, resp)
 
   
 
 return(SSI_main)

 
}



get_SSI_excluded <- function(){
  CN_df <- read_csv(here("data/01b_annotated_data/exp2/CN/SSI.csv"))
  US_df <- read_csv(here("data/01b_annotated_data/exp2/US/SSI.csv"))
  
  SSI_raw <- bind_rows(CN_df, US_df)
  
  # first take out all the participants who are not codeable 
  to_be_excluded <- SSI_raw %>% 
    group_by(subject,) %>% 
    summarise(mean_codeable = mean(codeable)) %>% 
    filter(mean_codeable < 1) %>% 
    select(subject) %>% 
    mutate(exclude_reason = "SSI_failtask")
  

  return(to_be_excluded)
  
  
}



