get_CP_raw <- function(raw_d){
  CP_table <- raw_d %>% filter(
    grepl("pen_choice", stimulus)
  ) %>% 
    select(subject, culture, button_pressed, unique_color, unique_position, rt) %>% 
    mutate(
      task_name = "PC", 
      button_pressed = as.numeric(button_pressed), 
      unique_choice = case_when(
        button_pressed == unique_position ~ "yes", 
        TRUE ~ "no"
      )
    )
  return(CP_table)
}

get_CP_main <- function(raw_d){
  CP_table <- get_CP_raw(raw_d)
  CP_main <- CP_table %>% 
    mutate(
      task_name = "CP", 
      task_info = "CP", 
      trial_info = "CP", 
      resp_type = "choice", 
      resp = case_when(
        unique_choice == "no" ~  0, 
        unique_choice == "yes" ~ 1
      ), 
      
    ) %>% 
    select(subject, culture, task_name, task_info, trial_info, 
           resp_type, resp)
  
  return(CP_main)
  
}