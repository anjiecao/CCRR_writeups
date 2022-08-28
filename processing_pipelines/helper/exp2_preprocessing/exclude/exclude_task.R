
exclude_by_task <- function(full_d, exclude_df){
  
  exclude_df <- exclude_df %>% 
    separate(exclude_reason, into = c("task", "reason"), sep = "_") %>% 
    rowwise() %>% 
    mutate(task = ifelse(task == "triads", "TD", task))
  all_tasks <- unique(exclude_df$task)
  
  for (cur_task in all_tasks){
    
    cur_task_exclude_subject <- exclude_df %>% 
      filter(task == cur_task) %>% 
      pull(subject)
    
    full_d <- full_d %>% 
      rowwise() %>% 
      filter(if (task_name == cur_task) !(subject %in% cur_task_exclude_subject)
             else TRUE)
    
  }
  return (full_d)
  
}









