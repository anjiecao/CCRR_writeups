get_d1_reliability <- function(d1, tasks){
  d1 = d1 %>% mutate(resp = as.numeric(resp))
  lapply(tasks, 
         function(x){
           # get the key measures for certain task that has multiple measures
           
           if (x == "EBB"){
             task_d <- d1 %>% 
               filter(task_name == x) %>% 
               filter(task_info == "IL")
           }else if (x == "FD"){
             
             task_d <- d1 %>% 
               filter(task_name == x) %>% 
               filter(resp_type == "first_mention_focal")
           }else{
             task_d <- d1 %>% filter(task_name == x)
           }
           
           estimate_df <- (task_d %>% 
                             splithalf(outcome = "accuracy", score = "average",
                                       halftype = "random", permutations = 5000,
                                       var.ACC = "resp",var.participant = "subject", 
                                       average = "mean"))$final_estimates
           
           estimate_df %>% mutate(task_name = x)
           
         }) %>% 
    bind_rows()
  
}

get_d2_reliability <- function(d2, tasks){
  lapply(tasks, 
         function(x){
           # get the key measures for certain task that has multiple measures
           
           if (x == "TD"){
             task_d <- d2 %>% 
               mutate(resp = as.numeric(as.logical(resp))) %>% 
               filter(task_name == x) %>% 
               filter(task_info == "triads")
           }else if (x == "CA"){
             task_d <- d2 %>% 
               filter(task_name == x) %>% 
               filter(task_info == "situational") %>% 
               mutate(resp = as.numeric(resp))
           }else if (x == "CD"){
             task_d <- d2 %>% 
               filter(task_name == x) %>% 
               filter(task_info == "context") %>% 
               mutate(resp = as.numeric(resp))
           }else if(x == "SeI"){
             task_d <- d2 %>% 
               filter(task_name == x) %>% 
               filter(task_info == "critical") %>% 
               mutate(resp = case_when(
                 resp == "causal_historical" ~ 1,
                 TRUE ~ 0
               ))
             
           }else{
             task_d <- d2 %>% 
               filter(task_name == x) %>% 
               mutate(resp = as.numeric(resp))
           }
           
           estimate_df <- (task_d %>% 
                             splithalf(outcome = "accuracy", score = "average",
                                       halftype = "random", permutations = 5000,
                                       var.ACC = "resp",var.participant = "subject", 
                                       average = "mean"))$final_estimates
           
           estimate_df %>% mutate(task_name = x)
           
         }) %>% 
    bind_rows()
  
}