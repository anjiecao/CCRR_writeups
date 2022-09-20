plot_es <- function(d){
  
  d_abs_df <- d %>% 
    mutate(
      task_name_print = case_when(
        grepl("RMTS", task_name) ~ "RMTS", 
        grepl("RV", task_name) ~ "Ravens", 
        grepl("SI", task_name) | grepl("SSI", task_name) ~ "Symbolic Self Inflation", 
        grepl("FD", task_name) ~ "Free Description (first mention)", 
        grepl("CP", task_name) ~ "Uniqueness Preference", 
        grepl("CA", task_name) & study_num == "Study 1" ~ "Children Causal Attribution", 
        grepl("CA", task_name) & study_num == "Study 2" ~ "Adult Causal Attribution", 
        grepl("HZ", task_name) ~ "Horizon (Height)", 
        grepl("EBB", task_name) ~ "Ebbinghaus Illusion", 
        grepl("CD", task_name) ~ "Change Detection", 
        grepl("TD", task_name) ~ "Triads", 
        grepl("SeI", task_name) ~ "Semantic Intuition"
      )) %>% 
    mutate(significance =  !(d_ci_lower < 0 & d_ci_upper > 0), 
           consistent_with_prediction = case_when(
             task_name_print %in% c("Free Description (first mention)", "Adult Causal Attribution", 
                                    "Ravens","Semantic Intuition", "Triads") & significance ~ "Consistent",
             task_name_print == "Symbolic Self Inflation" & significance~ "Inconsistent",
             TRUE ~ "No difference"
           ),
           advantage = case_when(
             d > 0 & significance ~ "US", 
             d < 0 & significance ~ "CN", 
             TRUE ~ "No Difference"
           ))
  
  d_abs_df %>% 
    arrange(-d_abs) %>% 
    ggplot(aes(x = reorder(task_name_print,d_abs), y = d_abs, 
               ymin = d_abs_ci_lower, ymax = d_abs_ci_upper, 
               color = consistent_with_prediction, 
               fill = consistent_with_prediction, 
               shape = study_num)) + 
    geom_hline(yintercept = 0, lty = 2) + 
    geom_pointrange(aes( 
      x = reorder(task_name_print,d_abs), y = d_abs, 
      ymin = d_abs_ci_lower, ymax = d_abs_ci_upper, group = study_num
    ), position = position_dodge(width = .6)) + 
    scale_shape_manual(values = c(16, 17)) + 
    scale_color_manual(values = c("darkolivegreen", "darkorange1", "grey")) + 
    scale_fill_manual(values = c("darkolivegreen", "darkorange1", "grey")) + 
    coord_flip() + 
    theme_classic() + 
    ylab("Standardized Mean Differences") + 
    xlab("") + 
    theme(legend.title = element_blank())
  
}