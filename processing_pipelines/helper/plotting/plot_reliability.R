plot_rl <- function(df){
  
  df %>% 
    mutate(
      print_task_name = case_when(
        task_name == "TD" ~ "Triads", 
        task_name == "RV" ~ "Ravens", 
        task_name == "RMTS" ~ "RMTS", 
        task_name == "FD" ~ "Picture Free Description", 
        task_name == "EBB" ~ "Ebbinghaus Illusion",
        task_name == "CD" ~ "Change Detection", 
        task_name == "CA" ~ "Adult Causal Attribution"
      )
    ) %>% 
    filter(type == "all") %>% 
    ggplot(aes(x = print_task_name, 
               y = spearmanbrown, 
               ymin = SB_low, 
               ymax = SB_high,
               group = exp, 
               shape = exp
    )) + 
    ylab("Spearman-Brown Correlation") + 
    xlab("") + 
    geom_pointrange(position = position_dodge(width = 0.6)) + 
    guides(shape=guide_legend(title="Experiment")) + 
    ylim(0, 1) + 
    coord_flip() + 
    theme_classic()
  
}