plot_megaplogt <- function(){
  
  ###########data cleaning###########
  
  d1 <- read_csv(here("data/03_processed_data/exp1/tidy_main.csv"))
  d2 <- read_csv(here("data/03_processed_data/exp2/tidy_main.csv"))
  
  clean_d2_CD <- d2 %>% 
    filter(task_name == "CD", task_info == "context") %>% 
    group_by(subject, culture, task_name) %>% 
    filter(!resp == "null") %>% 
    summarise(resp_num = mean(as.numeric(resp)))
  
  clean_d2_SSI <- d2 %>% 
    filter(task_name == "SSI", resp_type == "task_score_ratio") %>%
    mutate(resp_num = as.numeric(resp))
  
  clean_d2_CA <- d2 %>% 
    filter(task_name == "CA", task_info == "situational") %>% 
    group_by(subject, culture, task_name) %>% 
    filter(!is.na(resp)) %>% 
    summarise(resp_num = mean(as.numeric(resp)))
  
  clean_d2_TD <- d2 %>% 
    filter(task_name == "TD", task_info == "triads") %>% 
    group_by(subject, culture, task_name) %>% 
    filter(!is.na(resp)) %>% 
    summarise(resp_num = mean(as.numeric(as.logical(resp))))
  
  clean_d2_SeI <- d2 %>% 
    filter(task_name == "SeI", task_info == "critical") %>% 
    mutate(resp = case_when(
      resp == "causal_historical" ~ 1, 
      TRUE ~ 0)) %>% 
    group_by(subject, culture, task_name) %>% 
    summarise(resp_num = mean(as.numeric(resp)))
  
  d1_generics <- d1 %>% 
    filter(
      !(task_name == "CA") & 
        !(task_name == "FD" & !(grepl("first_mention", resp_type))) & 
        !(task_name == "HZ" & !(grepl("height", resp_type))) & 
        !(task_name == "SI" & (grepl("diff", resp_type))) & 
        !(task_name == "EBB")
    ) %>% 
    group_by(subject, culture, task_name) %>% 
    summarise(resp = mean(resp)) %>% 
    ungroup()
  
  d2_generics <- bind_rows(
    clean_d2_SeI, 
    clean_d2_SSI, 
    clean_d2_TD, 
    d2  %>% 
      filter(task_name %in% c("RMTS","RV","FD"))  %>% 
      mutate(resp_num = as.numeric(resp))  %>% 
      group_by(subject, culture, task_name) %>% 
      summarise(resp_num = mean(resp_num)) 
  ) %>% 
    select(subject, culture, task_name, resp_num) %>% 
    rename(resp = resp_num) %>% 
    ungroup()
  
  ###########create generics plot for d1 and d2  ###########
  
  d1_base_plot_list <- d1_generics %>% 
    group_split(task_name) %>%
    setNames(sort(unique(d1_generics$task_name))) %>% 
    map(~ggplot(., aes(x = culture, y = resp, color = culture)) +
          geom_point(alpha = .2, position = position_jitter(width = .1, height = 0.02)) + 
          stat_summary(fun.data = "mean_cl_boot", color = "black") + 
          scale_color_manual(values = c("red", "blue"))+
          scale_fill_manual(values = c("red", "blue")) + 
          guides(fill = "none") +
          guides(color = "none") + 
          theme_classic() + 
          xlab("") +
          theme(text = element_text(size=6),
                axis.text=element_text(size=10),
                plot.title = element_text(hjust = 0.5, size = 7.2), 
                plot.subtitle = element_text(hjust = 0.5, size = 7.2)),
                plot.margin = unit(c(3, 3, 3, 3)))
  
  d2_base_plot_list <- d2_generics %>% 
    group_split(task_name) %>%
    setNames(sort(unique(d2_generics$task_name))) %>% 
    map(~ggplot(., aes(x = culture, y = resp, color = culture)) +
          geom_point(alpha = .2, position = position_jitter(width = .1, height = 0.02)) + 
          stat_summary(fun.data = "mean_cl_boot", color = "black") + 
          scale_color_manual(values = c("red", "blue"))+
          scale_fill_manual(values = c("red", "blue")) + 
          guides(fill = "none") +
          guides(color = "none") + 
          theme_classic() + 
          xlab("") + 
          theme(text = element_text(size=6),
                axis.text=element_text(size=10),
                plot.title = element_text(hjust = 0.5, size = 7.2), 
                plot.subtitle = element_text(hjust = 0.5, size = 7.2)) 
    )
  
  ###########d1 non generic plot###########
  IC_ms <- d1 %>%
    filter(task_name == "EBB") %>%
    filter(task_info %in% c("IL","NC")) %>%
    group_by(subject, task_info, trial_info, culture) %>%
    summarise(mean = mean(resp)) %>%
    group_by(task_info, trial_info, culture) %>%
    tidyboot_mean(mean, na.rm=T) %>% 
    mutate(
      task_info_print = case_when(
        task_info == "IL" ~ "Illusion", 
        task_info == "NC" ~ "No Context"
      )
    )
  
  d1_ebb_plot <- ggplot(IC_ms, aes(x = as.numeric(trial_info), y = mean, col = culture)) + 
    facet_wrap(~task_info_print) + 
    geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) + 
    geom_smooth(method = "lm", formula = y ~ x + I(x^2)) + 
    scale_color_manual(values = c("red", "blue"))+
    scale_fill_manual(values = c("red", "blue"))+
    scale_y_continuous(breaks = seq(0,1,0.5), 
                       labels = {function(x) paste0(as.character(x*100),"%")})+
    ylab("Accuracy") + 
    xlab("Size difference")+
    guides(color = FALSE)+
    theme_classic()+
    theme(text = element_text(size=9)) + 
    labs(title = "Ebbinghaus Illusion") +
    theme(text = element_text(size=6),
          axis.text.y=element_text(size=10),
          axis.title.x = element_text(size=8),
          strip.text = element_text(size=8),
          plot.title = element_text(hjust = 0.5, size = 7.2), 
          plot.subtitle = element_text(hjust = 0.5, size = 7.2))  
  
  CA_ms <- d1 %>%
    filter(task_name == "CA") %>% 
    group_by(culture, resp_type, subject) %>%
    summarise(subject_mean = mean(resp)) %>% 
    mutate(resp_type_print = case_when(
      resp_type == "person_attribution" ~ "Person Attribution", 
      resp_type == "situation_attribution" ~ "Situation Attribution"
    ))
  
  
  #plot means and CIs
  d1_ca_plot <- ggplot(data = CA_ms, 
                       aes(y = subject_mean, x = culture, color = culture)) +
    geom_point(alpha = .2, position = position_jitter(width = .1)) + 
    stat_summary(fun.data = "mean_cl_boot", color = "black")  +
    scale_color_manual(values = c("red", "blue"))+
    scale_fill_manual(values = c("red", "blue"))+
    guides(fill = "none") +
    guides(color = "none") +
    scale_color_manual(values = c("red", "blue"))+
    ylab("Average number") + 
    xlab("")+
    theme_classic() +
    #   theme(text = element_text(size=28), 
    #         axis.text.x=element_blank(), 
    #         plot.margin=grid::unit(c(0,0,0,0), "mm")
    #         )  + 
    facet_wrap(~resp_type_print)+
    labs(title = "Child Causal Attribution") +
    theme(text = element_text(size=6),
          axis.text=element_text(size=10),
          strip.text = element_text(size=8),
          plot.title = element_text(hjust = 0.5, size = 7.2), 
          plot.subtitle = element_text(hjust = 0.5, size = 7.2))  
  ###########d2 non generic plot###########
  raw_CD <- d2 %>% 
    filter(task_name == "CD") %>% 
    group_by(subject, task_info, culture) %>% 
    mutate(resp = log(as.numeric(resp))) %>% 
    summarise(mean_log_rt = mean(resp, na.rm = TRUE)) %>% 
    mutate(
      resp_type_print = case_when(
        task_info == "context" ~ "Context Change", 
        TRUE ~ "Focal Change"
      ))
  
  d2_cd_plot <- ggplot(data = raw_CD, 
                       aes(y = mean_log_rt, x = culture, color = culture)) +
    geom_point(alpha = .2, position = position_jitter(width = .1)) + 
    stat_summary(fun.data = "mean_cl_boot", color = "black")+
    scale_color_manual(values = c("red", "blue"))+
    guides(fill = "none") +
    guides(color = "none") +
    ylab("Log-Transformed RT") + 
    xlab("")+
    theme_classic() +
    facet_wrap(~resp_type_print)+
    labs(title = "Change Detection")  + 
    theme(text = element_text(size=6),
          strip.text = element_text(size=8),
          axis.text=element_text(size=10),
          plot.title = element_text(hjust = 0.5, size = 7.2), 
          plot.subtitle = element_text(hjust = 0.5, size = 7.2))  
  
  raw_CA <- d2 %>% 
    filter(task_name == "CA") %>% 
    group_by(subject, task_info, culture) %>% 
    mutate(resp = (as.numeric(resp))) %>% 
    summarise(mean_resp = mean(resp, na.rm = TRUE)) %>% 
    mutate(
      resp_type_print = case_when(
        task_info == "personal" ~ "Personal Attribution", 
        TRUE ~ "Situational Attribution"
      ))
  
  d2_ca_plot <- ggplot(data = raw_CA, 
                       aes(y = mean_resp, x = culture, color = culture)) +
    geom_point(alpha = .2, position = position_jitter(width = .1)) + 
    stat_summary(fun.data = "mean_cl_boot", color = "black")+
    scale_color_manual(values = c("red", "blue"))+
    guides(fill = "none") +
    guides(color = "none") +
    ylab("Rating") + 
    xlab("")+
    theme_classic() +
    facet_wrap(~resp_type_print)+
    labs(title = "Adult Causal Attribution")  + 
    theme(text = element_text(size=6),
          axis.text=element_text(size=10),
          strip.text = element_text(size=8),
          plot.title = element_text(hjust = 0.5, size = 7.2), 
          plot.subtitle = element_text(hjust = 0.5, size = 7.2)) 
  
  
  ###########fine tuning d1 plot###########
  
  d1_up_plot <- d1_base_plot_list$CP + 
    ylab("Preference for unique option") + 
    labs(title = "Uniqueness Preference")
  d1_fd_plot <- d1_base_plot_list$FD + 
    ylab("Proportion first mention focal") + 
    labs(title = "Free Description")
  d1_hz_plot <- d1_base_plot_list$HZ + 
    ylab("Horizon Height") + 
    labs(title = "Horizon Collage")
  d1_rmts_plot <- d1_base_plot_list$RMTS + 
    ylab("Proportion choosing relational match") + 
    labs(title = "Ambiguous cRMTS")
  d1_rv_plot <- d1_base_plot_list$RV + 
    ylab("Proportion correct") + 
    labs(title = "Raven's SPM")
  d1_ssi_plot <- d1_base_plot_list$SI + 
    ylab("Ratio of inflation") + 
    labs(title = "Symbolic Self-Inflation")
  
  
  ###########fine tuning d2 plot###########
  
  d2_fd_plot <- d2_base_plot_list$FD + 
    ylab("Proportion first mention focal") + 
    labs(title = "Free Description")
  d2_rmts_plot <- d2_base_plot_list$RMTS + 
    ylab("Proportion choosing relational match") + 
    labs(title = "Ambiguous cRMTS")
  d2_rv_plot <- d2_base_plot_list$RV + 
    ylab("Proportion correct") + 
    labs(title = "Raven's SPM")
  d2_sei_plot <- d2_base_plot_list$SeI + 
    ylab("N of Causal Historical Response") + 
    labs(title = "Semantic Intuition")
  d2_ssi_plot <- d2_base_plot_list$SSI + 
    ylab("N of Causal Historical Response") + 
    labs(title = "Symbolic Self-Inflation")
  d2_td_plot <- d2_base_plot_list$TD + 
    ylab("Proportion Taxonomic Match") + 
    labs(title = "Taxonomic-Thematic Similarity")
  
  ###########putting d1 plots together###########
  
  
  
  #d1_regular_plot = cowplot::plot_grid(d1_rmts_plot,d1_fd_plot,d1_hz_plot,d1_ssi_plot,d1_up_plot,d1_rv_plot, ncol = 2)
  #d1_wide_plot = cowplot::plot_grid(d1_ca_plot, d1_ebb_plot, ncol = 1)
  
  #d1_all = plot_grid(d1_regular_plot, d1_wide_plot, ncol = 1, greedy = FALSE)
  
  d1_regular_plot_first_half = cowplot::plot_grid(d1_rmts_plot,d1_fd_plot, ncol = 2)
  d1_first_half = plot_grid(d1_regular_plot_first_half, d1_ebb_plot, ncol = 1, rel_heights = c(1, 1))
  d1_regular_plot_second_half = cowplot::plot_grid(d1_hz_plot,d1_ssi_plot, d1_up_plot, d1_rv_plot, 
                                                   ncol = 2, rel_heights = c(1, 1))
  d2_second_half =  plot_grid(d1_regular_plot_second_half, d1_ca_plot, ncol = 1, 
                              rel_heights = c(2, 1))
  
  d1_all = plot_grid(d1_first_half, d2_second_half, ncol = 1, greedy = FALSE, rel_heights = c(2, 3))
  
  
  
  ###########putting d2 plots together###########
  
  #d2_regular_plot = cowplot::plot_grid(d2_fd_plot,d2_rmts_plot,d2_rv_plot,
                                       #d2_sei_plot, d2_ssi_plot, d2_td_plot, ncol = 2)
  #d2_wide_plot = cowplot::plot_grid(d2_ca_plot, d2_cd_plot, ncol = 1)
  
  #d2_all = plot_grid(d2_regular_plot, d2_wide_plot, ncol = 1)
  
  d2_regular_plot_first_singles = cowplot::plot_grid(d2_rmts_plot,d2_fd_plot, ncol = 2)
  d2_regular_plot_first_segment = plot_grid(d2_regular_plot_first_singles, 
                                            d2_cd_plot, ncol = 1, rel_heights = c(1, 1))
  
  d2_regular_plot_second_singles = cowplot::plot_grid(d2_ssi_plot,d2_td_plot, ncol = 2)
  d2_regular_plot_second_segment = plot_grid(d2_regular_plot_second_singles, 
                                             d2_ca_plot, ncol = 1, rel_heights = c(1, 1))
  
  d2_up =cowplot::plot_grid(d2_regular_plot_first_segment,d2_regular_plot_second_segment, 
                            ncol = 1, rel_heights = c(1, 1))
  
  d2_bottom = cowplot::plot_grid(d2_sei_plot,d2_rv_plot, ncol = 2)
  
  d2_all = plot_grid(d2_up, d2_bottom, rel_heights = c(4, 1), ncol = 1)
  
  ###########putting all plots together###########
  
  plot_grid(
    NULL, NULL, NULL,
    d1_all, NULL, d2_all, 
            rel_heights = c(0.05, 1),
            rel_widths = c(1, 0.05, 1),
    nrow = 2,
            labels = c("A", "", "B", "", "",""), label_size = 10)
  
  

}