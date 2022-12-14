plot_table <- function(){
  
  
  d1 <- read_csv(here("data/03_processed_data/exp1/tidy_main.csv"))
  d2 <- read_csv(here("data/03_processed_data/exp2/tidy_main.csv"))
  
  ########## get the sample size #########
  
  d1_order <- c("RMTS", "FD", "EBB", "HZ", "SI", "CP", "CA", "RV")
  d1_df_summary <- d1 %>% 
    group_by(task_name,culture) %>% 
    distinct(subject) %>% 
    count() %>% 
    ungroup() %>% 
    pivot_wider(names_from = culture, values_from = n) %>% 
    mutate(
      str = glue("CN:{CN}; US:{US}")
    ) %>% 
    slice(match(d1_order, task_name))
  d2_order <- c("RMTS", "FD", "CD", "SSI", "CA", "TD", "SeI", "RV")
  d2_df_summary <- d2 %>% 
    group_by(task_name,culture) %>% 
    distinct(subject) %>% 
    count() %>% 
    ungroup() %>% 
    pivot_wider(names_from = culture, values_from = n) %>% 
    mutate(
      str = glue("CN:{CN}; US:{US}")
    ) %>% 
    slice(match(d2_order, task_name))
  sample_sizes <- c(d1_df_summary$str, d2_df_summary$str)
  
  ########## table #########
  
  collapse_rows_dt <- data.frame(
                                 Task = c("Ambiguous Relational Match-To-Sample (RMTS)", 
                                          "Picture Free Description", 
                                          "Ebbinghaus Illusion", 
                                          "Horizon Collage", 
                                          "Symbolic Self-Inflation (Family)", 
                                          "Uniqueness Preference", 
                                          "Child Causal Attribution", 
                                          "Raven's Progressive Matrices", 
                                          "Ambiguous Relational Match-To-Sample (RMTS)", 
                                          "Picture Free Description", 
                                          "Change Detection", 
                                          "Symbolic Self-Inflation (Friends)", 
                                          "Adult Causal Attribution", 
                                          "Taxonomic-Thematic Similariy", 
                                          "Semantic Intuition", 
                                          "Raven's Progressive Matrices"), 
                                 `Relevant Citation` = c(
                                   "Carstensen et al. (2019)", 
                                   "Imada, Carlson, & Ktakura (2013)", 
                                   "Imada, Carlson, & Itakura (2013)", 
                                   "Senzaki, Masuda, & Nand (2014)",
                                   "Kitayama et al. (2009)",
                                   "Kim & Markus (1999)", 
                                   "Seiver, Gopnik, & Goodman (2013)",
                                   "Su (2020)",
                                   "Carstensen et al. (2019)",
                                   "Imada, Carlson, & Itakura (2013)", 
                                   "Mausda & Nisbett (2007)", 
                                   "Kitayama et al. (2009)", 
                                   "Morris & Peng (1994)", 
                                   "Ji, Zhang, & Nisbett (2004)", 
                                   "Li, Liu, Chalmers, & Snedeker (2018)", 
                                   "Su (2020)"
                                 ), 
                                 `Task Description` = c(
                                   "Infer whether an object or relation is causally relevant",
                                   "Describe pictures from memory after a brief study period",
                                   "Judge the size of circles in a context designed to bias size judgments",
                                   "Make an image by dragging and dropping stickers onto a display",
                                   "Draw self and family members as circles",
                                   "Choose a sticker from five stickers, four of which are the same color",
                                   "Watch short vignettes and explain the decisions of the characters
                              ",
                                   "Use analogic reasoning to complete visually-presented patterns",
                                   "Infer whether an object or relation is causally relevant",
                                   "Describe pictures from memory after a brief study period",
                                   "Find differences in the foreground or background of two images",
                                   "Draw a sociogram with self and friends as nodes, relationships as edges",
                                   "Read a crime story and explain the criminal???s motivations",
                                   "Match items based on taxonomic or thematic similarity (e.g., cow: chicken / grass)",
                                   
                                   "Decide whether a story refers to a named character (whose actions are mischaracterized) or the person who performed the actions (but had a different name)",
                                   "Use analogical reasoning to complete visually-presented patterns"
                                 ),
                                 "Sample Size" = sample_sizes
  )
  
  kable(collapse_rows_dt, "latex", align = "l",
        row.names = FALSE, 
        col.names = c(
                      "Task", 
                      "Relevant Citation", 
                      "Task Description",
                      "Task Sample Size")) %>%
    kable_styling(full_width = TRUE, 
                  font_size = 7,
                  latex_options = "scale_down")  %>% 
    group_rows("Group 1", 1, 8) %>%
    group_rows("Group 2", 9, 16)
  
  
  #%>% 
    #column_spec(1,width = "2cm", bold = T, color = "red") %>%
    #landscape()
  
  
}