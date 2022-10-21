library(brms)
library(tidyverse)


d2 <- read.csv("~/ccrr_cluster/e2_tidy_main.csv")


cd_df <- d2 %>% 
  filter(task_name == "CD") %>% 
  mutate(reaction_time = as.numeric(resp), 
         type_of_change = task_info,
         picture = trial_info) %>% 
  select(culture, subject,reaction_time,type_of_change, picture) %>% 
  filter(!is.na(reaction_time))


full_b_cd_model <- brm(
  log(reaction_time) ~ culture * type_of_change + (type_of_change | subject) + (culture | picture), 
  data = cd_df, 
  family = "gaussian",
  prior =  c(
    set_prior("normal(0, 0.92)", class = "b", coef = "cultureUS"),
    set_prior("normal(0, 0.92)", class = "Intercept")),
  save_pars = save_pars(all = TRUE), 
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  cores = 4, 
  iter = 5000,
  seed = 11111,
  file = "full_b_cd_model_log.Rds"
)


null_b_cd_model <- brm(
  log(reaction_time) ~ culture + type_of_change + (type_of_change | subject) + (culture | picture), 
  data = cd_df, 
  family = "gaussian",
  prior =  c(
    set_prior("normal(0, 0.92)", class = "b", coef = "cultureUS"),
    set_prior("normal(0, 0.92)", class = "Intercept")),
  save_pars = save_pars(all = TRUE), 
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  cores = 4, 
  iter = 5000,
  seed = 11111,
  file = "null_b_cd_model_log.Rds"
)