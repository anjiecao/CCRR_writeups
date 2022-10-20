library(brms)


d1 <- read.csv("~/ccrr_cluster/e1_tidy_main.csv")

#### rmts ##### 

rmts_df <- d1 %>% 
  filter(task_name == "RMTS") %>% 
  mutate(choice = case_when(
    resp == "1" ~ 1, # rel
    resp == "0" ~ 0 # obj
  )) %>%
  group_by(subject) %>% 
  mutate(trial_num = row_number()) %>% 
  select(-resp, -task_info, -trial_info, -resp_type)

full_b_rmts_model <- brm(choice ~ culture + (trial_num | subject), family = binomial, 
                         prior =  c(
                           set_prior("normal(0, 0.5)", class = "b", coef = "cultureUS"),
                           set_prior("normal(0,1.5)", class = "Intercept")),
                        save_pars = save_pars(all = TRUE), 
                        control = list(adapt_delta = .999, 
                                       max_treedepth = 15),
                        iter = 9000, seed = 11111,
                        data = rmts_df, 
                        file= "full_b_rmts_model.Rds",
                        cores=4)
null_b_rmts_model <- brm(choice ~ 1 + (trial_num | subject), family = binomial, 
                         save_pars = save_pars(all = TRUE), 
                         prior =  c(
                           set_prior("normal(0, 0.5)", class = "b", coef = "cultureUS"),
                           set_prior("normal(0,1.5)", class = "Intercept")),
                         control = list(adapt_delta = .999, 
                                        max_treedepth = 15),
                         iter = 9000, seed = 11111,
                         data = rmts_df, 
                         file= "null_b_rmts_model.Rds",
                         cores=4)