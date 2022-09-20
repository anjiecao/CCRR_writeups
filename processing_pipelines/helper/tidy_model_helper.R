

tidy_model_main <- function(model_name){
  # only horizon model is using a fixed effect model 
  # so the format is slightly different
  print(model_name)
  model <- readRDS(paste0(GLMER_MODEL_DIR,model_name))
  
  raw_summary <- (summary(model)$coefficients) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    rename(predictor = rowname)
  
  
  
  
  if ("z value" %in% names(raw_summary)){
    res <- tidy_model_table_for_z(readRDS(paste0(GLMER_MODEL_DIR,model_name)))
  }else{
    res <- tidy_model_table_for_t(readRDS(paste0(GLMER_MODEL_DIR,model_name)))
  }
  
  return (res %>% mutate(model_name = sub(".RDS", "", model_name)))  
}



tidy_model_table_for_z <- function(model){
  
  raw_summary <- (summary(model)$coefficients) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    rename(predictor = rowname)
  
  tidy_summary <- raw_summary %>% 
    mutate(
      estimate_print = case_when(
        round(Estimate, 2) == 0 ~ "< 0.01", 
        TRUE ~  as.character(round(Estimate, 2))),
      se_print = round(`Std. Error`, 2),
      z_print = round(`z value`, 2),
      p_print = case_when(
        `Pr(>|z|)` < 0.01 ~ "< 0.01", 
        `Pr(>|z|)` < 0.05 ~ "< 0.05",
        TRUE ~ paste0("= ", as.character(round(`Pr(>|z|)`, 2)))
      )
    )
  
  return(tidy_summary %>% as.data.frame())
  
  
}

tidy_model_table_for_t <- function(model){
  
  raw_summary <- (summary(model)$coefficients) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    rename(predictor = rowname)
  
  tidy_summary <- raw_summary %>% 
    mutate(
      estimate_print = case_when(
        round(Estimate, 2) == 0 ~ "< 0.01", 
        TRUE ~  as.character(round(Estimate, 2))),
      se_print = round(`Std. Error`, 2),
      t_print = round(`t value`, 2),
      p_print = case_when(
        `Pr(>|t|)` < 0.01 ~ "< 0.01", 
        `Pr(>|t|)` < 0.05 ~ "< 0.05",
        TRUE ~ paste0("= ", as.character(round(`Pr(>|t|)`, 2)))
      )
    )
  
  return(tidy_summary)
  
  
}

save_model <- function(model, main_dir){
  saveRDS(model, paste0(main_dir, deparse(substitute(model)),".RDS"))
}