## Todo: the visual inspection does not work quite well so far 






library(png)

get_dim_h <- function(path){
  pic <- readPNG(path)
  dim <- dim(pic)
  return (dim[[1]])
}

get_dim_w <- function(path){
  pic <- readPNG(path)
  dim <- dim(pic)
  return (dim[[2]])
}

create_hzsticker_df <- function(raw_d){
  #
  sticker_d <- raw_d %>%filter(trial_type == "horizon-sticker")
  
  #focus on the moved sticker for each participant 
  init_lc_df <- sticker_d %>% 
    select(subject, rt, trial_type, init_locations) %>% 
    toJSON() %>% 
    fromJSON() %>% 
    mutate(
      init_locations = as.character(init_locations),
      init_locations = map(init_locations, ~ fromJSON(.) %>% as.data.frame())) %>% 
    unnest(init_locations) %>% 
    select(-rt) %>% 
    group_by(subject) %>% 
    mutate(
      init_x = round(x),
      init_y = round(y), 
      init_src = src,
      temp_id = paste(subject, src),
      type = "INIT"
    ) %>% 
    select(subject, temp_id, init_src, init_x, init_y)
  
  final_lc_df <- sticker_d %>% 
    select(subject, rt, trial_type, final_locations) %>% 
    toJSON() %>% 
    fromJSON() %>% 
    mutate(
      final_locations = as.character(final_locations),
      final_locations = map(final_locations, ~ fromJSON(.) %>% as.data.frame())) %>% 
    unnest(final_locations) %>% 
    #mutate(src = as.character(src)) %>% 
    #filter(str_detect(src, "horizon", negate = TRUE)) %>% 
    mutate(
      final_x = round(x), 
      final_y = round(y),
      final_src = src, 
      temp_id = paste(subject, src),
      type = "FINAL",
    ) %>% 
    select(subject, temp_id, final_src, final_x, final_y)
  
  sum_lc_df <- left_join(init_lc_df, final_lc_df, by = "temp_id") %>% 
    mutate(subject = subject.x)
    
  sum_moved_df <- sum_lc_df %>% 
    filter(
      (init_x != final_x) | (init_y != final_y)
    ) %>% 
    select(subject, final_src, final_x, final_y) %>% 
    rowwise() %>% 
    mutate(
      final_src = gsub("images/sort/", final_src, replacement ="preprocessing/material_stickers/"),
      dim_h = get_dim_h(final_src) , # the original stickers were scaled as well 
      dim_w = get_dim_w(final_src) 
      #final_y = 334.4 - final_y
    )
  
  
  
  return(sum_moved_df)

}


# ##
# TEST <- create_hzsticker_df(d) %>% filter(subject == 13)
# ggplot(TEST, aes(x = final_x, y = final_y)) +
#   geom_image(aes(image = final_src), by ="width" ) + 
#   #facet_wrap(~subject) + 
#   coord_fixed(xlim = c(0,451.4), ylim = c(0, 334.4)) + 
#   scale_y_reverse()




create_horizon_df <- function(raw_d){
  horizon_df <- raw_d %>% 
    filter(trial_type == "horizon-sticker")
  return(horizon_df)
  
}






##### SymS: Save image that requires visual inspection #####

save_SymS_circle <- function(INPUT_PATH, SYMS_DIR){
  
  raw_data <- read.csv(INPUT_PATH)
  
  num_id <- unique(raw_data$subject)
  for (id in num_id){
    d_circle <- raw_data %>% 
      filter(subject == id) %>% 
      create_SymS_circle_df()
    id <- as.character(id)
    f_name <- paste(SYMS_DIR, id, ".png", sep="")
    d_circle %>% 
      ggplot() + 
      geom_circle(aes(x0 = x, y0 = y, r = radius)) + 
      geom_text(aes(x = x, y = y, label = label)) + 
      coord_fixed(xlim = c(0,1024), ylim = c(0, 800)) + 
      scale_y_reverse()
    ggsave(f_name)
  }
  
}
#save_SymS_circle(d,SYMS_DIR)










##### SymS: Write to csv that check Label #####
write_HIT_checkLabel <- function(INPUT_PATH, OUT_FILE_PATH){
  raw_data <- read.csv(INPUT_PATH)
  
  horizon_df <- create_horizon_df(raw_data)
  syms_label_df <- create_SymS_label_df(raw_data)
  
  syms_label_to_check <- syms_label_df %>% 
    filter(check_basic_label(label_all)) %>% 
    mutate(
      task_name = "symsLabel",
      task_resp = label_all, 
      human_rate = ""
    ) %>% 
    select(subject, task_name,task_resp, human_rate)
  
  write_csv(syms_label_to_check, OUT_FILE_PATH)
  
}

#write_HIT_checkLabel(d,OUT_FILE_PATH)


