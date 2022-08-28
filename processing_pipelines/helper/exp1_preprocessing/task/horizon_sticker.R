get_HZ_raw <- function(raw_d){
  sticker_dir = here("processing_pipelines/helper/exp1_preprocessing/material_stickers/")
  stickers = list.files(sticker_dir)
  
  d <- Dict$new()
  for (s in stickers){
    img <- readPNG(file.path(sticker_dir, s)) 
    dim <- dim(img)
    size <- dim[[1]] * dim[[2]]
    src_name <- paste("images/sort/",s,sep = "")
    d[src_name] <- size[[1]]
  }
  
  # get all the trials with sticker 
  sticker_d <- raw_d %>%filter(trial_type == "horizon-sticker")
  
  #focus on the moved sticker for each participant 
  moved_sticker <- sticker_d %>% 
    select(subject, rt, culture,trial_type, moves,final_locations) %>% 
    toJSON() %>% 
    fromJSON() %>% 
    mutate(
      moves = as.character(moves),
      moves = map(moves, ~ fromJSON(.) %>% as.data.frame())) %>% 
    unnest(moves) %>% 
    group_by(subject) %>% 
    distinct(src, .keep_all = TRUE)
  
  
  
  # helper function to get each sticker's dimension from dictionary
  get_sticker_dim <- function(src, d){
    return (d[src][[1]])
  }
  
  # convert from src name to sticker dimension 
  moved_sticker$size <- unlist(map(moved_sticker$src,get_sticker_dim, d))
  
  # caculate total sticker areas for each subject 
  sticker_area <- moved_sticker %>% 
    group_by(subject) %>% 
    summarize(
      sum_area = sum(size)
    ) 
  
  # function that map back to the dataframe
  get_sticker_total_area <- function(id, caculated_df){
    area <- caculated_df %>% 
      filter(subject == id) %>% 
      select(sum_area) %>% 
      pull()
    return (area)
  }
  
  # add a column back to the raw dataframe 
  moved_sticker$stkr_area <- unlist(map(moved_sticker$subject,get_sticker_total_area, sticker_area))
  
  # calculate final height  
  CANVAS_HEIGHT <-334.4
  
  #focus on the moved sticker for each participant 
  final_sticker <- sticker_d %>% 
    select(subject, culture,rt, trial_type, moves,final_locations) %>% 
    toJSON() %>% 
    fromJSON() %>% 
    mutate(
      final_locations = as.character(final_locations),
      final_locations = map(final_locations, ~ fromJSON(.) %>% as.data.frame())) %>% 
    unnest(final_locations) %>% 
    filter(src == "images/sort/horizon_2.png") %>% 
    mutate(
      hz_height = ((CANVAS_HEIGHT - y)/CANVAS_HEIGHT)
    ) %>% 
    select(subject, culture,hz_height)
  
  count_stkr <- moved_sticker %>% 
    group_by(subject) %>% 
    summarise(
      stkr_count = n()
    )
  
  
  stkr_df <- left_join(moved_sticker, final_sticker, by = "subject")
  
  HZ_table <- left_join(stkr_df, count_stkr, by = "subject") %>% 
    mutate(task_name = "HZ", 
           culture = culture.x) %>% 
    select(subject, culture,task_name,  stkr_count, stkr_area, hz_height)
  
  return(HZ_table)
  
}

get_HZ_main <- function(raw_d){
  HZ_table <- get_HZ_raw(raw_d)
  HZ_main <-HZ_table %>% 
    mutate(
      task_info = "HZ", 
      trial_info = "HZ", 
    ) %>% 
    pivot_longer(
      stkr_count:hz_height, 
      names_to = "resp_type", 
      values_to = "resp"
    ) %>% 
    select(
      subject, culture,task_name, task_info, trial_info, resp_type, resp 
    ) %>% 
    distinct(subject,culture, task_name, task_info, trial_info, resp_type, resp)
  
  return(HZ_main)
  
}