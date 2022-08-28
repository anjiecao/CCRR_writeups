# library(NLP)
# library(tm)  # make sure to load this prior to openNLP
# library(openNLP)
# library(openNLPmodels.en)

# 
# parse_key_words <- function(s){
#   
#   WORD_TAGS <- c("NN","NNP","NNPS", "JJ", "JJR", "JJS")
#   #s <- "black bird hits"
#   s <- as.String(s)
#   init_s_w = annotate(s, list(Maxent_Sent_Token_Annotator(),
#                               Maxent_Word_Token_Annotator()))
#   pos_res = annotate(s, Maxent_POS_Tag_Annotator(), init_s_w)
#   word_subset = subset(pos_res, type=='word')
#   tags = sapply(word_subset$features , '[[', "POS")
#   
#   
#   
#   word=s[word_subset]
#   word <- paste(word, collapse = " ")
#   # single word blues 
#   if(str_detect(word, " ")){
#     
#     tagged = data_frame(word=s[word_subset], pos=tags) %>% 
#       filter(!str_detect(pos, pattern='[[:punct:]]'))
#     
#     key_words <- tagged %>% 
#       filter(pos %in% WORD_TAGS) %>% 
#       select(word) %>% 
#       pull() 
#     
#     key_words <- gsub(", "," ",toString(key_words))
#   }else{
#     key_words <- word 
#   }
#   
#   return (key_words)
# }

get_FD_main <- function(raw_d){
  
  CN_FD_ANNOTATED_PATH <- here("data/01b_annotated_data/exp1/CN/cn_fd_merged_coded.csv")
 
  US_FD_ANNOTATED_PATH <- here("data/01b_annotated_data/exp1/US/us_fd_coded.csv")
  included_subject <- raw_d %>% 
    select(subject) %>% 
    distinct() %>% 
    pull()
  
  ##### Extract the FD ###########
  
  cn_fd <- read_csv(CN_FD_ANNOTATED_PATH) %>% 
    mutate(culture = "CN")
  us_fd <- read_csv(US_FD_ANNOTATED_PATH) %>% 
    mutate(culture = "US")
  fd <- cn_fd %>%  
    bind_rows(us_fd, cn_fd) %>% 
    filter(subject %in% included_subject) %>% 
    mutate(
      task_name = "FD", 
      task_info = "FD", 
      trial_info = stimulus
    ) %>% 
    pivot_longer(cols = first_mention_focal:full_focal_description, 
                 names_to = "resp_type", 
                 values_to = "resp") %>% 
    select(
      subject, culture, task_name, task_info, 
      trial_info, resp_type, resp
    )
    
  return(fd)
  
  
  
  
  
  
}

