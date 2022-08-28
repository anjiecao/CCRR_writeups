 
make_redundant_coding_sheets <- function(){
  
  # read in coded sheets 
  cn_fd <- read_csv(here("data/annotated/CN/FD.csv"))
  cn_ssi <- read_csv(here("data/annotated/CN/SSI.csv"))
  cn_CD_wr <- read_csv(here("data/annotated/CN/CD_wr.csv"))
  
  us_fd <- read_csv(here("data/annotated/US/FD.csv"))
  us_ssi <- read_csv(here("data/annotated/US/SSI.csv"))
  #cn_CD_wr <- read_csv(here("data/annotated/CN/CD_wr.csv"))
  
  # sample 10%
  cn_sampled_CD_wr <- cn_CD_wr %>% 
    sample_n(round(0.1 * nrow(cn_CD_wr), 0)) %>% 
    mutate(correct = 1, 
           coder = "") %>% 
    arrange(prompt) %>% 
    select(subject, culture, prompt, response, correct, coder)
  
  cn_sampled_fd <- cn_fd %>% 
    sample_n(round(0.1 * nrow(cn_fd), 0)) %>% 
    mutate(first_mention_focal = 0, 
           codeable = 1, 
           coder = "") %>% 
    arrange(stimulus) %>% 
    select(subject, culture, task_name, stimulus, trial_raw, trial_num, codeable,
           first_mention_focal, coder)
  
  cn_ssi_sampled_subject <- sample(cn_ssi$subject, 
                                   round(0.1 * length(unique(cn_ssi$subject)), 0))
  cn_sampled_ssi <- cn_ssi %>% 
    filter(subject %in% cn_ssi_sampled_subject) %>% 
    arrange(ssi_id) %>% 
    mutate(codeable = 1, me_circle = 0, 
           coder = "")
  
  
  us_sampled_fd <-  us_fd %>% 
    sample_n(round(0.1 * nrow(us_fd), 0)) %>% 
    mutate(first_mention_focal = 0, 
           codeable = 1, 
           coder = "") %>% 
    arrange(stimulus) %>% 
    select(subject, culture, task_name, stimulus, trial_raw, trial_num, codeable,
           first_mention_focal, coder)
  
  
  us_ssi_sampled_subject <- sample(us_ssi$subject, 
                                   round(0.1 * length(unique(us_ssi$subject)), 0))
  us_sampled_ssi <- us_ssi %>% 
    filter(subject %in% us_ssi_sampled_subject) %>% 
    arrange(ssi_id) %>% 
    mutate(codeable = 1, me_circle = 0, 
           coder = "")
  
  
  # write out sheets 
  write_csv(cn_sampled_CD_wr, here("data/to_be_annotated/CN/redundant_coding/rCD_wr.csv"))
  write_csv(cn_sampled_fd, here("data/to_be_annotated/CN/redundant_coding/rFD.csv"))
  write_csv(cn_sampled_ssi, here("data/to_be_annotated/CN/redundant_coding/rSSI.csv"))
  write_csv(us_sampled_fd, here("data/to_be_annotated/US/redundant_coding/rFD.csv"))
  write_csv(us_sampled_ssi, here("data/to_be_annotated/US/redundant_coding/rSSI.csv"))
  
}