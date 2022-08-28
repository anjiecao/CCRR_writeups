

summarize_codeable_rate <- function(df){
  codeable_df <- df %>% 
    group_by(subject) %>% 
    summarise(
      codeable_rate = (sum(codeable) / n()), # 1 = codeable; 0 = uncodeable
      include = if_else(codeable_rate > 0.75, "yes", "no")
    )
  
  return (codeable_df)
}