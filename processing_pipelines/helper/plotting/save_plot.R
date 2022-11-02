save_all_figures <- function(figure_list, w, h){
  
  lapply(seq(1, length(figure_list)), 
         function(i){
           path = here(glue("cached_results/figures/{names(figure_list[i])}.pdf"))
           print(path)
           ggsave(path,
                  figure_list[[i]], 
                  width = w, height = h, device = NULL)
         }
  )
}
