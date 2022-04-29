library(tidyverse)
pred <- read_csv("data/20220426_proportions_bibliography_complete_pred.csv")

predicted <- pred %>% 
  filter(biblio_class == 1) %>% 
  select(pdf_name, n_pag)

