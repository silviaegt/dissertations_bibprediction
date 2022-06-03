library(tidyverse)
et1 <- read_csv("data/20220219_bib_sample_etiquetado.csv")
et2_simple <- read_csv("data/20220321_bib_sample_etiquetado2.csv")
et2 <- read_csv("data/20220321_bib_sample_etiquetado2.csv")

et2 %>%
  count(bibtest)

nameset1 <- names(et1) 
nameset2 <- names(et2)
same <- intersect(nameset2, nameset1)
et3 <- full_join(et1, et2, by=c(same))

et3_filtered <-  et3 %>% 
  select(pdf_name, n_pag, bibtest)

et3_feat <- inner_join(et3_filtered, df3_clean, by = c("pdf_name", "n_pag"))

write_csv(et3_feat, "data/20220603_bib_sample_etiquetado.csv")
