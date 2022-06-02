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
write_csv(et3, "data/20220322_bib_sample_etiquetado.csv")
