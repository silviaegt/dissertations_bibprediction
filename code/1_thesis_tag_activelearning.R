library(tidyverse)

### 
susp <- read_csv("data/20220302_suspicious_tagging_annotated.csv")
inhalt <- susp %>% 
  filter(note1 == "inhaltsverzeichnis")

susp %>% 
  count(note1, sort = T)
siglen <- susp %>% 
  filter(note1 == "siglenverzeichnis")

prop <- read_csv("data/proportions_bibliography_complete_20220321.csv")

siglen_u <- unique(siglen$pdf_name)
inhalt_u <- unique(inhalt$pdf_name)
foot_u

etiquetar3 <- prop %>% 
  filter(pdf_name %in% siglen_u)

etiquetar <- prop %>% 
  filter(pdf_name %in% etiquetar2)
write_csv(etiquetar3, "data/etiquetar3.csv")

etiquetado %>% 
  filter(bibtest==1)

etiquetar2 <-setdiff(etiquetar2$pdf_name, etiquetar$pdf_name)


etiquetado <- read_csv("data/20220219_bib_sample_etiquetado.csv")

names(etiquetado)
etiquetado <- etiquetado %>% 
  unite(filename_pag, pdf_name, n_pag, remove = F)

prop <- prop %>% 
  unite(filename_pag, pdf_name, n_pag, remove = F)


etiquetado <- etiquetado %>% 
  select(filename_pag, bibtest)

prop_test <- prop %>% 
  left_join(etiquetado, by = "filename_pag")

prop <- prop_test %>% 
  select(-filename_pag) %>% 
  rename(bibtest = bibtest.x) %>% 
  select(-bibtest.y)

names(etiquetado1)
names(etiquetado2)

chng <- do.call(rbind, list(prop, etiquetado1, etiquetado2))


etiquetado2 <- read_csv("data/etiquetado2.csv")
etiquetado1 <- read_csv("data/etiquetado1.csv")

test1 <- merge(prop, etiquetado1, all.y = T)
test2 <- merge(test1, etiquetado2, all.y = T)
test2 %>% 
  unite(filename_pag, pdf_name, n_pag, remove = F) %>% 
  count(filename_pag, sort = T)

write_csv(chng, "data/20220321_bib_sample_etiquetado.csv")
