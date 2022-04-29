
###### Load packages ####
library(tidytext)
library(tidyverse)

#### Read file with predicted bibliographies  ####


rf2 <- read_csv("data/20220426_proportions_bibpred_RF2.csv")

#### Filter and create df with pages containing a bibliography according to RF2 + filename_pag, filename, text, n_pag ####

rf2_biblio <- rf2 %>% 
  filter(biblio_class == 1) %>%
  select(pdf_name, n_pag) %>% 
  unite(filename_pag, pdf_name, n_pag, remove = F)

#### Get text for all predicted pages ####

#complete_text <- readRDS("data/dissertations_text_npags.rds") 

rf2_biblio_text <- complete_text %>% 
  filter(filename_pag %in% rf2_biblio$filename_pag)


#saveRDS(rf2_biblio_text, file = "rf2_biblio_text.rds")
#rf2_biblio_text <- readRDS("data/rf2_biblio_text.rds") 


###### Read file with manually-tagged bibliographies ####
etiquetado <- read_csv("data/20220321_bib_sample_etiquetado.csv")
names(etiquetado)

###### Filter only pages with a bibliography ####
#1,454 pages
bibliographies_et <- etiquetado %>% 
  filter(bibtest == 1) %>% 
  select(pdf_name, n_pag) %>% 
  unite(filename_pag, pdf_name, n_pag, remove = F)


###### Get text for pages with a bibliography ####
bibliographies_text_et <- rf2_biblio_text %>% 
  filter(filename_pag %in% bibliographies_et$filename_pag)

###### Tokenize words in predicted bibliography pages ####

bibtokens <- rf2_biblio_text %>% 
  ungroup() %>% 
  unnest_tokens(word, text)

bibtokens_filtered <- bibtokens %>% 
  anti_join(stop_german, by = c('word'))%>% 
  anti_join(stop_english, by = c('word'))%>% 
  anti_join(stop_english, by = c('word'))

bibtokens_complete_count <- bibtokens_filtered %>%
  count(word, sort = TRUE)

###### Tokenize words in tagged bibliography pages ####

bibtokens_et <- bibliographies_text_et %>%
  ungroup() %>% 
  unnest_tokens(word, text)

bibtokens_et_filtered <- bibtokens_et %>% 
  anti_join(stop_german, by = c('word'))%>% 
  anti_join(stop_english, by = c('word'))%>% 
  anti_join(stop_english, by = c('word'))

bibtokens_et_count <- bibtokens_et_filtered %>%
  count(word, sort = TRUE)

extra_words <- bibtokens_filtered %>% 
  filter(!word %in% bibtokens_et_filtered$word)

extra_words_count <- extra_words %>%
  count(word, sort = TRUE)

extra_words_count

bibliographies_text <- bibliographies_text %>% 
  ungroup() %>% 
  select(-filename)
bibliographies_text
bibliographies_text %>% 
  slice_head(n=10) %>% 
  unnest_tokens(sentences, text, token = "regex", pattern = "\\n")

##### Slice sentences of predicted bibliographic text

bibliographies_text_sent <- rf2_biblio_text %>% ungroup() %>% unnest_tokens(sentences, text, token = "regex", pattern = "\\n")
bibliographies_text_sent
bibliographies_text_sent %>% 
  count(sentences, sort = TRUE)

ny <- dplyr::filter(bibliographies_text_sent, grepl("new york", sentences, ignore.case=TRUE))
fm <- dplyr::filter(bibliographies_text_sent, grepl("frankfurt m", sentences, ignore.case=TRUE))
vgl <- dplyr::filter(bibliographies_text_sent, grepl("vgl", sentences, ignore.case=TRUE))
acc <- dplyr::filter(bibliographies_text_sent, grepl("\\<acc\\>", sentences, ignore.case=TRUE))
https <- dplyr::filter(bibliographies_text_sent, grepl("https", sentences, ignore.case=TRUE))
isbn <- dplyr::filter(bibliographies_text_sent, grepl("isbn", sentences, ignore.case=TRUE))

unique(acc$filename_pag)
# de_aw_1973_herberg_m linguistik
# de_gaug_1999_reimar linguistik
# de_hub_1998_steinbach_m linguistik

# predicciones extrañas:
# de_rub_2014_rattay_f -> tiene tablas con literatura 