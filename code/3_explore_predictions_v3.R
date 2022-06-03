
###### Load packages ####
library(tidytext)
library(tidyverse)

#### Read file with predicted bibliographies  ####


rf3 <- read_csv("data/20220603_proportions_bibpred_RF3.csv")

#### Filter and create df with pages containing a bibliography according to RF3 + filename_pag, filename, text, n_pag ####

rf3_biblio <- rf3 %>% 
  filter(biblio_class == 1) %>%
  select(pdf_name, n_pag) %>% 
  unite(filename_pag, pdf_name, n_pag, remove = F)

#### Get text for all predicted pages ####

complete_text <- readRDS("data/dissertations_text_npags.rds") 
complete_text
rf3_biblio_text <- inner_join(complete_text, rf3_biblio, by = c("filename_pag", "n_pag"))
rf3_biblio_text %>% ungroup() %>% select(-pdf_name) %>% names()
saveRDS(rf3_biblio_text, file = "data/rf3_biblio_text.rds")
#rf3_biblio_text <- readRDS("data/rf3_biblio_text.rds") 


###### Read file with manually-tagged bibliographies ####
etiquetado <-  read_csv("data/20220603_bib_sample_etiquetado.csv")
names(etiquetado)

###### Filter only pages with a bibliography ####
#1,454 pages
bibliographies_et <- etiquetado %>% 
  filter(bibtest == 1) %>% 
  select(pdf_name, n_pag) %>% 
  unite(filename_pag, pdf_name, n_pag, remove = F)
bibliographies_et

###### Get text for pages with a bibliography ####
bibliographies_text_et <- complete_text %>% 
  filter(filename_pag %in% bibliographies_et$filename_pag)

bibtokens_et_filtered <- bibliographies_text_et%>% 
  ungroup() %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_german, by = c('word'))%>% 
  anti_join(stop_english, by = c('word'))%>% 
  anti_join(stop_spanish, by = c('word'))

bibtokens_et_filtered
###### Tokenize words in predicted bibliography pages ####

bibtokens_rf3 <- rf3_biblio_text %>% 
  ungroup() %>% 
  unnest_tokens(word, text)

bibtokens_filtered <- bibtokens_rf3 %>% 
  anti_join(stop_german, by = c('word'))%>% 
  anti_join(stop_english, by = c('word'))%>% 
  anti_join(stop_spanish, by = c('word'))

# Get bibtokens freq. by number of documents ----

doc_freq_total_rf3 <- bibtokens_filtered %>% 
  group_by(word) %>%
  summarise(unique_files = n_distinct(filename))

# Get extra tokens freq. by number of documents ----

extra_words <- bibtokens_filtered %>% 
  filter(!word %in% bibtokens_et_filtered$word)
extra_words
extra_words_count <- extra_words %>%
  count(word, sort = TRUE)
extra_words_doc_count <- extra_words %>% 
  group_by(word) %>%
  summarise(unique_files = n_distinct(filename))
extra_words_doc_count

##### Slice sentences of predicted bibliographic text

bibliographies_text_sent <- rf3_biblio_text %>% ungroup() %>% unnest_tokens(sentences, text, token = "regex", pattern = "\\n")
bibliographies_text_sent_count <- bibliographies_text_sent %>% 
  count(sentences, sort = TRUE)

bibliographies_text_sent %>% 
  group_by(filename) %>% 
  filter(row_number()<3)

##### Extract first sentence

bibliographies_1st_sent <- bibliographies_text_sent %>% 
  group_by(filename) %>% 
  slice(1)

bibliographies_1st_sent_tokens <- bibliographies_1st_sent %>% 
  ungroup() %>% 
  unnest_tokens(word, sentences) %>% 
  count(word, sort = T)
bibliographies_1st_sent

suspicious <- bibliographies_1st_sent %>% 
  dplyr::filter(!grepl("literaturverzeichnis|bibliographie|bibliography|bibliograf[í|i]a|cited|quellen|litera[tur]|werke|primärliteratur|quellenverzeichnis|bibliographical|references|bibliografie", sentences)) %>% 
  filter(!grepl("\\d{2,4}", sentences)) %>% 
  mutate(len = nchar(sentences))

write_csv(suspicious, "data/suspicious.csv")