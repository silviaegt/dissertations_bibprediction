library(tidyverse)
pred <- read_csv("data/20220302_proportions_bibliography_complete_pred.csv")

suspicious_tagging <- pred %>% 
  filter(biblio_class == 1) %>% 
  select(pdf_name, n_pag, biblio_class) %>% 
  filter(n_pag < 50)

write_csv(suspicious_tagging, "data/20220302_suspicious_tagging.csv")

#de_alu_1999_kratochvil_m - has word literaturverzeichnis page 8
#de_bugw_2010_miehe_f - has word  literaturverzeichnis page 6 & many dates in page 7
#de_fub_2003_kopcke_m - has word literaturverzeichnis & many dates in page 6
#de_gaug_2019_krason_f  - has word literaturverzeichnis & many dates in page 4
#de_mlu_2016_beer_m - inhaltsverzeichnis page 3 & 4 + page with many dates in page 5 - hablar con Julis
#de_phe_1999_hunig_f - inhaltsverzeichnis page 6 & then some dates in page 8 - hablar con Julis

#de_pum_2004_landzettel_f - big foot note
#de_jlu_2006_bautz_f - page 41 mystery - has perhaps many dates?
#de_fau_2016_adorf_f - page 45 mystery - not literary studies

#de_fub_2001_ausilia_f - 17-24 /27-41 is the bibliography because de pdf is strangely constructed! - https://refubium.fu-berlin.de/handle/fub188/7845
#de_fub_2001_kampen_f - perfect tagging! - it is a linguistics thesis though

st_annotated <- read_csv("data/20220302_suspicious_tagging_annotated.csv")
