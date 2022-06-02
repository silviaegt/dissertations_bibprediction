library(tidyverse)
#library(pdftools)
#### Leer título y texto de todos los pdfs
# nombre de archivos para archivos que terminan con pdf en carpeta test
#pdf_files <- list.files(path = "pdfs/", pattern = ".pdf$", full.names = T)
# leer archivos
#pdf_text <- purrr::map(pdf_files, .f = pdftools::pdf_text)
#saveRDS(pdf_text, file = "pdf_text.rds")
pdf_text <- readRDS("~/Documents/alemania_test/pdf_text.rds")
# extraer título de archivo
#pdf_titles <- stringr::str_match(pdf_files, "pdfs//(.*)\\..*$")[,2]
#saveRDS(pdf_titles, file = "pdf_titles.rds")
pdf_titles <- readRDS("~/Documents/alemania_test/pdf_titles.rds")


#### Extraer features
#identifico potenciales señas de bibliografía por página (fechas, lugar de publicación, palabras típicas de la bibliografía) con stringr
#y cuento cuántas veces aparece cada una de estas señas por página (cada página es una fila)
#el resultado son listas de 10 (los 10 pdfs) y en cada uno están listas con el número de veces que aparece una característica (ver ejemplo abajo)
#ejemplo:
#thesis_dates[[1]][331] <- arroja 4, es decir, que en esa página hay cuatro fechas, 1 es el primer pdf, 331 es la página


#### Types of dates
thesis_dates <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "([1-2][0-9]{3})"), length))
thesis_dates_par <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "\\(([1-2][0-9]{3})\\)"), length))

### Tipos de encabezamiento
thesis_litverz <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Literaturverzeichnis|Quellenverzeichnis|Quellen- und Literaturverzeichnis|Literaturangaben|Zitierte Werke|[B|b]enutzte[n] Literatur"), length))
thesis_biblio <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Bibliographie|Bibliography|Bibliografia|Références bibliographiques"), length))
thesis_literatur <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Primärliteratur|Sekundärliteratur|Primary [S|s]ources|Primärquellen"), length))

### Publication places
thesis_pub_fr <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Frankfurt"), length))
thesis_pub_paris <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Paris"), length))
thesis_pub_ny <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "NY|New York"), length))
thesis_pub_mue <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "München"), length))
thesis_pub_stu <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Stuttgart"), length))
thesis_pub_tue <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Tübingen"), length))
thesis_pub_ber <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Berlin"), length))
thesis_pub_lon <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "London"), length))
thesis_pub_wien <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Wien"), length))
thesis_pub_cam <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Cambridge"), length))
thesis_pub_oxf <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Oxford"), length))
thesis_pub_ham <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Hamburg"), length))
thesis_pub_darm <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Darmstadt"), length))
thesis_pub_lei <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Leipzig"), length))
thesis_pub_hei <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Heidelberg"), length))



thesis_pubplaces <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Frankfurt|Paris|NY|New York|Stuttgart|München|Tübingen|Berlin|London|Wien|Cambridge|Oxford|Hamburg|Darmstadt|Leipzig|Göttingen|Heidelberg"), length))

### Abkürzungen
thesis_kw_hrsg <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "[H|h]rsg\\."), length))
thesis_kw_ed <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "[E|e]d\\."), length))
thesis_kw_bd <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "[B|b]d\\.|Band"), length))
thesis_kw_nr <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "[N|n]r\\."), length))
thesis_kw_ders <-purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "[D|d]ers\\."), length))

thesis_abkuerzungen <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "[D|d]ers\\.|[N|n]r\\.|[B|b]d\\.|Band|[E|e]d\\.|[H|h]rsg[\\.|v]|Verla[g|ges|gs]|Ausgab[e|en]|Gesamtausgabe|Studienausgabe"), length))

### Negative features 
thesis_ebd <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "ebda|ebenda|ebd"), length))
thesis_zb <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "z.B"), length))
thesis_vgl <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "[V|g]l\\."), length))
thesis_verzeichnis <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Inhaltsverzeichnis|Abbildungsverzeichnis|Anhang"), length))
thesis_anhang <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "Anhang"), length))

#tomo el número total de palabras por página
thesis_words <- purrr::map(pdf_text, ~ lapply(stringr::str_extract_all(string = .x, "\\b(\\w+)\\b"), length))

#creo un data frame con cada una de esas listas ennestadas
df1 <- tibble(
  pdf_name = pdf_titles,
  dates = purrr::map(thesis_dates, .f=tibble),
  dates_par = purrr::map(thesis_dates_par, .f=tibble),
  litverz = purrr::map(thesis_litverz, .f=tibble),
  biblio = purrr::map(thesis_biblio, .f=tibble),
  literatur = purrr::map(thesis_literatur, .f=tibble),
  pubplaces = purrr::map(thesis_pubplaces, .f=tibble),
  abkuerzungen = purrr::map(thesis_abkuerzungen, .f=tibble),
  pub_fr = purrr::map(thesis_pub_fr, .f=tibble),
  pub_paris = purrr::map(thesis_pub_paris, .f=tibble),
  pub_ny = purrr::map(thesis_pub_ny, .f=tibble),
  pub_mue = purrr::map(thesis_pub_mue, .f=tibble),
  pub_stu = purrr::map(thesis_pub_stu, .f=tibble),
  pub_tue = purrr::map(thesis_pub_tue, .f=tibble),
  pub_ber = purrr::map(thesis_pub_ber, .f=tibble),
  pub_lon = purrr::map(thesis_pub_lon, .f=tibble),
  pub_wien = purrr::map(thesis_pub_wien, .f=tibble),
  pub_cam = purrr::map(thesis_pub_cam, .f=tibble),
  pub_oxf = purrr::map(thesis_pub_oxf, .f=tibble),
  pub_ham = purrr::map(thesis_pub_ham, .f=tibble),
  pub_darm = purrr::map(thesis_pub_darm, .f=tibble),
  pub_lei = purrr::map(thesis_pub_lei, .f=tibble),
  pub_hei = purrr::map(thesis_pub_hei, .f=tibble),
  kw_hrsg = purrr::map(thesis_kw_hrsg, .f=tibble),
  kw_ed = purrr::map(thesis_kw_ed, .f=tibble),
  kw_bd = purrr::map(thesis_kw_bd, .f=tibble),
  kw_nr = purrr::map(thesis_kw_nr, .f=tibble),
  kw_ders = purrr::map(thesis_kw_ders, .f=tibble),
  neg_ebd = purrr::map(thesis_ebd, .f=tibble),
  neg_zb = purrr::map(thesis_zb, .f=tibble),
  neg_ver = purrr::map(thesis_verzeichnis, .f=tibble),
  neg_anh = purrr::map(thesis_anhang, .f=tibble),
  words = purrr::map(thesis_words, .f=tibble)
)

#desennesto las variables que contienen una lista con los números
df <- df1 %>% unnest(dates, dates_par, litverz, biblio, literatur, pubplaces, abkuerzungen, pub_fr, pub_paris, pub_ny, pub_mue, pub_stu, pub_tue, pub_ber, pub_lon, pub_wien, pub_cam,
                     pub_oxf, pub_ham, pub_darm, pub_lei, pub_hei, kw_hrsg, kw_ed, kw_bd, kw_nr, kw_ders, neg_ebd, neg_zb, neg_ver, neg_anh, words)

#renombro esas columnas desennestadas
names(df)<-names(df1)
#saveRDS(df, file = "data/df.rds")
names(pub_)

#desenlisto las variablas para poder hacer operaciones y cambiar de frec. absoluta a relativa, es decir,
#sacar la proporción de i.e. fechas que aparecen en relación al total de palabras
df2 <- tibble(
  pdf_name = unlist(df["pdf_name"]),
  dates = unlist(df["dates"])/unlist(df["words"]),
  dates_par = unlist(df["dates_par"])/unlist(df["words"]),
  litverz = unlist(df["litverz"])/unlist(df["words"]),
  biblio = unlist(df["biblio"])/unlist(df["words"]),
  literatur = unlist(df["literatur"])/unlist(df["words"]),
  pubplaces = unlist(df["pubplaces"])/unlist(df["words"]),
  abkuerzungen = unlist(df["abkuerzungen"])/unlist(df["words"]),
  pub_fr = unlist(df["pub_fr"])/unlist(df["words"]),
  pub_paris = unlist(df["pub_paris"])/unlist(df["words"]),
  pub_ny  = unlist(df["pub_ny"])/unlist(df["words"]),
  pub_mue  = unlist(df["pub_mue"])/unlist(df["words"]),
  pub_stu  = unlist(df["pub_stu"])/unlist(df["words"]),
  pub_tue  = unlist(df["pub_tue"])/unlist(df["words"]),
  pub_ber  = unlist(df["pub_ber"])/unlist(df["words"]),
  pub_lon  = unlist(df["pub_lon"])/unlist(df["words"]),
  pub_wien = unlist(df["pub_wien"])/unlist(df["words"]),
  pub_cam = unlist(df["pub_cam"])/unlist(df["words"]),
  pub_oxf= unlist(df["pub_oxf"])/unlist(df["words"]),
  pub_ham= unlist(df["pub_ham"])/unlist(df["words"]),
  pub_darm= unlist(df["pub_darm"])/unlist(df["words"]),
  pub_lei= unlist(df["pub_lei"])/unlist(df["words"]),
  pub_hei= unlist(df["pub_hei"])/unlist(df["words"]),
  kw_hrsg  = unlist(df["kw_hrsg"])/unlist(df["words"]),
  kw_ed  = unlist(df["kw_ed"])/unlist(df["words"]),
  kw_bd  = unlist(df["kw_bd"])/unlist(df["words"]),
  kw_nr = unlist(df["kw_nr"])/unlist(df["words"]),
  kw_ders = unlist(df["kw_ders"])/unlist(df["words"]),
  neg_ebd = unlist(df["neg_ebd"])/unlist(df["words"]),
  neg_zb= unlist(df["neg_zb"])/unlist(df["words"]),
  neg_ver= unlist(df["neg_ver"])/unlist(df["words"]),
  neg_anh= unlist(df["neg_anh"])/unlist(df["words"]),
)


#aquí hago una columna n_pag donde pongo el número de página por tesis (por eso hago el groupby)

df2 <- df2 %>% group_by(pdf_name) %>% mutate(n_pag = row_number(pdf_name))

#acá tomo el número más alto del count más alto para obtener cuántas páginas tiene el pdf
num_paginas <- df2 %>%
  group_by(pdf_name) %>%
  top_n(1, n_pag) %>% 
  select(pdf_name, n_pag)

#hago un join para que aparezca una columna con el total de páginas (n_pags) y otra con el número de página (cada fila es una página del pdf)
df3 <- left_join(df2, num_paginas, by ="pdf_name", suffix = c("", "s"))

#aquí hago el cálculo
df3["position"] <- df3["n_pag"]/df3["n_pags"]


#exporto el csv
write_csv(df3, "data/proportions_bibliography_complete_20220528.csv")


