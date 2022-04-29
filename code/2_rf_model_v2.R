# Load packages
library("stats")
library("readr")
library("zoo")
library("cluster")
library("dplyr")
library("randomForest")

# Load data
thesis <- read_csv("data/20220321_bib_sample_etiquetado.csv")

# Selected variables:
# dates (proportion of dates vs total words on page)
# litverz
# biblio (proportion of the word "bibliography" vs total words)
# pubplaces (publication places)
# abkuerzungen (abbreviations)
# position
names(thesis)
# All thesis' names
uniq_t <- unique(thesis$pdf_name)

thesis_list <- list()
for (i in 1:length(uniq_t)){
  
  print(i)
  thesis_i <- thesis[thesis$pdf_name == uniq_t[i],] 
  
  thesis_i$dates_sw <- rollapply(thesis_i$dates, width = 5, by = 1, mean,
                                 na.rm = TRUE, fill = NA)
  
  thesis_i$litverz_sw <- rollapply(thesis_i$litverz, width = 5, by = 1, mean,
                                na.rm = TRUE, fill = NA)
  
  thesis_i$biblio_sw <- rollapply(thesis_i$biblio, width = 5, by = 1, mean,
                                na.rm = TRUE, fill = NA)
  
  thesis_i$pubplaces_sw <- rollapply(thesis_i$pubplaces, width = 5, by = 1, mean,
                                  na.rm = TRUE, fill = NA)
  
  thesis_i$abkuerzungen_sw <- rollapply(thesis_i$abkuerzungen, width = 5, by = 1, mean,
                                  na.rm = TRUE, fill = NA)
  thesis_i$kw_anhang <- rollapply(thesis_i$kw_anhang, width = 5, by = 1, mean,
                                  na.rm = TRUE, fill = NA)
  
  thesis_i$kw_inhalt <- rollapply(thesis_i$kw_inhalt, width = 5, by = 1, mean,
                                     na.rm = TRUE, fill = NA)
  
  thesis_i$kw_vgl <- rollapply(thesis_i$kw_vgl, width = 5, by = 1, mean,
                                        na.rm = TRUE, fill = NA)
  thesis_list[[i]] <- thesis_i
}

train_table <- bind_rows(thesis_list, .id = "column_label")
train_table <- train_table[,c("pdf_name","bibtest","dates","litverz","biblio","sekundaerlit","waerke","pubplaces",
                              "abkuerzungen","pub_fr","pub_paris","pub_ny","pub_mue","pub_stu","pub_tue","pub_ber","kw_hrsg",        
                              "kw_ed","kw_bd","kw_nr","kw_ders","n_pag", "position","dates_sw","litverz_sw","biblio_sw","pubplaces_sw","abkuerzungen_sw", "kw_anhang", "kw_inhalt", "kw_vgl")]

train_table_clean <- train_table[complete.cases(train_table),]

ssize <- floor(0.632*min(table(train_table_clean$bibtest)))

rf <- randomForest(y=as.factor(train_table_clean$bibtest),
                   x=train_table_clean[,3:ncol(train_table_clean)],
                   ntree = 1000,
                   sampsize = c(ssize,ssize),
                   importance = TRUE)
plot(rf)
rf$confusion
importance <- as.data.frame(importance(rf, type=1))

varImpPlot(rf)

# Add OOB prediction
train_table_clean$bibtest_rf <- rf$predicted
table(train_table_clean$bibtest, train_table_clean$bibtest)
?randomForest
# Prediction on all thesis
thesis <- read_csv("data/proportions_bibliography_complete_20220321.csv")

# Selected variables:
# dates (proportion of dates vs total words on page)
# litverz (proportion of the word "Literaturverzeichnis" vs total words)
# biblio (proportion of the word "Bibliographie" vs total words)
# pubplaces (publication places)
# abkuerzungen (abbreviations)
# position

# All thesis
uniq_t <- unique(thesis$pdf_name)

thesis_list <- list()
for (i in 1:length(uniq_t)){
  
  print(i)
  thesis_i <- thesis[thesis$pdf_name == uniq_t[i],] 
  
  thesis_i$dates_sw <- rollapply(thesis_i$dates, width = 5, by = 1, mean,
                                 na.rm = TRUE, fill = NA)
  
  thesis_i$litverz_sw <- rollapply(thesis_i$litverz, width = 5, by = 1, mean,
                                   na.rm = TRUE, fill = NA)
  
  thesis_i$biblio_sw <- rollapply(thesis_i$biblio, width = 5, by = 1, mean,
                                  na.rm = TRUE, fill = NA)
  
  thesis_i$pubplaces_sw <- rollapply(thesis_i$pubplaces, width = 5, by = 1, mean,
                                     na.rm = TRUE, fill = NA)
  
  thesis_i$abkuerzungen_sw <- rollapply(thesis_i$abkuerzungen, width = 5, by = 1, mean,
                                        na.rm = TRUE, fill = NA)
  thesis_list[[i]] <- thesis_i
}

full_table <- bind_rows(thesis_list, .id = "column_label")

full_table <- full_table[,c("pdf_name", "dates","litverz","biblio","sekundaerlit","waerke","pubplaces",
                            "abkuerzungen","pub_fr","pub_paris","pub_ny","pub_mue","pub_stu","pub_tue","pub_ber","kw_hrsg",        
                            "kw_ed","kw_bd","kw_nr","kw_ders","n_pag", "position","dates_sw","litverz_sw","biblio_sw","pubplaces_sw","abkuerzungen_sw", "kw_anhang", "kw_inhalt", "kw_vgl")]

full_table_clean <- full_table[complete.cases(full_table),]
full_table_clean
prediction <- predict(rf, full_table_clean)
#Error in predict.randomForest(rf, full_table_clean) : variables in the training data missing in newdata
full_table_clean$biblio_class <- prediction

write_csv(full_table_clean, "data/20220426_proportions_bibpred_RF2.csv")
