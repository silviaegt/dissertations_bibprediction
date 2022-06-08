# Load packages
library("stats")
library("readr")
library("zoo")
library("cluster")
library("dplyr")
library("randomForest")
citation("randomForest")
# Load data
thesis <- read_csv("data/20220603_bib_sample_etiquetado.csv")

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
  
  thesis_i$dates_par_sw <- rollapply(thesis_i$dates_par, width = 5, by = 1, mean,
                                 na.rm = TRUE, fill = NA)
  
  thesis_i$litverz_sw <- rollapply(thesis_i$litverz, width = 5, by = 1, mean,
                                   na.rm = TRUE, fill = NA)
  
  thesis_i$biblio_sw <- rollapply(thesis_i$biblio, width = 5, by = 1, mean,
                                  na.rm = TRUE, fill = NA)
  
  thesis_i$pubplaces_sw <- rollapply(thesis_i$pubplaces, width = 5, by = 1, mean,
                                     na.rm = TRUE, fill = NA)
  
  thesis_i$abkuerzungen_sw <- rollapply(thesis_i$abkuerzungen, width = 5, by = 1, mean,
                                        na.rm = TRUE, fill = NA)
  thesis_i$negativ_sw <- rollapply(thesis_i$negativ, width = 5, by = 1, mean,
                                  na.rm = TRUE, fill = NA)
  
  thesis_i$position_sw <- rollapply(thesis_i$position, width = 5, by = 1, mean,
                                  na.rm = TRUE, fill = NA)
  
  thesis_i$n_pag_sw <- rollapply(thesis_i$n_pag, width = 5, by = 1, mean,
                               na.rm = TRUE, fill = NA)
  thesis_list[[i]] <- thesis_i
}

train_table <- bind_rows(thesis_list, .id = "column_label")
train_table <- train_table[,c(names(thesis_list[[1]]))]
train_table_clean <- train_table[complete.cases(train_table),]

ssize <- floor(0.632*min(table(train_table_clean$bibtest)))

rf <- randomForest(y=as.factor(train_table_clean$bibtest),
                   x=train_table_clean[,-c(1,3)],
                   ntree = 1000,
                   sampsize = c(ssize,ssize),
                   importance = TRUE)
plot(rf)
rf$confusion
importance <- as.data.frame(importance(rf, type=1))

img <- varImpPlot(rf)
img_df <- as.data.frame(img)
# Add OOB prediction
train_table_clean$bibtest_rf <- rf$predicted
table(train_table_clean$bibtest, train_table_clean$bibtest)
?randomForest

# Other accuracy measures
conf_matrix <- rf$confusion
precision <- conf_matrix[2,2]/(sum(conf_matrix[2:1,2]))
recall <- conf_matrix[2,2]/(sum(conf_matrix[2,1:2]))

precision
recall

# F1 Score = 2 * (Precision * Recall) / (Precision + Recall)
f1 <- 2 * (precision * recall) / (precision + recall)
f1

## ----  Prediction on all thesis
thesis <- df3_clean

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
  
  thesis_i$dates_par_sw <- rollapply(thesis_i$dates_par, width = 5, by = 1, mean,
                                     na.rm = TRUE, fill = NA)
  
  thesis_i$litverz_sw <- rollapply(thesis_i$litverz, width = 5, by = 1, mean,
                                   na.rm = TRUE, fill = NA)
  
  thesis_i$biblio_sw <- rollapply(thesis_i$biblio, width = 5, by = 1, mean,
                                  na.rm = TRUE, fill = NA)
  
  thesis_i$pubplaces_sw <- rollapply(thesis_i$pubplaces, width = 5, by = 1, mean,
                                     na.rm = TRUE, fill = NA)
  
  thesis_i$abkuerzungen_sw <- rollapply(thesis_i$abkuerzungen, width = 5, by = 1, mean,
                                        na.rm = TRUE, fill = NA)
  thesis_i$negativ_sw <- rollapply(thesis_i$negativ, width = 5, by = 1, mean,
                                   na.rm = TRUE, fill = NA)
  
  thesis_i$position_sw <- rollapply(thesis_i$position, width = 5, by = 1, mean,
                                    na.rm = TRUE, fill = NA)
  
  thesis_i$n_pag_sw <- rollapply(thesis_i$n_pag, width = 5, by = 1, mean,
                                 na.rm = TRUE, fill = NA)
  thesis_list[[i]] <- thesis_i
}

full_table <- bind_rows(thesis_list, .id = "column_label")
full_table <- full_table[,c(names(thesis_list[[1]]))]
full_table_clean <- full_table[complete.cases(full_table),]
prediction <- predict(rf, full_table_clean)
full_table_clean$biblio_class <- prediction

write_csv(full_table_clean, "data/20220603_proportions_bibpred_RF3.csv")
