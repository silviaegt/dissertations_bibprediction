# Load packages
library("stats")
library("readr")
library("zoo")
library("factoextra")
library("cluster")

# Load data
thesis <- read_csv("proportions_bibliography_clean_2022b.csv")

# Selected variables:

# dates (proportion of dates vs total words on page)
# litverz
# biblio (proportion of the word "bibliography" vs total words)
# pubplaces (publication places)
# abkuerzungen (abbreviations)
# position

# All thesis
uniq_t <- unique(thesis$pdf_name)
clust_table <- data.frame(matrix(0,
                                 length(uniq_t), 13))

names(clust_table) <- c("pdf_name",
                   "dates_sw", "dates_p",
                   "litverz_sw", "litverz_p",
                   "biblio_sw", "biblio_p",
                   "pubplaces_sw", "pubplaces_p",
                   "abkuerzungen_sw","abkuerzungen_p",
                   "p_na",
                   "issues")

for (i in 1:length(uniq_t)){
  clust_table$pdf_name[i] <- uniq_t[i]
  
  print(i)
  thesis_i <- thesis[thesis$pdf_name == uniq_t[i],] 
  
  prop_na <- sum(is.na(thesis_i))/(nrow(thesis_i)*ncol(thesis_i))
  prop_na
  clust_table$p_na[i] <- prop_na
  if (prop_na < 0.8){
  # No issues then 1
  clust_table$issues[i] <- 1
    
  # Dates
  dates_sw <- rollapply(thesis_i$dates, width = 5, by = 1, mean,
                        na.rm = TRUE, fill = NA)
  dates_sw_max <- max(dates_sw, na.rm = TRUE)
  clust_table$dates_sw[i] <- dates_sw_max
  if (dates_sw_max>0){
    idx_max <- which(dates_sw == dates_sw_max)
    clust_table$dates_p[i] <- thesis_i$position[idx_max[length(idx_max)]]
  }
  
  # Litverz
  litverz_sw <- rollapply(thesis_i$litverz, width = 5, by = 1, mean,
                        na.rm = TRUE, fill = NA)
  litverz_sw_max <- max(litverz_sw, na.rm = TRUE)
  clust_table$litverz_sw[i] <- litverz_sw_max
  if (litverz_sw_max>0){
    idx_max <- which(litverz_sw == litverz_sw_max)
    clust_table$litverz_p[i] <- thesis_i$position[idx_max[length(idx_max)]]
  }
  
  # Biblio
  biblio_sw <- rollapply(thesis_i$biblio, width = 5, by = 1, mean,
                        na.rm = TRUE, fill = NA)
  biblio_sw_max <- max(biblio_sw, na.rm = TRUE)
  clust_table$biblio_sw[i] <- biblio_sw_max
  if (biblio_sw_max>0){
  idx_max <- which(biblio_sw == biblio_sw_max)
  clust_table$biblio_p[i] <- thesis_i$position[idx_max[length(idx_max)]]
  }

  # Pubplaces
  pubplaces_sw <- rollapply(thesis_i$pubplaces, width = 5, by = 1, mean,
                         na.rm = TRUE, fill = NA)
  pubplaces_sw_max <- max(pubplaces_sw, na.rm = TRUE)
  clust_table$pubplaces_sw[i] <- pubplaces_sw_max
  if (pubplaces_sw_max>0){
    idx_max <- which(pubplaces_sw == pubplaces_sw_max)
    clust_table$pubplaces_p[i] <- thesis_i$position[idx_max[length(idx_max)]]
  }
  
  # Abkuerzungen
  abkuerzungen_sw <- rollapply(thesis_i$abkuerzungen, width = 5, by = 1, mean,
                            na.rm = TRUE, fill = NA)
  abkuerzungen_sw_max <- max(abkuerzungen_sw, na.rm = TRUE)
  clust_table$abkuerzungen_sw[i] <- abkuerzungen_sw_max
  if (abkuerzungen_sw_max>0){
    idx_max <- which(abkuerzungen_sw == abkuerzungen_sw_max)
    clust_table$abkuerzungen_p[i] <- thesis_i$position[idx_max[length(idx_max)]]
  }

  }
}

# Save to disk
write_csv2(clust_table, "thesis_clustering_data.csv")

# Clean Nas
clust_table_clean <- clust_table[clust_table$issues==1,]

### K-means clustering
names(clust_table_clean)
scaled <- scale(clust_table_clean[,c(3,5,7,9,11)])

set.seed(777)

## Select k
# Number of Clusters vs. the Total Within Sum of Squares

fviz_nbclust(scaled, kmeans, method = "wss")

# Silo
fviz_nbclust(scaled, kmeans, method = "silhouette", k.max=15)
?fviz_nbclust
# Gap stat
#calculate gap statistic based on number of clusters
gap_stat <- clusGap(scaled,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 15,
                    B = 50,
                    iter.max = 30)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat, 
              maxSE = list(method = "Tibs2001SEmax"))

### Final clustering
km <- kmeans(scaled, centers = 11, nstart = 30)

#view results
km$size

# Assign clusters to data
clust_table_clean$k <- km$cluster
sample_df <- list()
ks <- unique(clust_table_clean$k)
for (k in 1:length(ks)){
  k_luster <- clust_table_clean[clust_table_clean$k == k,]
  idx <- 1:nrow(k_luster)
  samp <- sample(idx, 5)
  k_luster_sampled <- k_luster[samp,]
  sample_df[[k]] <- k_luster_sampled
}

out_df <- do.call(rbind, sample_df)

write_csv2(out_df, "thesis_sample_v2.csv")

View(out_df)
