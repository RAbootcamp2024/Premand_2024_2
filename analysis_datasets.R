#library
#install.packages("tidyverse")
library(tidyverse)
library(haven)

#passを定義
int_dir <- "C:/Users/Owner/Desktop/Premand_2024_2/replication/datasets/intermediate/"
file_name <- paste(int_dir, "SPConflict_Village.v7.6.dta", sep = "")
village_data <- haven::read_dta(file_name)

# Summarize dataset
summary(village_data)
village_data <- village_data %>%
  arrange(communeid)

# 変数を作成
village_data <- village_data %>%
  mutate(
    hadconflict6 = ifelse(nbfeatures18_10k>0 & !is.na(nbfeatures18_10k), 1, 0),
    hadconflict3_250 = ifelse(nbfeatures_10k>0 & !is.na(nbfeatures_10k) & excludesmallvillage_250==0, 1, 0),
    hadconflict5_250 = ifelse(nbfeatures18_5k>0 & !is.na(nbfeatures18_5k) & excludesmallvillage_250==0, 1, 0),
    hadconflict6_250 = ifelse(nbfeatures18_10k>0 & !is.na(nbfeatures18_10k) & excludesmallvillage_250==0, 1, 0),
    hadconflict7_250 = ifelse(nbfeatures18_15k>0 & !is.na(nbfeatures18_15k) & excludesmallvillage_250==0, 1 ,0),
    #radius variable for names_only
    hagdconflict6_namesonly = ifelse(nbfeatures18_10k>0 & !is.na(nbfeatures18_10k) & excludesmallvillage_namesonly==0, 1, 0), 
    Foreign4_18_250 = ifelse(nbfeat_foreign_4_250>0 & nbfeatures18_250==1 & !is.na(nbfeat_foreign18_4_10k), 1, 0),
    NonForeign4_18_250 = ifelse(nbfeatures18_250==0,0,NA),
    NonForeign4_18_250 = ifelse(Foreign4_18_250==0 & nbfeatures18_250==1,1,NonForeign4_18_250),
    NonForeign4_18_250 = ifelse(Foreign4_18_250==1 & nbfeatures18_250==1,0,NonForeign4_18_250)
    )

village_data2 <- haven::read_dta(file_name)
village_data2 <- village_data2%>%
  dplyr::select(codelocalite)
file_name <- paste(int_dir, "ConflictDataNigerSP_daily_Aug23_2021.dta", sep = "")
conflict_daily <- haven::read_dta(file_name)


