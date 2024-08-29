#library
#install.packages("tidyverse")
library(tidyverse)
library(haven)

# Define paths
file_name <- paste(int_dir, "SPConflict_Village.v7.6.dta", sep = "")

# Load village data 
village_data <- haven::read_dta(file_name)
int_dir <- "C:/Users/Owner/Desktop/Premand_2024_2_new/replication/datasets/intermediate/"

# Summarize dataset
summary(village_data)
village_data <- village_data %>%
  arrange(communeid)

# Generate new variables
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

# Select subsample of the village data
village_data2 <- haven::read_dta(file_name)
village_data2 <- village_data2 %>%
  filter(year==2021) %>%
  dplyr::select(codelocalite) 
  
# Load conflict_4daily
file_name <- paste(int_dir, "ConflictDataNigerSP_daily_Aug23_2021.dta", sep = "")
conflict_daily <- haven::read_dta(file_name)

# Load conflict_entries
file_name <- paste(int_dir,"ConflictDataNigerSP_entries_July8_2021.dta" ,sep = "" )
conflict_entries <- haven::read_dta(file_name)
conflict_entries <- conflict_entries%>%
  rename(ufi = actiongeo_featureid)

conflict_entries <- conflict_entries%>%
  mutate(
    `___daily___` = NA
  )

# Merge conflict_entries and conflict_daily (inner join, because stata file drops unmatched obs after left join)
conflict_entries <- inner_join(conflict_entries, conflict_daily, by = c("ufi", "sqldate", "monthyear", "year", "fractiondate"))

conflict_entries <- conflict_entries %>%
  select(globaleventid:`___daily___`,`___Renaloc_ALL___`:treatedcommune_namesonly) %>%
  select(-`___daily___`)

conflict_entries <- inner_join(conflict_entries, village_data2, by = "codelocalite")

#define "foreign terror "
text <- 'gen foreign_terror = 0 replace foreign_terror=1 if strpos(sourceurl, "boko")>=1 | strpos(sourceurl, "haram")>=1 | 
  strpos(sourceurl, "terror")>=1 | strpos(sourceurl, "qaeda")>=1 | 
  strpos(sourceurl, "jihad")>=1 | strpos(sourceurl, "qaida")>=1 |
  strpos(sourceurl, "islamic-state")>=1  | strpos(sourceurl, "isgs")>=1 | 
  strpos(sourceurl, "gsim")>=1 | strpos(sourceurl, "isis")>=1 | 
  strpos(sourceurl, "mujao")>=1 | strpos(sourceurl, "extremist")>=1 | 
  strpos(sourceurl, "suicide-bomb")>=1 | strpos(sourceurl, "militant")>=1 | 
  strpos(sourceurl, "kidnap")>=1 | strpos(sourceurl, "islamist")>=1 | 
  strpos(sourceurl, "abduct")>=1 | strpos(sourceurl, "sharia")>=1 | 
  strpos(sourceurl, "suicide-attack")>=1 | strpos(sourceurl, "daesh")>=1 | 
  strpos(sourceurl, "raid")>=1 | strpos(sourceurl, "training-camp")>=1 | 
  strpos(sourceurl, "hijack")>=1 | strpos(sourceurl, "radical")>=1 | 
  strpos(sourceurl, "execut")>=1 | strpos(sourceurl, "insurg")>=1 | 
  strpos(sourceurl, "jnim")>=1 | strpos(sourceurl, "jamaat")>=1 | 
  strpos(sourceurl, "iswa")>=1 | strpos(sourceurl, "mourabitoun")>=1 | 
  strpos(sourceurl, "ansar-dine")>=1 | strpos(sourceurl, "explos")>=1 | 
  strpos(sourceurl, "massacr")>=1'

matches <- unlist(regmatches(text, gregexpr('"[^"]*"', text)))
matches_clean <- gsub('(^"|"$)','', matches)
print(matches_clean)

conflict_entries <- conflict_entries%>%
  mutate(foreign_terror = NA)

conflict_entries$foreign_terror <- apply(conflict_entries, 1, function(row){
  if (any(sapply(matches_clean, function(keyword) grepl(keyword, row["sourceurl"])))){return(1)}
  else{return(0)}
})

text2 <- '	replace foreign_terror=. if strpos(sourceurl, "http://af.reuters.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://africacenter.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://allafrica.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://axisoflogic.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://china.org.cn")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://en.alalam.ir")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://en.ce.cn")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://en.farsnews.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://en.shafaqna.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://english.sina.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://eurasia.ro")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://freerepublic.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://gbcghana.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://gulftoday.ae")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://hamptonroads.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://mobile.reuters.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://moderntokyotimes.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://moonreports.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://murnosti.blogspot.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://news.sudanvisiondaily.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://news.trust.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://news.webindia123.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://news.xinhuanet.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://one.trust.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://osundefender.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://schema-root.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://smartraveller.gov.au")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://somaliamediamonitoring.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://sputniknews.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://timesofoman.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.africaleader.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.africanelections.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.afronline.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.almanar.com.lb")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.arabnews.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.azerbaijannews.net")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.bbc.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.bernama.com.my")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.bna.bh")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.bssnews.net")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.catholicculture.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.centralctcommunications.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.cfr.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.china.org.cn")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.chinadaily.com.cn")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.copts-united.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.crisisgroup.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.ekklesia.co.uk")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.ennaharonline.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.gbcghana.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.gistmania.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.globalsecurity.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.globaltimes.cn")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.google.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.heraldtribune.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.informationclearinghouse.info")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.infozine.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.irishsun.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.isn.ethz.ch")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.jamestown.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.kforcegov.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.kfqd.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.ktbb.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.kuna.net.kw")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.laht.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.legalbrief.co.za")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.londonmercury.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.macon.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.middle-east-online.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.nerc.ac.uk")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.netindia123.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.netnebraska.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.newkerala.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.newsonair.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.newstimeafrica.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.nigeriasun.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.omantribune.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.philippinetimes.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.refworld.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.sanantoniopost.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.saudigazette.com.sa")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.scnow.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.shanghaidaily.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.spa.gov.sa")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.starrfmonline.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.strategypage.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.taipeitimes.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.tolerance.ca")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.trust.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.turkishpress.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.vision.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.wgme.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.wwmt.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "http://www.xinhuanet.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://af.reuters.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://allafrica.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://isp.netscape.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://news.trust.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://news.un.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://sierraexpressmedia.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://strategypage.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://today.rtl.lu")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://www.bbc.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://www.charlotteobserver.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://www.globalsecurity.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://www.haaretz.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://www.indcatholicnews.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://www.jagonews24.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://www.justsecurity.org")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://www.kuna.net.kw")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://www.msf.org.uk")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://www.osac.gov")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://www.strategypage.com")>=1
	replace foreign_terror=. if strpos(sourceurl, "https://www.themalaysianinsight.com")>=1'

matches2 <- unlist(regmatches(text2, gregexpr('"[^"]*"', text2)))
matches_clean2 <- gsub('(^"|"$)','', matches2)
print(matches_clean2)

conflict_entries$foreign_terror <- apply(conflict_entries, 1, function(row){
  if (any(sapply(matches_clean2, function(keyword) grepl(keyword, row["sourceurl"])))){return(NA)}
  else{return(row["foreign_terror"])}
})


max(conflict_entries$foreign_terror)

conflict_entries <- conflict_entries%>%
  mutate(
    foreign_terror = case_when(
      sourceurl == "" ~ NA,
      str_detect(sourceurl,"^http") == FALSE ~ NA,
      TRUE ~ foreign_terror
    )  
  )

conflict_grouped <- conflict_entries %>%
  group_by(codelocalite, year) %>%
  summarize(foreign_terror_max = if(all(is.na(foreign_terror))) NA else max(foreign_terror, na.rm=TRUE))
  #summarize(foreign_terror_max = max(foreign_terror, na.rm=TRUE))

conflict_grouped <- conflict_grouped%>%
  select(codelocalite, year, foreign_terror_max)