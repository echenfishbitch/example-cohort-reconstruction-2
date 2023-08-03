###############################################
############## CWT Ocean Fishery #############
###############################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

setwd("~/example-cohort-reconstruction-2-main")

#############################################
##########     Release Data     #############
#############################################
CWT_Releases<-read.csv("releases.csv")
#turning NAs into 0 for addition
CWT_Releases$cwt_1st_mark_count[is.na(CWT_Releases$cwt_1st_mark_count)] <- 0 
CWT_Releases$cwt_2nd_mark_count[is.na(CWT_Releases$cwt_2nd_mark_count)] <- 0
CWT_Releases$non_cwt_1st_mark_count[is.na(CWT_Releases$non_cwt_1st_mark_count)] <- 0
CWT_Releases$non_cwt_2nd_mark_count[is.na(CWT_Releases$non_cwt_2nd_mark_count)] <- 0

#Phi is the proportion of released fish that have CWT and Ad Clips
#Fish with CWT and Ad Clip: cwt_1st_mark_count
#Fish with CWT and No clip: cwt_2nd_mark_count
#Fish with no CWT and Ad Clip: non_cwt_1st_mark_count
#Fish with no CWT and No Clip: non_cwt_2nd_mark_count
CWT_Releases$Total_Released<-(CWT_Releases$cwt_1st_mark_count+ CWT_Releases$cwt_2nd_mark_count+ CWT_Releases$non_cwt_1st_mark_count+ CWT_Releases$non_cwt_2nd_mark_count)
CWT_Releases$Phi <- CWT_Releases$cwt_1st_mark_count/CWT_Releases$Total_Released
names(CWT_Releases)[7]<-"tag_code"
BY<-CWT_Releases %>% #BY for each batch
  select(tag_code, brood_year, Phi)
#Getting Fry released
CWT_Releases<- CWT_Releases %>%
  group_by(brood_year) %>%
  summarise(Individuals_Released = sum(Total_Released))
#############################################
############## Ocean Harvest ################
#############################################
CWT_Recoveries<-read.csv("recoveries.csv")
CWT_Recoveries<-left_join(CWT_Recoveries, BY, by="tag_code")
CWT_Recoveries<-CWT_Recoveries %>% #adding Month of record and Age
  mutate(Month =month(ymd(CWT_Recoveries$recovery_date))) %>%
  mutate(Age = run_year - brood_year)
#sampling site -> region
SiteCodes<-read.csv("sitearea.modified.csv")
SiteCodes<-SiteCodes %>%
  select(sampsite,area.1)
colnames(SiteCodes)<-c("sampling_site", "Location")
#Assigning region for each recovery location name. Region determines whether the minimum size limit is 20 in or 24 in or 26 in.
#Regulations are set by region. North of Pt. Arena (FB, KO, KC). San Francisco (SF). Monterrey (MO).

#Commercial Fisheries
CWT_Recoveries.Com<-CWT_Recoveries %>%
  filter(fishery==10) %>%
  left_join(SiteCodes)

#Recreation Fisheries
CWT_Recoveries.Rec<-CWT_Recoveries %>%
  filter(fishery==40)%>%
  left_join(SiteCodes)
Size_Limits<-read.csv("Size_limits.csv")        
#appending size limit for every recovery by year*fishery&location*month 
CWT_Recoveries.Com<-left_join(CWT_Recoveries.Com, Size_Limits, by = c("run_year", "fishery", "Location", "Month"))
CWT_Recoveries.Rec<-left_join(CWT_Recoveries.Rec, Size_Limits, by = c("run_year", "fishery", "Location", "Month"))
#appending release mortality rate for every recovery by year*fishery&location*month 
Release_mort<-read.csv("release.mort.rate.csv")
CWT_Recoveries.Com<-left_join(CWT_Recoveries.Com, Release_mort, by = c("run_year", "fishery", "Location", "Month"))
CWT_Recoveries.Rec<-left_join(CWT_Recoveries.Rec, Release_mort, by = c("run_year", "fishery", "Location", "Month"))
CWT_Recoveries.Rec$Release.mort.rate[is.na(CWT_Recoveries.Rec$Release.mort.rate)]<-.14 #WA recovery has no rate. Using .14, the standard rate
#appending percent harvetable based on Month, Age, and Size Limit
SizeAge<-read.csv("length.at.age.csv")
#Function: Present Harvest requires Location (e.g. FB, SF, MO), Month, Brood Year, Run Year, and 
#produces the percentage of fish in that class that can be taken by the fishery. 

Percent_Harvest<-function(Month, Age, Size_Limit){
    1-pnorm(Size_Limit, mean = SizeAge$mean[which(SizeAge$age == Age+1 & SizeAge$month == Month)], sd = SizeAge$sd[which(SizeAge$age == Age+1 & SizeAge$month == Month)])
}

#Calculating p,C,H,S,D,I for Commerical
CWT_Recoveries.Com$Percent_Harvestable<-as.numeric(as.character(mapply(Percent_Harvest, CWT_Recoveries.Com$Month, CWT_Recoveries.Com$Age, CWT_Recoveries.Com$limit)))
CWT_Recoveries.Com<-CWT_Recoveries.Com %>%
  mutate(Harvest = estimated_number/Phi) %>%
  mutate(Catch=Harvest/Percent_Harvestable) #Catch is C=H/p
CWT_Recoveries.Com<-CWT_Recoveries.Com %>% 
  mutate(Release_Mort = (Catch-Harvest)*Release.mort.rate) %>% #Percent of those caught but not harvested that die after release (Standard 26% catch and release mortality-commerical fishery)
  mutate(Drop_Mort = (Catch)*.05) #Drop off mortality 5% of Catch
CWT_Recoveries.Com<-CWT_Recoveries.Com %>% 
  mutate(Impact = Harvest+Release_Mort+Drop_Mort) # I = H+S+D

Commercial_Harvest<-CWT_Recoveries.Com %>%
  group_by(brood_year, run_year, Month) %>%
  summarise(Tags_Collected = n(),Catch =sum (Catch), Harvested=sum(Harvest), Release_Mort=sum(Release_Mort), Drop_Mort=sum(Drop_Mort), Impact=sum(Impact))
# write.csv(Commercial_Harvest, "Commercial_Harvest_Impact.csv", row.names = FALSE)

#Calculating p,C,S for Recreational
CWT_Recoveries.Rec$Percent_Harvestable<-as.numeric(as.character(mapply(Percent_Harvest, CWT_Recoveries.Rec$Month, CWT_Recoveries.Rec$Age, CWT_Recoveries.Rec$limit)))
CWT_Recoveries.Rec<-CWT_Recoveries.Rec %>%
  mutate(Harvest = estimated_number/Phi) %>%
  mutate(Catch=Harvest/Percent_Harvestable) #Catch is C=H/p
CWT_Recoveries.Rec<-CWT_Recoveries.Rec %>% 
  mutate(Release_Mort = (Catch-Harvest)*Release.mort.rate)%>% #Percent of those caught but not harvested that die after release (Standard 14% catch and release mortality-Recreational fishery)
  mutate(Drop_Mort = (Catch)*.05) #Drop off mortality 5% of Catch
CWT_Recoveries.Rec<-CWT_Recoveries.Rec %>% 
  mutate(Impact = Harvest+Release_Mort+Drop_Mort) # I = H+S+D
Recreational_Harvest<-CWT_Recoveries.Rec %>%
  group_by(brood_year, run_year, Month) %>%
  summarise(Tags_Collected = n(),Catch =sum (Catch),Harvested=sum(Harvest), Release_Mort=sum(Release_Mort), Drop_Mort=sum(Drop_Mort), Impact=sum(Impact))
# write.csv(Recreational_Harvest, "Recreational_Harvest_Impact.csv", row.names = FALSE)

#Combining recreation and commercial data
Impact<-full_join(Recreational_Harvest, Commercial_Harvest, by=c("brood_year", "run_year","Month"))
Impact$Impact<-rowSums(Impact[,c(9,15)], na.rm = TRUE) #adding together impact of Commercial and Recreation Harvest
Impact$Catch<-rowSums(Impact[,c(5,11)], na.rm = TRUE) #adding together catch of Commercial and Recreation Harvest
Impact$Age<-Impact$run_year-Impact$brood_year

#Summarizing impact data by month for each brood year
Impact<- Impact%>%
  pivot_wider(names_from = c(Age,Month), values_from = Impact, names_sort=TRUE)%>%
  group_by(brood_year) %>%
  summarize(Jun2= sum(`2_6`, na.rm = TRUE), Jul2 = sum(`2_7`, na.rm = TRUE), Aug2 = sum(`2_8`, na.rm = TRUE), Sept2 = sum(`2_9`, na.rm = TRUE),
            Mar3 = sum(`3_3`, na.rm = TRUE), Apr3 = sum(`3_4`, na.rm = TRUE), May3 = sum(`3_5`, na.rm = TRUE), Jun3= sum(`3_6`, na.rm = TRUE) , Jul3 = sum(`3_7`, na.rm = TRUE), 
            Aug3 = sum(`3_8`, na.rm = TRUE), Sept3 = sum(`3_9`, na.rm = TRUE), Oct3 = sum(`3_10`, na.rm = TRUE),
            May4 = sum(`4_5`, na.rm = TRUE),Jul4 = sum(`4_7`, na.rm = TRUE),Sept4 = sum(`4_9`, na.rm = TRUE))

# write.csv(Impact, "Fishing_Impact.csv", row.names = FALSE)


###Bootstrapping. 
niterations = 1000
#Recreational
CWT_Recoveries.Rec$Harvested_Sample<-NA
CWT_Recoveries.Rec$Catch_Sample<-NA
CWT_Recoveries.Rec$Drop_Sample<-NA
CWT_Recoveries.Rec$Release_Sample<-NA
CWT_Recoveries.Rec$Impact_Sample<-NA

CWT_Recoveries.Rec.list<-list()
Recreational_Harvest.list<-list()
Recreational_Catch.rep<-list()

for(j in 1:niterations){
  CWT_Recoveries.Rec.list[[j]]<-CWT_Recoveries.Rec
  for(i in 1:length(CWT_Recoveries.Rec$estimated_number)){
    #resampling the number of harvested individuals per tag
    #Harvested_Sample = (1+resampled unrecovered tags)/Phi
    CWT_Recoveries.Rec.list[[j]]$Harvested_Sample[i]<-sum(c(1,rnbinom(1, 1,1/(CWT_Recoveries.Rec$estimated_number[i]))))/CWT_Recoveries.Rec$Phi[i]
    CWT_Recoveries.Rec.list[[j]]$Catch_Sample[i]<-CWT_Recoveries.Rec.list[[j]]$Harvested_Sample[i]/CWT_Recoveries.Rec.list[[j]]$Percent_Harvestable[i]
    CWT_Recoveries.Rec.list[[j]]$Drop_Sample[i]<-CWT_Recoveries.Rec.list[[j]]$Catch_Sample[i]*.05
    CWT_Recoveries.Rec.list[[j]]$Release_Sample[i]<-(CWT_Recoveries.Rec.list[[j]]$Catch_Sample[i]-CWT_Recoveries.Rec.list[[j]]$Harvested_Sample[i])*CWT_Recoveries.Rec.list[[j]]$Release.mort.rate[i]
    CWT_Recoveries.Rec.list[[j]]$Impact_Sample[i]<-CWT_Recoveries.Rec.list[[j]]$Drop_Sample[i]+CWT_Recoveries.Rec.list[[j]]$Harvested_Sample[i]+CWT_Recoveries.Rec.list[[j]]$Release_Sample[i]
       }
  Recreational_Harvest.list[[j]]<-CWT_Recoveries.Rec.list[[j]]%>%
    group_by(brood_year, run_year, Month) %>%
    summarise(Tags_Collected = n(), Catch_Sample =sum (Catch_Sample), Harvested_Sample=sum(Harvested_Sample), Release_Sample=sum(Release_Sample), Drop_Sample=sum(Drop_Sample), Impact_Sample=sum(Impact_Sample)) %>%
    mutate(Age = run_year-brood_year)
}
test<-CWT_Recoveries.Rec.list[[1]]

# saveRDS(Recreational_Harvest.list, file = "Recreational_Harvest.list.Rds")
# saveRDS(CWT_Recoveries.Rec.list, file = "CWT_Recoveries.Rec.list.Rds")
#Commercial
CWT_Recoveries.Com$Harvested_Sample<-NA
CWT_Recoveries.Com$Catch_Sample<-NA
CWT_Recoveries.Com$Drop_Sample<-NA
CWT_Recoveries.Com$Release_Sample<-NA
CWT_Recoveries.Com$Impact_Sample<-NA


CWT_Recoveries.Com.list<-list()
Commercial_Harvest.list<-list()
Commercial_Catch.rep<-list()
Harvest.list<-list()
Commercial_Harvest.list<-list()
Catch.rep.list<-list()
for(j in 1:niterations){
  CWT_Recoveries.Com.list[[j]]<-CWT_Recoveries.Com
  for(i in 1:length(CWT_Recoveries.Com$estimated_number)){
    CWT_Recoveries.Com.list[[j]]$Harvested_Sample[i]<-sum(c(1,rnbinom(1, 1,1/(CWT_Recoveries.Com$estimated_number[i]))))/CWT_Recoveries.Com$Phi[i]
    CWT_Recoveries.Com.list[[j]]$Catch_Sample[i]<-CWT_Recoveries.Com.list[[j]]$Harvested_Sample[i]/CWT_Recoveries.Com.list[[j]]$Percent_Harvestable[i]
    CWT_Recoveries.Com.list[[j]]$Drop_Sample[i]<-CWT_Recoveries.Com.list[[j]]$Catch_Sample[i]*.05
    CWT_Recoveries.Com.list[[j]]$Release_Sample[i]<-(CWT_Recoveries.Com.list[[j]]$Catch_Sample[i]-CWT_Recoveries.Com.list[[j]]$Harvested_Sample[i])*CWT_Recoveries.Com.list[[j]]$Release.mort.rate[i]
    CWT_Recoveries.Com.list[[j]]$Impact_Sample[i]<-CWT_Recoveries.Com.list[[j]]$Drop_Sample[i]+CWT_Recoveries.Com.list[[j]]$Harvested_Sample[i]+CWT_Recoveries.Com.list[[j]]$Release_Sample[i]
  }
    Commercial_Harvest.list[[j]]<-CWT_Recoveries.Com.list[[j]] %>%
    group_by(brood_year, run_year, Month) %>%
    summarise(Tags_Collected = n(),Catch_Sample =sum (Catch_Sample), Harvested_Sample=sum(Harvested_Sample), Release_Sample=sum(Release_Sample), Drop_Sample=sum(Drop_Sample), Impact_Sample=sum(Impact_Sample))%>%
    mutate(Age = run_year-brood_year)
}

# saveRDS(Commercial_Harvest.list, file = "Commercial_Harvest.list.Rds")

Impact_Bootstrap.list<-list() 
#Merging Commercial and Recreation Impact bootstrapping
for (i in 1:niterations){
  Harvest.list[[i]]<-full_join(Recreational_Harvest.list[[i]],Commercial_Harvest.list[[i]] , by=c("brood_year", "run_year","Month", "Age"))
  Harvest.list[[i]]$Impact<-rowSums(Harvest.list[[i]][,c(9,16)], na.rm = TRUE)
  Impact_temp<-Harvest.list[[i]] %>%
    pivot_wider(names_from = c(Age,Month),values_from= Impact, names_sort = TRUE )
  Impact_temp2<- Impact_temp %>%
    group_by(brood_year) %>%
    summarize(Jun2= sum(`2_6`, na.rm = TRUE), Jul2 = sum(`2_7`, na.rm = TRUE), Aug2 = sum(`2_8`, na.rm = TRUE), Sept2 = sum(`2_9`, na.rm = TRUE),
              Mar3 = sum(`3_3`, na.rm = TRUE), Apr3 = sum(`3_4`, na.rm = TRUE), May3 = sum(`3_5`, na.rm = TRUE), Jun3= sum(`3_6`, na.rm = TRUE) , Jul3 = sum(`3_7`, na.rm = TRUE), 
              Aug3 = sum(`3_8`, na.rm = TRUE), Sept3 = sum(`3_9`, na.rm = TRUE), Oct3 = sum(`3_10`, na.rm = TRUE),
              May4 = sum(`4_5`, na.rm = TRUE),Jul4 = sum(`4_7`, na.rm = TRUE),Sept4 = sum(`4_9`, na.rm = TRUE))
  Impact_Bootstrap.list[[i]] <-Impact_temp2
}

saveRDS(Impact_Bootstrap.list, file = "Impact_Bootstrap.list.Rds")
