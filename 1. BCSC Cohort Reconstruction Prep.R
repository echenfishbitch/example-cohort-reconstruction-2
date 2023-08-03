###############################################
######### Butte Creek CWT Reconstruction ######
#################CWT Data Prep#################
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
write.csv(CWT_Releases, file="CWT Release.csv", row.names=FALSE)
#############################################
##########Escapement to Hatchery#############
#############################################
CWT_Recoveries<-read.csv("recoveries.csv")
CWT_Recoveries<-left_join(CWT_Recoveries, BY, by="tag_code")   

CWT_Hatchery<-CWT_Recoveries %>%
  filter(fishery ==50) %>%
  group_by(run_year, brood_year) %>%
  summarise(Escapement_to_Hatchery = sum(estimated_number/Phi)) %>%
  mutate(Age = run_year-brood_year)%>%
  pivot_wider(names_from = Age, values_from = Escapement_to_Hatchery, names_sort=TRUE) %>%
  group_by(brood_year) %>%
  summarize(Age4Hat = sum(`4`, na.rm = TRUE))
write.csv(CWT_Hatchery, file = "Escapement to Hatchery.csv", row.names = FALSE)
#############################################
#######  Escapement to Spawning Grounds #####
#############################################
#point estimate
CWT_SG<- CWT_Recoveries %>%
  select(reporting_agency,run_year,period, fishery, sex, tag_code, estimated_number, recovery_location_code, brood_year, estimated_number, Phi) %>%
  filter(fishery == 54) %>% #from spawning ground surveys
  mutate(Age = run_year - brood_year) %>%
  group_by(run_year, brood_year, Age) %>%
  summarise(Individuals= sum(estimated_number/Phi)) %>%
  pivot_wider(names_from = Age, values_from = Individuals, names_sort=TRUE) %>%
  group_by(brood_year) %>%
  summarize(Age2Sp = sum(`2`, na.rm = TRUE), Age3Sp = sum(`3`, na.rm = TRUE)
            , Age4Sp = sum(`4`, na.rm = TRUE))
#BOOTSTRAP for uncertainty
niterations = 1000
CWT_SG<- CWT_Recoveries %>%
  select(reporting_agency,run_year,period, fishery, sex, tag_code, estimated_number, recovery_location_code, brood_year,  estimated_number, Phi) %>%
  filter(fishery == 54) 
CWT_SG$Individuals<-NA #Empty space for resampled value
CWT_SG_null<-CWT_SG
Cohort<-list() #Cohort will be a table of Spawners from each age from 20 years, sampled 1000 times
for(j in 1:niterations){
    CWT_SG<-CWT_SG_null #at the start of every iteration, reset Individuals column
  for(i in 1:268){
    #how many individuals each tag represents equals (1 + resampled number of unrecovered)/Phi
    CWT_SG$Individuals[i]<-sum(c(1,rnbinom(1, 1, (1/(CWT_SG$estimated_number[i])))))/CWT_SG$Phi[i]
  }
    #summing up age-specific escapement
  CWT_SG<-CWT_SG %>%
      mutate(Age = run_year - brood_year) %>%
      group_by(run_year, brood_year, Age) %>%
      summarise(Individuals= sum(Individuals))
  CWT_SG<-CWT_SG %>% #making each Age into its own column
    pivot_wider(names_from = Age, values_from = Individuals, names_sort=TRUE)
  CWT_SG<-CWT_SG %>%
    group_by(brood_year) %>%
    summarize(Age2Sp = sum(`2`, na.rm = TRUE), Age3Sp = sum(`3`, na.rm = TRUE)
              , Age4Sp = sum(`4`, na.rm = TRUE))
  Cohort[[j]] <- CWT_SG
}
saveRDS(Cohort, file = "CWTBootstraps.Rds")
#view as array
# Cohort.array<-array(NA, dim = c(11,4,1000))
# for(i in 1:1000){
#   Cohort.array[,,i]<-as.matrix(Cohort[[i]])
# }

#############################################
#############  In-River Harvest #############
#############################################
CWT_River<-CWT_Recoveries %>%
  select(reporting_agency,run_year,period, fishery, sex, tag_code, estimated_number, recovery_location_code, brood_year,  estimated_number, Phi) %>%
  filter(fishery == 46) %>% #from spawning ground surveys
  mutate(Age = run_year - brood_year) %>%
  group_by(run_year, brood_year, Age) %>%
  summarise(Individuals= sum(estimated_number/Phi))

colnames(CWT_River)[4]<-"InRiver4"
CWT_River<-CWT_River[,c(2,4)]
write.csv(CWT_River, "River_Harvest.csv", row.names = FALSE)
