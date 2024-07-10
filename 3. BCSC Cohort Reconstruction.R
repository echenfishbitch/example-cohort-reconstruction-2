###############################################
############## CWT Reconstruction #############
###############################################
library(dplyr)
library(tidyr)
library(ggplot2)
setwd("~/example-cohort-reconstruction-2-main")

Cohort<-readRDS("CWTBootstraps.Rds") #Escapement by Year and Age 
To_Hatchery<-read.csv("Escapement to Hatchery.csv") #Hatchery Escapement
River_Harvest<-read.csv("River_Harvest.csv")
Recruits<-read.csv("CWT Release.csv") #Released

#appending fix rates (Hatchery escapement, river escapement, and recruits to "Cohort")
for(i in 1:length(Cohort)){
Cohort[[i]]<-left_join(Cohort[[i]],Recruits,  by="brood_year") #merging with recruits
Cohort[[i]]<-Cohort[[i]] %>%
  select(brood_year, Individuals_Released, Age2Sp,Age3Sp,Age4Sp)
Cohort[[i]]<-left_join(Cohort[[i]],To_Hatchery,by="brood_year")
Cohort[[i]]<-left_join(Cohort[[i]],River_Harvest,by="brood_year")
Cohort[[i]][is.na(Cohort[[i]])]<-0
}
#Impact of fishing
Fishing_Impact<-readRDS("Impact_Bootstrap.list.Rds") #For Bootstrap, monthly

for(i in 1:length(Cohort)){ #For Monthly
Cohort[[i]]<-left_join(Cohort[[i]],Fishing_Impact[[i]],by="brood_year")%>%
  select(c("brood_year","Individuals_Released","Age2Sp","Age3Sp","Age4Sp","Age4Hat", "InRiver4",
           "Jun2","Jul2","Aug2","Sept2","Mar3", "Apr3","May3","Jun3","Jul3","Aug3","Sept3","Oct3","May4", "Jul4", "Sept4"))
Cohort[[i]][is.na(Cohort[[i]])] <- 0

}

###############################################
########## *Let the fun begin * ###############
###############################################
#Age.Start of Month (includes fish that will die naturally that month 
#but not fished)
for (i in 1:length(Cohort)){
Cohort[[i]]$Age5.9<-Cohort[[i]]$Sept4
Cohort[[i]]$Age5.8<-Cohort[[i]]$Age5.9/(1-0.0184)
Cohort[[i]]$Age5.7<-Cohort[[i]]$Age5.8/(1-0.0184)+Cohort[[i]]$Jul4
Cohort[[i]]$Age5.6<-Cohort[[i]]$Age5.7/(1-0.0184)
Cohort[[i]]$Age5.5<-Cohort[[i]]$Age5.6/(1-0.0184)+Cohort[[i]]$May4
#birthday is april 1, fish turn 5
Cohort[[i]]$Age5.4<-Cohort[[i]]$Age5.5/(1-0.0184)
#4 year old spawners. At the start of march, there are age 4 spawners, march mortalities and april fish
Cohort[[i]]$Age4.3<-(Cohort[[i]]$Age5.4+Cohort[[i]]$Age4Sp+Cohort[[i]]$Age4Hat+Cohort[[i]]$InRiver4)/(1-0.0184)

#Third Year at Sea. 4 year olds are caught. (monthly)
Cohort[[i]]$Age4.2<-Cohort[[i]]$Age4.3/(1-0.0184)
Cohort[[i]]$Age4.1<-Cohort[[i]]$Age4.2/(1-0.0184)
Cohort[[i]]$Age4.12<-Cohort[[i]]$Age4.1/(1-0.0184)
Cohort[[i]]$Age4.11<-Cohort[[i]]$Age4.12/(1-0.0184)
Cohort[[i]]$Age4.10<-Cohort[[i]]$Age4.11/(1-0.0184)+Cohort[[i]]$Oct3
Cohort[[i]]$Age4.9<-Cohort[[i]]$Age4.10/(1-0.0184)+Cohort[[i]]$Sept3
Cohort[[i]]$Age4.8<-Cohort[[i]]$Age4.9/(1-0.0184)+Cohort[[i]]$Aug3
Cohort[[i]]$Age4.7<-Cohort[[i]]$Age4.8/(1-0.0184)+Cohort[[i]]$Jul3
Cohort[[i]]$Age4.6<-Cohort[[i]]$Age4.7/(1-0.0184)+Cohort[[i]]$Jun3
Cohort[[i]]$Age4.5<-Cohort[[i]]$Age4.6/(1-0.0184)+Cohort[[i]]$May3
Cohort[[i]]$Age4.4<-Cohort[[i]]$Age4.5/(1-0.0184)+Cohort[[i]]$Apr3
#fish turn 4
Cohort[[i]]$Age3.3<-(Cohort[[i]]$Age4.4+Cohort[[i]]$Age3Sp)/(1-0.0184)+Cohort[[i]]$Mar3


#Second Year at Sea. 3 year olds are caught.(monthly) 
Cohort[[i]]$Age3.2<-Cohort[[i]]$Age3.3/(1-0.0184)
Cohort[[i]]$Age3.1<-Cohort[[i]]$Age3.2/(1-0.0184)
Cohort[[i]]$Age3.12<-Cohort[[i]]$Age3.1/(1-0.0184)
Cohort[[i]]$Age3.11<-Cohort[[i]]$Age3.12/(1-0.0184)
Cohort[[i]]$Age3.10<-Cohort[[i]]$Age3.11/(1-0.0184)
Cohort[[i]]$Age3.9<-Cohort[[i]]$Age3.10/(1-0.0184)+Cohort[[i]]$Sept2
Cohort[[i]]$Age3.8<-Cohort[[i]]$Age3.9/(1-0.0184)+Cohort[[i]]$Aug2
Cohort[[i]]$Age3.7<-Cohort[[i]]$Age3.8/(1-0.0184)+Cohort[[i]]$Jul2
Cohort[[i]]$Age3.6<-Cohort[[i]]$Age3.7/(1-0.0184)+Cohort[[i]]$Jun2
Cohort[[i]]$Age3.5<-Cohort[[i]]$Age3.6/(1-0.0184)
Cohort[[i]]$Age3.4<-Cohort[[i]]$Age3.5/(1-0.0184)
#Fish turn 3 April 1
Cohort[[i]]$Age2.3<-(Cohort[[i]]$Age3.4+Cohort[[i]]$Age2Sp)/(1-0.0561)

#First Year at Sea (monthly)
Cohort[[i]]$Age2.2<-Cohort[[i]]$Age2.3/(1-0.0561)
Cohort[[i]]$Age2.1<-Cohort[[i]]$Age2.2/(1-0.0561)
Cohort[[i]]$Age2.12<-Cohort[[i]]$Age2.1/(1-0.0561)
Cohort[[i]]$Age2.11<-Cohort[[i]]$Age2.12/(1-0.0561)
Cohort[[i]]$Age2.10<-Cohort[[i]]$Age2.11/(1-0.0561)
Cohort[[i]]$Age2.9<-Cohort[[i]]$Age2.10/(1-0.0561)
Cohort[[i]]$Age2.8<-Cohort[[i]]$Age2.9/(1-0.0561)
Cohort[[i]]$Age2.7<-Cohort[[i]]$Age2.8/(1-0.0561)
Cohort[[i]]$Age2.6<-Cohort[[i]]$Age2.7/(1-0.0561)
Cohort[[i]]$Age2.5<-Cohort[[i]]$Age2.6/(1-0.0561)
Cohort[[i]]$Age2.4<-Cohort[[i]]$Age2.5/(1-0.0561)

#Outmigration Survival
Cohort[[i]]$Out_Survival<-Cohort[[i]]$Age2.4/Cohort[[i]]$Individuals_Released
#Estimating Maturation
Cohort[[i]]$Mat4<-(Cohort[[i]]$Age4Sp+Cohort[[i]]$Age4Hat+Cohort[[i]]$InRiver4)/(Cohort[[i]]$Age4Sp+Cohort[[i]]$Age4Hat+Cohort[[i]]$Age5.4+Cohort[[i]]$InRiver4)
Cohort[[i]]$Mat3<-Cohort[[i]]$Age3Sp/(Cohort[[i]]$Age3Sp+Cohort[[i]]$Age4.4)
Cohort[[i]]$Mat2<-Cohort[[i]]$Age2Sp/(Cohort[[i]]$Age2Sp+Cohort[[i]]$Age3.4)

#Estimating Impact Rate
Cohort[[i]]$Imp4<-rowSums(Cohort[[i]][,13:19])/Cohort[[i]]$Age4.4 #Age 4 Impact is from Apr3-Oct3
Cohort[[i]]$Imp3<-rowSums(Cohort[[i]][,8:12])/Cohort[[i]]$Age3.4 #Age 3 Impact is from Jun2-Mar3
# Cohort[[i]]$Imp3_4<-Cohort[[i]]$Apr2/Cohort[[i]]$Age2.4
# Cohort[[i]]$Imp3_5<-Cohort[[i]]$May2/Cohort[[i]]$Age2.5
# Cohort[[i]]$Imp3_6<-Cohort[[i]]$Jun2/Cohort[[i]]$Age2.6
# Cohort[[i]]$Imp3_7<-Cohort[[i]]$Jul2/Cohort[[i]]$Age2.7
# Cohort[[i]]$Imp3_8<-Cohort[[i]]$Aug2/Cohort[[i]]$Age2.8
# Cohort[[i]]$Imp3_9<-Cohort[[i]]$Sept2/Cohort[[i]]$Age2.9
# Cohort[[i]]$Imp3_10<-Cohort[[i]]$Oct2/Cohort[[i]]$Age2.10
# Cohort[[i]]$Imp3_11<-Cohort[[i]]$Nov2/Cohort[[i]]$Age2.11
# 
# Cohort[[i]]$Imp4_4<-Cohort[[i]]$Apr3/Cohort[[i]]$Age3.4
# Cohort[[i]]$Imp4_5<-Cohort[[i]]$May3/Cohort[[i]]$Age3.5
# Cohort[[i]]$Imp4_6<-Cohort[[i]]$Jun3/Cohort[[i]]$Age3.6
# Cohort[[i]]$Imp4_7<-Cohort[[i]]$Jul3/Cohort[[i]]$Age3.7
# Cohort[[i]]$Imp4_8<-Cohort[[i]]$Aug3/Cohort[[i]]$Age3.8
# Cohort[[i]]$Imp4_9<-Cohort[[i]]$Sept3/Cohort[[i]]$Age3.9
# Cohort[[i]]$Imp4_10<-Cohort[[i]]$Oct3/Cohort[[i]]$Age3.10
# Cohort[[i]]$Imp4_11<-Cohort[[i]]$Nov3/Cohort[[i]]$Age3.11

Cohort[[i]][is.na(Cohort[[i]])]<-0 #data frame
}
test<-Cohort[[1]]

is.na(Cohort)<-0 #list

######
saveRDS(Cohort, file = "CWT Cohort Reconstruction_Bootstrap.Rds")

#Getting confidence intervals for maturation
Maturation_Uncertainty_Bootstrap<-array(NA, c(11,3,length(Cohort)))
for(i in 1:length(Cohort)){
   Maturation_Uncertainty_Bootstrap[,1,i]<-Cohort[[i]]$Mat2
   Maturation_Uncertainty_Bootstrap[,2,i]<-Cohort[[i]]$Mat3
   Maturation_Uncertainty_Bootstrap[,3,i]<-Cohort[[i]]$Mat4
}
Maturation_Uncertainty<-matrix(nrow = 11, ncol=12)
for(i in 1:11){
  for(j in 1:3){
    Maturation_Uncertainty[i,c(1+(j-1)*3,2+(j-1)*3,3+(j-1)*3)]<-quantile(Maturation_Uncertainty_Bootstrap[i,j,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE) #Mat 2
    Maturation_Uncertainty[i,9+j]<-mean(Maturation_Uncertainty_Bootstrap[i,j,c(1:1000)])
    
     }
}
brood_year<-c(1997:2007)
Maturation_Uncertainty<-as.data.frame(cbind(brood_year,Maturation_Uncertainty))
names(Maturation_Uncertainty)<-c("brood_year","Mat2Lower","Mat2Med",  "Mat2Upper", "Mat3Lower", "Mat3Med","Mat3Upper", "Mat4Lower", "Mat4Med","Mat4Upper", "Mat2Mean", "Mat3Mean", "Mat4Mean")
Maturation_Uncertainty$Source = "CWT"
write.csv(Maturation_Uncertainty,"Maturation_Uncertainty_CWT.csv", row.names = FALSE)
#Getting confidence intervals for impact
Impact_Uncertainty_Bootstrap<-array(NA, c(11,3,length(Cohort)))
for(i in 1:length(Cohort)){
  Impact_Uncertainty_Bootstrap[,1,i]<-Cohort[[i]]$Imp3
  Impact_Uncertainty_Bootstrap[,2,i]<-Cohort[[i]]$Imp4
}
Impact_Uncertainty<-matrix(nrow = 11, ncol=8)
#5,50,95 quantile
for(i in 1:11){
  for(j in 1:2){
    Impact_Uncertainty[i,c(1+(j-1)*3,2+(j-1)*3,3+(j-1)*3)]<-quantile(Impact_Uncertainty_Bootstrap[i,j,c(1:1000)], probs=c(.025,.5,.975), na.rm = TRUE) #Mat 2
    Impact_Uncertainty[i,6+j]<-mean(Impact_Uncertainty_Bootstrap[i,j,c(1:1000)])
    }
}
brood_year<-c(1997:2007)
Impact_Uncertainty<-as.data.frame(cbind(brood_year,Impact_Uncertainty))

names(Impact_Uncertainty)<-c("brood_year","Imp3Lower","Imp3Med",  "Imp3Upper", "Imp4Lower", "Imp4Med","Imp4Upper", "Imp3Mean", "Imp4Mean")
write.csv(Impact_Uncertainty,"Impact_Uncertainty_CWT.csv", row.names = FALSE)

