##Loading required libraries
require(stringr)
require(XLConnect)
require(dplyr)
require(tidyr)
require(data.table)
setwd("~/R_Files")

users<- read.csv(file="Users.csv", header=TRUE, sep=",")
users %>% mutate_if(is.factor, as.character) -> users


users$Timestamp<-substr(users$Timestamp,1,19)

QuitTeam$UserId<-QuitTeam$JanrainId2
QuitTeam$Timestamp<-QuitTeam$CreatedTimeStamp2
QuitTeam$TimeStamp<-substr(QuitTeam$Timestamp,1,19)

setupQuitTeam<-merge(users,QuitTeam,by=c("UserId"))

QuitTeam_Report<-setupQuitTeam %>%
  group_by(OrganizationName) %>%
  summarise(count = n_distinct(UserId))

###QUit Date####
QuitDate$UserId<-QuitDate_Jan$JanrainId2
QuitDate$Timestamp<-QuitDate_Jan$CreatedTimeStamp2
QuitDate$TimeStamp<-substr(QuitDate_Jan$Timestamp,1,19)

setQuitDate<-merge(users,QuitDate,by=c("UserId"))

QuitDate<-setQuitDate %>%
  group_by(OrganizationName) %>%
  summarise(count = n_distinct(UserId))


###CompleteHCPVisit#####
CompleteHCPVisit$UserId<-CompleteHCPVisit$FebrainId2
CompleteHCPVisit$Timestamp<-CompleteHCPVisit$CreatedTimeStamp2
CompleteHCPVisit$Timestamp<-substr(CompleteHCPVisit$Timestamp,1,19)

CompleteHCP<-merge(users,CompleteHCPVisit,by=c("UserId"))

CompleteHCP_Report<-CompleteHCP %>%
  group_by(OrganizationName) %>%
  summarise(count = n_distinct(UserId))


###ThirtyDaySmokeFree#####
ThirtyDaySmokeFree$UserId<-ThirtyDaySmokeFree$FebrainId2
ThirtyDaySmokeFree$Timestamp<-ThirtyDaySmokeFree$CreatedTimeStamp2
ThirtyDaySmokeFree$Timestamp<-substr(ThirtyDaySmokeFree$Timestamp,1,19)

ThirtyDaySmokeF<-merge(users,ThirtyDaySmokeFree,by=c("UserId"))

ThirtyDaySmokeFree_Report<-ThirtyDaySmokeF %>%
  group_by(OrganizationName) %>%
  summarise(count = n_distinct(UserId))

###CertificateOfQuit#####
CertificateofQuit$UserId<-CertificateofQuit$FebrainId2
CertificateofQuit$Timestamp<-CertificateofQuit$CreatedTimeStamp2
CertificateofQuit$Timestamp<-substr(CertificateofQuit$Timestamp,1,19)

COQ<-merge(users,CertificateofQuit,by=c("UserId"))

COQ_Report<-COQ %>%
  group_by(OrganizationName) %>%
  summarise(count = n_distinct(UserId))

###CertificateOfCompletion#####
CertificateofCompletion$UserId<-CertificateofCompletion$FebrainId2
CertificateofCompletion$Timestamp<-CertificateofCompletion$CreatedTimeStamp2
CertificateofCompletion$Timestamp<-substr(CertificateofCompletion$Timestamp,1,19)

COC<-merge(users,CertificateofCompletion,by=c("UserId"))

COC_Report<-COC %>%
  group_by(OrganizationName) %>%
  summarise(count = n_distinct(UserId))














