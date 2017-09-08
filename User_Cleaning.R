##Loading required libraries
require(stringr)
require(XLConnect)
require(dplyr)
require(tidyr)
require(data.table)
setwd("~/R_Files")

##Reading Data, relevant file must be present in the working directory with the name used below in the following code
UserData <- read.csv(file="Users_04_2017.csv", header=FALSE, sep=",")
UserData %>% mutate_if(is.factor, as.character) -> UserData
#UserData_Filter<-select(UserData,matches("CreatedTimestamp| UserID | organizationName"))




z<-apply(UserData, 2, function(x) any(grepl(".*UserType.*",x)|grepl(".*UserId.*",x)|grepl(".*\\{.*",x)|grepl(".*CreatedTimestamp.*",x)))
UserData_Filter<-UserData[,z]

#temp_col<-UserData_Filter[,3]
#present<-str_detect(temp_col,"CreatedTimestamp")
#present  

UserData_Filter$Timestamp<-NA
for (i in 1:ncol(UserData_Filter)){
  present<-str_detect(UserData_Filter[,i],"CreatedTimestamp")
  for (j in 1:nrow(UserData_Filter)){
    
    if(present[j]==TRUE){ UserData_Filter$Timestamp[j] <- UserData_Filter[j,i]}
  }
}

UserData_Filter$UserId<-NA
for (i in 1:ncol(UserData_Filter)){
  present<-str_detect(UserData_Filter[,i],"UserId")
  for (j in 1:nrow(UserData_Filter)){
    
    if(present[j]==TRUE){ UserData_Filter$UserId[j] <- UserData_Filter[j,i]}
  }
}

UserData_Filter$Event<-NA

for (i in 1:ncol(UserData_Filter)){
  present<-str_detect(UserData_Filter[,i],"\\{")
  for (j in 1:nrow(UserData_Filter)){
    
    if(present[j]==TRUE){ UserData_Filter$Event[j] <- UserData_Filter[j,i]}
  }
}


UserData_Filter$UserType<-UserData_Filter$V1
UserData_Filter$OrganizationName<-str_extract(UserData_Filter$Event,"organizationName\":\"[A-Za-z\\s\\.\\']+")
UserData_Filter$OrganizationId<-str_extract(UserData_Filter$Event,"organizationId\":\"[A-Za-z\\\\.s\\']+")
UserData_Filter_Temp<-UserData_Filter[,c("UserType","Timestamp","UserId","OrganizationName","OrganizationId")]
UserData_Filter_Temp$OrganizationName<-gsub("organizationName\":\"","",UserData_Filter_Temp$OrganizationName)
UserData_Filter_Temp$OrganizationId<-gsub("organizationId\":\"","",UserData_Filter_Temp$OrganizationId)
UserData_Apr<-UserData_Filter_Temp
#users<-rbind(users,UserData_Apr)
#users$UserId<-substr(users$UserId,8,47)
#users$Timestamp<-substr(users$Timestamp,22,40)
write.csv(UserData_Filter_Temp,file="Clean_User.csv")



###########################################################################################################################################################################################
UserData_Nov$UserId<-substr(UserData_Nov$UserId,12,47)
UserData_Dec$UserId<-substr(UserData_Dec$UserId,12,47)
UserData_Jan$UserId<-substr(UserData_Jan$UserId,12,47)
UserData_Feb$UserId<-substr(UserData_Feb$UserId,8,43)
UserData_Mar$UserId<-substr(UserData_Mar$UserId,8,43)
UserData_Apr$UserId<-substr(UserData_Apr$UserId,8,43)



ThirtyDaySmokeFree_Nov$UserId<-ThirtyDaySmokeFree_Nov$JanrainId2
ThirtyDaySmokeFree_Dec$UserId<-ThirtyDaySmokeFree_Dec$JanrainId2
CertificateofCompletion_Jan$UserId<-CertificateofCompletion_Jan$JanrainId2
CertificateofCompletion_Feb$UserId<-CertificateofCompletion_Feb$JanrainId2
CertificateofCompletion_Mar$UserId<-CertificateofCompletion_Mar$JanrainId2
ThirtyDaySmokeFree_Apr$UserId<-ThirtyDaySmokeFree_Apr$JanrainId2


setCertificateofCompletion_Jan<-merge(userex2,CertificateofCompletion_Jan,by=c("UserId"))

QuitDate_Mar_Report<-setQuitDate_Mar %>%
  group_by(OrganizationName) %>%
  summarise(count = n_distinct(UserId))



setQuitDate_Dec<-merge(user_master,QuitDate_Dec,by=c("UserId"))

QuitDate_Dec_Report<-setQuitDate_Dec %>%
  group_by(OrganizationName) %>%
  summarise(count = n_distinct(UserId))


setQuitDate_Jan<-merge(user_master,QuitDate_Jan,by=c("UserId"))

QuitDate_Jan_Report<-setQuitDate_Jan %>%
  group_by(OrganizationName) %>%
  summarise(count = n_distinct(UserId))

setQuitDate_Feb<-merge(user_master,QuitDate_Feb,by=c("UserId"))

QuitDate_Feb_Report<-setQuitDate_Feb %>%
  group_by(OrganizationName) %>%
  summarise(count = n_distinct(UserId))

setQuitDate_Mar<-merge(user_master,QuitDate_Mar,by=c("UserId"))

QuitDate_Mar_Report<-setQuitDate_Mar %>%
  group_by(OrganizationName) %>%
  summarise(count = n_distinct(UserId))

setQuitDate_Master<-merge(user_master,QuitDate_Master,by=c("UserId"))

QuitDate_Report<-setQuitDate_Master %>%
  group_by(OrganizationName,CreatedTimeStamp2) %>%
  summarise(count = n_distinct(UserId))



