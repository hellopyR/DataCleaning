
##Loading required libraries
require(stringr)
require(XLConnect)
require(dplyr)
require(tidyr)
require(data.table)
setwd("~/R_Files")

survey<-MyData1[MyData1$EventType2=="SurveyEvent",]

survey$UserId<-survey$JanrainId2
survey$Timestamp<-survey$CreatedTimeStamp2
survey$Timestamp<-substr(survey$Timestamp,1,19)

survey_merge<-merge(userex4,survey_Mar,by="UserId")

Survey_Report<-survey_merge_Mar %>%
  group_by(OrganizationName,Event_Col_6) %>%
  summarise(count = n_distinct(UserId))