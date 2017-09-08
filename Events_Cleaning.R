
##Loading required libraries
require(stringr)
require(XLConnect)
require(dplyr)
require(tidyr)
require(data.table)
setwd("~/R_Files")

##Reading Data, relevant file must be present in the working directory with the name used below in the following code
MyData <- read.csv(file="Events_06_2017.csv", header=FALSE, sep=",")

#Making a copy of the original dataset
MyData1<-MyData


#Changing the columny type from factor to character
MyData1 %>% mutate_if(is.factor, as.character) -> MyData1



#Below code is used to split the columns of interest on the basis of the term of interest present in the column
MyData1$V9<-MyData1$V6

MyData1$V9<-ifelse(str_detect(MyData1$V9,"GroupId"),MyData1$V9," ")

MyData1$V10<-MyData1$V6

MyData1$V10<-ifelse(str_detect(MyData1$V10,"\\{"),MyData1$V10,ifelse(MyData1$V7=="",MyData1$V5,MyData1$V7))
#MyData1$V11<-ifelse(!(str_detect(MyData1$V10,"GroupId")|str_detect(MyData1$V10,"JanrainId")),MyData1$V10,ifelse(MyData$V7=="",MyData1$V5,MyData1$V7))

MyData1$V11<-MyData1$V7

MyData1$V11<-ifelse(str_detect(MyData1$V11,"JanrainIdEventType"),MyData1$V11,MyData1$V8)


x=ncol(MyData1)
y=nrow(MyData1)


MyData1$JanrainId<-NA
for (i in 1:x){
  present<-str_detect(MyData1[,i],"JanrainId:")
  for (j in 1:y){
    
    if(present[j]==TRUE){ MyData1$JanrainId[j] <- MyData1[j,i]}
  }
}

MyData1$IsActive<-NA
for (i in 1:x){
  present<-str_detect(MyData1[,i],"IsActive:")
  for (j in 1:y){
    
    if(present[j]==TRUE){ MyData1$IsActive[j] <- MyData1[j,i]}
  }
}


MyData1$EventId<-NA
for (i in 1:8){
  present<-str_detect(MyData1[,i],"EventId:")
  for (j in 1:nrow(MyData1)){
    
    if(present[j]==TRUE){ MyData1$EventId[j] <- MyData1[j,i]}
  }
}

MyData1$CreatedTimestamp<-NA
for (i in 1:8){
  present<-str_detect(MyData1[,i],"CreatedTimestamp")
  for (j in 1:nrow(MyData1)){
    
    if(present[j]==TRUE){ MyData1$CreatedTimestamp[j] <- MyData1[j,i]}
  }
}

MyData1$EventType<-NA
for (i in 1:8){
  present<-str_detect(MyData1[,i],"^EventType:")
  for (j in 1:nrow(MyData1)){
    
    if(present[j]==TRUE){ MyData1$EventType[j] <- MyData1[j,i]}
  }
}

#tokenization of column on the basis of the delimiter
#MyData1<-MyData1 %>% separate(isActive, into = paste0('Active', 1:2), sep = ':')

MyData1<-MyData1 %>% separate(JanrainId, into = paste0('JanrainId', 1:2), sep = ':')

MyData1<-MyData1 %>% separate(EventId, into = paste0('EventId', 1:2), sep = ':')

MyData1<-MyData1 %>% separate(CreatedTimestamp, into = paste0('CreatedTimeStamp', 1:2), sep = 'p:')

MyData1<-MyData1 %>% separate(EventType, into = paste0('EventType', 1:2), sep = ':')

#for the column recording the user event in the original file, calculates the number of columns into which to explode the user-event column using the column delimiter 
n<-max(str_count(MyData1$V10,","))

MyData1<-MyData1 %>% separate(V10, into = paste0('Event_Col_', 1:n), sep = ',')

MyData1<-MyData1 %>% separate(V11, into = paste0('JanrainIdEventType', 1:2), sep = ':')


#intermediate step, commenting out
#write.csv(MyData1,file="Clean_Logs.csv",replace=TRUE)


#droping the columns acting as indentifier for the next column present
drops <- c("V1","V2","V3","V4","V5","V6","V7","V8")

#Change events_* to MyData1
MyData1<-MyData1[,!(names(MyData1) %in% drops)]
#colnames(MyData1)[1]="GroupId"
#preparing different dataframe as per the requirement for different report suite


#QuitTeam Report
QuitTeam<-subset(Events_Mar,EventType2=="supporterQuitTeam")


#QuitDate Report
#initial filter
QuitDate<-subset(MyData1,EventType2=="addMilestone" & IsActive=="IsActive:1")

#below code prepares the grep logic to be used in filtering the rows for the Quit Date report
k<-NULL


for ( i in 1:n){
  x<-paste0("grepl('\"milestoneType\":\"3\"| \"milestoneTypeId\":\"3\" ',Event_Col_",i)
  if(i!=n){
    y<-paste0(x,") | ")
    k<-paste0(k,y)}
  else {
    y<-paste0(x,")")
    k<-paste0(k,y)}
}

#Filtering the rows for the QuitDate report
QuitDate<-with(QuitDate,QuitDate[eval(parse(text=k)),])


#Completed HCP Visit Report
CompleteHCPVisit<-subset(MyData1,EventType2=="visitedDoctor")

#below code prepares the grep logic to be used in filtering the rows for the Completed HCP Visit Report
k<-NULL

for ( i in 1:n){
  x<-paste0("grepl('\"isDoctorVisited\":\"true\"',Event_Col_",i)
  if(i!=n){
    y<-paste0(x,") | ")
    k<-paste0(k,y)}
  else {
    y<-paste0(x,")")
    k<-paste0(k,y)}
}

#Filtering the rows for the Completed HCP Visit Report
CompleteHCPVisit<-with(CompleteHCPVisit,CompleteHCPVisit[eval(parse(text=k)),])



#ThirtyDaySmokeFree Report
ThirtyDaySmokeFree<-subset(MyData1,EventType2=="setTileState" & IsActive=="IsActive:1")

#below code prepares the grep logic to be used in filtering the rows for the ThirtyDaySmokeFree Report
k<-NULL

for ( i in 1:n){
  x<-paste0("grepl('1MS_CT2',Event_Col_",i)
  if(i!=n){
    y<-paste0(x,") | ")
    k<-paste0(k,y)}
  else {
    y<-paste0(x,")")
    k<-paste0(k,y)}
}

#Filtering the rows for the ThirtyDaySmokeFree Report
ThirtyDaySmokeFree<-with(ThirtyDaySmokeFree,ThirtyDaySmokeFree[eval(parse(text=k)),])

#ThirtyDaySmokeFree<-with(ThirtyDaySmokeFree,ThirtyDaySmokeFree[grepl('1MS_CT2',Event_Col_7)|grepl('1MS_CT2',Event_Col_8)|grepl('1MS_CT2',Event_Col_9),])


#CertificateofCompletion Report
CertificateofCompletion<-subset(MyData1,EventType2=="setTileState" & IsActive=="IsActive:1")

#below code prepares the grep logic to be used in filtering the rows for the CertificateofCompletion Report
k<-NULL

for ( i in 1:n){
  x<-paste0("grepl('COC_MT4',Event_Col_",i)
  if(i!=n){
    y<-paste0(x,") | ")
    k<-paste0(k,y)}
  else {
    y<-paste0(x,")")
    k<-paste0(k,y)}
}


#Filtering the rows for the CertificateofCompletion Report
CertificateofCompletion<-with(CertificateofCompletion,CertificateofCompletion[eval(parse(text=k)),])

#CertificateofQuit Report
CertificateofQuit<-subset(MyData1,EventType2=="setTileState" & IsActive=="IsActive:1")

#below code prepares the grep logic to be used in filtering the rows for the CertificateofQuit Report
k<-NULL

for ( i in 1:n){
  x<-paste0("grepl('COQ_MT4',Event_Col_",i)
  if(i!=n){
    y<-paste0(x,") | ")
    k<-paste0(k,y)}
  else {
    y<-paste0(x,")")
    k<-paste0(k,y)}
}


#Filtering the rows for the CertificateofQuitReport
CertificateofQuit<-with(CertificateofQuit,CertificateofQuit[k,])


#writing the files for the individual reports
write.csv(QuitTeam,file="QuitTeam.csv")

write.csv(QuitDate,file="QuitDate.csv")

write.csv(CompleteHCPVisit,file="CompleteHCPVisit.csv")

write.csv(ThirtyDaySmokeFree,file="ThirtyDaySmokeFree.csv")

write.csv(CertificateofCompletion,file="CertificateofCompletion.csv")

write.csv(CertificateofQuit,file="CertificateofQuit.csv")


