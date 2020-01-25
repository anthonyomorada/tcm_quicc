#This updates the master file will the daily reports. This should be ran everyday.

setwd("~/Documents/GitHub/tcm-quicc-project")

#pulling in master
library(readxl)
master <- read_excel("~/Documents/safedata/tcm-quicc/master.xlsx", col_types = c("text", "text", "date", "text", "date", "date", "date", "text", "date", "date", "text", "text", "numeric", "text", "text"))

#pulling in daily report
update <- read_excel("~/Documents/safedata/tcm-quicc/dailyreport.xls", col_types = c("text", "text", "date", "text", "date", "date", "date", "text", "date", "date", "text", "text", "numeric", "text", "text"), skip = 2)
update <- subset(update, !is.na(update$`Admit Date`))
colnames(update) <- colnames(master)
update$`TCM Order` <- !is.na(update$`TCM Order`) 

master <- rbind(master,update) #bind
master <- master[order(master$Discharged),] 
master <- master[!duplicated(master),]
write.xlsx(master,"~/Documents/safedata/tcm-quicc/master.xlsx")


