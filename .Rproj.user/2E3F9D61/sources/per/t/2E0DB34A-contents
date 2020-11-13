#This updates the master file will the daily reports. This should be ran everyday.
#Make sure the update file is entitled 'dailyreport.xls'

setwd("~/Documents/GitHub/tcm-quicc-project")
#pulling in master
library(readxl)
master <- read_excel("~/Documents/safedata/tcm-quicc/master.xlsx", col_types = c("text", "text", "date", "text", "date", "date", "date", "text", "date", "date", "text", "text", "numeric", "text", "text", "text"))

#pulling in daily report
update <- read_excel("~/Documents/safedata/tcm-quicc/dailyreport.xls", col_types = c("text", "text", "date", "text", "date", "date", "date", "date", "text", "date", "date", "text","text", "numeric", "text", "text", "text"), skip = 2)

update <- subset(update, !is.na(update$`Admit Date`))
for(i in 1: nrow(update)){
  if (is.na(update$`PC/Cardiology Appt
(1-7)`[i] && !is.na(update$`PC/Cardiology Appt
(8-14)`[i]))){
    update$`PC/Cardiology Appt
(1-7)`[i] <- update$`PC/Cardiology Appt
(8-14)`[i]
  } 
}

update <- update[-7] ## Removing 8-14 column

colnames(update) <- colnames(master)
update$`TCM Order` <- !is.na(update$`TCM Order`) 
update$`Follow up With__ Order` <- !is.na(update$`Follow up With__ Order`)
rm(i)

library(openxlsx)
master <- rbind(master,update) #bind
master <- master[order(master$Discharged),] 
master <- master[!duplicated(master),]
write.xlsx(master,"~/Documents/safedata/tcm-quicc/master.xlsx")
rm(update)

#pulling in monitor-original for colnames 
monitor_original <- read_excel("~/Documents/safedata/tcm-quicc/monitor-original.xlsx", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
monitor <- monitor_original[0,]
rm(monitor_original)
fill <- monitor


#loop for creating montior report

for (i in 1:length(unique(as.Date(master$Discharged)))) {
  fill[1,]<-NA
  fill$`Discharge Date` <- unique(as.Date(master$Discharged))[i]
  master$Discharged <- as.Date(master$Discharged)
  sub <- subset(master, master$Discharged == as.Date(fill$`Discharge Date`))
  
  fill$`Total discharges` <- nrow(sub)
  fill$`Total Discharge to home` <- sum(grepl("home", sub$Disposition, ignore.case = T))
  fill$`TCM Order for Home or Self Care Disposition` <- sum(sub$`TCM Order` == TRUE & grepl("home", sub$Disposition, ignore.case = T))
  fill$`% TCM Order Placed Appropriately (Expect 100%)` <- fill$`TCM Order for Home or Self Care Disposition`/fill$`Total Discharge to home` * 100
  fill$`Total TCM Appts made for home dischrg` <- sum(grepl("home", sub$Disposition, ignore.case = T) & !is.na(sub$`TCM Appt Date (Within 2 Weeks)`))
  fill$`% TCM Appt Scheduled for Home or Self Care (Expect 100%)` <- fill$`Total TCM Appts made for home dischrg`/fill$`Total Discharge to home` * 100
  fill$`Total TCM's Scheduled for SNF (Expect 0)` <- sum(!grepl("home", sub$Disposition, ignore.case = T) & sub$`TCM Order` == TRUE)
  
  # subset here for home patients only with TCM appointments
  sub <- sub[grepl("home", sub$Disposition, ignore.case = T) & !is.na(sub$`TCM Appt Date (Within 2 Weeks)`),]
  fill$`% scheduled within 7 days` <- sum(as.numeric(as.Date(sub$`TCM Appt Date (Within 2 Weeks)`)-as.Date(sub$Discharged)) <= 7)/fill$`Total TCM Appts made for home dischrg` * 100
  fill$`% scheduled iwthin 8-14 days` <- sum(as.Date(sub$`TCM Appt Date (Within 2 Weeks)`)-as.Date(sub$Discharged) <= 14 & as.Date(sub$`TCM Appt Date (Within 2 Weeks)`)-as.Date(sub$Discharged) > 7)/fill$`Total TCM Appts made for home dischrg` * 100
  fill$`%scheduled within 14 days` <- sum(as.Date(sub$`TCM Appt Date (Within 2 Weeks)`)-as.Date(sub$Discharged) <= 14)/fill$`Total TCM Appts made for home dischrg` * 100
  
  #rbinding to monitor
  monitor <- rbind(monitor,fill)
  monitor <- monitor[!duplicated(monitor),]
  }

rm(i)
rm(sub)
rm(fill)

write.xlsx(monitor,"~/Documents/safedata/tcm-quicc/monitor.xlsx")
# graph for monitor data
library(ggplot2)

monitor$`% TCM Appt Scheduled for Home or Self Care (Expect 100%)`
monitor$`% TCM Order Placed Appropriately (Expect 100%)`

ggplot(monitor, aes(`Discharge Date`, `% TCM Appt Scheduled for Home or Self Care (Expect 100%)`)) + 
  geom_point() +
  geom_line()+
  geom_smooth(method = "lm", se = FALSE)
ggsave("~/Documents/safedata/tcm-quicc/TCM-apt-schedule.pdf")


ggplot(monitor, aes(`Discharge Date`, `% TCM Order Placed Appropriately (Expect 100%)`)) + 
  geom_point() +
  geom_line()+
  geom_smooth(method = "lm", se = FALSE)
ggsave("~/Documents/safedata/tcm-quicc/TCM-ord-placed.pdf")



##quick report
nrow(master)
sum(grepl("home", master$Disposition, ignore.case = T))
t <- master[grepl("home", master$Disposition, ignore.case = T),]
table(as.logical(t$`TCM Order`))
table(t$`TCM Order`, t$Service)
rm(t)

