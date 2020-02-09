#This updates the master file will the daily reports. This should be ran everyday.
#Make sure the update file is entitled 'dailyreport.xls'

setwd("~/Documents/GitHub/tcm-quicc-project")
#pulling in master
library(readxl)
master <- read_excel("~/Documents/safedata/tcm-quicc/master.xlsx", col_types = c("text", "text", "date", "text", "date", "date", "date", "text", "date", "date", "text", "text", "numeric", "text", "text"))

#pulling in daily report
update <- read_excel("~/Documents/safedata/tcm-quicc/dailyreport.xls", col_types = c("text", "text", "date", "text", "date", "date", "date", "text", "date", "date", "text", "text", "numeric", "text", "text"), skip = 2)
update <- subset(update, !is.na(update$`Admit Date`))
colnames(update) <- colnames(master)
update$`TCM Order` <- !is.na(update$`TCM Order`) 

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
  fill$`Total TCM Appts made for home dischrg` <- sum(grepl("home", sub$Disposition, ignore.case = T) & !is.na(sub$`TCM (Any Specialty)`))
  fill$`% TCM Appt Scheduled for Home or Self Care (Expect 100%)` <- fill$`Total TCM Appts made for home dischrg`/fill$`Total Discharge to home` * 100
  fill$`Total TCM's Scheduled for SNF (Expect 0)` <- sum(!grepl("home", sub$Disposition, ignore.case = T) & sub$`TCM Order` == TRUE)
  
  # subset here for home patients only with TCM appointments
  sub <- sub[grepl("home", sub$Disposition, ignore.case = T) & !is.na(sub$`TCM (Any Specialty)`),]
  fill$`% scheduled within 7 days` <- sum(as.numeric(as.Date(sub$`TCM (Any Specialty)`)-as.Date(sub$Discharged)) <= 7)/fill$`Total TCM Appts made for home dischrg` * 100
  fill$`% scheduled iwthin 8-14 days` <- sum(as.Date(sub$`TCM (Any Specialty)`)-as.Date(sub$Discharged) <= 14 & as.Date(sub$`TCM (Any Specialty)`)-as.Date(sub$Discharged) > 7)/fill$`Total TCM Appts made for home dischrg` * 100
  fill$`%scheduled within 14 days` <- sum(as.Date(sub$`TCM (Any Specialty)`)-as.Date(sub$Discharged) <= 14)/fill$`Total TCM Appts made for home dischrg` * 100
  
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
ggsave("TCM-apt-schedule.pdf")


ggplot(monitor, aes(`Discharge Date`, `% TCM Order Placed Appropriately (Expect 100%)`)) + 
  geom_point() +
  geom_line()+
  geom_smooth(method = "lm", se = FALSE)
ggsave("TCM-ord-placed.pdf")
