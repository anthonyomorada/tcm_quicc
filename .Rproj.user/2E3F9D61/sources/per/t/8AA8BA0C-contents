setwd("~/Documents/GitHub/tcm-quicc-project")

#original started on 01/24/2020
library(readxl)
og <- read_excel("~/Documents/safedata/tcm-quicc/original.xlsx", col_types = c("text", "text", "date","text", "date", "date", "date", "text", "date", "date", "text", "text", "numeric", "text", "text", "text", "text"))
library(dplyr)
og <- select(og, -`Pass or Fail`, -Notes)
og$`TCM Order` <- !is.na(og$`TCM Order`) 


#first update and hopefully this stays the same
update <- read_excel("~/Documents/safedata/tcm-quicc/dailyreport.xls", col_types = c("text", "text", "date", "text", "date", "date", "date", "text", "date", "date", "text", "text", "numeric", "text", "text"), skip = 2)

update <- subset(update, !is.na(update$`Admit Date`))
colnames(update) <- colnames(og)
update$`TCM Order` <- !is.na(update$`TCM Order`) 

master <- rbind(og,update)

library(openxlsx)
write.xlsx(master,"~/Documents/safedata/tcm-quicc/master.xlsx")
