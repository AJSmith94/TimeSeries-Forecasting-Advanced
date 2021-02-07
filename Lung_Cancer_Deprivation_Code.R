#Measure of deprivation against cancer rates in Wales
library("readxl")
set.seed(999)

WalesDataDep<-as.data.frame(read_excel("incidence_extracts_2001-2017_FINAL.xlsx",sheet = "By deprivation"))

lmsex<-lm(WalesDataDep$Count~WalesDataDep$Sex)
summary(lmsex)
lmdep<-lm(WalesDataDep$Count~WalesDataDep$Quintile)
summary(lmdep)

WalesDataDepLung<-as.data.frame(read_excel("Deprivation regression lung.xlsx"))

lmdeplung<-lm(WalesDataDepLung$Count~WalesDataDepLung$Quintile)
summary(lmdeplung)
lmsexlung<-lm(WalesDataDepLung$Count~WalesDataDepLung$Sex)
summary(lmsexlung)
