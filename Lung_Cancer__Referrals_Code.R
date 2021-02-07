library("readxl")
set.seed(999)

#Create time series for all hospitals, all referrals
Dataset_subset_by_DateSuspicion<-as.data.frame(read_excel("Dataset subset by DateSuspicion.xlsx"))
LungSuspicion<-as.data.frame(Dataset_subset_by_DateSuspicion)
TSDataAllReferralsAllHospitalsWeekly<-cbind(seq(1:100),as.integer(tabulate(LungSuspicion$Week,100)))
plot(TSDataAllReferralsAllHospitalsWeekly,type="l")

hist(tabulate(LungSuspicion$Week,100),breaks = 20)

TSDataAllReferralsAllHospitalsMonthly<-cbind(seq(1:23),as.integer(tabulate(LungSuspicion$Month,23)))
plot(TSDataAllReferralsAllHospitalsMonthly,type="l")

TSDataAllReferralsAllHospitalsDaily<-cbind(seq(1:698),as.integer(tabulate(LungSuspicion$Days,698)))
plot(TSDataAllReferralsAllHospitalsDaily,type="l")


#Create time series for each hospital, all referrals

table(LungSuspicion$CareFacility)

LungSuspicionPrinceCh<-LungSuspicion[LungSuspicion$CareFacility=="Prince Charles Hospital",]
LungSuspicionRoyalGlam<-LungSuspicion[LungSuspicion$CareFacility=="Royal Glamorgan Hospital",]

table(LungSuspicion$CareFacility)

#Prince Charles
TSDataAllReferralsPrinceChWeekly<-cbind(seq(1:100),as.integer(tabulate(LungSuspicionPrinceCh$Week,100)))
TSDataAllReferralsPrinceChWeekly
plot(TSDataAllReferralsPrinceChWeekly,type="l")
table(tabulate(LungSuspicionPrinceCh$Week,100))
hist(tabulate(LungSuspicionPrinceCh$Week,100),breaks = 10)

TSDataAllReferralsPrinceChMonthly<-cbind(seq(1:23),as.integer(tabulate(LungSuspicionPrinceCh$Month,23)))
TSDataAllReferralsPrinceChMonthly
plot(TSDataAllReferralsPrinceChMonthly,type="l")
table(tabulate(LungSuspicionPrinceCh$Month,23))
hist(tabulate(LungSuspicionPrinceCh$Month,23),breaks=15)

TSDataAllReferralsPrinceChDaily<-cbind(seq(1:698),as.integer(tabulate(LungSuspicionPrinceCh$Days,698)))
TSDataAllReferralsPrinceChDaily
plot(TSDataAllReferralsPrinceChDaily,type="l")
table(tabulate(LungSuspicionPrinceCh$Days,698))
hist(tabulate(LungSuspicionPrinceCh$Days,698),breaks=15)

#Royal Glamorgan
TSDataAllReferralsRoyalGlamWeekly<-cbind(seq(1:100),as.integer(tabulate(LungSuspicionRoyalGlam$Week,100)))
TSDataAllReferralsRoyalGlamWeekly
plot(TSDataAllReferralsRoyalGlamWeekly,type="l")
table(tabulate(LungSuspicionRoyalGlam$Week,698))
hist(tabulate(LungSuspicionRoyalGlam$Week,698),breaks = 10)

TSDataAllReferralsRoyalGlamMonthly<-cbind(seq(1:23),as.integer(tabulate(LungSuspicionRoyalGlam$Month,23)))
TSDataAllReferralsRoyalGlamMonthly
plot(TSDataAllReferralsRoyalGlamMonthly,type="l")
table(tabulate(LungSuspicionRoyalGlam$Month,23))
hist(tabulate(LungSuspicionRoyalGlam$Month,23),breaks=15)

TSDataAllReferralsRoyalGlamDaily<-cbind(seq(1:698),as.integer(tabulate(LungSuspicionRoyalGlam$Days,698)))
TSDataAllReferralsRoyalGlamDaily
plot(TSDataAllReferralsRoyalGlamDaily,type="l")
table(tabulate(LungSuspicionRoyalGlam$Days,698))
hist(tabulate(LungSuspicionRoyalGlam$Days,698),breaks=15)

plot(TSDataAllReferralsPrinceChWeekly,type="l",col="red",lwd=2)
points(TSDataAllReferralsRoyalGlamWeekly,type="l",col="blue",lwd=2)

plot(TSDataAllReferralsPrinceChMonthly,type="l",col="red",lwd=2)
points(TSDataAllReferralsRoyalGlamMonthly,type="l",col="blue",lwd=2)

plot(TSDataAllReferralsPrinceChDaily,type="l",col="red",lwd=2)
points(TSDataAllReferralsRoyalGlamDaily,type="l",col="blue",lwd=2)

#Create time series for each referral type, across all hospitals

table(LungSuspicion$SCPReferralSource)

LungSuspicionReferralGP<-LungSuspicion[LungSuspicion$SCPReferralSource=="Referral From GP",]
LungSuspicionEmergency<-LungSuspicion[LungSuspicion$SCPReferralSource=="A & E/Medical Assessment/ Emergency Admission",]
LungSuspicionOutPatient<-LungSuspicion[LungSuspicion$SCPReferralSource=="Out patient upgrade",]
LungSuspicionDiagnostic<-LungSuspicion[LungSuspicion$SCPReferralSource=="Referral following diagnostic - Endoscopy"|LungSuspicion$SCPReferralSource=="Referral following diagnostic - Imaging"|LungSuspicion$SCPReferralSource=="Referral following diagnostic - Other",]
LungSuspicionWard<-LungSuspicion[LungSuspicion$SCPReferralSource=="Ward",]
LungSuspicionConsultantInternal<-LungSuspicion[LungSuspicion$SCPReferralSource=="Consultant Internal",]
LungSuspicionConsultantExternal<-LungSuspicion[LungSuspicion$SCPReferralSource=="Consultant External",]
LungSuspicionOtherHealthProf<-LungSuspicion[LungSuspicion$SCPReferralSource=="Other healthcare professional",]

#Weekly
#Referral from GP
TSDataReferralGPAllHospitalsWeekly<-cbind(seq(1:100),as.integer(tabulate(LungSuspicionReferralGP$Week,100)))
plot(TSDataReferralGPAllHospitalsWeekly,type="l")
table(tabulate(LungSuspicionReferralGP$Week,100))
hist(tabulate(LungSuspicionReferralGP$Week,100),breaks = 15)

#Referral from Emergency
TSDataEmergencyAllHospitalsWeekly<-cbind(seq(1:100),as.integer(tabulate(LungSuspicionEmergency$Week,100)))
plot(TSDataEmergencyAllHospitalsWeekly,type="l")
table(tabulate(LungSuspicionEmergency$Week,100))
hist(tabulate(LungSuspicionEmergency$Week,100),breaks=8)

#Referral from Outpatients
TSDataOutPatientAllHospitalsWeekly<-cbind(seq(1:100),as.integer(tabulate(LungSuspicionOutPatient$Week,100)))
plot(TSDataOutPatientAllHospitalsWeekly,type="l")
table(tabulate(LungSuspicionOutPatient$Week,100))
hist(tabulate(LungSuspicionOutPatient$Week,100))

#Referral from Diagnostics
TSDataDiagnosticAllHospitalsWeekly<-cbind(seq(1:100),as.integer(tabulate(LungSuspicionDiagnostic$Week,100)))
plot(TSDataDiagnosticAllHospitalsWeekly,type="l")
table(tabulate(LungSuspicionDiagnostic$Week,100))
hist(tabulate(LungSuspicionDiagnostic$Week,100),breaks=8)

#Referral from Ward
TSDataWardAllHospitalsWeekly<-cbind(seq(1:100),as.integer(tabulate(LungSuspicionWard$Week,100)))
plot(TSDataWardAllHospitalsWeekly,type="l")
table(tabulate(LungSuspicionWard$Week,100))
hist(tabulate(LungSuspicionWard$Week,100),breaks=8)

#Referral from Consultant - Internal
TSDataConsultantInternalAllHospitalsWeekly<-cbind(seq(1:100),as.integer(tabulate(LungSuspicionConsultantInternal$Week,100)))
plot(TSDataConsultantInternalAllHospitalsWeekly,type="l")
table(tabulate(LungSuspicionConsultantInternal$Week,100))
hist(tabulate(LungSuspicionConsultantInternal$Week,100),breaks=8)

#Referral from Consultant - External
TSDataConsultantExternalAllHospitalsWeekly<-cbind(seq(1:100),as.integer(tabulate(LungSuspicionConsultantExternal$Week,100)))
plot(TSDataConsultantExternalAllHospitalsWeekly,type="l")
table(tabulate(LungSuspicionConsultantExternal$Week,100))
hist(tabulate(LungSuspicionConsultantExternal$Week,100),breaks=8)

#Referral from Other Health Prof
TSDataOtherHealthProfAllHospitalsWeekly<-cbind(seq(1:100),as.integer(tabulate(LungSuspicionOtherHealthProf$Week,100)))
plot(TSDataOtherHealthProfAllHospitalsWeekly,type="l")
table(tabulate(LungSuspicionOtherHealthProf$Week,100))
hist(tabulate(LungSuspicionOtherHealthProf$Week,100),breaks=8)

table(LungSuspicion$SCPReferralSource)
plot(TSDataReferralGPAllHospitalsWeekly,type="l",ylim = c(-1,25),lwd=2)
points(TSDataConsultantInternalAllHospitalsWeekly,type="l",col="red",lwd=2)
points(TSDataEmergencyAllHospitalsWeekly,type="l",col="blue",lwd=2)
points(TSDataDiagnosticAllHospitalsWeekly,type="l",col="red",lwd=2)
points(TSDataOtherHealthProfAllHospitalsWeekly,type="l",col="purple",lwd=2)
points(TSDataWardAllHospitalsWeekly,type="l",col="gold",lwd=2)
points(TSDataOutPatientAllHospitalsWeekly,type="l",col="brown",lwd=2)
points(TSDataConsultantExternalAllHospitalsWeekly,type="l",col="light blue",lwd=2)

#Monthly
#Referral from GP
TSDataReferralGPAllHospitalsMonthly<-cbind(seq(1:23),as.integer(tabulate(LungSuspicionReferralGP$Month,23)))
plot(TSDataReferralGPAllHospitalsMonthly,type="l")
table(tabulate(LungSuspicionReferralGP$Month,23))
hist(tabulate(LungSuspicionReferralGP$Month,23),breaks = 15)

#Referral from Emergency
TSDataEmergencyAllHospitalsMonthly<-cbind(seq(1:23),as.integer(tabulate(LungSuspicionEmergency$Month,23)))
plot(TSDataEmergencyAllHospitalsMonthly,type="l")
table(tabulate(LungSuspicionEmergency$Month,23))
hist(tabulate(LungSuspicionEmergency$Month,23),breaks=8)

#Referral from Outpatients
TSDataOutPatientAllHospitalsMonthly<-cbind(seq(1:23),as.integer(tabulate(LungSuspicionOutPatient$Month,23)))
plot(TSDataOutPatientAllHospitalsMonthly,type="l")
table(tabulate(LungSuspicionOutPatient$Month,23))
hist(tabulate(LungSuspicionOutPatient$Month,23))

#Referral from Diagnostics
TSDataDiagnosticAllHospitalsMonthly<-cbind(seq(1:23),as.integer(tabulate(LungSuspicionDiagnostic$Month,23)))
plot(TSDataDiagnosticAllHospitalsMonthly,type="l")
table(tabulate(LungSuspicionDiagnostic$Month,23))
hist(tabulate(LungSuspicionDiagnostic$Month,23),breaks=8)

#Referral from Ward
TSDataWardAllHospitalsMonthly<-cbind(seq(1:23),as.integer(tabulate(LungSuspicionWard$Month,23)))
plot(TSDataWardAllHospitalsMonthly,type="l")
table(tabulate(LungSuspicionWard$Month,23))
hist(tabulate(LungSuspicionWard$Month,23),breaks=8)

#Referral from Consultant - Internal
TSDataConsultantInternalAllHospitalsMonthly<-cbind(seq(1:23),as.integer(tabulate(LungSuspicionConsultantInternal$Month,23)))
plot(TSDataConsultantInternalAllHospitalsMonthly,type="l")
table(tabulate(LungSuspicionConsultantInternal$Month,23))
hist(tabulate(LungSuspicionConsultantInternal$Month,23),breaks=8)

#Referral from Consultant - External
TSDataConsultantExternalAllHospitalsMonthly<-cbind(seq(1:23),as.integer(tabulate(LungSuspicionConsultantExternal$Month,23)))
plot(TSDataConsultantExternalAllHospitalsMonthly,type="l")
table(tabulate(LungSuspicionConsultantExternal$Month,23))
hist(tabulate(LungSuspicionConsultantExternal$Month,23),breaks=8)

#Referral from Other Health Prof
TSDataOtherHealthProfAllHospitalsMonthly<-cbind(seq(1:23),as.integer(tabulate(LungSuspicionOtherHealthProf$Month,23)))
plot(TSDataOtherHealthProfAllHospitalsMonthly,type="l")
table(tabulate(LungSuspicionOtherHealthProf$Month,23))
hist(tabulate(LungSuspicionOtherHealthProf$Month,23),breaks=8)

table(LungSuspicion$SCPReferralSource)
plot(TSDataReferralGPAllHospitalsMonthly,type="l",lwd=2,ylim = c(-1,60))
points(TSDataConsultantInternalAllHospitalsMonthly,type="l",col="red",lwd=2)
points(TSDataEmergencyAllHospitalsMonthly,type="l",col="blue",lwd=2)
points(TSDataDiagnosticAllHospitalsMonthly,type="l",col="red",lwd=2)
points(TSDataOtherHealthProfAllHospitalsMonthly,type="l",col="purple",lwd=2)
points(TSDataWardAllHospitalsMonthly,type="l",col="gold",lwd=2)
points(TSDataOutPatientAllHospitalsMonthly,type="l",col="brown",lwd=2)
points(TSDataConsultantExternalAllHospitalsMonthly,type="l",col="light blue",lwd=2)



#Plot against dates

#Daily all referrals, Prince Charles and Royal Glamorgan
days<-seq(as.Date("2018-01-02"), as.Date("2019-11-30"), by = "day")
plot(days,TSDataAllReferralsAllHospitalsDaily[,2],type="l",xlab="Year",ylab="Referrals",main="Daily time series for all referral types across all hospitals",lwd=2)

#Weekly all referrals, Prince Charles and Royal Glamorgan
weeks<-seq(as.Date("2018-01-02"), as.Date("2019-11-30"), by = "week")
plot(weeks,TSDataAllReferralsAllHospitalsWeekly[,2],type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types across all hospitals",lwd=2)

#Monthly all referrals, Prince Charles and Royal Glamorgan
months<-seq(as.Date("2018-01-02"), as.Date("2019-11-30"), by = "month")
plot(weeks,TSDataAllReferralsAllHospitalsWeekly[,2],type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types across all hospitals",lwd=2)

plot(days,TSDataAllReferralsAllHospitalsDaily[,2],type="l",xlab="Year",ylab="Referrals",main="Daily time series for all referral types across all hospitals",lwd=2)
plot(weeks,TSDataAllReferralsAllHospitalsWeekly[,2],type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types across all hospitals",lwd=2)
plot(months,TSDataAllReferralsAllHospitalsMonthly[,2],type="l",xlab="Year",ylab="Referrals",main="Monthly time series for all referral types across all hospitals",lwd=2)



#Daily all referrals, Prince Charles
plot(days,TSDataAllReferralsPrinceChDaily[,2],type="l",xlab="Year",ylab="Referrals",main="Daily time series for all referral types in Prince Charles",lwd=2)

#Weekly all referrals, Prince Charles
plot(weeks,TSDataAllReferralsPrinceChWeekly[,2],type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types in Prince Charles",lwd=2)

#Monthly all referrals, Prince Charles
plot(months,TSDataAllReferralsPrinceChMonthly[,2],type="l",xlab="Year",ylab="Referrals",main="Monthly time series for all referral types in Prince Charles",lwd=2)

#Daily all referrals, Royal Glamorgan
plot(days,TSDataAllReferralsRoyalGlamDaily[,2],type="l",xlab="Year",ylab="Referrals",main="Daily time series for all referral types in Royal Glamorgan",lwd=2)

#Weekly all referrals, Royal Glamorgan
plot(weeks,TSDataAllReferralsRoyalGlamWeekly[,2],type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types in Royal Glamorgan",lwd=2)

#Monthly all referrals, Royal Glamorgan
plot(months,TSDataAllReferralsRoyalGlamMonthly[,2],type="l",xlab="Year",ylab="Referrals",main="Monthly time series for all referral types in Royal Glamorgan",lwd=2)

#Comparison Daily
plot(days,TSDataAllReferralsPrinceChDaily[,2],type="l",xlab="Year",ylab="Referrals",main="Daily time series for all referral types at Prince Charles",lwd=2,col="red")

#Comparison Weekly
plot(weeks,TSDataAllReferralsPrinceChWeekly[,2],type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types at Prince Charles",lwd=2,col="red",ylim = c(-1,13))

#Comparison Monthly
plot(months,TSDataAllReferralsPrinceChMonthly[,2],type="l",xlab="Year",ylab="Referrals",main="Monthly time series for all referral types at Prince Charles",lwd=2,col="red",,ylim=c(10,50))

#Comparison Daily
plot(days,TSDataAllReferralsRoyalGlamDaily[,2],type="l",xlab="Year",ylab="Referrals",main="Daily time series for all referral types at Royal Glamorgan",lwd=2,col="blue")

#Comparison Weekly
plot(weeks,TSDataAllReferralsRoyalGlamWeekly[,2],type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types at Royal Glamorgan",lwd=2,col="blue",ylim = c(-1,13))

#Comparison Monthly
plot(months,TSDataAllReferralsRoyalGlamMonthly[,2],type="l",xlab="Year",ylab="Referrals",main="Monthly time series for all referral types at Royal Glamorgan",lwd=2,col="blue",,ylim=c(10,50))

#Create time series for each referral type, across all hospitals and plot by date

#Weekly
plot(weeks,TSDataReferralGPAllHospitalsWeekly[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Weekly time series for all GP referrals in Prince Charles and Royal Glamorgan")

plot(weeks,TSDataEmergencyAllHospitalsWeekly[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Weekly time series for all Emergency referrals in Prince Charles and Royal Glamorgan")

plot(weeks,TSDataOutPatientAllHospitalsWeekly[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Weekly time series for all OutPatient referrals in Prince Charles and Royal Glamorgan",yaxt = "n")
axis(2,at=0:2)

plot(weeks,TSDataDiagnosticAllHospitalsWeekly[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Weekly time series for all Diagnostics referrals in Prince Charles and Royal Glamorgan")

plot(weeks,TSDataWardAllHospitalsWeekly[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Weekly time series for all Ward referrals in Prince Charles and Royal Glamorgan",yaxt = "n")
axis(2,at=0:2)

plot(weeks,TSDataConsultantInternalAllHospitalsWeekly[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Weekly time series for all Consultant-Internals referrals in Prince Charles and Royal Glamorgan")

plot(weeks,TSDataConsultantExternalAllHospitalsWeekly[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Weekly time series for all Consultant-Externals referrals in Prince Charles and Royal Glamorgan",yaxt = "n")
axis(2,at=0:2)

plot(weeks,TSDataOtherHealthProfAllHospitalsWeekly[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Weekly time series for all other health professional referrals in Prince Charles and Royal Glamorgan")







#Monthly
plot(months,TSDataReferralGPAllHospitalsMonthly[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Monthly time series for all GP referrals in Prince Charles and Royal Glamorgan")

plot(months,TSDataEmergencyAllHospitalsMonthly[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Monthly time series for all Emergency referrals in Prince Charles and Royal Glamorgan")

plot(months,TSDataOutPatientAllHospitalsMonthly[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Monthly time series for all OutPatient referrals in Prince Charles and Royal Glamorgan",yaxt = "n")
axis(2,at=0:2)

plot(months,TSDataDiagnosticAllHospitalsMonthly[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Monthly time series for all Diagnostics referrals in Prince Charles and Royal Glamorgan")

plot(months,TSDataWardAllHospitalsMonthly[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Monthly time series for all Ward referrals in Prince Charles and Royal Glamorgan",yaxt = "n")
axis(2,at=0:3)

plot(months,TSDataConsultantInternalAllHospitalsMonthly[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Monthly time series for all Consultant-Internals referrals in Prince Charles and Royal Glamorgan")

plot(months,TSDataConsultantExternalAllHospitalsMonthly[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Monthly time series for all Consultant-Externals referrals in Prince Charles and Royal Glamorgan",yaxt = "n")
axis(2,at=0:2)

plot(months,TSDataOtherHealthProfAllHospitalsMonthly[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Monthly time series for all other health professional referrals in Prince Charles and Royal Glamorgan")

#Weekly comparison of each referral type in Prince Charles and Royal Glamorgan
plot(weeks,TSDataReferralGPAllHospitalsWeekly[,2],type="l",ylim = c(-1,25),lwd=2,xlab="Year",ylab="Referrals",main="Weekly time series for each referral type across all hospitals")
points(weeks,TSDataConsultantInternalAllHospitalsWeekly[,2],type="l",col="red",lwd=2)
points(weeks,TSDataEmergencyAllHospitalsWeekly[,2],type="l",col="blue",lwd=2)
points(weeks,TSDataDiagnosticAllHospitalsWeekly[,2],type="l",col="red",lwd=2)
points(weeks,TSDataOtherHealthProfAllHospitalsWeekly[,2],type="l",col="purple",lwd=2)
points(weeks,TSDataWardAllHospitalsWeekly[,2],type="l",col="gold",lwd=2)
points(weeks,TSDataOutPatientAllHospitalsWeekly[,2],type="l",col="brown",lwd=2)
points(weeks,TSDataConsultantExternalAllHospitalsWeekly[,2],type="l",col="light blue",lwd=2)
legend("topleft", legend=c("GP Referrals", "Consultant-Internal","Emergency","Diagnostics","Other Health Prof","Ward","Outpatient","Consultant-External"),
       col=c("black", "red","blue","red","purple","gold","brown","light blue"), lty=1:1,bty = "n", cex=0.6,lwd=2)

plot(months,TSDataReferralGPAllHospitalsMonthly[,2],type="l",lwd=2,ylim = c(-1,60),xlab="Year",ylab="Referrals",main="Monthly time series for each referral type across all hospitals")
points(months,TSDataConsultantInternalAllHospitalsMonthly[,2],type="l",col="red",lwd=2)
points(months,TSDataEmergencyAllHospitalsMonthly[,2],type="l",col="blue",lwd=2)
points(months,TSDataDiagnosticAllHospitalsMonthly[,2],type="l",col="red",lwd=2)
points(months,TSDataOtherHealthProfAllHospitalsMonthly[,2],type="l",col="purple",lwd=2)
points(months,TSDataWardAllHospitalsMonthly[,2],type="l",col="gold",lwd=2)
points(months,TSDataOutPatientAllHospitalsMonthly[,2],type="l",col="brown",lwd=2)
points(months,TSDataConsultantExternalAllHospitalsMonthly[,2],type="l",col="light blue",lwd=2)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft",legend=c("GP Referrals", "Consultant-Internal","Emergency","Diagnostics","Other Health Prof","Ward","Outpatient","Consultant-External"),
       col=c("black", "red","blue","red","purple","gold","brown","light blue"), lty=1:1,bty = "n", cex=0.60,lwd=2,horiz = T)

#Statistical summaries

#All data
summary(TSDataAllReferralsAllHospitalsDaily[,2])
summary(TSDataAllReferralsAllHospitalsWeekly[,2])
summary(TSDataAllReferralsAllHospitalsMonthly[,2])

#By hospital
# Prince Charles
summary(TSDataAllReferralsPrinceChDaily[,2])
summary(TSDataAllReferralsPrinceChWeekly[,2])
summary(TSDataAllReferralsPrinceChMonthly[,2])
#Royal Glamorgan
summary(TSDataAllReferralsRoyalGlamDaily[,2])
summary(TSDataAllReferralsRoyalGlamWeekly[,2])
summary(TSDataAllReferralsRoyalGlamMonthly[,2])

#Boxplots
layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE))

library(readxl)
sum(dailyrefsall[,1])
DateSuspected_Maindata_unique_dates <- read_excel("DateSuspected Maindata unique dates.xlsx")
dailyrefsall<-cbind(TSDataAllReferralsAllHospitalsDaily[,2],DateSuspected_Maindata_unique_dates[,7])
boxplot(dailyrefsall[,1]~dailyrefsall[,2],xaxt = "n",xlab="Day",ylab="Referrals",main="Boxplot of all referrals across all hospitals by day")
days.of.week <- c("Sun","Mon", "Tues", "Weds", "Thurs", "Fri", "Sat")
axis(1, at=1:7, labels=days.of.week)

dailyrefsPrinceCh<-cbind(TSDataAllReferralsPrinceChDaily[,2],DateSuspected_Maindata_unique_dates[,7])
boxplot(dailyrefsPrinceCh[,1]~dailyrefsPrinceCh[,2],,xaxt = "n",xlab="Day",ylab="Referrals",main="Boxplot of all referrals at Prince Charles by day",col="red")
axis(1, at=1:7, labels=days.of.week)

dailyrefsRoyalGlam<-cbind(TSDataAllReferralsRoyalGlamDaily[,2],DateSuspected_Maindata_unique_dates[,7])
boxplot(dailyrefsRoyalGlam[,1]~dailyrefsRoyalGlam[,2],,xaxt = "n",xlab="Day",ylab="Referrals",main="Boxplot of all referrals at Royal Glamorgan by day",col="blue")
axis(1, at=1:7, labels=days.of.week)

monthlyrefsall<-cbind(TSDataAllReferralsAllHospitalsMonthly[,2],na.omit(DateSuspected_Maindata_unique_dates[,4]))
boxplot(monthlyrefsall[,1]~monthlyrefsall[,2],xaxt = "n",xlab="Month",ylab="Referrals",main="Boxplot of all referrals across all hospitals by month")
axis(1, at=1:12, labels=month.abb[1:12])


monthlyrefsPrinceCh<-cbind(TSDataAllReferralsPrinceChMonthly[,2],na.omit(DateSuspected_Maindata_unique_dates[,4]))
boxplot(monthlyrefsPrinceCh[,1]~monthlyrefsPrinceCh[,2],xaxt = "n",xlab="Month",ylab="Referrals",main="Boxplot of all referrals at Prince Charles by month",col="red")
axis(1, at=1:12, labels=month.abb[1:12])

monthlyrefsRoyalGlam<-cbind(TSDataAllReferralsRoyalGlamMonthly[,2],na.omit(DateSuspected_Maindata_unique_dates[,4]))
boxplot(monthlyrefsRoyalGlam[,1]~monthlyrefsRoyalGlam[,2],xaxt = "n",xlab="Month",ylab="Referrals",main="Boxplot of all referrals at Royal Glamorgan by month",col="blue")
axis(1, at=1:12, labels=month.abb[1:12])



#Summary statistics for Daily, Weekly and Monthly total referrals

summary(TSDataAllReferralsAllHospitalsDaily[,2])

summary(TSDataAllReferralsAllHospitalsWeekly[,2])

summary(TSDataAllReferralsAllHospitalsMonthly[,2])


#Moving Averages

install.packages("forecast")
library(forecast)

#Daily Moving Average 7 days, all referrals, all hospitals


TSDataAllReferralsAllHospitalsDaily7MA<-ma(TSDataAllReferralsAllHospitalsDaily[,2],7)

plot(days,TSDataAllReferralsAllHospitalsDaily[,2],type="l",xlab="Year",ylab="Referrals",main="Daily time series for all referral types across all hospitals")
points(days,TSDataAllReferralsAllHospitalsDaily7MA,col="red",type="l",lwd=3,lty=2)
legend("top", legend=("7 day moving average"),
       col="red", lty=2,bty = "n", cex=1,lwd=3)


#Weekly Moving Average 4 weeks, all referrals, all hospitals
TSDataAllReferralsAllHospitalsWeekly4MA<-ma(TSDataAllReferralsAllHospitalsWeekly[,2],4)

plot(weeks,TSDataAllReferralsAllHospitalsWeekly[,2],type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types across all hospitals")
points(weeks,TSDataAllReferralsAllHospitalsWeekly4MA,col="red",type="l",lwd=3,lty=2)
legend("top", legend=("4 week moving average"),
       col="red", lty=2,bty = "n", cex=1,lwd=2)

#Monthly Moving Average 3 months, all referrals, all hospitals
TSDataAllReferralsAllHospitalsMonthly3MA<-ma(TSDataAllReferralsAllHospitalsMonthly[,2],3)

plot(months,TSDataAllReferralsAllHospitalsMonthly[,2],type="l",xlab="Year",ylab="Referrals",main="Monthly time series for all referral types across all hospitals")
points(months,TSDataAllReferralsAllHospitalsMonthly3MA,col="red",type="l",lwd=3,lty=2)
legend("top", legend=("3 month moving average"),
       col="red", lty=2,bty = "n", cex=1,lwd=2)

#Daily Moving Average 7 days, all referrals, by hospital
TSDataAllReferralsPrinceChDaily7MA<-ma(TSDataAllReferralsPrinceChDaily[,2],7)
TSDataAllReferralsRoyalGlamDaily7MA<-ma(TSDataAllReferralsRoyalGlamDaily[,2],7)

#Prince Charles
plot(days,TSDataAllReferralsAllHospitalsDaily[,2],type="l",xlab="Year",ylab="Referrals",main="Daily time series for all referral types across all hospitals")
points(days,TSDataAllReferralsPrinceChDaily7MA,col="red",type="l",lwd=3,lty=2)
legend("topleft",legend="7 day moving average: Prince Charles",col="red", lty=1:1,bty = "n", cex=0.9,lwd=3)

plot(weeks,TSDataAllReferralsAllHospitalsWeekly[,2],type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types across all hospitals",ylim=c(0,25))
points(weeks,TSDataAllReferralsPrinceChWeekly4MA,col="red",type="l",lwd=3)
legend("top", legend="4 week moving average: Prince Charles",col="red", lty=1:1,bty = "n", cex=0.9,lwd=3,horiz = T)

plot(months,TSDataAllReferralsAllHospitalsMonthly[,2],type="l",xlab="Year",ylab="Referrals",main="Monthly time series for all referral types across all hospitals",ylim=c(0,100))
points(months,TSDataAllReferralsPrinceChMonthly3MA,col="red",type="l",lwd=3,lty=2)
legend("top", legend="3 month moving average: Prince Charles",col="red", lty=2,bty = "n", cex=0.9,lwd=3,horiz = T)


#Royal Glamorgan
plot(days,TSDataAllReferralsAllHospitalsDaily[,2],type="l",xlab="Year",ylab="Referrals",main="Daily time series for all referral types across all hospitals")
points(days,TSDataAllReferralsRoyalGlamDaily7MA,col="blue",type="l",lwd=3)
legend("top", legend="7 day moving average: Royal Glamorgan",col="blue", lty=1:1,bty = "n", cex=0.9,lwd=3,horiz = T)

plot(weeks,TSDataAllReferralsAllHospitalsWeekly[,2],type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types across all hospitals",ylim=c(0,25))
points(weeks,TSDataAllReferralsRoyalGlamWeekly4MA,col="blue",type="l",lwd=3)
legend("top", legend="4 week moving average: Royal Glamorgan",col="blue", lty=1:1,bty = "n", cex=0.9,lwd=3,horiz = T)

plot(months,TSDataAllReferralsAllHospitalsMonthly[,2],type="l",xlab="Year",ylab="Referrals",main="Monthly time series for all referral types across all hospitals",ylim=c(0,100))
points(months,TSDataAllReferralsRoyalGlamMonthly3MA,col="blue",type="l",lwd=3)
legend("top", legend="3 month moving average: Royal Glamorgan",col="blue", lty=1:1,bty = "n", cex=0.9,lwd=3,horiz = T)


#Weekly Moving Average 4 weeks, all referrals, by hospital
TSDataAllReferralsPrinceChWeekly4MA<-ma(TSDataAllReferralsPrinceChWeekly[,2],4)
TSDataAllReferralsRoyalGlamWeekly4MA<-ma(TSDataAllReferralsRoyalGlamWeekly[,2],4)

plot(weeks,TSDataAllReferralsAllHospitalsWeekly[,2],type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types across all hospitals",ylim=c(0,30))
points(weeks,TSDataAllReferralsPrinceChWeekly4MA,col="red",type="l",lwd=3)
points(weeks,TSDataAllReferralsRoyalGlamWeekly4MA,col="blue",type="l",lwd=3)
legend("top", legend=c("4 week moving average: Prince Charles","4 week moving average: Royal Glamorgan"),col=c("red","blue"), lty=1:1,bty = "n", cex=0.9,lwd=3,horiz = T)

#Monthly Moving Average 3 months, all referrals, by hospital
TSDataAllReferralsPrinceChMonthly3MA<-ma(TSDataAllReferralsPrinceChMonthly[,2],3)
TSDataAllReferralsRoyalGlamMonthly3MA<-ma(TSDataAllReferralsRoyalGlamMonthly[,2],3)

plot(months,TSDataAllReferralsAllHospitalsMonthly[,2],type="l",xlab="Year",ylab="Referrals",main="Monthly time series for all referral types across all hospitals",ylim=c(10,100))
points(months,TSDataAllReferralsPrinceChMonthly3MA,col="red",type="l",lwd=3)
points(months,TSDataAllReferralsRoyalGlamMonthly3MA,col="blue",type="l",lwd=3)
legend("top", legend=c("3 month moving average: Prince Charles","3 month moving average: Royal Glamorgan"),col=c("red","blue"), lty=1:1,bty = "n", cex=0.85,lwd=3,horiz = T)




#Daily Moving Average 30 days, all referrals, all hospitals
TSDataAllReferralsAllHospitalsDaily30MA<-ma(TSDataAllReferralsAllHospitalsDaily[,2],30)

plot(days,TSDataAllReferralsAllHospitalsDaily[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Daily time series for all referral types across all hospitals")
points(days,TSDataAllReferralsAllHospitalsDaily30MA,col="red",type="l",lwd=2)
legend("top", legend=("30 day moving average"),
       col="red", lty=1:1,bty = "n", cex=0.8,lwd=2)

#Daily Moving Average 90 days, all referrals, all hospitals
TSDataAllReferralsAllHospitalsDaily90MA<-ma(TSDataAllReferralsAllHospitalsDaily[,2],90)

plot(days,TSDataAllReferralsAllHospitalsDaily[,2],type="l",lwd=2,xlab="Year",ylab="Referrals",main="Daily time series for all referral types across all hospitals")
points(days,TSDataAllReferralsAllHospitalsDaily90MA,col="red",type="l",lwd=2)
legend("top", legend=("90 day moving average"),
       col="red", lty=1:1,bty = "n", cex=0.8,lwd=2)

#Weekly Moving Average 5 weeks, all referrals, all hospitals

TSDataAllReferralsAllHospitalsWeekly5MA<-ma(TSDataAllReferralsAllHospitalsWeekly[,2],5)

plot(weeks,TSDataAllReferralsAllHospitalsWeekly[,2],type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types across all hospitals")
points(weeks,TSDataAllReferralsAllHospitalsWeekly5MA,col="red",type="l",lwd=3)
legend("top", legend=("5 week moving average"),
       col="red", lty=1:1,bty = "n", cex=0.8,lwd=2)

#Weekly Moving Average 10 weeks, all referrals, all hospitals

TSDataAllReferralsAllHospitalsWeekly10MA<-ma(TSDataAllReferralsAllHospitalsWeekly[,2],10)

plot(weeks,TSDataAllReferralsAllHospitalsWeekly[,2],type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types across all hospitals")
points(weeks,TSDataAllReferralsAllHospitalsWeekly10MA,col="red",type="l",lwd=3)
legend("top", legend=("10 week moving average"),
       col="red", lty=1:1,bty = "n", cex=0.8,lwd=2)

#Weekly Moving Average 10 weeks, all referrals, all hospitals

TSDataAllReferralsAllHospitalsWeekly20MA<-ma(TSDataAllReferralsAllHospitalsWeekly[,2],20)

plot(weeks,TSDataAllReferralsAllHospitalsWeekly[,2],type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types across all hospitals")
points(weeks,TSDataAllReferralsAllHospitalsWeekly20MA,col="red",type="l",lwd=3)
legend("top", legend=("20 week moving average"),
       col="red", lty=1:1,bty = "n", cex=0.8,lwd=2)


#Monthly Moving Average 3 months, all referrals, all hospitals

TSDataAllReferralsAllHospitalsMonthly3MA<-ma(TSDataAllReferralsAllHospitalsMonthly[,2],3)

plot(months,TSDataAllReferralsAllHospitalsMonthly[,2],type="l",xlab="Year",ylab="Referrals",main="Monthly time series for all referral types across all hospitals")
points(months,TSDataAllReferralsAllHospitalsMonthly3MA,col="red",type="l",lwd=3)
legend("top", legend=("3 month moving average"),
       col="red", lty=1:1,bty = "n", cex=0.8,lwd=2)

#Monthly Moving Average 5 months, all referrals, all hospitals

TSDataAllReferralsAllHospitalsMonthly5MA<-ma(TSDataAllReferralsAllHospitalsMonthly[,2],5)

plot(months,TSDataAllReferralsAllHospitalsMonthly[,2],type="l",xlab="Year",ylab="Referrals",main="Monthly time series for all referral types across all hospitals")
points(months,TSDataAllReferralsAllHospitalsMonthly5MA,col="red",type="l",lwd=3)
legend("top", legend=("5 month moving average"),
       col="red", lty=1:1,bty = "n", cex=0.8,lwd=2)

#Monthly Moving Average 10 months, all referrals, all hospitals

TSDataAllReferralsAllHospitalsMonthly10MA<-ma(TSDataAllReferralsAllHospitalsMonthly[,2],10)

plot(months,TSDataAllReferralsAllHospitalsMonthly[,2],type="l",xlab="Year",ylab="Referrals",main="Monthly time series for all referral types across all hospitals")
points(months,TSDataAllReferralsAllHospitalsMonthly10MA,col="red",type="l",lwd=3)
legend("top", legend=("10 month moving average"),
       col="red", lty=1:1,bty = "n", cex=0.8,lwd=2)

lm(TSDataAllReferralsAllHospitalsDaily[,2]~days)


#Augmented Dickey-Fuller Test for stationarity

library(tseries)

#Create time series and examine decompositions
#All hospitals
#Daily 
#frequency set as largest possible value for time series to have 2 periods
TimeSeriesAllReferralsAllHospitalsDaily<-ts(TSDataAllReferralsAllHospitalsDaily[,2],start=c(2018,01,02),freq=349)

TimeSeriesAllReferralsPrinceChDaily<-ts(TSDataAllReferralsPrinceChDaily[,2],start=c(2018,01,02),freq=349)

TimeSeriesAllReferralsRoyalGlamDaily<-ts(TSDataAllReferralsRoyalGlamDaily[,2],start=c(2018,01,02),freq=349)

#Weekly
#frequency set as largest possible value for time series to have 2 periods
TimeSeriesAllReferralsAllHospitalsWeekly<-ts(TSDataAllReferralsAllHospitalsWeekly[,2],start=c(2018,01,02),freq=50)

TimeSeriesAllReferralsPrinceChWeekly<-ts(TSDataAllReferralsPrinceChWeekly[,2],start=c(2018,01,02),freq=50)

TimeSeriesAllReferralsRoyalGlamWeekly<-ts(TSDataAllReferralsRoyalGlamWeekly[,2],start=c(2018,01,02),freq=50)

#Monthly
#frequency set as largest possible value for time series to have 2 periods

TimeSeriesAllReferralsAllHospitalsMonthly<-ts(TSDataAllReferralsAllHospitalsMonthly[,2],start=c(2018,01,02),freq=11)

TimeSeriesAllReferralsPrinceChMonthly<-ts(TSDataAllReferralsPrinceChMonthly[,2],start=c(2018,01,02),freq=11)

TimeSeriesAllReferralsRoyalGlamMonthly<-ts(TSDataAllReferralsRoyalGlamMonthly[,2],start=c(2018,01,02),freq=11)

adf.test(TimeSeriesAllReferralsAllHospitalsDaily)

adf.test(TimeSeriesAllReferralsAllHospitalsWeekly)
adf.test(TimeSeriesAllReferralsPrinceChWeekly)
adf.test(TimeSeriesAllReferralsRoyalGlamWeekly)

adf.test(TimeSeriesAllReferralsAllHospitalsMonthly)
adf.test(TimeSeriesAllReferralsPrinceChMonthly)
adf.test(TimeSeriesAllReferralsRoyalGlamMonthly)



#frequency set as lowest possible value for time series to have 2 periods

decompose(TimeSeriesAllReferralsAllHospitalsMonthly)
plot(decompose(TimeSeriesAllReferralsAllHospitalsMonthly),xaxt="n")

deco<-decompose(TimeSeriesAllReferralsAllHospitalsWeekly)
mean(na.omit(deco$random))
plot(decompose(TimeSeriesAllReferralsAllHospitalsWeekly),xaxt="n")

deco<-decompose(TimeSeriesAllReferralsAllHospitalsMonthly)
plot(decompose(TimeSeriesAllReferralsAllHospitalsMonthly),xaxt="n")

plot(decompose(TimeSeriesAllReferralsPrinceChWeekly))
plot(decompose(TimeSeriesAllReferralsPrinceChMonthly))

plot(decompose(TimeSeriesAllReferralsRoyalGlamWeekly))
plot(decompose(TimeSeriesAllReferralsRoyalGlamMonthly))

#Decomposition of Princess of Wales to investigate
LungSuspicionPrincess<-LungSuspicion[LungSuspicion$CareFacility=="Princess Of Wales Hospital",]

#Weekly
TSDataAllReferralsPrincessWeekly<-cbind(seq(1:100),as.integer(tabulate(LungSuspicionPrincess$Week,100)))
TimeSeriesAllReferralsPrincessWeekly<-ts(TSDataAllReferralsPrincessWeekly[,2],start=c(2018,01,02),freq=50)

plot(decompose(TimeSeriesAllReferralsPrincessWeekly))


plot(decompose(TimeSeriesAllReferralsPrincessMonthly))

#Split data into training set (75%) and test set (25%)
library(forecast)
TSDataAllReferralsAllHospitalsDailyTrain<-subset(TSDataAllReferralsAllHospitalsDaily, end=length(TSDataAllReferralsAllHospitalsDaily)*0.75)
TSDataAllReferralsAllHospitalsDailyTest<-subset(TSDataAllReferralsAllHospitalsDaily, start=length(TSDataAllReferralsAllHospitalsDaily)*0.75)

TimeSeriesAllReferralsAllHospitalsDailyTrain<-subset(TimeSeriesAllReferralsAllHospitalsDaily, end=length(TimeSeriesAllReferralsAllHospitalsDaily)*0.75)
TimeSeriesAllReferralsAllHospitalsDailyTest<-subset(TimeSeriesAllReferralsAllHospitalsDaily, start=length(TimeSeriesAllReferralsAllHospitalsDaily)*0.75)

TSDataAllReferralsAllHospitalsWeeklyTrain<-subset(TSDataAllReferralsAllHospitalsWeekly, end=length(TSDataAllReferralsAllHospitalsWeekly)*0.75)
TSDataAllReferralsAllHospitalsWeeklyTest<-subset(TSDataAllReferralsAllHospitalsWeekly, start=length(TSDataAllReferralsAllHospitalsWeekly)*0.75)

TimeSeriesAllReferralsAllHospitalsWeeklyTrain<-subset(TimeSeriesAllReferralsAllHospitalsWeekly, end=length(TimeSeriesAllReferralsAllHospitalsWeekly)*0.75)
TimeSeriesAllReferralsAllHospitalsWeeklyTest<-subset(TimeSeriesAllReferralsAllHospitalsWeekly, start=length(TimeSeriesAllReferralsAllHospitalsWeekly)*0.75)

TSDataAllReferralsAllHospitalsMonthlyTrain<-subset(TSDataAllReferralsAllHospitalsMonthly, end=length(TSDataAllReferralsAllHospitalsMonthly)*0.75)
TSDataAllReferralsAllHospitalsMonthlyTest<-subset(TSDataAllReferralsAllHospitalsMonthly, start=length(TSDataAllReferralsAllHospitalsMonthly)*0.75)

TimeSeriesAllReferralsAllHospitalsMonthlyTrain<-subset(TimeSeriesAllReferralsAllHospitalsMonthly, end=length(TimeSeriesAllReferralsAllHospitalsMonthly)*0.75)
TimeSeriesAllReferralsAllHospitalsMonthlyTest<-subset(TimeSeriesAllReferralsAllHospitalsMonthly, start=length(TimeSeriesAllReferralsAllHospitalsMonthly)*0.75)


TSDataAllReferralsPrinceChDailyTrain<-subset(TSDataAllReferralsPrinceChDaily, end=length(TSDataAllReferralsPrinceChDaily)*0.75)
TSDataAllReferralsPrinceChDailyTest<-subset(TSDataAllReferralsPrinceChDaily, start=length(TSDataAllReferralsPrinceChDaily)*0.75)

TimeSeriesAllReferralsPrinceChDailyTrain<-subset(TimeSeriesAllReferralsPrinceChDaily, end=length(TimeSeriesAllReferralsPrinceChDaily)*0.75)
TimeSeriesAllReferralsPrinceChDailyTest<-subset(TimeSeriesAllReferralsPrinceChDaily, start=length(TimeSeriesAllReferralsPrinceChDaily)*0.75)

TSDataAllReferralsPrinceChWeeklyTrain<-subset(TSDataAllReferralsPrinceChWeekly, end=length(TSDataAllReferralsPrinceChWeekly)*0.75)
TSDataAllReferralsPrinceChWeeklyTest<-subset(TSDataAllReferralsPrinceChWeekly, start=length(TSDataAllReferralsPrinceChWeekly)*0.75)

TimeSeriesAllReferralsPrinceChWeeklyTrain<-subset(TimeSeriesAllReferralsPrinceChWeekly, end=length(TimeSeriesAllReferralsPrinceChWeekly)*0.75)
TimeSeriesAllReferralsPrinceChWeeklyTest<-subset(TimeSeriesAllReferralsPrinceChWeekly, start=length(TimeSeriesAllReferralsPrinceChWeekly)*0.75)

TSDataAllReferralsPrinceChMonthlyTrain<-subset(TSDataAllReferralsPrinceChMonthly, end=length(TSDataAllReferralsPrinceChMonthly)*0.75)
TSDataAllReferralsPrinceChMonthlyTest<-subset(TSDataAllReferralsPrinceChMonthly, start=length(TSDataAllReferralsPrinceChMonthly)*0.75)

TimeSeriesAllReferralsPrinceChMonthlyTrain<-subset(TimeSeriesAllReferralsPrinceChMonthly, end=length(TimeSeriesAllReferralsPrinceChMonthly)*0.75)
TimeSeriesAllReferralsPrinceChMonthlyTest<-subset(TimeSeriesAllReferralsPrinceChMonthly, start=length(TimeSeriesAllReferralsPrinceChMonthly)*0.75)


TSDataAllReferralsRoyalGlamDailyTrain<-subset(TSDataAllReferralsRoyalGlamDaily, end=length(TSDataAllReferralsRoyalGlamDaily)*0.75)
TSDataAllReferralsRoyalGlamDailyTest<-subset(TSDataAllReferralsRoyalGlamDaily, start=length(TSDataAllReferralsRoyalGlamDaily)*0.75)

TimeSeriesAllReferralsRoyalGlamDailyTrain<-subset(TimeSeriesAllReferralsRoyalGlamDaily, end=length(TimeSeriesAllReferralsRoyalGlamDaily)*0.75)
TimeSeriesAllReferralsRoyalGlamDailyTest<-subset(TimeSeriesAllReferralsRoyalGlamDaily, start=length(TimeSeriesAllReferralsRoyalGlamDaily)*0.75)

TSDataAllReferralsRoyalGlamWeeklyTrain<-subset(TSDataAllReferralsRoyalGlamWeekly, end=length(TSDataAllReferralsRoyalGlamWeekly)*0.75)
TSDataAllReferralsRoyalGlamWeeklyTest<-subset(TSDataAllReferralsRoyalGlamWeekly, start=length(TSDataAllReferralsRoyalGlamWeekly)*0.75)

TimeSeriesAllReferralsRoyalGlamWeeklyTrain<-subset(TimeSeriesAllReferralsRoyalGlamWeekly, end=length(TimeSeriesAllReferralsRoyalGlamWeekly)*0.75)
TimeSeriesAllReferralsRoyalGlamWeeklyTest<-subset(TimeSeriesAllReferralsRoyalGlamWeekly, start=length(TimeSeriesAllReferralsRoyalGlamWeekly)*0.75)

TSDataAllReferralsRoyalGlamMonthlyTrain<-subset(TSDataAllReferralsRoyalGlamMonthly, end=length(TSDataAllReferralsRoyalGlamMonthly)*0.75)
TSDataAllReferralsRoyalGlamMonthlyTest<-subset(TSDataAllReferralsRoyalGlamMonthly, start=length(TSDataAllReferralsRoyalGlamMonthly)*0.75)

TimeSeriesAllReferralsRoyalGlamMonthlyTrain<-subset(TimeSeriesAllReferralsRoyalGlamMonthly, end=length(TimeSeriesAllReferralsRoyalGlamMonthly)*0.75)
TimeSeriesAllReferralsRoyalGlamMonthlyTest<-subset(TimeSeriesAllReferralsRoyalGlamMonthly, start=length(TimeSeriesAllReferralsRoyalGlamMonthly)*0.75)


#Other Modelling methods:

#Naive baseline

#Daily Naive

TimeSeriesAllReferralsAllHospitalsDailyNaive<-naive(TimeSeriesAllReferralsAllHospitalsDailyTrain)
plot(TimeSeriesAllReferralsAllHospitalsDailyNaive,ylim=c(0,15),main="Naive method on all daily referrals across all hospitals",xlab="Year",ylab="Referrals")


TimeSeriesAllReferralsPrinceChDailyNaive<-naive(TimeSeriesAllReferralsPrinceChDailyTrain)
plot(TimeSeriesAllReferralsPrinceChDailyNaive,ylim=c(0,15),main="Naive method on all daily referrals across all hospitals",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsRoyalGlamDailyNaive<-naive(TimeSeriesAllReferralsRoyalGlamDailyTrain)
plot(TimeSeriesAllReferralsRoyalGlamDailyNaive,ylim=c(0,15),main="Naive method on all daily referrals across all hospitals",xlab="Year",ylab="Referrals")




#Weekly


TimeSeriesAllReferralsAllHospitalsWeeklyTrain<-subset(TimeSeriesAllReferralsAllHospitalsWeekly, end=length(TimeSeriesAllReferralsAllHospitalsWeekly)*0.75)
TimeSeriesAllReferralsAllHospitalsWeeklyTest<-subset(TimeSeriesAllReferralsAllHospitalsWeekly, start=1+length(TimeSeriesAllReferralsAllHospitalsWeekly)*0.75)

TimeSeriesAllReferralsAllHospitalsWeeklyNaive<-naive(TimeSeriesAllReferralsAllHospitalsWeeklyTrain)

plot(TimeSeriesAllReferralsAllHospitalsWeeklyNaive,ylim=c(0,30),main="Naive method on all weekly referrals across all hospitals",xlab="Year",ylab="Referrals")


TimeSeriesAllReferralsPrinceChWeeklyTrain<-subset(TimeSeriesAllReferralsPrinceChWeekly, end=length(TimeSeriesAllReferralsPrinceChWeekly)*0.75)
TimeSeriesAllReferralsPrinceChWeeklyTest<-subset(TimeSeriesAllReferralsPrinceChWeekly, start=1+length(TimeSeriesAllReferralsPrinceChWeekly)*0.75)

TimeSeriesAllReferralsPrinceChWeeklyNaive<-naive(TimeSeriesAllReferralsPrinceChWeeklyTrain)

plot(TimeSeriesAllReferralsPrinceChWeeklyNaive,ylim=c(0,30),main="Naive method on all weekly referrals in Prince Charles",xlab="Year",ylab="Referrals")


TimeSeriesAllReferralsRoyalGlamWeeklyTrain<-subset(TimeSeriesAllReferralsRoyalGlamWeekly, end=length(TimeSeriesAllReferralsRoyalGlamWeekly)*0.75)
TimeSeriesAllReferralsRoyalGlamWeeklyTest<-subset(TimeSeriesAllReferralsRoyalGlamWeekly, start=1+length(TimeSeriesAllReferralsRoyalGlamWeekly)*0.75)

TimeSeriesAllReferralsRoyalGlamWeeklyNaive<-naive(TimeSeriesAllReferralsRoyalGlamWeeklyTrain)

plot(TimeSeriesAllReferralsRoyalGlamWeeklyNaive,ylim=c(0,30),main="Naive method on all weekly referrals in Royal Glamorgan",xlab="Year",ylab="Referrals")



TimeSeriesAllReferralsAllHospitalsMonthlyTrain<-subset(TimeSeriesAllReferralsAllHospitalsMonthly, end=length(TimeSeriesAllReferralsAllHospitalsMonthly)*0.75)
TimeSeriesAllReferralsAllHospitalsMonthlyTest<-subset(TimeSeriesAllReferralsAllHospitalsMonthly, start=1+length(TimeSeriesAllReferralsAllHospitalsMonthly)*0.75)

TimeSeriesAllReferralsAllHospitalsMonthlyNaive<-naive(TimeSeriesAllReferralsAllHospitalsMonthlyTrain)

plot(TimeSeriesAllReferralsAllHospitalsMonthlyNaive,ylim=c(0,150),main="Naive method on all monthly referrals across all hospitals",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsPrinceChMonthlyTrain<-subset(TimeSeriesAllReferralsPrinceChMonthly, end=length(TimeSeriesAllReferralsPrinceChMonthly)*0.75)
TimeSeriesAllReferralsPrinceChMonthlyTest<-subset(TimeSeriesAllReferralsPrinceChMonthly, start=1+length(TimeSeriesAllReferralsPrinceChMonthly)*0.75)

TimeSeriesAllReferralsPrinceChMonthlyNaive<-naive(TimeSeriesAllReferralsPrinceChMonthlyTrain)

plot(TimeSeriesAllReferralsPrinceChMonthlyNaive,ylim=c(0,150),main="Naive method on all monthly referrals in Prince Charles",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsRoyalGlamMonthlyTrain<-subset(TimeSeriesAllReferralsRoyalGlamMonthly, end=length(TimeSeriesAllReferralsRoyalGlamMonthly)*0.75)
TimeSeriesAllReferralsRoyalGlamMonthlyTest<-subset(TimeSeriesAllReferralsRoyalGlamMonthly, start=1+length(TimeSeriesAllReferralsRoyalGlamMonthly)*0.75)

TimeSeriesAllReferralsRoyalGlamMonthlyNaive<-naive(TimeSeriesAllReferralsRoyalGlamMonthlyTrain)

plot(TimeSeriesAllReferralsRoyalGlamMonthlyNaive,ylim=c(0,150),main="Naive method on all monthly referrals in Royal Glamorgan",xlab="Year",ylab="Referrals")


#Seasonal naive

#Daily
TimeSeriesAllReferralsAllHospitalsDailySNaive<-snaive(TimeSeriesAllReferralsAllHospitalsDailyTrain,h=10)
plot(TimeSeriesAllReferralsAllHospitalsDailySNaive,ylim=c(0,15),main="Daily seasonal naive method on all referrals across all hospitals",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsPrinceChDailySNaive<-snaive(TimeSeriesAllReferralsPrinceChDailyTrain)
plot(TimeSeriesAllReferralsPrinceChDailySNaive,ylim=c(0,15),main="Daily seasonal naive method on all referrals in Prince Charles",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsRoyalGlamDailySNaive<-snaive(TimeSeriesAllReferralsRoyalGlamDailyTrain)
plot(TimeSeriesAllReferralsRoyalGlamDailySNaive,ylim=c(0,15),main="Daily seasonal naive method on all referrals in Royal Glamorgan",xlab="Year",ylab="Referrals")


#Weekly
TimeSeriesAllReferralsAllHospitalsWeeklyTrain<-subset(TimeSeriesAllReferralsAllHospitalsWeekly, end=length(TimeSeriesAllReferralsAllHospitalsWeekly)*0.75)
TimeSeriesAllReferralsAllHospitalsWeeklyTest<-subset(TimeSeriesAllReferralsAllHospitalsWeekly, start=1+length(TimeSeriesAllReferralsAllHospitalsWeekly)*0.75)

TimeSeriesAllReferralsAllHospitalsWeeklySNaive<-snaive(TimeSeriesAllReferralsAllHospitalsWeeklyTrain,h=10)

plot(TimeSeriesAllReferralsAllHospitalsWeeklySNaive,ylim=c(0,30),main="Weekly seasonal naive method on all referrals across all hospitals",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsPrinceChWeeklyTrain<-subset(TimeSeriesAllReferralsPrinceChWeekly, end=length(TimeSeriesAllReferralsPrinceChWeekly)*0.75)
TimeSeriesAllReferralsPrinceChWeeklyTest<-subset(TimeSeriesAllReferralsPrinceChWeekly, start=1+length(TimeSeriesAllReferralsPrinceChWeekly)*0.75)

TimeSeriesAllReferralsPrinceChWeeklySNaive<-snaive(TimeSeriesAllReferralsPrinceChWeeklyTrain)

plot(TimeSeriesAllReferralsPrinceChWeeklySNaive,ylim=c(0,30),main="Weekly seasonal naive method on all referrals in Prince Charles",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsRoyalGlamWeeklyTrain<-subset(TimeSeriesAllReferralsRoyalGlamWeekly, end=length(TimeSeriesAllReferralsRoyalGlamWeekly)*0.75)
TimeSeriesAllReferralsRoyalGlamWeeklyTest<-subset(TimeSeriesAllReferralsRoyalGlamWeekly, start=1+length(TimeSeriesAllReferralsRoyalGlamWeekly)*0.75)

TimeSeriesAllReferralsRoyalGlamWeeklySNaive<-snaive(TimeSeriesAllReferralsRoyalGlamWeeklyTrain)

plot(TimeSeriesAllReferralsRoyalGlamWeeklySNaive,ylim=c(0,30),main="Weekly seasonal naive method on all referrals in Royal Glamorgan",xlab="Year",ylab="Referrals")


#Monthly
TimeSeriesAllReferralsAllHospitalsMonthly<-ts(TSDataAllReferralsAllHospitalsMonthly[,2],start=c(2018,01,02),freq=12)

TimeSeriesAllReferralsAllHospitalsMonthlyTrain<-subset(TimeSeriesAllReferralsAllHospitalsMonthly, end=length(TimeSeriesAllReferralsAllHospitalsMonthly)*0.75)
TimeSeriesAllReferralsAllHospitalsMonthlyTest<-subset(TimeSeriesAllReferralsAllHospitalsMonthly, start=1+length(TimeSeriesAllReferralsAllHospitalsMonthly)*0.75)

TimeSeriesAllReferralsAllHospitalsMonthlySNaive<-snaive(TimeSeriesAllReferralsAllHospitalsMonthlyTrain,h=10)

plot(TimeSeriesAllReferralsAllHospitalsMonthlySNaive,ylim=c(0,150),main="Monthly seasonal naive method on all referrals across all hospitals",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsPrinceChMonthly<-ts(TSDataAllReferralsPrinceChMonthly[,2],start=c(2018,01,02),freq=12)

TimeSeriesAllReferralsPrinceChMonthlyTrain<-subset(TimeSeriesAllReferralsPrinceChMonthly, end=length(TimeSeriesAllReferralsPrinceChMonthly)*0.75)
TimeSeriesAllReferralsPrinceChMonthlyTest<-subset(TimeSeriesAllReferralsPrinceChMonthly, start=1+length(TimeSeriesAllReferralsPrinceChMonthly)*0.75)

TimeSeriesAllReferralsPrinceChMonthlySNaive<-snaive(TimeSeriesAllReferralsPrinceChMonthlyTrain)

plot(TimeSeriesAllReferralsPrinceChMonthlySNaive,ylim=c(0,150),lwd=2,main="Monthly seasonal naive method on all referrals in Prince Charles",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsRoyalGlamMonthly<-ts(TSDataAllReferralsRoyalGlamMonthly[,2],start=c(2018,01,02),freq=12)

TimeSeriesAllReferralsRoyalGlamMonthlyTrain<-subset(TimeSeriesAllReferralsRoyalGlamMonthly, end=length(TimeSeriesAllReferralsRoyalGlamMonthly)*0.75)
TimeSeriesAllReferralsRoyalGlamMonthlyTest<-subset(TimeSeriesAllReferralsRoyalGlamMonthly, start=1+length(TimeSeriesAllReferralsRoyalGlamMonthly)*0.75)

TimeSeriesAllReferralsRoyalGlamMonthlySNaive<-snaive(TimeSeriesAllReferralsRoyalGlamMonthlyTrain)

plot(TimeSeriesAllReferralsRoyalGlamMonthlySNaive,ylim=c(0,150),lwd=2,main="Monthly seasonal naive method on all referrals in Royal Glamorgan",xlab="Year",ylab="Referrals")


#SES

#Daily
TimeSeriesAllReferralsAllHospitalsDailySES<-ses(TimeSeriesAllReferralsAllHospitalsDailyTrain)

plot(TimeSeriesAllReferralsAllHospitalsDailySES,ylim=c(0,10),main="Daily SES method on all referrals across all hospitals",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsPrinceChDailySES<-ses(TimeSeriesAllReferralsPrinceChDailyTrain)

plot(TimeSeriesAllReferralsPrinceChDailySES,ylim=c(0,10),main="Daily SES method on all referrals in Prince Charles",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsRoyalGlamDailySES<-ses(TimeSeriesAllReferralsRoyalGlamDailyTrain)

plot(TimeSeriesAllReferralsRoyalGlamDailySES,ylim=c(0,10),main="Daily SES method on all referrals in Royal Glamorgan",xlab="Year",ylab="Referrals")

#Weekly
TimeSeriesAllReferralsAllHospitalsWeeklySES<-ses(TimeSeriesAllReferralsAllHospitalsWeeklyTrain)

plot(TimeSeriesAllReferralsAllHospitalsWeeklySES,ylim=c(0,30),main="Weekly SES method on all referrals across all hospitals",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsPrinceChWeeklySES<-ses(TimeSeriesAllReferralsPrinceChWeeklyTrain)

plot(TimeSeriesAllReferralsPrinceChWeeklySES,ylim=c(0,30),main="Weekly SES method on all referrals in Prince Charles",xlab="Year",ylab="Referrals")


TimeSeriesAllReferralsRoyalGlamWeeklySES<-ses(TimeSeriesAllReferralsRoyalGlamWeeklyTrain)

plot(TimeSeriesAllReferralsRoyalGlamWeeklySES,ylim=c(0,30),main="Weekly SES method on all referrals in Royal Glamorgan",xlab="Year",ylab="Referrals")

#Monthly
TimeSeriesAllReferralsAllHospitalsMonthlySES<-ses(TimeSeriesAllReferralsAllHospitalsMonthlyTrain)

plot(TimeSeriesAllReferralsAllHospitalsMonthlySES,ylim=c(0,150),main="Monthly SES method on all referrals across all hospitals",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsPrinceChMonthlySES<-ses(TimeSeriesAllReferralsPrinceChMonthlyTrain)

plot(TimeSeriesAllReferralsPrinceChMonthlySES,ylim=c(0,150),main="Monthly SES method on all referrals in Prince Charles",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsRoyalGlamMonthlySES<-ses(TimeSeriesAllReferralsRoyalGlamMonthlyTrain)

plot(TimeSeriesAllReferralsRoyalGlamMonthlySES,ylim=c(0,150),main="Monthly SES method on all referrals in Royal Glamorgan",xlab="Year",ylab="Referrals")

#Holt linear
#Daily
TimeSeriesAllReferralsAllHospitalsDailyHolt<-holt(TimeSeriesAllReferralsAllHospitalsDailyTrain)

plot(TimeSeriesAllReferralsAllHospitalsDailyHolt,ylim=c(0,10),main="Daily Holt method on all referrals across all hospitals",xlab="Year",ylab="Referrals")


TimeSeriesAllReferralsPrinceChDailyHolt<-holt(TimeSeriesAllReferralsPrinceChDailyTrain)

plot(TimeSeriesAllReferralsPrinceChDailyHolt,ylim=c(0,10),main="Daily Holt method on all referrals in Prince Charles",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsRoyalGlamDailyHolt<-holt(TimeSeriesAllReferralsRoyalGlamDailyTrain)

plot(TimeSeriesAllReferralsRoyalGlamDailyHolt,ylim=c(0,10),main="Daily Holt method on all referrals in Royal Glamorgan",xlab="Year",ylab="Referrals")


#Weekly
TimeSeriesAllReferralsAllHospitalsWeeklyHolt<-holt(TimeSeriesAllReferralsAllHospitalsWeeklyTrain)

plot(TimeSeriesAllReferralsAllHospitalsWeeklyHolt,ylim=c(0,45),main="Weekly Holt method on all referrals across all hospitals",xlab="Year",ylab="Referrals")


TimeSeriesAllReferralsPrinceChWeeklyHolt<-holt(TimeSeriesAllReferralsPrinceChWeeklyTrain)

plot(TimeSeriesAllReferralsPrinceChWeeklyHolt,ylim=c(0,45),main="Weekly Holt method on all referrals in Prince Charles",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsRoyalGlamWeeklyHolt<-holt(TimeSeriesAllReferralsRoyalGlamWeeklyTrain)

plot(TimeSeriesAllReferralsRoyalGlamWeeklyHolt,ylim=c(0,45),main="Weekly Holt method on all referrals in Royal Glamorgan",xlab="Year",ylab="Referrals")

#Monthly
TimeSeriesAllReferralsAllHospitalsMonthlyHolt<-holt(TimeSeriesAllReferralsAllHospitalsMonthlyTrain)

plot(TimeSeriesAllReferralsAllHospitalsMonthlyHolt,ylim=c(0,150),main="Monthly Holt method on all referrals across all hospitals",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsPrinceChMonthlyHolt<-holt(TimeSeriesAllReferralsPrinceChMonthlyTrain)

plot(TimeSeriesAllReferralsPrinceChMonthlyHolt,ylim=c(0,150),main="Monthly Holt method on all referrals in Prince Charles",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsRoyalGlamMonthlyHolt<-holt(TimeSeriesAllReferralsRoyalGlamMonthlyTrain)

plot(TimeSeriesAllReferralsRoyalGlamMonthlyHolt,ylim=c(0,150),main="Monthly Holt method on all referrals in Royal Glamorgan",xlab="Year",ylab="Referrals")

#Holt-Winters
#Daily

TimeSeriesAllReferralsAllHospitalsForHoltWintersDaily<-ts(TimeSeriesAllReferralsAllHospitalsDailyTrain,start=c(2018,01,02),freq=7)
TimeSeriesAllReferralsAllHospitalsDailyHoltWinters<-hw(TimeSeriesAllReferralsAllHospitalsForHoltWintersDaily)
plot(TimeSeriesAllReferralsAllHospitalsDailyHoltWinters,ylim=c(0,12),main="Daily Holt-Winters method on all referrals across all hospitals",xlab="Time",ylab="Referrals",xaxt="n")

TimeSeriesAllReferralsPrinceChForHoltWintersDaily<-ts(TimeSeriesAllReferralsPrinceChDailyTrain,start=c(2018,01,02),freq=7)
TimeSeriesAllReferralsPrinceChDailyHoltWinters<-hw(TimeSeriesAllReferralsPrinceChForHoltWintersDaily)
plot(TimeSeriesAllReferralsPrinceChDailyHoltWinters,ylim=c(0,12),main="Daily Holt-Winters method on all referrals in Prince Charles",xlab="Time",ylab="Referrals",xaxt="n")

TimeSeriesAllReferralsRoyalGlamForHoltWintersDaily<-ts(TimeSeriesAllReferralsRoyalGlamDailyTrain,start=c(2018,01,02),freq=7)
TimeSeriesAllReferralsRoyalGlamDailyHoltWinters<-hw(TimeSeriesAllReferralsRoyalGlamForHoltWintersDaily)
plot(TimeSeriesAllReferralsRoyalGlamDailyHoltWinters,ylim=c(0,12),main="Daily Holt-Winters method on all referrals in Royal Glamorgan",xlab="Time",ylab="Referrals",xaxt="n")


#Weekly
TimeSeriesAllReferralsAllHospitalsForHoltWintersWeekly<-ts(TimeSeriesAllReferralsAllHospitalsWeeklyTrain,start=c(2018,01,02),freq=24)
TimeSeriesAllReferralsAllHospitalsWeeklyHoltWinters<-hw(TimeSeriesAllReferralsAllHospitalsForHoltWintersWeekly)
plot(TimeSeriesAllReferralsAllHospitalsWeeklyHoltWinters,ylim=c(0,50),main="Weekly Holt-Winters method on all referrals across all hospitals",xlab="Time",ylab="Referrals",xaxt="n")

TimeSeriesAllReferralsPrinceChForHoltWintersWeekly<-ts(TimeSeriesAllReferralsPrinceChWeeklyTrain,start=c(2018,01,02),freq=24)
TimeSeriesAllReferralsPrinceChWeeklyHoltWinters<-hw(TimeSeriesAllReferralsPrinceChForHoltWintersWeekly)
plot(TimeSeriesAllReferralsPrinceChWeeklyHoltWinters,ylim=c(0,50),main="Weekly Holt-Winters method on all referrals Prince Charles",xlab="Time",ylab="Referrals",xaxt="n")

TimeSeriesAllReferralsRoyalGlamForHoltWintersWeekly<-ts(TimeSeriesAllReferralsRoyalGlamWeeklyTrain,start=c(2018,01,02),freq=24)
TimeSeriesAllReferralsRoyalGlamWeeklyHoltWinters<-hw(TimeSeriesAllReferralsRoyalGlamForHoltWintersWeekly)
plot(TimeSeriesAllReferralsRoyalGlamWeeklyHoltWinters,ylim=c(0,50),main="Weekly Holt-Winters method on all referrals Royal Glamorgan",xlab="Time",ylab="Referrals",xaxt="n")

#Monthly
TSDataAllReferralsAllHospitalsMonthlyForHoltWintersTrain<-c(as.vector(TimeSeriesAllReferralsAllHospitalsMonthlyTrain)[3:12],as.vector(TimeSeriesAllReferralsAllHospitalsMonthly))

TimeSeriesAllReferralsAllHospitalsForHoltWintersMonthly<-ts(TSDataAllReferralsAllHospitalsMonthlyForHoltWintersTrain,start=c(2017,03,02),freq=12)
TimeSeriesAllReferralsAllHospitalsMonthlyHoltWinters<-hw(TimeSeriesAllReferralsAllHospitalsForHoltWintersMonthly)
plot(TimeSeriesAllReferralsAllHospitalsMonthlyHoltWinters,ylim=c(0,150),main="Monthly Holt-Winters method on all referrals across all hospitals",xlab="Year",ylab="Referrals")

TSDataAllReferralsPrinceChMonthlyForHoltWintersTrain<-c(as.vector(TimeSeriesAllReferralsPrinceChMonthlyTrain)[3:12],as.vector(TimeSeriesAllReferralsPrinceChMonthly))

TimeSeriesAllReferralsPrinceChForHoltWintersMonthly<-ts(TSDataAllReferralsPrinceChMonthlyForHoltWintersTrain,start=c(2017,03,02),freq=12)
TimeSeriesAllReferralsPrinceChMonthlyHoltWinters<-hw(TimeSeriesAllReferralsPrinceChForHoltWintersMonthly)
plot(TimeSeriesAllReferralsPrinceChMonthlyHoltWinters,ylim=c(0,150),main="Monthly Holt-Winters method on all referrals in Prince Charles",xlab="Year",ylab="Referrals")


TSDataAllReferralsRoyalGlamMonthlyForHoltWintersTrain<-c(as.vector(TimeSeriesAllReferralsRoyalGlamMonthlyTrain)[3:12],as.vector(TimeSeriesAllReferralsRoyalGlamMonthly))

TimeSeriesAllReferralsRoyalGlamForHoltWintersMonthly<-ts(TSDataAllReferralsRoyalGlamMonthlyForHoltWintersTrain,start=c(2017,03,02),freq=12)
TimeSeriesAllReferralsRoyalGlamMonthlyHoltWinters<-hw(TimeSeriesAllReferralsRoyalGlamForHoltWintersMonthly)
plot(TimeSeriesAllReferralsRoyalGlamMonthlyHoltWinters,ylim=c(0,150),main="Monthly Holt-Winters method on all referrals in Royal Glamorgan",xlab="Year",ylab="Referrals")

#Linear regression models

#Daily simple linear regression model, all Referrals, all hospitals

TimeSeriesAllReferralsAllHospitalsDailyTrainSimpReg<-lm(TimeSeriesAllReferralsAllHospitalsDailyTrain~days[1:(0.75*length(TimeSeriesAllReferralsAllHospitalsDaily))])

plot(days[1:(0.75*length(TimeSeriesAllReferralsAllHospitalsDaily))],TimeSeriesAllReferralsAllHospitalsDailyTrain,type="l",xlab="Year",ylab="Referrals",main="Daily time series for all referral types across all hospitals",lwd=1)
abline(TimeSeriesAllReferralsAllHospitalsDailyTrainSimpReg,lwd=3,col="red")
legend("top", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

TimeSeriesAllReferralsPrinceChDailyTrainSimpReg<-lm(TimeSeriesAllReferralsPrinceChDailyTrain~days[1:(0.75*length(TimeSeriesAllReferralsPrinceChDaily))])

plot(days[1:(0.75*length(TimeSeriesAllReferralsPrinceChDaily))],TimeSeriesAllReferralsPrinceChDailyTrain,type="l",xlab="Year",ylab="Referrals",main="Daily time series for all referral types in Prince Charles",lwd=1)
abline(TimeSeriesAllReferralsPrinceChDailyTrainSimpReg,lwd=3,col="red")
legend("top", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

TimeSeriesAllReferralsRoyalGlamDailyTrainSimpReg<-lm(TimeSeriesAllReferralsRoyalGlamDailyTrain~days[1:(0.75*length(TimeSeriesAllReferralsRoyalGlamDaily))])

plot(days[1:(0.75*length(TimeSeriesAllReferralsRoyalGlamDaily))],TimeSeriesAllReferralsRoyalGlamDailyTrain,type="l",xlab="Year",ylab="Referrals",main="Daily time series for all referral types in Royal Glamorgan",lwd=1)
abline(TimeSeriesAllReferralsRoyalGlamDailyTrainSimpReg,lwd=3,col="red")
legend("top", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

#Weekly simple linear regression model, all Referrals, all hospitals

TimeSeriesAllReferralsAllHospitalsWeeklyTrainSimpReg<-lm(TimeSeriesAllReferralsAllHospitalsWeeklyTrain~weeks[1:(0.75*length(TimeSeriesAllReferralsAllHospitalsWeekly))])

plot(weeks[1:(0.75*length(TimeSeriesAllReferralsAllHospitalsWeekly))],TimeSeriesAllReferralsAllHospitalsWeeklyTrain,type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types across all hospitals",lwd=1)
abline(TimeSeriesAllReferralsAllHospitalsWeeklyTrainSimpReg,lwd=3,col="red")
legend("top", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

TimeSeriesAllReferralsPrinceChWeeklyTrainSimpReg<-lm(TimeSeriesAllReferralsPrinceChWeeklyTrain~weeks[1:(0.75*length(TimeSeriesAllReferralsPrinceChWeekly))])

plot(weeks[1:(0.75*length(TimeSeriesAllReferralsPrinceChWeekly))],TimeSeriesAllReferralsPrinceChWeeklyTrain,type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types in Prince Charles",lwd=1)
abline(TimeSeriesAllReferralsPrinceChWeeklyTrainSimpReg,lwd=3,col="red")
legend("top", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

TimeSeriesAllReferralsRoyalGlamWeeklyTrainSimpReg<-lm(TimeSeriesAllReferralsRoyalGlamWeeklyTrain~weeks[1:(0.75*length(TimeSeriesAllReferralsRoyalGlamWeekly))])

plot(weeks[1:(0.75*length(TimeSeriesAllReferralsRoyalGlamWeekly))],TimeSeriesAllReferralsRoyalGlamWeeklyTrain,type="l",xlab="Year",ylab="Referrals",main="Weekly time series for all referral types in Royal Glamorgan",lwd=1)
abline(TimeSeriesAllReferralsRoyalGlamWeeklyTrainSimpReg,lwd=3,col="red")
legend("top", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

#Monthly simple linear regression model, all Referrals, all hospitals

TimeSeriesAllReferralsAllHospitalsMonthlyTrainSimpReg<-lm(TimeSeriesAllReferralsAllHospitalsMonthlyTrain~months[1:(0.75*length(TimeSeriesAllReferralsAllHospitalsMonthly))])

plot(months[1:(0.75*length(TimeSeriesAllReferralsAllHospitalsMonthly))],TimeSeriesAllReferralsAllHospitalsMonthlyTrain,type="l",xlab="Year",ylab="Referrals",main="Monthly time series for all referral types across all hospitals",lwd=1)
abline(TimeSeriesAllReferralsAllHospitalsMonthlyTrainSimpReg,lwd=3,col="red")
legend("top", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

TimeSeriesAllReferralsPrinceChMonthlyTrainSimpReg<-lm(TimeSeriesAllReferralsPrinceChMonthlyTrain~months[1:(0.75*length(TimeSeriesAllReferralsPrinceChMonthly))])

plot(months[1:(0.75*length(TimeSeriesAllReferralsPrinceChMonthly))],TimeSeriesAllReferralsPrinceChMonthlyTrain,type="l",xlab="Year",ylab="Referrals",main="Monthly time series for all referral types in Prince Charles",lwd=1)
abline(TimeSeriesAllReferralsPrinceChMonthlyTrainSimpReg,lwd=3,col="red")
legend("top", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

TimeSeriesAllReferralsRoyalGlamMonthlyTrainSimpReg<-lm(TimeSeriesAllReferralsRoyalGlamMonthlyTrain~months[1:(0.75*length(TimeSeriesAllReferralsRoyalGlamMonthly))])

plot(months[1:(0.75*length(TimeSeriesAllReferralsRoyalGlamMonthly))],TimeSeriesAllReferralsRoyalGlamMonthlyTrain,type="l",xlab="Year",ylab="Referrals",main="Monthly time series for all referral types in Royal Glamorgan",lwd=1)
abline(TimeSeriesAllReferralsRoyalGlamMonthlyTrainSimpReg,lwd=3,col="red")
legend("top", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

#Multiple Linear regression

write.xlsx(cbind(TimeSeriesAllReferralsAllHospitalsDailyTrain,as.Date(days[1:(0.75*length(TimeSeriesAllReferralsAllHospitalsDaily))])), "DailyreferralstrainingdataforexcelMultReg.xlsx")

TimeSeriesAllReferralsAllHospitalsDailyTrainMultRegExcel<-read_excel("Daily referrals training data for excel Multi Regression.xlsx")

TimeSeriesAllReferralsAllHospitalsDailyTrainMultRegData<-as.data.frame(TimeSeriesAllReferralsAllHospitalsDailyTrainMultRegExcel)

TimeSeriesAllReferralsAllHospitalsDailyTrainMultReg<-lm(Referrals~Day+Month+Year,data=TSDataAllReferralsAllHospitalsDailyTrainMultRegData)

summary(TimeSeriesAllReferralsAllHospitalsDailyTrainMultReg)

write.xlsx(cbind(TimeSeriesAllReferralsPrinceChDailyTrain,as.Date(days[1:(0.75*length(TimeSeriesAllReferralsAllHospitalsDaily))])), "DailyreferralstrainingdataforexcelPrinceChMultReg.xlsx")

TimeSeriesAllReferralsPrinceChDailyTrainMultRegExcel<-read_excel("Daily referrals training data for excel Multi Regression Prince Ch.xlsx")

TimeSeriesAllReferralsPrinceChDailyTrainMultRegData<-as.data.frame(TimeSeriesAllReferralsPrinceChDailyTrainMultRegExcel)

TimeSeriesAllReferralsPrinceChDailyTrainMultReg<-lm(Referrals~Day+Month+Year,data=TimeSeriesAllReferralsPrinceChDailyTrainMultRegData)

summary(TimeSeriesAllReferralsPrinceChDailyTrainMultReg)

write.xlsx(cbind(TimeSeriesAllReferralsRoyalGlamDailyTrain,as.Date(days[1:(0.75*length(TimeSeriesAllReferralsAllHospitalsDaily))])), "DailyreferralstrainingdataforexcelRoyalGlamMultReg.xlsx")

TimeSeriesAllReferralsRoyalGlamDailyTrainMultRegExcel<-read_excel("Daily referrals training data for excel Multi Regression Royal Glam.xlsx")

TimeSeriesAllReferralsRoyalGlamDailyTrainMultRegData<-as.data.frame(TimeSeriesAllReferralsRoyalGlamDailyTrainMultRegExcel)

TimeSeriesAllReferralsRoyalGlamDailyTrainMultReg<-lm(Referrals~Day+Month+Year,data=TimeSeriesAllReferralsRoyalGlamDailyTrainMultRegData)

summary(TimeSeriesAllReferralsRoyalGlamDailyTrainMultReg)

#ARIMAS

acf(TimeSeriesAllReferralsAllHospitalsDailyTrain,lag.max=100,main="ACF for daily referrals data, all hospitals")
acf(TimeSeriesAllReferralsAllHospitalsWeeklyTrain,lag.max=100,main="ACF for weekly referrals data, all hospitals")
acf(TimeSeriesAllReferralsAllHospitalsMonthlyTrain,lag.max=100,main="ACF for monthly referrals data, all hospitals")

acf(TimeSeriesAllReferralsPrinceChDailyTrain,lag.max=100,main="ACF for daily referrals data, all hospitals")
acf(TimeSeriesAllReferralsPrinceChWeeklyTrain,lag.max=100,main="ACF for weekly referrals data, all hospitals")
acf(TimeSeriesAllReferralsPrinceChMonthlyTrain,lag.max=100,main="ACF for monthly referrals data, all hospitals")

acf(TimeSeriesAllReferralsRoyalGlamDailyTrain,lag.max=100,main="ACF for daily referrals data, all hospitals")
acf(TimeSeriesAllReferralsRoyalGlamWeeklyTrain,lag.max=100,main="ACF for weekly referrals data, all hospitals")
acf(TimeSeriesAllReferralsRoyalGlamMonthlyTrain,lag.max=100,main="ACF for monthly referrals data, all hospitals")

pacf(TimeSeriesAllReferralsAllHospitalsDailyTrain,lag.max=100,main="PACF for daily referrals data, all hospitals")
pacf(TimeSeriesAllReferralsAllHospitalsWeeklyTrain,lag.max=100,main="PACF for weekly referrals data, all hospitals")
pacf(TimeSeriesAllReferralsAllHospitalsMonthlyTrain,lag.max=100,main="PACF for monthly referrals data, all hospitals")

pacf(TimeSeriesAllReferralsPrinceChDailyTrain,lag.max=100,main="PACF for daily referrals data, all hospitals")
pacf(TimeSeriesAllReferralsPrinceChWeeklyTrain,lag.max=100,main="PACF for weekly referrals data, all hospitals")
pacf(TimeSeriesAllReferralsPrinceChMonthlyTrain,lag.max=100,main="PACF for monthly referrals data, all hospitals")

pacf(TimeSeriesAllReferralsRoyalGlamDailyTrain,lag.max=100,main="PACF for daily referrals data, all hospitals")
pacf(TimeSeriesAllReferralsRoyalGlamWeeklyTrain,lag.max=100,main="PACF for weekly referrals data, all hospitals")
pacf(TimeSeriesAllReferralsRoyalGlamMonthlyTrain,lag.max=100,main="PACF for monthly referrals data, all hospitals")

TimeSeriesAllReferralsAllHospitalsDailyTrainARIMA<-arima(TimeSeriesAllReferralsAllHospitalsDailyTrain,order=c(7,0,4))
tsdiag(TimeSeriesAllReferralsAllHospitalsDailyTrainARIMA)
plot(forecast(TimeSeriesAllReferralsAllHospitalsDailyTrainARIMA,h=10),main="ARIMA on daily referrals data, all hospitals")

TimeSeriesAllReferralsPrinceChDailyTrainARIMA<-arima(TimeSeriesAllReferralsPrinceChDailyTrain,order=c(7,0,0))
tsdiag(TimeSeriesAllReferralsPrinceChDailyTrainARIMA)
plot(forecast(TimeSeriesAllReferralsPrinceChDailyTrainARIMA,h=10),main="ARIMA on daily referrals data, Prince Charles")

TimeSeriesAllReferralsRoyalGlamDailyTrainARIMA<-arima(TimeSeriesAllReferralsRoyalGlamDailyTrain,order=c(5,0,0))
tsdiag(TimeSeriesAllReferralsRoyalGlamDailyTrainARIMA)
plot(forecast(TimeSeriesAllReferralsRoyalGlamDailyTrainARIMA,h=10),main="ARIMA on daily referrals data, Royal Glamorgan")

TimeSeriesAllReferralsAllHospitalsWeeklyTrainARIMA<-arima(TimeSeriesAllReferralsAllHospitalsWeeklyTrain,order=c(1,1,1))
tsdiag(TimeSeriesAllReferralsAllHospitalsWeeklyTrainARIMA)
plot(forecast(TimeSeriesAllReferralsAllHospitalsWeeklyTrainARIMA,h=10),main="ARIMA on weekly referrals data, all hospitals")

TimeSeriesAllReferralsPrinceChWeeklyTrainARIMA<-arima(TimeSeriesAllReferralsPrinceChWeeklyTrain,order=c(0,0,0))
tsdiag(TimeSeriesAllReferralsPrinceChWeeklyTrainARIMA)
plot(forecast(TimeSeriesAllReferralsPrinceChWeeklyTrainARIMA,h=10),main="ARIMA on weekly referrals data, Prince Charles")

TimeSeriesAllReferralsRoyalGlamWeeklyTrainARIMA<-arima(TimeSeriesAllReferralsRoyalGlamWeeklyTrain,order=c(0,0,0))
tsdiag(TimeSeriesAllReferralsRoyalGlamWeeklyTrainARIMA)
plot(forecast(TimeSeriesAllReferralsRoyalGlamWeeklyTrainARIMA,h=10),main="ARIMA on weekly referrals data, Royal Glamorgan")

TimeSeriesAllReferralsAllHospitalsMonthlyTrainARIMA<-arima(TimeSeriesAllReferralsAllHospitalsMonthlyTrain,order=c(0,1,0))
tsdiag(TimeSeriesAllReferralsAllHospitalsMonthlyTrainARIMA)
plot(forecast(TimeSeriesAllReferralsAllHospitalsMonthlyTrainARIMA,h=10),,main="ARIMA on monthly referrals data, all hospitals")

TimeSeriesAllReferralsPrinceChMonthlyTrainARIMA<-arima(TimeSeriesAllReferralsPrinceChMonthlyTrain,order=c(0,1,1))
tsdiag(TimeSeriesAllReferralsPrinceChMonthlyTrainARIMA)
plot(forecast(TimeSeriesAllReferralsPrinceChMonthlyTrainARIMA,h=10),,main="ARIMA on monthly referrals data, Prince Charles")

TimeSeriesAllReferralsRoyalGlamMonthlyTrainARIMA<-arima(TimeSeriesAllReferralsRoyalGlamMonthlyTrain,order=c(0,0,1))
tsdiag(TimeSeriesAllReferralsRoyalGlamMonthlyTrainARIMA)
plot(forecast(TimeSeriesAllReferralsRoyalGlamMonthlyTrainARIMA,h=10),,main="ARIMA on monthly referrals data, Royal Glamorgan")

#SSA Experimentation
install.packages("Rssa")
library(Rssa)
#Daily
SSAAllReferralsAllHospitalsDailyTrain <- ssa(TimeSeriesAllReferralsAllHospitalsDailyTrain)
SSAAllReferralsAllHospitalsDailyTrain

SSAAllReferralsAllHospitalsDailyForecast<-forecast(SSAAllReferralsAllHospitalsDailyTrain,groups=list(c(1,10)),h=10)
SSAAllReferralsAllHospitalsDailyForecast
plot(SSAAllReferralsAllHospitalsDailyForecast,main="SSA method on all daily referrals across all hospitals",xlab="Year",ylab="Referrals")

SSAAllReferralsPrinceChDailyTrain <- ssa(TimeSeriesAllReferralsPrinceChDailyTrain)
SSAAllReferralsPrinceChDailyTrain

SSAAllReferralsPrinceChDailyForecast<-forecast(SSAAllReferralsPrinceChDailyTrain,groups=list(c(1,10)),h=10)
SSAAllReferralsPrinceChDailyForecast
plot(SSAAllReferralsPrinceChDailyForecast,main="SSA method on all daily referrals in Prince Charles",xlab="Year",ylab="Referrals")

SSAAllReferralsRoyalGlamDailyTrain <- ssa(TimeSeriesAllReferralsRoyalGlamDailyTrain)
SSAAllReferralsRoyalGlamDailyTrain

SSAAllReferralsRoyalGlamDailyForecast<-forecast(SSAAllReferralsRoyalGlamDailyTrain,groups=list(c(1,10)),h=10)
SSAAllReferralsRoyalGlamDailyForecast
plot(SSAAllReferralsRoyalGlamDailyForecast,main="SSA method on all daily referrals in Royal Glamorgan",xlab="Year",ylab="Referrals")

rforDaily <- rforecast(SSAAllReferralsAllHospitalsDaily, groups = list(c(1,4), 1:4), len = 100, only.new=FALSE)
matplot(data.frame(c(TimeSeriesAllReferralsAllHospitalsDailyTrain, rep(NA, 100)), rforDaily), type = "l",xlab="Day",ylab="Referrals",main="Daily SSA predictions for referrals across all hospitals")
rforDaily
plot(SSAAllReferralsAllHospitalsDailyTrain,type="vectors")

#Weekly

SSAAllReferralsAllHospitalsWeeklyTrain <- ssa(TimeSeriesAllReferralsAllHospitalsWeeklyTrain)
SSAAllReferralsAllHospitalsWeeklyTrain
SSAAllReferralsAllHospitalsWeeklyForecast<-forecast(SSAAllReferralsAllHospitalsWeeklyTrain,groups=list(c(1,10)),h=10)
SSAAllReferralsAllHospitalsWeeklyForecast
plot(SSAAllReferralsAllHospitalsWeeklyForecast,main="SSA method on all weekly referrals across all hospitals",xlab="Year",ylab="Referrals")

SSAAllReferralsPrinceChWeeklyTrain <- ssa(TimeSeriesAllReferralsPrinceChWeeklyTrain)
SSAAllReferralsPrinceChWeeklyTrain
SSAAllReferralsPrinceChWeeklyForecast<-forecast(SSAAllReferralsPrinceChWeeklyTrain,groups=list(c(1,10)),h=10)
SSAAllReferralsPrinceChWeeklyForecast
plot(SSAAllReferralsPrinceChWeeklyForecast,main="SSA method on all weekly referrals in Prince Charles",xlab="Year",ylab="Referrals")

SSAAllReferralsRoyalGlamWeeklyTrain <- ssa(TimeSeriesAllReferralsRoyalGlamWeeklyTrain)
SSAAllReferralsRoyalGlamWeeklyTrain
SSAAllReferralsRoyalGlamWeeklyForecast<-forecast(SSAAllReferralsRoyalGlamWeeklyTrain,groups=list(c(1,10)),h=10)
SSAAllReferralsRoyalGlamWeeklyForecast
plot(SSAAllReferralsRoyalGlamWeeklyForecast,main="SSA method on all weekly referrals in Royal Glamorgan",xlab="Year",ylab="Referrals")

SSAAllReferralsAllHospitalsWeekly <- ssa(TimeSeriesAllReferralsAllHospitalsWeeklyTrain)
rforWeekly <- rforecast(SSAAllReferralsAllHospitalsWeekly, groups = list(c(1,4), 1:4), len = 10, only.new=FALSE)
matplot(data.frame(c(TimeSeriesAllReferralsAllHospitalsWeeklyTrain, rep(NA, 10)), rforWeekly), type = "l",xlab="Week",ylab="Referrals",main="Weekly SSA predictions for referrals across all hospitals")
rforWeekly
plot(SSAAllReferralsAllHospitalsWeekly,type="vectors")

SSAAllReferralsPrinceChWeekly <- ssa(TimeSeriesAllReferralsPrinceChWeeklyTrain)
rforWeekly <- rforecast(SSAAllReferralsPrinceChWeekly, groups = list(c(1,4), 1:4), len = 10, only.new=FALSE)
matplot(data.frame(c(TimeSeriesAllReferralsPrinceChWeeklyTrain, rep(NA, 10)), rforWeekly), type = "l",xlab="Week",ylab="Referrals",main="Weekly SSA predictions for referrals in Prince Charles")
rforWeekly
plot(SSAAllReferralsPrinceChWeekly,type="vectors")

SSAAllReferralsRoyalGlamWeekly <- ssa(TimeSeriesAllReferralsRoyalGlamWeeklyTrain)
SSAAllReferralsRoyalGlamWeekly
rforWeekly <- rforecast(SSAAllReferralsRoyalGlamWeekly, groups = list(c(1,4), 1:4), len = 10, only.new=FALSE)
matplot(data.frame(c(TimeSeriesAllReferralsRoyalGlamWeeklyTrain, rep(NA, 10)), rforWeekly), type = "l",xlab="Week",ylab="Referrals",main="Weekly SSA predictions for referrals in Royal Glamorgan")
rforWeekly
plot(SSAAllReferralsRoyalGlamWeekly,type="vectors")
        
#Monthly
SSAAllReferralsAllHospitalsMonthly <- ssa(TimeSeriesAllReferralsAllHospitalsMonthlyTrain)
SSAAllReferralsAllHospitalsMonthly
rforMonthly <- rforecast(SSAAllReferralsAllHospitalsMonthly, groups = list(c(1,4), 1:4), len = 5, only.new=FALSE)
matplot(data.frame(c(TimeSeriesAllReferralsAllHospitalsMonthlyTrain, rep(NA, 5)), rforMonthly), type = "l",xlab="Month",ylab="Referrals",main="Monthly SSA predictions for referrals across all hospitals")
rforMonthly
plot(SSAAllReferralsAllHospitalsMonthly,type="vectors")

SSAAllReferralsPrinceChMonthly <- ssa(TimeSeriesAllReferralsPrinceChMonthlyTrain)
SSAAllReferralsPrinceChMonthly
rforMonthly <- rforecast(SSAAllReferralsPrinceChMonthly, groups = list(c(1,4), 1:4), len = 5, only.new=FALSE)
matplot(data.frame(c(TimeSeriesAllReferralsPrinceChMonthlyTrain, rep(NA, 5)), rforMonthly), type = "l",xlab="Month",ylab="Referrals",main="Monthly SSA predictions for referrals in Prince Charles")
rforMonthly
plot(SSAAllReferralsPrinceChMonthly,type="vectors")

SSAAllReferralsRoyalGlamMonthly <- ssa(TimeSeriesAllReferralsRoyalGlamMonthlyTrain)
SSAAllReferralsRoyalGlamMonthly
rforMonthly <- rforecast(SSAAllReferralsRoyalGlamMonthly, groups = list(c(1,4), 1:4), len = 5, only.new=FALSE)
matplot(data.frame(c(TimeSeriesAllReferralsRoyalGlamMonthlyTrain, rep(NA, 5)), rforMonthly), type = "l",xlab="Month",ylab="Referrals",main="Monthly SSA predictions for referrals in Royal Glamorgan")
rforMonthly
plot(SSAAllReferralsRoyalGlamMonthly,type="vectors")
SSAAllReferralsAllHospitalsMonthlyTrain <- ssa(TimeSeriesAllReferralsAllHospitalsMonthlyTrain)
SSAAllReferralsAllHospitalsMonthlyTrain
SSAAllReferralsAllHospitalsMonthlyForecast<-forecast(SSAAllReferralsAllHospitalsMonthlyTrain,groups=list(c(1,4)),h=10)
SSAAllReferralsAllHospitalsMonthlyForecastPoints<-c(57.73, 59.44, 59.77, 57.23, 63.37, 58.91, 64.88, 63.10,63.13,65.43)



plot(SSAAllReferralsAllHospitalsMonthlyForecast,main="SSA method on all monthly referrals across all hospitals",xlab="Year",ylab="Referrals")

SSAAllReferralsPrinceChMonthlyTrain <- ssa(TimeSeriesAllReferralsPrinceChMonthlyTrain)
SSAAllReferralsPrinceChMonthlyTrain
SSAAllReferralsPrinceChMonthlyForecast<-forecast(SSAAllReferralsPrinceChMonthlyTrain,groups=list(c(1,4)),h=10)
SSAAllReferralsPrinceChMonthlyForecast
plot(SSAAllReferralsPrinceChMonthlyForecast,main="SSA method on all monthly referrals in Prince Charles",xlab="Year",ylab="Referrals")

SSAAllReferralsRoyalGlamMonthlyTrain <- ssa(TimeSeriesAllReferralsRoyalGlamMonthlyTrain)
SSAAllReferralsRoyalGlamMonthlyTrain
SSAAllReferralsRoyalGlamMonthlyForecast<-forecast(SSAAllReferralsRoyalGlamMonthlyTrain,groups=list(c(1,4)),h=10)
SSAAllReferralsRoyalGlamMonthlyForecast
plot(SSAAllReferralsRoyalGlamMonthlyForecast,main="SSA method on all monthly referrals in Royal Glamorgan",xlab="Year",ylab="Referrals")

library(openxlsx)
write.xlsx(TimeSeriesAllReferralsAllHospitalsDailyTrain,file = "Dailyreferralstrainingdataforexcel.xlsx")
write.xlsx(TimeSeriesAllReferralsAllHospitalsWeeklyTrain,file = "Weeklyreferralstrainingdataforexcel.xlsx")
write.xlsx(TimeSeriesAllReferralsAllHospitalsMonthlyTrain,file = "Monthlyreferralstrainingdataforexcel.xlsx")

#Artificial Neural Networks (Machine learning)
TimeSeriesAllReferralsAllHospitalsDailyTrainNNet<-nnetar(TimeSeriesAllReferralsAllHospitalsDailyTrain)
TimeSeriesAllReferralsAllHospitalsDailyTrainNNetPlot<-forecast(TimeSeriesAllReferralsAllHospitalsDailyTrainNNet,PI=T,h=10)
plot(TimeSeriesAllReferralsAllHospitalsDailyTrainNNetPlot,main="Daily ANN predictions for referrals across all hospitals",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsPrinceChDailyTrainNNet<-nnetar(TimeSeriesAllReferralsPrinceChDailyTrain)
TimeSeriesAllReferralsPrinceChDailyTrainNNetPlot<-forecast(TimeSeriesAllReferralsPrinceChDailyTrainNNet,PI=T,h=10)
plot(TimeSeriesAllReferralsPrinceChDailyTrainNNetPlot,main="Daily ANN predictions for referrals in Prince Charles")

TimeSeriesAllReferralsRoyalGlamDailyTrainNNet<-nnetar(TimeSeriesAllReferralsRoyalGlamDailyTrain)
TimeSeriesAllReferralsRoyalGlamDailyTrainNNetPlot<-forecast(TimeSeriesAllReferralsRoyalGlamDailyTrainNNet,PI=T,h=10)
plot(TimeSeriesAllReferralsRoyalGlamDailyTrainNNetPlot,main="Daily ANN predictions for referrals in Royal Glamorgan")

TimeSeriesAllReferralsAllHospitalsWeeklyTrainNNet<-nnetar(TimeSeriesAllReferralsAllHospitalsWeeklyTrain)
TimeSeriesAllReferralsAllHospitalsWeeklyTrainNNetPlot<-forecast(TimeSeriesAllReferralsAllHospitalsWeeklyTrainNNet,PI=T,h=10)
plot(TimeSeriesAllReferralsAllHospitalsWeeklyTrainNNetPlot,main="Weekly ANN predictions for referrals across all hospitals",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsPrinceChWeeklyTrainNNet<-nnetar(TimeSeriesAllReferralsPrinceChWeeklyTrain)
TimeSeriesAllReferralsPrinceChWeeklyTrainNNetPlot<-forecast(TimeSeriesAllReferralsPrinceChWeeklyTrainNNet,PI=T,h=10)
plot(TimeSeriesAllReferralsPrinceChWeeklyTrainNNetPlot,main="Weekly ANN predictions for referrals in Prince Charles")

TimeSeriesAllReferralsRoyalGlamWeeklyTrainNNet<-nnetar(TimeSeriesAllReferralsRoyalGlamWeeklyTrain)
TimeSeriesAllReferralsRoyalGlamWeeklyTrainNNetPlot<-forecast(TimeSeriesAllReferralsRoyalGlamWeeklyTrainNNet,PI=T,h=10)
plot(TimeSeriesAllReferralsRoyalGlamWeeklyTrainNNetPlot,main="Weekly ANN predictions for referrals in Royal Glamorgan")

TimeSeriesAllReferralsAllHospitalsMonthlyTrainNNet<-nnetar(TimeSeriesAllReferralsAllHospitalsMonthlyTrain)
TimeSeriesAllReferralsAllHospitalsMonthlyTrainNNetPlot<-forecast(TimeSeriesAllReferralsAllHospitalsMonthlyTrainNNet,PI=T,h=5)
plot(TimeSeriesAllReferralsAllHospitalsMonthlyTrainNNetPlot,main="Monthly ANN predictions for referrals across all hospitals",xlab="Year",ylab="Referrals")

TimeSeriesAllReferralsPrinceChMonthlyTrainNNet<-nnetar(TimeSeriesAllReferralsPrinceChMonthlyTrain)
TimeSeriesAllReferralsPrinceChMonthlyTrainNNetPlot<-forecast(TimeSeriesAllReferralsPrinceChMonthlyTrainNNet,PI=T,h=10)
plot(TimeSeriesAllReferralsPrinceChMonthlyTrainNNetPlot,main="Monthly ANN predictions for referrals in Prince Charles")
TimeSeriesAllReferralsPrinceChMonthlyTrain

TimeSeriesAllReferralsRoyalGlamMonthlyTrainNNet<-nnetar(TimeSeriesAllReferralsRoyalGlamMonthlyTrain)
TimeSeriesAllReferralsRoyalGlamMonthlyTrainNNetPlot<-forecast(TimeSeriesAllReferralsRoyalGlamMonthlyTrainNNet,PI=T,h=10)
plot(TimeSeriesAllReferralsRoyalGlamMonthlyTrainNNetPlot,main="Monthly ANN predictions for referrals in Royal Glamorgan")
TimeSeriesAllReferralsRoyalGlamMonthlyTrain

#Error statistics for training sets

accuracy(TimeSeriesAllReferralsAllHospitalsDailyNaive)
accuracy(TimeSeriesAllReferralsAllHospitalsDailySNaive)
accuracy(TimeSeriesAllReferralsAllHospitalsDailySES)
accuracy(TimeSeriesAllReferralsAllHospitalsDailyHolt)
accuracy(TimeSeriesAllReferralsAllHospitalsDailyHoltWinters)
accuracy(TimeSeriesAllReferralsAllHospitalsDailyTrainSimpReg)
accuracy(TimeSeriesAllReferralsAllHospitalsDailyTrainMultReg)
accuracy(TimeSeriesAllReferralsAllHospitalsDailyTrainARIMA)
accuracy(SSAAllReferralsAllHospitalsDailyForecast)
accuracy(TimeSeriesAllReferralsAllHospitalsDailyTrainNNet)

accuracy(TimeSeriesAllReferralsPrinceChDailyNaive)
accuracy(TimeSeriesAllReferralsPrinceChDailySNaive)
accuracy(TimeSeriesAllReferralsPrinceChDailySES)
accuracy(TimeSeriesAllReferralsPrinceChDailyHolt)
accuracy(TimeSeriesAllReferralsPrinceChDailyHoltWinters)
accuracy(TimeSeriesAllReferralsPrinceChDailyTrainSimpReg)
accuracy(TimeSeriesAllReferralsPrinceChDailyTrainMultReg)
accuracy(TimeSeriesAllReferralsPrinceChDailyTrainARIMA)
accuracy(SSAAllReferralsPrinceChDailyForecast)
accuracy(TimeSeriesAllReferralsPrinceChDailyTrainNNet)

accuracy(TimeSeriesAllReferralsRoyalGlamDailyNaive)
accuracy(TimeSeriesAllReferralsRoyalGlamDailySNaive)
accuracy(TimeSeriesAllReferralsRoyalGlamDailySES)
accuracy(TimeSeriesAllReferralsRoyalGlamDailyHolt)
accuracy(TimeSeriesAllReferralsRoyalGlamDailyHoltWinters)
accuracy(TimeSeriesAllReferralsRoyalGlamDailyTrainSimpReg)
accuracy(TimeSeriesAllReferralsRoyalGlamDailyTrainMultReg)
accuracy(TimeSeriesAllReferralsRoyalGlamDailyTrainARIMA)
accuracy(SSAAllReferralsRoyalGlamDailyForecast)
accuracy(TimeSeriesAllReferralsRoyalGlamDailyTrainNNet)

accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyNaive)
accuracy(TimeSeriesAllReferralsAllHospitalsWeeklySNaive)
accuracy(TimeSeriesAllReferralsAllHospitalsWeeklySES)
accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyHolt)
accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyHoltWinters)
accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyTrainSimpReg)
accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyTrainMultReg)
accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyTrainARIMA)
accuracy(SSAAllReferralsAllHospitalsWeeklyForecast)
accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyTrainNNet)

accuracy(TimeSeriesAllReferralsPrinceChWeeklyNaive)
accuracy(TimeSeriesAllReferralsPrinceChWeeklySNaive)
accuracy(TimeSeriesAllReferralsPrinceChWeeklySES)
accuracy(TimeSeriesAllReferralsPrinceChWeeklyHolt)
accuracy(TimeSeriesAllReferralsPrinceChWeeklyHoltWinters)
accuracy(TimeSeriesAllReferralsPrinceChWeeklyTrainSimpReg)
accuracy(TimeSeriesAllReferralsPrinceChWeeklyTrainMultReg)
accuracy(TimeSeriesAllReferralsPrinceChWeeklyTrainARIMA)
accuracy(SSAAllReferralsPrinceChWeeklyForecast)
accuracy(TimeSeriesAllReferralsPrinceChWeeklyTrainNNet)

accuracy(TimeSeriesAllReferralsRoyalGlamWeeklyNaive)
accuracy(TimeSeriesAllReferralsRoyalGlamWeeklySNaive)
accuracy(TimeSeriesAllReferralsRoyalGlamWeeklySES)
accuracy(TimeSeriesAllReferralsRoyalGlamWeeklyHolt)
accuracy(TimeSeriesAllReferralsRoyalGlamWeeklyHoltWinters)
accuracy(TimeSeriesAllReferralsRoyalGlamWeeklyTrainSimpReg)
accuracy(TimeSeriesAllReferralsRoyalGlamWeeklyTrainMultReg)
accuracy(TimeSeriesAllReferralsRoyalGlamWeeklyTrainARIMA)
accuracy(SSAAllReferralsRoyalGlamWeeklyForecast)
accuracy(TimeSeriesAllReferralsRoyalGlamWeeklyTrainNNet)

accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyNaive)
accuracy(TimeSeriesAllReferralsAllHospitalsMonthlySNaive)
accuracy(TimeSeriesAllReferralsAllHospitalsMonthlySES)
accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyHolt)
accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyHoltWinters)
accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyTrainSimpReg)
accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyTrainMultReg)
accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyTrainARIMA)
accuracy(SSAAllReferralsAllHospitalsMonthlyForecast)
accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyTrainNNet)

accuracy(TimeSeriesAllReferralsPrinceChMonthlyNaive)
accuracy(TimeSeriesAllReferralsPrinceChMonthlySNaive)
accuracy(TimeSeriesAllReferralsPrinceChMonthlySES)
accuracy(TimeSeriesAllReferralsPrinceChMonthlyHolt)
accuracy(TimeSeriesAllReferralsPrinceChMonthlyHoltWinters)
accuracy(TimeSeriesAllReferralsPrinceChMonthlyTrainSimpReg)
accuracy(TimeSeriesAllReferralsPrinceChMonthlyTrainMultReg)
accuracy(TimeSeriesAllReferralsPrinceChMonthlyTrainARIMA)
accuracy(SSAAllReferralsPrinceChMonthlyForecast)
accuracy(TimeSeriesAllReferralsPrinceChMonthlyTrainNNet)

accuracy(TimeSeriesAllReferralsRoyalGlamMonthlyNaive)
accuracy(TimeSeriesAllReferralsRoyalGlamMonthlySNaive)
accuracy(TimeSeriesAllReferralsRoyalGlamMonthlySES)
accuracy(TimeSeriesAllReferralsRoyalGlamMonthlyHolt)
accuracy(TimeSeriesAllReferralsRoyalGlamMonthlyHoltWinters)
accuracy(TimeSeriesAllReferralsRoyalGlamMonthlyTrainSimpReg)
accuracy(TimeSeriesAllReferralsRoyalGlamMonthlyTrainMultReg)
accuracy(TimeSeriesAllReferralsRoyalGlamMonthlyTrainARIMA)
accuracy(SSAAllReferralsRoyalGlamMonthlyForecast)
accuracy(TimeSeriesAllReferralsRoyalGlamMonthlyTrainNNet)

accuracy(TimeSeriesAllReferralsPrinceChMonthlyTrainNNet)
as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamMonthlyTrainNNet,h=10))


as.data.frame(predict(TimeSeriesAllReferralsRoyalGlamDailyTrainMultReg$fitted.values,h=10))[,1]
accuracy(TimeSeriesAllReferralsRoyalGlamDailyTrainMultReg)

#Error statistics for test set

accuracy(TimeSeriesAllReferralsAllHospitalsDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsDailyNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsDailySNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsDailySES,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsDailyHolt,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsDailyHoltWinters,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsDailyTest[0:10],as.data.frame(predict(TimeSeriesAllReferralsAllHospitalsDailyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsDailyTest[0:10],as.data.frame(predict(TimeSeriesAllReferralsAllHospitalsDailyTrainMultReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsDailyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsDailyTest[0:10],as.data.frame(forecast(SSAAllReferralsAllHospitalsDailyForecast,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsDailyTrainNNet,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChDailyNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChDailySNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChDailySES,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChDailyHolt,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChDailyHoltWinters,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChDailyTest[0:10],as.data.frame(predict(TimeSeriesAllReferralsPrinceChDailyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChDailyTest[0:10],as.data.frame(predict(TimeSeriesAllReferralsPrinceChDailyTrainMultReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChDailyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChDailyTest[0:10],as.data.frame(forecast(SSAAllReferralsPrinceChDailyForecast,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChDailyTrainNNet,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamDailyNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamDailySNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamDailySES,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamDailyHolt,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamDailyHoltWinters,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamDailyTest[0:10],as.data.frame(predict(TimeSeriesAllReferralsRoyalGlamDailyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamDailyTest[0:10],as.data.frame(predict(TimeSeriesAllReferralsRoyalGlamDailyTrainMultReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamDailyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamDailyTest[0:10],as.data.frame(forecast(SSAAllReferralsRoyalGlamDailyForecast,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamDailyTrainNNet,h=10))[,1])


accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsWeeklyNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsWeeklySNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsWeeklySES,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsWeeklyHolt,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsWeeklyHoltWinters,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyTest[0:10],as.data.frame(predict(TimeSeriesAllReferralsAllHospitalsWeeklyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsWeeklyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyTest[0:10],as.data.frame(forecast(SSAAllReferralsAllHospitalsWeeklyForecast,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsWeeklyTrainNNet,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChWeeklyNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChWeeklySNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChWeeklySES,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChWeeklyHolt,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChWeeklyHoltWinters,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChWeeklyTest[0:10],as.data.frame(predict(TimeSeriesAllReferralsPrinceChWeeklyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChWeeklyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChWeeklyTest[0:10],as.data.frame(forecast(SSAAllReferralsPrinceChWeeklyForecast,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChWeeklyTrainNNet,h=10))[,1])


accuracy(TimeSeriesAllReferralsRoyalGlamWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamWeeklyNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamWeeklySNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamWeeklySES,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamWeeklyHolt,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamWeeklyHoltWinters,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamWeeklyTest[0:10],as.data.frame(predict(TimeSeriesAllReferralsRoyalGlamWeeklyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamWeeklyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamWeeklyTest[0:10],as.data.frame(forecast(SSAAllReferralsRoyalGlamWeeklyForecast,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamWeeklyTrainNNet,h=10))[,1])


accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsMonthlyNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsMonthlySNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsMonthlySES,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsMonthlyHolt,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsMonthlyHoltWinters,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyTest[0:10],as.data.frame(predict(TimeSeriesAllReferralsAllHospitalsMonthlyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsMonthlyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyTest[0:10],as.vector(SSAAllReferralsAllHospitalsMonthlyForecast$mean))

accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyTest[0:10],as.vector(TimeSeriesAllReferralsAllHospitalsMonthlyTrainNNetPlot$mean))


accuracy(TimeSeriesAllReferralsPrinceChMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChMonthlyNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChMonthlySNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChMonthlySES,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChMonthlyHolt,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChMonthlyHoltWinters,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChMonthlyTest[0:10],as.data.frame(predict(TimeSeriesAllReferralsPrinceChMonthlyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChMonthlyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesAllReferralsPrinceChMonthlyTest[0:10],as.vector(SSAAllReferralsPrinceChMonthlyForecast$mean))

accuracy(TimeSeriesAllReferralsPrinceChMonthlyTest[0:10],as.vector(TimeSeriesAllReferralsPrinceChMonthlyTrainNNetPlot$mean))


accuracy(TimeSeriesAllReferralsRoyalGlamMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamMonthlyNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamMonthlySNaive,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamMonthlySES,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamMonthlyHolt,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamMonthlyHoltWinters,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamMonthlyTest[0:10],as.data.frame(predict(TimeSeriesAllReferralsRoyalGlamMonthlyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamMonthlyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesAllReferralsRoyalGlamMonthlyTest[0:10],as.vector(SSAAllReferralsRoyalGlamMonthlyForecast$mean))

accuracy(TimeSeriesAllReferralsRoyalGlamMonthlyTest[0:10],as.vector(TimeSeriesAllReferralsRoyalGlamMonthlyTrainNNetPlot$mean))




plot(days[500:532],TimeSeriesAllReferralsAllHospitalsDaily[500:532],type="l",ylim=c(0,20),lwd=2,xlab="2019",ylab="Referrals",main="All referrals, all hospitals daily forecasts")
abline(v = days[523], col="red", lwd=3)
points(days[523:532],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsDailyTrainARIMA,h=10))[,1],type="l",col="green",lwd=2,lty=2)
points(days[523:532],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsDailyTrainNNet,h=10))[,1],type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","ARIMA","ANN"),
       col=c("red","green","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

plot(weeks[50:84],TimeSeriesAllReferralsAllHospitalsWeekly[50:84],type="l",lwd=2,ylim=c(0,40),xlab="2019",ylab="Referrals",main="All referrals, all hospitals weekly forecasts")
abline(v = weeks[75], col="red", lwd=3)
points(weeks[75:84],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsWeeklyTrainARIMA,h=10))[,1],type="l",col="green",lwd=2,lty=2)
points(weeks[75:84],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsWeeklyTrainNNet,h=10))[,1],type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","ARIMA", "ANN"),
       col=c("red","green","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

as.data.frame(forecast(arima(TimeSeriesAllReferralsAllHospitalsWeekly,order=c(1,1,1)),h=10))[,1]

plot(months[5:19],TimeSeriesAllReferralsAllHospitalsMonthly[5:19],type="l",lwd=2,xlab="Time",ylab="Referrals",main="All referrals, all hospitals monthly forecasts")
abline(v = months[17], col="red", lwd=3)
points(months[17:19],as.vector(TimeSeriesAllReferralsAllHospitalsMonthlyTrainNNetPlot$mean)[0:3],type="l",col="green",lwd=2,lty=2)
points(months[17:19],as.data.frame(predict(TimeSeriesAllReferralsAllHospitalsMonthlyTrainSimpReg$fitted.values,h=3))[,1],col="red",type="l",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","ANN", "Simple linear regression"),
       col=c("red","green","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)


#Consider hybrid model- as text in lit review suggested.
install.packages("forecastHybrid")


TimeSeriesAllReferralsAllHospitalsDailyHybrid<-forecastHybrid::hybridModel(TimeSeriesAllReferralsAllHospitalsDailyTrain)
summary(TimeSeriesAllReferralsAllHospitalsDailyHybrid)
TimeSeriesAllReferralsAllHospitalsWeeklyHybrid<-forecastHybrid::hybridModel(TimeSeriesAllReferralsAllHospitalsWeeklyTrain)
TimeSeriesAllReferralsAllHospitalsMonthlyHybrid<-forecastHybrid::hybridModel(TimeSeriesAllReferralsAllHospitalsMonthlyTrain)

accuracy(TimeSeriesAllReferralsAllHospitalsDailyHybrid)
accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyHybrid)
accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyHybrid)

accuracy(TimeSeriesAllReferralsAllHospitalsDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsDailyHybrid,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsWeeklyHybrid,h=10))[,1])

accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsMonthlyHybrid,h=10))[,1])


plot(weeks[50:84],TimeSeriesAllReferralsAllHospitalsWeekly[50:84],type="l",lwd=2,ylim=c(0,40),xlab="2019",ylab="Referrals",main="All referrals, all hospitals weekly forecasts")
abline(v = weeks[75], col="red", lwd=3)
points(weeks[75:84],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsWeeklyHybrid,h=10))[,1],type="l",col="green",lwd=2,lty=2)
points(weeks[75:84],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsWeeklyTrainNNet,h=10))[,1],type="l",col="red",lwd=2,lty=2)
legend("topleft", legend=c("Training/test partition","Hybrid","ANN"),
       col=c("red","purple","red"), lty=c(1,2,2),bty = "n", cex=0.8,lwd=2)

plot(months[5:19],TimeSeriesAllReferralsAllHospitalsMonthly[5:19],type="l",lwd=2,xlab="2019",ylab="Referrals",main="All referrals, all hospitals monthly forecasts")
abline(v = months[17], col="red", lwd=3)
points(months[17:19],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsMonthlyHybrid,h=3))[,1],type="l",col="purple",lwd=2,lty=2)
TimeSeriesAllReferralsAllHospitalsMonthlyTrainNNetForecast<-c(78.84433, 94.11058, 95.58019, 83.35728, 95.94944, 82.99615, 74.69777)
points(months[17:19],TimeSeriesAllReferralsAllHospitalsMonthlyTrainNNetForecast[1:3],type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","Hybrid","ANN"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)


forecast(TimeSeriesAllReferralsAllHospitalsMonthlyTrainNNet,h=7)

accuracy(TimeSeriesAllReferralsAllHospitalsMonthlyHybrid)

#Predictions on entire time series

TimeSeriesAllReferralsAllHospitalsDailyNNet<-nnetar(TimeSeriesAllReferralsAllHospitalsDaily)
TimeSeriesAllReferralsAllHospitalsDailyNNetPlot<-forecast(TimeSeriesAllReferralsAllHospitalsDailyNNet,PI=T,h=10)
as.data.frame(TimeSeriesAllReferralsAllHospitalsDailyNNetPlot)[,1]

TimeSeriesAllReferralsPrinceChDailySSA<-ssa(TimeSeriesAllReferralsPrinceChDaily)
TimeSeriesAllReferralsPrinceChDailySSAPlot<-forecast(TimeSeriesAllReferralsPrinceChDailySSA,groups=list(c(1,10)),PI=T,h=10)
as.data.frame(TimeSeriesAllReferralsPrinceChDailySSAPlot)[,1]

TSDataAllReferralsAllHospitalsWeeklyForHoltWinters<-c(as.vector(TimeSeriesAllReferralsAllHospitalsWeekly)[3:12],as.vector(TimeSeriesAllReferralsAllHospitalsWeekly))
TimeSeriesAllReferralsAllHospitalsForHoltWintersWeekly<-ts(TSDataAllReferralsAllHospitalsWeeklyForHoltWinters,start=c(2017,03,02),freq=12)
TimeSeriesAllReferralsAllHospitalsWeeklyHoltWinters<-hw(TimeSeriesAllReferralsAllHospitalsForHoltWintersWeekly)
plot(TimeSeriesAllReferralsAllHospitalsWeeklyHoltWinters,ylim=c(0,150),main="Weekly Holt-Winters method on all referrals across all hospitals",xlab="Year",ylab="Referrals")
TimeSeriesAllReferralsAllHospitalsWeeklyHoltWinters

TimeSeriesAllReferralsAllHospitalsMonthlyNNet<-nnetar(TimeSeriesAllReferralsAllHospitalsMonthly)
TimeSeriesAllReferralsAllHospitalsMonthlyNNetPlot<-forecast(TimeSeriesAllReferralsAllHospitalsMonthlyNNet,PI=T,h=4)
as.data.frame(TimeSeriesAllReferralsAllHospitalsMonthlyNNetPlot)[,1]


accuracy(TimeSeriesAllReferralsAllHospitalsMonthly[17:23],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsMonthlyHybrid,h=7))[,1])

TimeSeriesAllReferralsAllHospitalsMonthly[17:23]
as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsMonthlyHybrid,h=7))[,1]

accuracy(TimeSeriesAllReferralsPrinceChMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChMonthlyHoltWinters,h=5))[,1])

TimeSeriesAllReferralsPrinceChMonthlyTest[0:10]


plot(days[500:532],TimeSeriesAllReferralsAllHospitalsDaily[500:532],type="l",ylim=c(0,20),lwd=2,xlab="2019",ylab="Referrals",main="All referrals, all hospitals daily forecasts")
abline(v = days[523], col="red", lwd=3)
points(days[523:532],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsDailyHybrid,h=10))[,1],type="l",col="purple",lwd=2,lty=2)
points(days[523:532],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsDailyTrainNNet,h=10))[,1],type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","Hybrid","ANN"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

plot(days[500:532],TimeSeriesAllReferralsPrinceChDaily[500:532],type="l",ylim=c(0,20),lwd=2,xlab="2019",ylab="Referrals",main="All referrals, Prince Charles daily forecasts")
abline(v = days[523], col="red", lwd=3)
points(days[523:532],as.data.frame(forecast(SSAAllReferralsPrinceChDailyForecast,h=10))[,1],type="l",col="purple",lwd=2,lty=2)
points(days[523:532],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChDailyTrainNNet,h=10))[,1],type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","SSA","ANN"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

plot(forecast(SSAAllReferralsPrinceChDailyForecast,h=10),type="l",lwd=2,xlab="Year",ylab="Referrals",main="All referrals, Prince Charles daily forecasts")


plot(days[500:532],TimeSeriesAllReferralsRoyalGlamDaily[500:532],type="l",ylim=c(0,20),lwd=2,xlab="2019",ylab="Referrals",main="All referrals, Royal Glamorgan daily forecasts")
abline(v = days[523], col="red", lwd=3)
points(days[523:532],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamDailyNaive,h=10))[,1],type="l",col="purple",lwd=2,lty=2)
points(days[523:532],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamDailyTrainNNet,h=10))[,1],type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","Naive","ANN"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

forecast(TimeSeriesAllReferralsRoyalGlamDaily,h=10)
plot(forecast(TimeSeriesAllReferralsRoyalGlamDailyTrainNNet,h=10),type="l",lwd=2,xlab="Year",ylab="Referrals",main="All referrals, Royal Glamorgan daily forecasts")


plot(weeks[50:84],TimeSeriesAllReferralsAllHospitalsWeekly[50:84],type="l",lwd=2,ylim=c(0,40),xlab="2019",ylab="Referrals",main="All referrals, all hospitals weekly forecasts")
abline(v = weeks[75], col="red", lwd=3)
points(weeks[75:84],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsWeeklyTrainARIMA,h=10))[,1],type="l",col="red",lwd=2,lty=3)
points(weeks[75:84],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsWeeklyHybrid,h=10))[,1],type="l",col="purple",lwd=2,lty=2)
legend("topleft", legend=c("Training/test partition","Hybrid","ARIMA"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

plot(weeks[50:84],TimeSeriesAllReferralsPrinceChWeekly[50:84],type="l",lwd=2,ylim=c(0,40),xlab="2019",ylab="Referrals",main="All referrals, Prince Charles weekly forecasts")
abline(v = weeks[75], col="red", lwd=3)
points(weeks[75:84],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChWeeklyHoltWinters,h=10))[,1],type="l",col="red",lwd=2,lty=3)
points(weeks[75:84],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChWeeklyTrainNNet,h=10))[,1],type="l",col="purple",lwd=2,lty=2)
legend("topleft", legend=c("Training/test partition","Holt-Winters","ANN"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

plot(weeks[50:84],TimeSeriesAllReferralsRoyalGlamWeekly[50:84],type="l",lwd=2,ylim=c(0,40),xlab="2019",ylab="Referrals",main="All referrals, Royal Glamorgan weekly forecasts")
abline(v = weeks[75], col="red", lwd=3)
points(weeks[75:84],as.data.frame(predict(TimeSeriesAllReferralsRoyalGlamWeeklyTrainSimpReg$fitted.values,h=10))[,1],type="l",col="red",lwd=2,lty=3)
points(weeks[75:84],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamWeeklyTrainNNet,h=10))[,1],type="l",col="purple",lwd=2,lty=2)
legend("topleft", legend=c("Training/test partition","Simple linear regression","ANN"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)


plot(months[5:19],TimeSeriesAllReferralsAllHospitalsMonthly[5:19],type="l",lwd=2,xlab="2019",ylab="Referrals",main="All referrals, all hospitals weekly forecasts")
abline(v = months[17], col="red", lwd=3)
points(months[17:19],as.data.frame(forecast(TimeSeriesAllReferralsAllHospitalsMonthlyHybrid,h=3))[,1],type="l",col="purple",lwd=2,lty=2)
points(months[17:19],as.vector(TimeSeriesAllReferralsAllHospitalsMonthlyTrainNNetPlot$mean)[0:3],type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","Hybrid","ANN"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

plot(months[5:19],TimeSeriesAllReferralsPrinceChMonthly[5:19],type="l",lwd=2,xlab="2019",ylab="Referrals",main="All referrals, Prince Charles monthly forecasts")
abline(v = months[17], col="red", lwd=3)
points(months[17:19],as.data.frame(forecast(TimeSeriesAllReferralsPrinceChMonthlySES,h=3))[,1],type="l",col="purple",lwd=2,lty=2)
points(months[17:19],as.vector(TimeSeriesAllReferralsPrinceChMonthlyTrainNNetPlot$mean)[0:3],type="l",col="red",lwd=2,lty=3)
legend("top", legend=c("Training/test partition","SES","ANN"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

plot(months[5:19],TimeSeriesAllReferralsRoyalGlamMonthly[5:19],type="l",lwd=2,xlab="2019",ylab="Referrals",main="All referrals, Royal Glamorgan monthly forecasts")
abline(v = months[17], col="red", lwd=3)
points(months[17:19],as.data.frame(forecast(TimeSeriesAllReferralsRoyalGlamMonthlyHoltWinters,h=3))[,1],type="l",col="purple",lwd=2,lty=2)
points(months[17:19],as.vector(TimeSeriesAllReferralsRoyalGlamMonthlyTrainNNetPlot$mean)[0:3],type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","Holt-Winters","ANN"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)


forecast(nnetar(TimeSeriesAllReferralsAllHospitalsDaily),h=10)
plot(forecast(nnetar(TimeSeriesAllReferralsAllHospitalsDaily),h=10),type="l",lwd=2,xlab="Year",ylab="Referrals",main="All referrals, all hospitals daily forecasts")

plot(forecast(auto.arima(TimeSeriesAllReferralsAllHospitalsWeekly),h=10),type="l",lwd=2,xlab="Year",ylab="Referrals",main="All referrals, all hospitals weekly forecasts")

plot(forecast(TimeSeriesAllReferralsAllHospitalsMonthlyNNet,h=3),type="l",lwd=2,xlab="Year",ylab="Referrals",main="All referrals, all hospitals monthly forecasts")

forecast(ssa(TimeSeriesAllReferralsPrinceChDaily),h=10,groups=list(c(1,10)))
plot(forecast(ssa(TimeSeriesAllReferralsPrinceChDaily),h=10,groups=list(c(1,10))),type="l",lwd=2,xlab="Year",ylab="Referrals",main="All referrals, Prince Charles Daily forecasts")

forecast(nnetar(TimeSeriesAllReferralsRoyalGlamDaily),h=10)
plot(forecast(nnetar(TimeSeriesAllReferralsRoyalGlamDaily),h=10),type="l",lwd=2,xlab="Year",ylab="Referrals",main="All referrals, Royal Glamorgan Daily forecasts")

forecast(nnetar(TimeSeriesAllReferralsPrinceChWeekly),h=10)
plot(forecast(nnetar(TimeSeriesAllReferralsPrinceChWeekly),h=10),type="l",lwd=2,xlab="Year",ylab="Referrals",main="All referrals, Prince Charles weekly forecasts")



as.data.frame(predict(lm(TimeSeriesAllReferralsRoyalGlamWeekly~weeks)$fitted.values,h=10))[,1]
plot(forecast(holt(TimeSeriesAllReferralsRoyalGlamWeekly),h=10),type="l",lwd=2,xlab="Year",ylab="Referrals",main="All referrals, Royal Glamorgan weekly forecasts")

forecast(ses(TimeSeriesAllReferralsPrinceChMonthly),h=3)
plot(forecast(ses(TimeSeriesAllReferralsPrinceChMonthly),h=3),type="l",lwd=2,xlab="Year",ylab="Referrals",main="All referrals, Prince Charles monthly forecasts")

forecast(hw(TimeSeriesAllReferralsRoyalGlamMonthly),h=3)
plot(forecast(hw(TimeSeriesAllReferralsRoyalGlamMonthly),h=3),type="l",lwd=2,xlab="Year",ylab="Referrals",main="All referrals, Royal Glamorgan monthly forecasts")

#Barplot- comparisons of error statistics
meanMAE<-data.frame(daily=c(2.012,2.33
),weekly=c(4.623333333,5.978888889
),monthly=c(15.77,21.45888889

))
rownames(meanMAE)<-c("referrals","tests")
meanMAE

barplot(as.matrix(meanMAE),beside=T,col=c("cornflowerblue","orange"),ylim=c(0,50),ylab="Mean MAE across all methods",main="Mean MAE across all methods (referrals and tests)")
legend("topright", legend = rownames(meanMAE), box.lty = 0, cex = 1,fill=c("cornflowerblue","orange"))

meanMAPE<-data.frame(daily=c(94.655
,163.212
),weekly=c(26.20555556
,30.18555556
),monthly=c(23.56555556
,24.85777778
))
rownames(meanMAPE)<-c("referrals","tests")
meanMAPE

barplot(as.matrix(meanMAPE),beside=T,col=c("cornflowerblue","orange"),ylim=c(0,200),ylab="Mean MAPE across all methods",main="Mean MAPE across all methods (referrals and tests)")
legend("topright", legend = rownames(meanMAPE), box.lty = 0, cex = 1,fill=c("cornflowerblue","orange"))
