library("readxl")
set.seed(999)

LungTestReq<-as.data.frame(read_excel("LungTrackerExtractMasterCopy with week and month by DateSuspicion and Requested tests.xlsx",sheet = "TestData"))

#all types of tests
table(LungTestReq$TestName)


#Daily
TSDataAllTestsByReqDateDaily<-cbind(seq(1:783),as.integer(tabulate(LungTestReq$Days,783)))
plot(TSDataAllTestsByReqDateDaily,type="l")

table(tabulate(LungTestReq$Days,783))
hist(tabulate(LungTestReq$Days,783),breaks = 50)

#Weekly
tabulate(LungTestReq$Week,112)
TSDataAllTestsByReqDateWeekly<-cbind(seq(1:112),as.integer(tabulate(LungTestReq$Week,112)))
plot(TSDataAllTestsByReqDateWeekly,type="l")

table(tabulate(LungTestReq$Week,112))
hist(tabulate(LungTestReq$Week,112),breaks = 10)

#Monthly
tabulate(LungTestReq$Month,26)
TSDataAllTestsByReqDateMonthly<-cbind(seq(1:26),as.integer(tabulate(LungTestReq$Month,26)))
plot(TSDataAllTestsByReqDateMonthly,type="l")

table(tabulate(LungTestReq$Month,26))
hist(tabulate(LungTestReq$Month,26),breaks = 15)

#Create time series for each hospital, all tests

LungTestReqPrinceCh<-LungTestReq[LungTestReq$CareFacility=="Prince Charles Hospital",]
LungTestReqRoyalGlam<-LungTestReq[LungTestReq$CareFacility=="Royal Glamorgan Hospital",]

#Prince Charles
TSDataAllTestsByReqDatePrinceChDaily<-cbind(seq(1:783),as.integer(tabulate(LungTestReqPrinceCh$Days,783)))
TSDataAllTestsByReqDatePrinceChDaily
plot(TSDataAllTestsByReqDatePrinceChDaily,type="l")
table(tabulate(LungTestReqPrinceCh$Days,783))
hist(tabulate(LungTestReqPrinceCh$Days,783))

TSDataAllTestsByReqDatePrinceChWeekly<-cbind(seq(1:112),as.integer(tabulate(LungTestReqPrinceCh$Week,112)))
TSDataAllTestsByReqDatePrinceChWeekly
plot(TSDataAllTestsByReqDatePrinceChWeekly,type="l")
table(tabulate(LungTestReqPrinceCh$Week,112))
hist(tabulate(LungTestReqPrinceCh$Week,112),breaks = 10)

TSDataAllTestsByReqDatePrinceChMonthly<-cbind(seq(1:26),as.integer(tabulate(LungTestReqPrinceCh$Month,26)))
TSDataAllTestsByReqDatePrinceChMonthly
plot(TSDataAllTestsByReqDatePrinceChMonthly,type="l")
table(tabulate(LungTestReqPrinceCh$Month,26))
hist(tabulate(LungTestReqPrinceCh$Month,26),breaks = 10)

#Royal Glamorgan
TSDataAllTestsByReqDateRoyalGlamDaily<-cbind(seq(1:783),as.integer(tabulate(LungTestReqRoyalGlam$Days,783)))
TSDataAllTestsByReqDateRoyalGlamDaily
plot(TSDataAllTestsByReqDateRoyalGlamDaily,type="l")
table(tabulate(LungTestReqRoyalGlam$Days,783))
hist(tabulate(LungTestReqRoyalGlam$Days,783))

TSDataAllTestsByReqDateRoyalGlamWeekly<-cbind(seq(1:112),as.integer(tabulate(LungTestReqRoyalGlam$Week,112)))
TSDataAllTestsByReqDateRoyalGlamWeekly
plot(TSDataAllTestsByReqDateRoyalGlamWeekly,type="l")
table(tabulate(LungTestReqRoyalGlam$Week,112))
hist(tabulate(LungTestReqRoyalGlam$Week,112),breaks = 10)

TSDataAllTestsByReqDateRoyalGlamMonthly<-cbind(seq(1:26),as.integer(tabulate(LungTestReqRoyalGlam$Month,26)))
TSDataAllTestsByReqDateRoyalGlamMonthly
plot(TSDataAllTestsByReqDateRoyalGlamMonthly,type="l")
table(tabulate(LungTestReqRoyalGlam$Month,26))
hist(tabulate(LungTestReqRoyalGlam$Month,26),breaks = 10)

#Plotting all hospitals
plot(TSDataAllTestsByReqDatePrinceChWeekly,type="l",col="red",lwd=2)
points(TSDataAllTestsByReqDateRoyalGlamWeekly,type="l",col="blue",lwd=2)

plot(TSDataAllTestsByReqDatePrinceChMonthly,type="l",col="red",lwd=2)
points(TSDataAllTestsByReqDateRoyalGlamMonthly,type="l",col="blue",lwd=2)

plot(TSDataAllTestsByReqDatePrinceChDaily,type="l",col="red",lwd=2)
points(TSDataAllTestsByReqDateRoyalGlamDaily,type="l",col="blue",lwd=2)

#Create time series for each test type, across all hospitals

table(LungTestReq$TestName)
LungTestReqCT<-LungTestReq[LungTestReq$TestName=="CT"|LungTestReq$TestName=="CT abdomen"|LungTestReq$TestName=="CT chest"|LungTestReq$TestName=="CT colon"|LungTestReq$TestName=="CT head"|LungTestReq$TestName=="CT TAP"|LungTestReq$TestName=="CT THORAX"|LungTestReq$TestName=="CT THORAX AND ABDOMEN"|LungTestReq$TestName=="CT Urinary Tract",]
LungTestReqPET<-LungTestReq[LungTestReq$TestName=="PET",]
LungTestReqBronchoscopy<-LungTestReq[LungTestReq$TestName=="Bronchoscopy",]
LungTestReqEBUS<-LungTestReq[LungTestReq$TestName=="EBUS",]
LungTestReqUSGuidedBiopsy<-LungTestReq[LungTestReq$TestName=="US guided biopsy",]
LungTestReqCTGuidedBiopsy<-LungTestReq[LungTestReq$TestName=="CT Guided Biopsy",]
LungTestReqMRI<-LungTestReq[LungTestReq$TestName=="MRI",]

#Create an object for "other" tests grouped together (tests except CT and PET)

LungTestReqOther<-LungTestReq[LungTestReq$TestName=="Bronchoscopy"|LungTestReq$TestName=="EBUS"|LungTestReq$TestName=="US guided biopsy"|LungTestReq$TestName=="CT Guided Biopsy"|LungTestReq$TestName=="MRI",]

#CT
TSTestCTAllHospitalsDaily<-cbind(seq(1:783),as.integer(tabulate(LungTestReqCT$Days,783)))
plot(TSTestCTAllHospitalsDaily,type="l")
table(tabulate(LungTestReqCT$Days,783))
hist(tabulate(LungTestReqCT$Days,783),breaks = 15)

TSTestCTAllHospitalsWeekly<-cbind(seq(1:112),as.integer(tabulate(LungTestReqCT$Week,112)))
plot(TSTestCTAllHospitalsWeekly,type="l")
table(tabulate(LungTestReqCT$Week,112))
hist(tabulate(LungTestReqCT$Week,112),breaks = 15)

TSTestCTAllHospitalsMonthly<-cbind(seq(1:26),as.integer(tabulate(LungTestReqCT$Month,26)))
plot(TSTestCTAllHospitalsMonthly,type="l")
table(tabulate(LungTestReqCT$Month,26))
hist(tabulate(LungTestReqCT$Month,26),breaks = 15)

#PET
TSTestPETAllHospitalsDaily<-cbind(seq(1:783),as.integer(tabulate(LungTestReqPET$Days,783)))
plot(TSTestPETAllHospitalsDaily,type="l")
table(tabulate(LungTestReqPET$Days,783))
hist(tabulate(LungTestReqPET$Days,783),breaks = 15)

TSTestPETAllHospitalsWeekly<-cbind(seq(1:112),as.integer(tabulate(LungTestReqPET$Week,112)))
plot(TSTestPETAllHospitalsWeekly,type="l")
table(tabulate(LungTestReqPET$Week,112))
hist(tabulate(LungTestReqPET$Week,112),breaks = 15)

TSTestPETAllHospitalsMonthly<-cbind(seq(1:26),as.integer(tabulate(LungTestReqPET$Month,26)))
plot(TSTestPETAllHospitalsMonthly,type="l")
table(tabulate(LungTestReqPET$Month,26))
hist(tabulate(LungTestReqPET$Month,26),breaks = 15)

#Bronchoscopy
TSTestBronchoscopyAllHospitalsWeekly<-cbind(seq(1:112),as.integer(tabulate(LungTestReqBronchoscopy$Week,112)))
plot(TSTestBronchoscopyAllHospitalsWeekly,type="l")
table(tabulate(LungTestReqBronchoscopy$Week,112))
hist(tabulate(LungTestReqBronchoscopy$Week,112),breaks = 15)

TSTestBronchoscopyAllHospitalsMonthly<-cbind(seq(1:26),as.integer(tabulate(LungTestReqBronchoscopy$Month,26)))
plot(TSTestBronchoscopyAllHospitalsMonthly,type="l")
table(tabulate(LungTestReqBronchoscopy$Month,26))
hist(tabulate(LungTestReqBronchoscopy$Month,26),breaks = 15)

#EBUS
TSTestEBUSAllHospitalsWeekly<-cbind(seq(1:112),as.integer(tabulate(LungTestReqEBUS$Week,112)))
plot(TSTestEBUSAllHospitalsWeekly,type="l")
table(tabulate(LungTestReqEBUS$Week,112))
hist(tabulate(LungTestReqEBUS$Week,112),breaks = 15)

TSTestEBUSAllHospitalsMonthly<-cbind(seq(1:26),as.integer(tabulate(LungTestReqEBUS$Month,26)))
plot(TSTestEBUSAllHospitalsMonthly,type="l")
table(tabulate(LungTestReqEBUS$Month,26))
hist(tabulate(LungTestReqEBUS$Month,26),breaks = 15)

#US Guided Biopsy
TSTestUSGuidedBiopsyAllHospitalsWeekly<-cbind(seq(1:112),as.integer(tabulate(LungTestReqUSGuidedBiopsy$Week,112)))
plot(TSTestUSGuidedBiopsyAllHospitalsWeekly,type="l")
table(tabulate(LungTestReqUSGuidedBiopsy$Week,112))
hist(tabulate(LungTestReqUSGuidedBiopsy$Week,112),breaks = 15)

TSTestUSGuidedBiopsyAllHospitalsMonthly<-cbind(seq(1:26),as.integer(tabulate(LungTestReqUSGuidedBiopsy$Month,26)))
plot(TSTestUSGuidedBiopsyAllHospitalsMonthly,type="l")
table(tabulate(LungTestReqUSGuidedBiopsy$Month,26))
hist(tabulate(LungTestReqUSGuidedBiopsy$Month,26),breaks = 15)

#CT Guided Biopsy
TSTestCTGuidedBiopsyAllHospitalsWeekly<-cbind(seq(1:112),as.integer(tabulate(LungTestReqCTGuidedBiopsy$Week,112)))
plot(TSTestCTGuidedBiopsyAllHospitalsWeekly,type="l")
table(tabulate(LungTestReqCTGuidedBiopsy$Week,112))
hist(tabulate(LungTestReqCTGuidedBiopsy$Week,112),breaks = 15)

TSTestCTGuidedBiopsyAllHospitalsMonthly<-cbind(seq(1:26),as.integer(tabulate(LungTestReqCTGuidedBiopsy$Month,26)))
plot(TSTestCTGuidedBiopsyAllHospitalsMonthly,type="l")
table(tabulate(LungTestReqCTGuidedBiopsy$Month,26))
hist(tabulate(LungTestReqCTGuidedBiopsy$Month,26),breaks = 15)

#MRI
TSTestMRIAllHospitalsWeekly<-cbind(seq(1:112),as.integer(tabulate(LungTestReqMRI$Week,112)))
plot(TSTestMRIAllHospitalsWeekly,type="l")
table(tabulate(LungTestReqMRI$Week,112))
hist(tabulate(LungTestReqMRI$Week,112),breaks = 15)

TSTestMRIAllHospitalsMonthly<-cbind(seq(1:26),as.integer(tabulate(LungTestReqMRI$Month,26)))
plot(TSTestMRIAllHospitalsMonthly,type="l")
table(tabulate(LungTestReqMRI$Month,26))
hist(tabulate(LungTestReqMRI$Month,26),breaks = 15)

#Other
TSTestOtherAllHospitalsDaily<-cbind(seq(1:783),as.integer(tabulate(LungTestReqOther$Days,783)))
plot(TSTestOtherAllHospitalsDaily,type="l")
table(tabulate(LungTestReqOther$Days,783))
hist(tabulate(LungTestReqOther$Days,783),breaks = 15)

TSTestOtherAllHospitalsWeekly<-cbind(seq(1:112),as.integer(tabulate(LungTestReqOther$Week,112)))
plot(TSTestOtherAllHospitalsWeekly,type="l")
table(tabulate(LungTestReqOther$Week,112))
hist(tabulate(LungTestReqOther$Week,112),breaks = 15)

TSTestOtherAllHospitalsMonthly<-cbind(seq(1:26),as.integer(tabulate(LungTestReqOther$Month,26)))
plot(TSTestOtherAllHospitalsMonthly,type="l")
table(tabulate(LungTestReqOther$Month,26))
hist(tabulate(LungTestReqOther$Month,26),breaks = 15)

#Plot all test types weekly
plot(TSTestCTAllHospitalsWeekly,type="l",lwd=2,col="black")
points(TSTestPETAllHospitalsWeekly,type="l",lwd=2,col="red")
points(TSTestBronchoscopyAllHospitalsWeekly,type="l",lwd=2,col="blue")
points(TSTestEBUSAllHospitalsWeekly,type="l",lwd=2,col="green")
points(TSTestUSGuidedBiopsyAllHospitalsWeekly,type="l",lwd=2,col="gold")
points(TSTestCTGuidedBiopsyAllHospitalsWeekly,type="l",lwd=2,col="red")
points(TSTestMRIAllHospitalsWeekly,type="l",lwd=2,col="light blue")

#Plot all test types monthly
plot(TSTestCTAllHospitalsMonthly,type="l",lwd=2,col="black")
points(TSTestPETAllHospitalsMonthly,type="l",lwd=2,col="red")
points(TSTestBronchoscopyAllHospitalsMonthly,type="l",lwd=2,col="blue")
points(TSTestEBUSAllHospitalsMonthly,type="l",lwd=2,col="green")
points(TSTestUSGuidedBiopsyAllHospitalsMonthly,type="l",lwd=2,col="gold")
points(TSTestCTGuidedBiopsyAllHospitalsMonthly,type="l",lwd=2,col="red")
points(TSTestMRIAllHospitalsMonthly,type="l",lwd=2,col="light blue")

#Plot all CT, PET and all others weekly
plot(TSTestCTAllHospitalsWeekly,type="l",lwd=2,col="black")
points(TSTestPETAllHospitalsWeekly,type="l",lwd=2,col="red")
points(TSTestOtherAllHospitalsWeekly,type="l",lwd=2,col="gold")

#Plot all CT, PET and all others monthly
plot(TSTestCTAllHospitalsMonthly,type="l",lwd=2,col="black")
points(TSTestPETAllHospitalsMonthly,type="l",lwd=2,col="red")
points(TSTestOtherAllHospitalsMonthly,type="l",lwd=2,col="gold")

#Plotting all hospitals with dates
days<-seq(as.Date("2017-11-02"), as.Date("2019-12-24"), by = "day")
weeks<-seq(as.Date("2017-11-02"), as.Date("2019-12-24"), by = "week")
months<-seq(as.Date("2017-11-02"), as.Date("2019-12-24"), by = "month")

par(mfrow=c(3,1))

plot(days,TSDataAllTestsByReqDateDaily[,2],type="l",lwd=2,xlab="Year",ylab="Tests requested",main="Daily time series for all tests across all hospitals")

plot(weeks,TSDataAllTestsByReqDateWeekly[,2],type="l",lwd=2,xlab="Year",ylab="Tests requested",main="Weekly time series for all tests across all hospitals")

plot(months,TSDataAllTestsByReqDateMonthly[,2],type="l",lwd=2,xlab="Year",ylab="Tests requested",main="Monthly time series for all tests across all hospitals")

table(LungTestReq$CareFacility)

plot(days,TSDataAllTestsByReqDatePrinceChDaily[,2],type="l",col="red",lwd=2,xlab="Year",ylab="Tests requested",main="Daily time series for all tests across all hospitals")
points(days,TSDataAllTestsByReqDateRoyalGlamDaily[,2],type="l",col="blue",lwd=2)
legend("topleft", legend=c("Prince Charles", "Royal Glamorgan"),
       col=c("red", "blue"), lty=1:1,bty = "n", cex=1,lwd=2)

plot(weeks,TSDataAllTestsByReqDatePrinceChWeekly[,2],type="l",col="red",lwd=2,xlab="Year",ylab="Tests requested",main="Weekly time series for all tests across all hospitals")
points(weeks,TSDataAllTestsByReqDateRoyalGlamWeekly[,2],type="l",col="blue",lwd=2)
legend("topleft", legend=c("Prince Charles", "Royal Glamorgan"),
       col=c("red", "blue"), lty=1:1,bty = "n", cex=1,lwd=2)

plot(months,TSDataAllTestsByReqDatePrinceChMonthly[,2],type="l",col="red",lwd=2,xlab="Year",ylab="Tests requested",main="Monthly time series for all tests across all hospitals")
points(months,TSDataAllTestsByReqDateRoyalGlamMonthly[,2],type="l",col="blue",lwd=2)
legend("topleft", legend=c("Prince Charles", "Royal Glamorgan"),
       col=c("red", "blue"), lty=1:1,bty = "n", cex=1,lwd=2)


#Plotting by Test type for Prince Charles and Royal Glamorgan Weekly

table(LungTestReq$TestName)

par(mfrow=c(2,1))


#Weekly by test types, all hospitals
plot(weeks,TSTestCTAllHospitalsWeekly[,2],type="l",xlab="Year",lwd=2,ylab="Tests requested",main="Weekly time series for each test type across all hospitals",ylim=c(-1,22),col="lightgreen")
points(weeks,TSTestPETAllHospitalsWeekly[,2],type="l",lwd=2,col="purple")
points(weeks,TSTestBronchoscopyAllHospitalsWeekly[,2],type="l",lwd=2,col="magenta")
points(weeks,TSTestEBUSAllHospitalsWeekly[,2],type="l",lwd=2,col="black")
points(weeks,TSTestUSGuidedBiopsyAllHospitalsWeekly[,2],type="l",lwd=2,col="gold")
points(weeks,TSTestCTGuidedBiopsyAllHospitalsWeekly[,2],type="l",lwd=2,col="red")
points(weeks,TSTestMRIAllHospitalsWeekly[,2],type="l",lwd=2,col="light blue")
legend("topleft", legend=c("CT","PET","Bronchoscopy","EBUS","US Guided Biopsy","CT Guided Biopsy","MRI"),
       col=c("lightgreen", "purple","magenta","black","gold","red","light blue"), lty=1:1,bty = "n", cex=0.7,lwd=2)

#Montly by test types, all hospitals
plot(months,TSTestCTAllHospitalsMonthly[,2],type="l",xlab="Year",col="lightgreen",lwd=2,ylab="Tests requested",main="Monthly time series for each test type across all hospitals",ylim=c(-1,65))
points(months,TSTestPETAllHospitalsMonthly[,2],type="l",lwd=2,col="purple")
points(months,TSTestBronchoscopyAllHospitalsMonthly[,2],type="l",lwd=2,col="magenta")
points(months,TSTestEBUSAllHospitalsMonthly[,2],type="l",lwd=2,col="black")
points(months,TSTestUSGuidedBiopsyAllHospitalsMonthly[,2],type="l",lwd=2,col="gold")
points(months,TSTestCTGuidedBiopsyAllHospitalsMonthly[,2],type="l",lwd=2,col="red")
points(months,TSTestMRIAllHospitalsMonthly[,2],type="l",lwd=2,col="light blue")
legend("topleft", legend=c("CT","PET","Bronchoscopy","EBUS","US Guided Biopsy","CT Guided Biopsy","MRI"),
       col=c("lightgreen", "purple","magenta","black","gold","red","light blue"), lty=1:1,bty = "n", cex=0.83,lwd=2,horiz = T)

par(mfrow=c(2,1))

#Weekly all hospitals, CT, PET and all other tests grouped
plot(weeks,TSTestCTAllHospitalsWeekly[,2],type="l",xlab="Year",lwd=2,ylab="Tests requested",main="Weekly time series for each test type across all hospitals",ylim=c(-1,22),col="lightgreen")
points(weeks,TSTestPETAllHospitalsWeekly[,2],type="l",lwd=2,col="purple",lty=2)
points(weeks,TSTestOtherAllHospitalsWeekly[,2],type="l",lwd=2,col="gold",lty=3)


#Monthly all hospitals, CT, PET and all other tests grouped
plot(months,TSTestCTAllHospitalsMonthly[,2],type="l",xlab="Year",lwd=2,ylab="Tests requested",main="Monthly time series for each test type across all hospitals",ylim=c(-1,65),col="lightgreen")
points(months,TSTestPETAllHospitalsMonthly[,2],type="l",lwd=2,col="purple",lty=2)
points(months,TSTestOtherAllHospitalsMonthly[,2],type="l",lwd=2,col="gold",lty=3)

dev.off()
#Create legend for plot
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend=c("CT","PET","All other tests"),
       col=c("lightgreen", "purple","gold"), bty = "n", cex=1,lwd=2,lty=c(1,2,3))

#Boxplots

days.of.week <- c("Sun","Mon", "Tues", "Weds", "Thurs", "Fri", "Sat")
Dataset_subset_by_DateReq_Test <- read_excel("Dataset subset by DateReq Test.xlsx")


dev.off()
layout(matrix(c(1,1,1,2,3,4), 3, 2, byrow = F))

dailyreqall<-cbind(TSDataAllTestsByReqDateDaily[,2],Dataset_subset_by_DateReq_Test[,2])
boxplot(dailyreqall[,1]~dailyreqall[,2],xaxt = "n",xlab="Day",ylab="Tests requested",main="Boxplot of all tests across all hospitals by day")
axis(1, at=1:7, labels=days.of.week)

dailyreqCT<-cbind(TSTestCTAllHospitalsDaily[,2],Dataset_subset_by_DateReq_Test[,2])
boxplot(dailyreqCT[,1]~dailyreqCT[,2],xaxt = "n",xlab="Day",ylab="Tests requested",main="Boxplot of CT tests across all hospitals by day",col="lightgreen")
axis(1, at=1:7, labels=days.of.week)

dailyreqPET<-cbind(TSTestPETAllHospitalsDaily[,2],Dataset_subset_by_DateReq_Test[,2])
boxplot(dailyreqPET[,1]~dailyreqPET[,2],xaxt = "n",xlab="Day",ylab="Tests requested",main="Boxplot of PET tests across all hospitals by day",col="purple")
axis(1, at=1:7, labels=days.of.week)

dailyreqOther<-cbind(TSTestOtherAllHospitalsDaily[,2],Dataset_subset_by_DateReq_Test[,2])
boxplot(dailyreqOther[,1]~dailyreqOther[,2],xaxt = "n",xlab="Day",ylab="Tests requested",main="Boxplot of all other tests across all hospitals by day",col="gold")
axis(1, at=1:7, labels=days.of.week)

monthlyreqall<-cbind(TSDataAllTestsByReqDateMonthly[,2],na.omit(Dataset_subset_by_DateReq_Test[,4]))
boxplot(monthlyreqall[,1]~monthlyreqall[,2],xaxt = "n",xlab="Month",ylab="Tests requested",main="Boxplot of all tests across all hospitals by month")
axis(1, at=1:12, labels=month.abb[1:12])

monthlyreqCT<-cbind(TSTestCTAllHospitalsMonthly[,2],na.omit(Dataset_subset_by_DateReq_Test[,4]))
boxplot(monthlyreqCT[,1]~monthlyreqCT[,2],xaxt = "n",xlab="Month",ylab="Tests requested",main="Boxplot of all CT tests across all hospitals by month",col="lightgreen")
axis(1, at=1:12, labels=month.abb[1:12])

monthlyreqPET<-cbind(TSTestPETAllHospitalsMonthly[,2],na.omit(Dataset_subset_by_DateReq_Test[,4]))
boxplot(monthlyreqPET[,1]~monthlyreqPET[,2],xaxt = "n",xlab="Month",ylab="Tests requested",main="Boxplot of all PET tests across all hospitals by month",col="purple")
axis(1, at=1:12, labels=month.abb[1:12])

monthlyreqOther<-cbind(TSTestOtherAllHospitalsMonthly[,2],na.omit(Dataset_subset_by_DateReq_Test[,4]))
boxplot(monthlyreqOther[,1]~monthlyreqOther[,2],xaxt = "n",xlab="Month",ylab="Tests requested",main="Boxplot of all other tests across all hospitals by month",col="gold")
axis(1, at=1:12, labels=month.abb[1:12])

#Summary statistics for Daily, Weekly and Monthly total tests
#All data
summary(TSDataAllTestsByReqDateDaily[,2])

summary(TSDataAllTestsByReqDateWeekly[,2])

summary(TSDataAllTestsByReqDateMonthly[,2])

#Summary by hospital for Daily, Weekly and Monthly total tests

#By hospital
#Prince Charles
summary(TSDataAllTestsByReqDatePrinceChDaily[,2])
#Royal Glamorgan
summary(TSDataAllTestsByReqDateRoyalGlamDaily[,2])

#Weekly

summary(TSDataAllTestsByReqDatePrinceChWeekly[,2])
#Royal Glamorgan

summary(TSDataAllTestsByReqDateRoyalGlamWeekly[,2])

#Monthly

summary(TSDataAllTestsByReqDatePrinceChMonthly[,2])

#By test
#CT tests
summary(TSTestCTAllHospitalsWeekly[,2])
summary(TSTestCTAllHospitalsMonthly[,2])

#PET tests
summary(TSTestPETAllHospitalsWeekly[,2])
summary(TSTestPETAllHospitalsMonthly[,2])

#Other tests
summary(TSTestOtherAllHospitalsWeekly[,2])
summary(TSTestOtherAllHospitalsMonthly[,2])

#Moving Averages, all tests, all hospitals

library(forecast)
#Daily Moving Average 7 days, all Tests, all hospitals
TSDataAllTestsByReqDateDaily7MA<-ma(TSDataAllTestsByReqDateDaily[,2],7)

plot(days,TSDataAllTestsByReqDateDaily[,2],type="l",lwd=1,xlab="Year",ylab="Tests requested",main="Daily time series for all test types across all hospitals")
points(days,TSDataAllTestsByReqDateDaily7MA,col="red",type="l",lwd=3,lty=2)
legend("topright", legend=("7 day moving average"),
       col="red",bty = "n", cex=1.2,lwd=3,lty=2)

#Weekly Moving Average 4 weeks, all Tests, all hospitals
TSDataAllTestsByReqDateWeekly4MA<-ma(TSDataAllTestsByReqDateWeekly[,2],4)

plot(weeks,TSDataAllTestsByReqDateWeekly[,2],type="l",xlab="Year",ylab="Tests requested",main="Weekly time series for all test types across all hospitals")
points(weeks,TSDataAllTestsByReqDateWeekly4MA,col="red",type="l",lwd=3,lty=2)
legend("topright", legend=("4 week moving average"),
       col="red", lty=2,bty = "n", cex=1.2,lwd=2)

#Monthly Moving Average 3 months, all Tests, all hospitals
TSDataAllTestsByReqDateMonthly3MA<-ma(TSDataAllTestsByReqDateMonthly[,2],3)

plot(months,TSDataAllTestsByReqDateMonthly[,2],type="l",xlab="Year",ylab="Tests requested",main="Monthly time series for all test types across all hospitals")
points(months,TSDataAllTestsByReqDateMonthly3MA,col="red",type="l",lwd=3,lty=2)
legend("topright", legend=("3 month moving average"),
       col="red", lty=2,bty = "n", cex=1.2,lwd=2)

#Weekly Moving Average 4 weeks, all Tests, all hospitals
TSTestCTAllHospitalsWeekly4MA<-ma(TSTestCTAllHospitalsWeekly[,2],4)
TSTestPETAllHospitalsWeekly4MA<-ma(TSTestPETAllHospitalsWeekly[,2],4)
TSTestOtherAllHospitalsWeekly4MA<-ma(TSTestOtherAllHospitalsWeekly[,2],4)

#Seperate plots MA for each test type

par(mfrow=c(2,1))
plot(weeks,TSDataAllTestsByReqDateWeekly[,2],type="l",lwd=1,xlab="Year",ylab="Tests requested",main="Weekly time series for all test types across all hospitals")
points(weeks,TSTestCTAllHospitalsWeekly4MA,col="lightgreen",type="l",lwd=3)
legend("topright", legend="4 week moving average: CT",col="lightgreen",bty = "n", cex=0.75,lwd=3)

plot(months,TSDataAllTestsByReqDateMonthly[,2],type="l",lwd=1,xlab="Year",ylab="Tests requested",main="Monthly time series for all test types across all hospitals")
points(months,TSTestCTAllHospitalsMonthly3MA,col="lightgreen",type="l",lwd=3)
legend("topright", legend="3 month moving average: CT",col="lightgreen",bty = "n", cex=0.75,lwd=3)

plot(weeks,TSDataAllTestsByReqDateWeekly[,2],type="l",lwd=1,xlab="Year",ylab="Tests requested",main="Weekly time series for all test types across all hospitals")
points(weeks,TSTestPETAllHospitalsWeekly4MA,col="purple",type="l",lwd=3)
legend("topright", legend="4 week moving average: PET",col="purple",bty = "n", cex=0.75,lwd=3)

plot(months,TSDataAllTestsByReqDateMonthly[,2],type="l",lwd=1,xlab="Year",ylab="Tests requested",main="Monthly time series for all test types across all hospitals")
points(months,TSTestPETAllHospitalsMonthly3MA,col="purple",type="l",lwd=3)
legend("topright", legend="3 month moving average: PET",col="purple",bty = "n", cex=0.75,lwd=3)

plot(weeks,TSDataAllTestsByReqDateWeekly[,2],type="l",lwd=1,xlab="Year",ylab="Tests requested",main="Weekly time series for all test types across all hospitals")
points(weeks,TSTestOtherAllHospitalsWeekly4MA,col="gold",type="l",lwd=3)
legend("topright", legend="4 week moving average: Other",col="gold",bty = "n", cex=0.75,lwd=3)

plot(months,TSDataAllTestsByReqDateMonthly[,2],type="l",lwd=1,xlab="Year",ylab="Tests requested",main="Monthly time series for all test types across all hospitals")
points(months,TSTestOtherAllHospitalsMonthly3MA,col="gold",type="l",lwd=3)
legend("topright", legend="3 month moving average: Other",col="gold",bty = "n", cex=0.75,lwd=3)

plot(weeks,TSDataAllTestsByReqDateWeekly[,2],type="l",lwd=1,xlab="Year",ylab="Tests requested",main="Weekly time series for all test types across all hospitals")
points(weeks,TSTestCTAllHospitalsWeekly4MA,col="lightgreen",type="l",lwd=3)
points(weeks,TSTestPETAllHospitalsWeekly4MA,col="purple",type="l",lwd=3)
points(weeks,TSTestOtherAllHospitalsWeekly4MA,col="gold",type="l",lwd=3)
legend("topright", legend=c("4 week moving average: CT","4 week moving average: PET","4 week moving average: Other"),col=c("lightgreen","purple","gold"), lty=1:1,bty = "n", cex=0.75,lwd=3)

#Monthly Moving Average 3 months, all Tests, all hospitals
TSTestCTAllHospitalsMonthly3MA<-ma(TSTestCTAllHospitalsMonthly[,2],3)
TSTestPETAllHospitalsMonthly3MA<-ma(TSTestPETAllHospitalsMonthly[,2],3)
TSTestOtherAllHospitalsMonthly3MA<-ma(TSTestOtherAllHospitalsMonthly[,2],3)

plot(months,TSDataAllTestsByReqDateMonthly[,2],type="l",lwd=1,xlab="Year",ylab="Tests requested",main="Monthly time series for all test types across all hospitals")
points(months,TSTestCTAllHospitalsMonthly3MA,col="lightgreen",type="l",lwd=3)
points(months,TSTestPETAllHospitalsMonthly3MA,col="purple",type="l",lwd=3)
points(months,TSTestOtherAllHospitalsMonthly3MA,col="gold",type="l",lwd=3)
legend("topright", legend=c("3 month moving average: CT","3 month moving average: PET","3 month moving average: Other"),col=c("lightgreen","purple","gold"), lty=1:1,bty = "n", cex=0.75,lwd=3)





#Daily Moving Average 30 days, all Tests, all hospitals
TSDataAllTestsByReqDateDaily30MA<-ma(TSDataAllTestsByReqDateDaily[,2],30)

plot(days,TSDataAllTestsByReqDateDaily[,2],type="l",lwd=2,xlab="Year",ylab="Tests requested",main="Daily time series for all test types across all hospitals")
points(days,TSDataAllTestsByReqDateDaily30MA,col="red",type="l",lwd=2)
legend("topright", legend=("30 day moving average"),
       col="red", lty=1:1,bty = "n", cex=0.8,lwd=2)

#Daily Moving Average 90 days, all Tests, all hospitals
TSDataAllTestsByReqDateDaily90MA<-ma(TSDataAllTestsByReqDateDaily[,2],90)

plot(days,TSDataAllTestsByReqDateDaily[,2],type="l",lwd=2,xlab="Year",ylab="Tests requested",main="Daily time series for all test types across all hospitals")
points(days,TSDataAllTestsByReqDateDaily90MA,col="red",type="l",lwd=2)
legend("topright", legend=("90 day moving average"),
       col="red", lty=1:1,bty = "n", cex=0.8,lwd=2)

#Weekly Moving Average 5 weeks, all Tests, all hospitals

TSDataAllTestsByReqDateWeekly5MA<-ma(TSDataAllTestsByReqDateWeekly[,2],5)

plot(weeks,TSDataAllTestsByReqDateWeekly[,2],type="l",xlab="Year",ylab="Tests requested",main="Weekly time series for all test types across all hospitals")
points(weeks,TSDataAllTestsByReqDateWeekly5MA,col="red",type="l",lwd=3)
legend("topright", legend=("5 week moving average"),
       col="red", lty=1:1,bty = "n", cex=0.8,lwd=2)

#Weekly Moving Average 10 weeks, all Tests, all hospitals

TSDataAllTestsByReqDateWeekly10MA<-ma(TSDataAllTestsByReqDateWeekly[,2],10)

plot(weeks,TSDataAllTestsByReqDateWeekly[,2],type="l",xlab="Year",ylab="Tests requested",main="Weekly time series for all test types across all hospitals")
points(weeks,TSDataAllTestsByReqDateWeekly10MA,col="red",type="l",lwd=3)
legend("topright", legend=("10 week moving average"),
       col="red", lty=1:1,bty = "n", cex=0.8,lwd=2)

#Weekly Moving Average 20 weeks, all Tests, all hospitals

TSDataAllTestsByReqDateWeekly20MA<-ma(TSDataAllTestsByReqDateWeekly[,2],20)

plot(weeks,TSDataAllTestsByReqDateWeekly[,2],type="l",xlab="Year",ylab="Tests requested",main="Weekly time series for all test types across all hospitals")
points(weeks,TSDataAllTestsByReqDateWeekly20MA,col="red",type="l",lwd=3)
legend("topright", legend=("20 week moving average"),
       col="red", lty=1:1,bty = "n", cex=0.8,lwd=2)


#Monthly Moving Average 3 months, all Tests, all hospitals

TSDataAllTestsByReqDateMonthly3MA<-ma(TSDataAllTestsByReqDateMonthly[,2],3)

plot(months,TSDataAllTestsByReqDateMonthly[,2],type="l",xlab="Year",ylab="Tests requested",main="Monthly time series for all test types across all hospitals")
points(months,TSDataAllTestsByReqDateMonthly3MA,col="red",type="l",lwd=3)
legend("topright", legend=("3 month moving average"),
       col="red", lty=1:1,bty = "n", cex=0.8,lwd=2)

#Monthly Moving Average 5 months, all Tests, all hospitals

TSDataAllTestsByReqDateMonthly5MA<-ma(TSDataAllTestsByReqDateMonthly[,2],5)

plot(months,TSDataAllTestsByReqDateMonthly[,2],type="l",xlab="Year",ylab="Tests requested",main="Monthly time series for all test types across all hospitals")
points(months,TSDataAllTestsByReqDateMonthly5MA,col="red",type="l",lwd=3)
legend("topright", legend=("5 month moving average"),
       col="red", lty=1:1,bty = "n", cex=0.8,lwd=2)

#Monthly Moving Average 10 months, all Tests, all hospitals

TSDataAllTestsByReqDateMonthly10MA<-ma(TSDataAllTestsByReqDateMonthly[,2],10)

plot(months,TSDataAllTestsByReqDateMonthly[,2],type="l",xlab="Year",ylab="Tests requested",main="Monthly time series for all test types across all hospitals")
points(months,TSDataAllTestsByReqDateMonthly10MA,col="red",type="l",lwd=3)
legend("topright", legend=("10 month moving average"),
       col="red", lty=1:1,bty = "n", cex=0.8,lwd=2)


#Create time series and examine decompositions
install.packages("TTR")
require(TTR)
#Daily
TimeSeriesAllTestsByReqDateDaily<-ts(TSDataAllTestsByReqDateDaily[,2],start=c(2017,11,02),freq=365)
TimeSeriesAllTestsByReqDatePrinceChDaily<-ts(TSDataAllTestsByReqDatePrinceChDaily[,2],start=c(2017,11,02),freq=365)

TimeSeriesAllTestsByReqDateRoyalGlamDaily<-ts(TSDataAllTestsByReqDateRoyalGlamDaily[,2],start=c(2017,11,02),freq=365)

decompose(TimeSeriesAllTestsByReqDateDaily)
plot(decompose(TimeSeriesAllTestsByReqDateDaily),xaxt="n")

#Weekly
TimeSeriesAllTestsByReqDateWeekly<-ts(TSDataAllTestsByReqDateWeekly[,2],start=c(2017,11,02),freq=52)

TimeSeriesTestCTByReqDateWeekly<-ts(TSTestCTAllHospitalsWeekly[,2],start=c(2017,11,02),freq=52)

TimeSeriesTestPETByReqDateWeekly<-ts(TSTestPETAllHospitalsWeekly[,2],start=c(2017,11,02),freq=52)

TimeSeriesTestOtherByReqDateWeekly<-ts(TSTestOtherAllHospitalsWeekly[,2],start=c(2017,11,02),freq=52)

#Perform Augmented Dickey-Fuller tests
library(tseries)

decompose(TimeSeriesAllTestsByReqDateWeekly)
plot(decompose(TimeSeriesAllTestsByReqDateWeekly),xaxt="n")

#Monthly
TimeSeriesAllTestsByReqDateMonthly<-ts(TSDataAllTestsByReqDateMonthly[,2],start=c(2017,11,02),freq=12)

TimeSeriesTestCTByReqDateMonthly<-ts(TSTestCTAllHospitalsMonthly[,2],start=c(2017,11,02),freq=12)

TimeSeriesTestPETByReqDateMonthly<-ts(TSTestPETAllHospitalsMonthly[,2],start=c(2017,11,02),freq=12)

TimeSeriesTestOtherByReqDateMonthly<-ts(TSTestOtherAllHospitalsMonthly[,2],start=c(2017,11,02),freq=12)

adf.test(TimeSeriesAllTestsByReqDateDaily)

adf.test(TimeSeriesAllTestsByReqDateWeekly)
adf.test(TimeSeriesTestCTByReqDateWeekly)
adf.test(TimeSeriesTestPETByReqDateWeekly)
adf.test(TimeSeriesTestOtherByReqDateWeekly)

adf.test(TimeSeriesAllTestsByReqDateMonthly)
adf.test(TimeSeriesTestCTByReqDateMonthly)
adf.test(TimeSeriesTestPETByReqDateMonthly)
adf.test(TimeSeriesTestOtherByReqDateMonthly)

decompose(TimeSeriesAllTestsByReqDateMonthly)
plot(decompose(TimeSeriesAllTestsByReqDateMonthly),xaxt="n")

plot(decompose(TimeSeriesAllTestsByReqDateMonthly))

#Naive baseline

library(forecast)

#Daily Naive

#Split into training and test set

TimeSeriesAllTestsByReqDateDailyTrain<-subset(TimeSeriesAllTestsByReqDateDaily, end=length(TimeSeriesAllTestsByReqDateDaily)*0.75)
TimeSeriesAllTestsByReqDateDailyTest<-subset(TimeSeriesAllTestsByReqDateDaily, start=length(TimeSeriesAllTestsByReqDateDaily)*0.75)

TimeSeriesAllTestsByReqDateDailyNaive<-naive(TimeSeriesAllTestsByReqDateDailyTrain)
plot(TimeSeriesAllTestsByReqDateDailyNaive,ylim=c(0,15),main="Naive method on all daily tests across all hospitals",xlab="Year",ylab="Tests requested")

#Weekly Naive


TimeSeriesAllTestsByReqDateWeeklyTrain<-subset(TimeSeriesAllTestsByReqDateWeekly, end=length(TimeSeriesAllTestsByReqDateWeekly)*0.75)
TimeSeriesAllTestsByReqDateWeeklyTest<-subset(TimeSeriesAllTestsByReqDateWeekly, start=length(TimeSeriesAllTestsByReqDateWeekly)*0.75)

TimeSeriesAllTestsByReqDateWeeklyNaive<-naive(TimeSeriesAllTestsByReqDateWeeklyTrain)
plot(TimeSeriesAllTestsByReqDateWeeklyNaive,ylim=c(0,50),main="Naive method on all weekly tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestCTByReqDateWeeklyTrain<-subset(TimeSeriesTestCTByReqDateWeekly, end=length(TimeSeriesTestCTByReqDateWeekly)*0.75)
TimeSeriesTestCTByReqDateWeeklyTest<-subset(TimeSeriesTestCTByReqDateWeekly, start=length(TimeSeriesTestCTByReqDateWeekly)*0.75)

TimeSeriesTestCTByReqDateWeeklyNaive<-naive(TimeSeriesTestCTByReqDateWeeklyTrain)
plot(TimeSeriesTestCTByReqDateWeeklyNaive,ylim=c(0,50),main="Naive method on all weekly CT tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestPETByReqDateWeeklyTrain<-subset(TimeSeriesTestPETByReqDateWeekly, end=length(TimeSeriesTestPETByReqDateWeekly)*0.75)
TimeSeriesTestPETByReqDateWeeklyTest<-subset(TimeSeriesTestPETByReqDateWeekly, start=length(TimeSeriesTestPETByReqDateWeekly)*0.75)

TimeSeriesTestPETByReqDateWeeklyNaive<-naive(TimeSeriesTestPETByReqDateWeeklyTrain)
plot(TimeSeriesTestPETByReqDateWeeklyNaive,ylim=c(0,50),main="Naive method on all weekly PET tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestOtherByReqDateWeeklyTrain<-subset(TimeSeriesTestOtherByReqDateWeekly, end=length(TimeSeriesTestOtherByReqDateWeekly)*0.75)
TimeSeriesTestOtherByReqDateWeeklyTest<-subset(TimeSeriesTestOtherByReqDateWeekly, start=length(TimeSeriesTestOtherByReqDateWeekly)*0.75)

TimeSeriesTestOtherByReqDateWeeklyNaive<-naive(TimeSeriesTestOtherByReqDateWeeklyTrain)
plot(TimeSeriesTestOtherByReqDateWeeklyNaive,ylim=c(0,50),main="Naive method on all weekly Other tests across all hospitals",xlab="Year",ylab="Tests requested")

#Monthly Naive

TimeSeriesAllTestsByReqDateMonthly<-ts(TSDataAllTestsByReqDateMonthly[,2],start=c(2017,11,02),freq=12)

TimeSeriesAllTestsByReqDateMonthlyTrain<-subset(TimeSeriesAllTestsByReqDateMonthly, end=length(TimeSeriesAllTestsByReqDateMonthly)*0.75)
TimeSeriesAllTestsByReqDateMonthlyTest<-subset(TimeSeriesAllTestsByReqDateMonthly, start=length(TimeSeriesAllTestsByReqDateMonthly)*0.75)

TimeSeriesAllTestsByReqDateMonthlyNaive<-naive(TimeSeriesAllTestsByReqDateMonthlyTrain)
plot(TimeSeriesAllTestsByReqDateMonthlyNaive,ylim=c(0,150),main="Naive method on all monthly tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestCTByReqDateMonthly<-ts(TSTestCTAllHospitalsMonthly[,2],start=c(2017,11,02),freq=12)

TimeSeriesTestCTByReqDateMonthlyTrain<-subset(TimeSeriesTestCTByReqDateMonthly, end=length(TimeSeriesTestCTByReqDateMonthly)*0.75)
TimeSeriesTestCTByReqDateMonthlyTest<-subset(TimeSeriesTestCTByReqDateMonthly, start=length(TimeSeriesTestCTByReqDateMonthly)*0.75)

TimeSeriesTestCTByReqDateMonthlyNaive<-naive(TimeSeriesTestCTByReqDateMonthlyTrain)
plot(TimeSeriesTestCTByReqDateMonthlyNaive,ylim=c(0,150),main="Naive method on all monthly CT tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestPETByReqDateMonthly<-ts(TSTestPETAllHospitalsMonthly[,2],start=c(2017,11,02),freq=12)

TimeSeriesTestPETByReqDateMonthlyTrain<-subset(TimeSeriesTestPETByReqDateMonthly, end=length(TimeSeriesTestPETByReqDateMonthly)*0.75)
TimeSeriesTestPETByReqDateMonthlyTest<-subset(TimeSeriesTestPETByReqDateMonthly, start=length(TimeSeriesTestPETByReqDateMonthly)*0.75)

TimeSeriesTestPETByReqDateMonthlyNaive<-naive(TimeSeriesTestPETByReqDateMonthlyTrain)
plot(TimeSeriesTestPETByReqDateMonthlyNaive,ylim=c(0,10),main="Naive method on all monthly PET tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestOtherByReqDateMonthly<-ts(TSTestOtherAllHospitalsMonthly[,2],start=c(2017,11,02),freq=12)

TimeSeriesTestOtherByReqDateMonthlyTrain<-subset(TimeSeriesTestOtherByReqDateMonthly, end=length(TimeSeriesTestOtherByReqDateMonthly)*0.75)
TimeSeriesTestOtherByReqDateMonthlyTest<-subset(TimeSeriesTestOtherByReqDateMonthly, start=length(TimeSeriesTestOtherByReqDateMonthly)*0.75)

TimeSeriesTestOtherByReqDateMonthlyNaive<-naive(TimeSeriesTestOtherByReqDateMonthlyTrain)
plot(TimeSeriesTestOtherByReqDateMonthlyNaive,ylim=c(0,10),main="Naive method on all monthly Other tests across all hospitals",xlab="Year",ylab="Tests requested")

#Daily Seasonal Naive

TimeSeriesAllTestsByReqDateDailySNaive<-snaive(TimeSeriesAllTestsByReqDateDailyTrain,h=10)
plot(TimeSeriesAllTestsByReqDateDailySNaive,ylim=c(0,30),main="Daily seasonal naive method on all tests across all hospitals",xlab="Year",ylab="Tests requested")

#Weekly Seasonal Naive

TimeSeriesAllTestsByReqDateWeeklySNaive<-snaive(TimeSeriesAllTestsByReqDateWeeklyTrain,h=10)
plot(TimeSeriesAllTestsByReqDateWeeklySNaive,ylim=c(0,80),main="Weekly seasonal naive method on all tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestCTByReqDateWeeklySNaive<-snaive(TimeSeriesTestCTByReqDateWeeklyTrain)
plot(TimeSeriesTestCTByReqDateWeeklySNaive,ylim=c(0,80),main="Weekly seasonal naive method on all CT tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestPETByReqDateWeeklySNaive<-snaive(TimeSeriesTestPETByReqDateWeeklyTrain)
plot(TimeSeriesTestPETByReqDateWeeklySNaive,ylim=c(0,80),main="Weekly seasonal naive method on all PET tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestOtherByReqDateWeeklySNaive<-snaive(TimeSeriesTestOtherByReqDateWeeklyTrain)
plot(TimeSeriesTestOtherByReqDateWeeklySNaive,ylim=c(0,80),main="Weekly seasonal naive method on all Other tests across all hospitals",xlab="Year",ylab="Tests requested")

#Monthly Seasonal Naive

TimeSeriesAllTestsByReqDateMonthlySNaive<-snaive(TimeSeriesAllTestsByReqDateMonthlyTrain,h=10)
plot(TimeSeriesAllTestsByReqDateMonthlySNaive,ylim=c(0,250),main="Monthly seasonal naive method on all tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestCTByReqDateMonthlySNaive<-snaive(TimeSeriesTestCTByReqDateMonthlyTrain)
plot(TimeSeriesTestCTByReqDateMonthlySNaive,ylim=c(0,10),main="Monthly seasonal naive method on all CT tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestPETByReqDateMonthlySNaive<-snaive(TimeSeriesTestPETByReqDateMonthlyTrain)
plot(TimeSeriesTestPETByReqDateMonthlySNaive,ylim=c(0,10),main="Monthly seasonal naive method on all PET tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestOtherByReqDateMonthlySNaive<-snaive(TimeSeriesTestOtherByReqDateMonthlyTrain)
plot(TimeSeriesTestOtherByReqDateMonthlySNaive,ylim=c(0,10),main="Monthly seasonal naive method on all Other tests across all hospitals",xlab="Year",ylab="Tests requested")

#SES Daily
TimeSeriesAllTestsByReqDateDailySES<-ses(TimeSeriesAllTestsByReqDateDailyTrain)
plot(TimeSeriesAllTestsByReqDateDailySES,ylim=c(0,30),main="Daily SES method on all tests across all hospitals",xlab="Year",ylab="Tests requested")

#SES Weekly
TimeSeriesAllTestsByReqDateWeeklySES<-ses(TimeSeriesAllTestsByReqDateWeeklyTrain)
plot(TimeSeriesAllTestsByReqDateWeeklySES,ylim=c(0,80),main="Weekly SES method on all tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestCTByReqDateWeeklySES<-ses(TimeSeriesTestCTByReqDateWeeklyTrain)
plot(TimeSeriesTestCTByReqDateWeeklySES,ylim=c(0,80),main="Weekly SES method on all CT tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestPETByReqDateWeeklySES<-ses(TimeSeriesTestPETByReqDateWeeklyTrain)
plot(TimeSeriesTestPETByReqDateWeeklySES,ylim=c(0,80),main="Weekly SES method on all PET tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestOtherByReqDateWeeklySES<-ses(TimeSeriesTestOtherByReqDateWeeklyTrain)
plot(TimeSeriesTestOtherByReqDateWeeklySES,ylim=c(0,80),main="Weekly SES method on all Other tests across all hospitals",xlab="Year",ylab="Tests requested")

#SES Monthly
TimeSeriesAllTestsByReqDateMonthlySES<-ses(TimeSeriesAllTestsByReqDateMonthlyTrain)
plot(TimeSeriesAllTestsByReqDateMonthlySES,ylim=c(0,250),main="Monthly SES method on all tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestCTByReqDateMonthlySES<-ses(TimeSeriesTestCTByReqDateMonthlyTrain)
plot(TimeSeriesTestCTByReqDateMonthlySES,ylim=c(0,10),main="Monthly SES method on all CT testss across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestPETByReqDateMonthlySES<-ses(TimeSeriesTestPETByReqDateMonthlyTrain)
plot(TimeSeriesTestPETByReqDateMonthlySES,ylim=c(0,10),main="Monthly SES method on all PET testss across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestOtherByReqDateMonthlySES<-ses(TimeSeriesTestOtherByReqDateMonthlyTrain)
plot(TimeSeriesTestOtherByReqDateMonthlySES,ylim=c(0,10),main="Monthly SES method on all Other testss across all hospitals",xlab="Year",ylab="Tests requested")

#Holt Linear Daily
TimeSeriesAllTestsByReqDateDailyHolt<-holt(TimeSeriesAllTestsByReqDateDailyTrain)
plot(TimeSeriesAllTestsByReqDateDailyHolt,ylim=c(0,30),main="Daily Holt method on all tests across all hospitals",xlab="Year",ylab="Tests requested")

#Holt Linear Weekly
TimeSeriesAllTestsByReqDateWeeklyHolt<-holt(TimeSeriesAllTestsByReqDateWeeklyTrain)
plot(TimeSeriesAllTestsByReqDateWeeklyHolt,ylim=c(0,80),main="Weekly Holt method on all tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestCTByReqDateWeeklyHolt<-holt(TimeSeriesTestCTByReqDateWeeklyTrain)
plot(TimeSeriesTestCTByReqDateWeeklyHolt,ylim=c(0,80),main="Weekly Holt method on all CT tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestPETByReqDateWeeklyHolt<-holt(TimeSeriesTestPETByReqDateWeeklyTrain)
plot(TimeSeriesTestPETByReqDateWeeklyHolt,ylim=c(0,80),main="Weekly Holt method on all PET tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestOtherByReqDateWeeklyHolt<-holt(TimeSeriesTestOtherByReqDateWeeklyTrain)
plot(TimeSeriesTestOtherByReqDateWeeklyHolt,ylim=c(0,80),main="Weekly Holt method on all Other tests across all hospitals",xlab="Year",ylab="Tests requested")

#Holt Linear Monthly
TimeSeriesAllTestsByReqDateMonthlyHolt<-holt(TimeSeriesAllTestsByReqDateMonthlyTrain)
plot(TimeSeriesAllTestsByReqDateMonthlyHolt,ylim=c(0,250),main="Monthly Holt method on all tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestCTByReqDateMonthlyHolt<-holt(TimeSeriesTestCTByReqDateMonthlyTrain)
plot(TimeSeriesTestCTByReqDateMonthlyHolt,ylim=c(0,50),main="Monthly Holt method on all CT tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestPETByReqDateMonthlyHolt<-holt(TimeSeriesTestPETByReqDateMonthlyTrain)
plot(TimeSeriesTestPETByReqDateMonthlyHolt,ylim=c(0,50),main="Monthly Holt method on all PET tests across all hospitals",xlab="Year",ylab="Tests requested")

TimeSeriesTestOtherByReqDateMonthlyHolt<-holt(TimeSeriesTestOtherByReqDateMonthlyTrain)
plot(TimeSeriesTestOtherByReqDateMonthlyHolt,ylim=c(0,50),main="Monthly Holt method on all Other tests across all hospitals",xlab="Year",ylab="Tests requested")

#Holt-Winters

TimeSeriesAllTestsByReqDateForHoltWintersDaily<-ts(TSDataAllTestsByReqDateDaily[,2],start=c(2017,11,02),freq=7)
TimeSeriesAllTestsByReqDateForHoltWintersDailyTrain<-hw(subset(TimeSeriesAllTestsByReqDateForHoltWintersDaily, end=length(TimeSeriesAllTestsByReqDateForHoltWintersDaily)*0.75))
plot(TimeSeriesAllTestsByReqDateForHoltWintersDailyTrain,ylim=c(0,30),main="Daily Holt-Winters method on all tests across all hospitals",xlab="Time",ylab="Tests requested",xaxt="n")

TimeSeriesAllTestsByReqDateForHoltWintersWeekly<-ts(TSDataAllTestsByReqDateWeekly[,2],freq=24,start=c(2017,11,02))
TimeSeriesAllTestsByReqDateForHoltWintersWeeklyTrain<-hw(subset(TimeSeriesAllTestsByReqDateForHoltWintersWeekly, end=length(TimeSeriesAllTestsByReqDateForHoltWintersWeekly)*0.75))
plot(TimeSeriesAllTestsByReqDateForHoltWintersWeeklyTrain,ylim=c(0,50),main="Weekly Holt-Winters method on all tests across all hospitals",xlab="Time",ylab="Tests requested",xaxt="n")

TimeSeriesTestCTByReqDateForHoltWintersWeekly<-ts(TSTestCTAllHospitalsWeekly[,2],freq=24,start=c(2017,11,02))
TimeSeriesTestCTByReqDateForHoltWintersWeeklyTrain<-hw(subset(TimeSeriesTestCTByReqDateForHoltWintersWeekly, end=length(TimeSeriesTestCTByReqDateForHoltWintersWeekly)*0.75))
plot(TimeSeriesTestCTByReqDateForHoltWintersWeeklyTrain,ylim=c(0,50),main="Weekly Holt-Winters method on all CT tests across all hospitals",xlab="Time",ylab="Tests requested",xaxt="n")

TimeSeriesTestPETByReqDateForHoltWintersWeekly<-ts(TSTestPETAllHospitalsWeekly[,2],freq=24,start=c(2017,11,02))
TimeSeriesTestPETByReqDateForHoltWintersWeeklyTrain<-hw(subset(TimeSeriesTestPETByReqDateForHoltWintersWeekly, end=length(TimeSeriesTestPETByReqDateForHoltWintersWeekly)*0.75))
plot(TimeSeriesTestPETByReqDateForHoltWintersWeeklyTrain,ylim=c(0,50),main="Weekly Holt-Winters method on all PET tests across all hospitals",xlab="Time",ylab="Tests requested",xaxt="n")

TimeSeriesTestOtherByReqDateForHoltWintersWeekly<-ts(TSTestOtherAllHospitalsWeekly[,2],freq=24,start=c(2017,11,02))
TimeSeriesTestOtherByReqDateForHoltWintersWeeklyTrain<-hw(subset(TimeSeriesTestOtherByReqDateForHoltWintersWeekly, end=length(TimeSeriesTestOtherByReqDateForHoltWintersWeekly)*0.75))
plot(TimeSeriesTestOtherByReqDateForHoltWintersWeeklyTrain,ylim=c(0,50),main="Weekly Holt-Winters method on all Other tests across all hospitals",xlab="Time",ylab="Tests requested",xaxt="n")

TimeSeriesAllTestsByReqDateForHoltWintersMonthly<-ts(TSDataAllTestsByReqDateMonthly[,2],freq=12,start=c(2017,11,02))
TimeSeriesAllTestsByReqDateForHoltWintersMonthlyTrain<-hw(subset(TimeSeriesAllTestsByReqDateForHoltWintersMonthly, end=length(TimeSeriesAllTestsByReqDateForHoltWintersMonthly)*0.75))
plot(TimeSeriesAllTestsByReqDateForHoltWintersMonthlyTrain,ylim=c(0,50),main="Monthly Holt-Winters method on all tests across all hospitals",xlab="Time",ylab="Tests requested",xaxt="n")

TimeSeriesTestCTByReqDateForHoltWintersMonthly<-ts(TSTestCTAllHospitalsMonthly[,2],freq=12,start=c(2017,11,02))
TimeSeriesTestCTByReqDateForHoltWintersMonthlyTrain<-hw(subset(TimeSeriesTestCTByReqDateForHoltWintersMonthly, end=length(TimeSeriesTestCTByReqDateForHoltWintersMonthly)*0.75))
plot(TimeSeriesTestCTByReqDateForHoltWintersMonthlyTrain,ylim=c(0,50),main="Monthly Holt-Winters method on all CT tests across all hospitals",xlab="Time",ylab="Tests requested",xaxt="n")

TimeSeriesTestPETByReqDateForHoltWintersMonthly<-ts(TSTestPETAllHospitalsMonthly[,2],freq=15,start=c(2017,11,02))
TimeSeriesTestPETByReqDateForHoltWintersMonthlyTrain<-hw(subset(TimeSeriesTestPETByReqDateForHoltWintersMonthly, end=length(TimeSeriesTestPETByReqDateForHoltWintersMonthly)*0.75))
plot(TimeSeriesTestPETByReqDateForHoltWintersMonthlyTrain,ylim=c(0,50),main="Monthly Holt-Winters method on all PET tests across all hospitals",xlab="Time",ylab="Tests requested",xaxt="n")

TimeSeriesTestOtherByReqDateForHoltWintersMonthly<-ts(TSTestOtherAllHospitalsMonthly[,2],freq=10,start=c(2017,11,02))
TimeSeriesTestOtherByReqDateForHoltWintersMonthlyTrain<-hw(subset(TimeSeriesTestOtherByReqDateForHoltWintersMonthly, end=length(TimeSeriesTestOtherByReqDateForHoltWintersMonthly)*0.75))
plot(TimeSeriesTestOtherByReqDateForHoltWintersMonthlyTrain,ylim=c(0,50),main="Monthly Holt-Winters method on all Other tests across all hospitals",xlab="Time",ylab="Tests requested",xaxt="n")

#Daily simple linear regression model, all Tests, all hospitals
dev.off()
par(mfrow=c(3,1))

TimeSeriesAllTestsAllHospitalsDailyTrainSimpReg<-lm(TimeSeriesAllTestsByReqDateDailyTrain~days[1:(0.75*length(TimeSeriesAllTestsByReqDateDaily))])
plot(days[1:(0.75*length(TimeSeriesAllTestsByReqDateDaily))],TimeSeriesAllTestsByReqDateDailyTrain,type="l",xlab="Year",ylab="Tests requested",main="Daily time series for all test types across all hospitals",lwd=1)
abline(TimeSeriesAllTestsAllHospitalsDailyTrainSimpReg,lwd=3,col="red")
legend("topleft", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)


#Weekly simple linear regression model, all Tests, all hospitals

TimeSeriesAllTestsAllHospitalsWeeklyTrainSimpReg<-lm(TimeSeriesAllTestsByReqDateWeeklyTrain~weeks[1:(0.75*length(TimeSeriesAllTestsByReqDateWeekly))])
plot(weeks[1:(0.75*length(TimeSeriesAllTestsByReqDateWeekly))],TimeSeriesAllTestsByReqDateWeeklyTrain,type="l",xlab="Year",ylab="Tests requested",main="Weekly time series for all test types across all hospitals",lwd=1)
abline(TimeSeriesAllTestsAllHospitalsWeeklyTrainSimpReg,lwd=3,col="red")
legend("topleft", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

TimeSeriesTestCTAllHospitalsWeeklyTrainSimpReg<-lm(TimeSeriesTestCTByReqDateWeeklyTrain~weeks[1:(0.75*length(TimeSeriesTestCTByReqDateWeekly))])
plot(weeks[1:(0.75*length(TimeSeriesTestCTByReqDateWeekly))],TimeSeriesTestCTByReqDateWeeklyTrain,type="l",xlab="Year",ylab="Tests requested",main="Weekly time series for all CT tests across all hospitals",lwd=1)
abline(TimeSeriesTestCTAllHospitalsWeeklyTrainSimpReg,lwd=3,col="red")
legend("topleft", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

TimeSeriesTestPETAllHospitalsWeeklyTrainSimpReg<-lm(TimeSeriesTestPETByReqDateWeeklyTrain~weeks[1:(0.75*length(TimeSeriesTestPETByReqDateWeekly))])
plot(weeks[1:(0.75*length(TimeSeriesTestPETByReqDateWeekly))],TimeSeriesTestPETByReqDateWeeklyTrain,type="l",xlab="Year",ylab="Tests requested",main="Weekly time series for all PET tests across all hospitals",lwd=1)
abline(TimeSeriesTestPETAllHospitalsWeeklyTrainSimpReg,lwd=3,col="red")
legend("topleft", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

TimeSeriesTestOtherAllHospitalsWeeklyTrainSimpReg<-lm(TimeSeriesTestOtherByReqDateWeeklyTrain~weeks[1:(0.75*length(TimeSeriesTestOtherByReqDateWeekly))])
plot(weeks[1:(0.75*length(TimeSeriesTestOtherByReqDateWeekly))],TimeSeriesTestOtherByReqDateWeeklyTrain,type="l",xlab="Year",ylab="Tests requested",main="Weekly time series for all Other tests across all hospitals",lwd=1)
abline(TimeSeriesTestOtherAllHospitalsWeeklyTrainSimpReg,lwd=3,col="red")
legend("topleft", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

#Monthly simple linear regression model, all Tests, all hospitals
str(TimeSeriesTestPETByReqDateMonthlyTrain)
TimeSeriesAllTestsAllHospitalsMonthlyTrainSimpReg<-lm(TimeSeriesAllTestsByReqDateMonthlyTrain~months[1:(0.75*length(TimeSeriesAllTestsByReqDateMonthly))])
plot(months[1:(0.75*length(TimeSeriesAllTestsByReqDateMonthly))],TimeSeriesAllTestsByReqDateMonthlyTrain,type="l",xlab="Year",ylab="Tests requested",main="Monthly time series for all test types across all hospitals",lwd=1)
abline(TimeSeriesAllTestsAllHospitalsMonthlyTrainSimpReg,lwd=3,col="red")
legend("topleft", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

TimeSeriesTestCTAllHospitalsMonthlyTrainSimpReg<-lm(TimeSeriesTestCTByReqDateMonthlyTrain~months[1:(0.75*length(TimeSeriesTestCTByReqDateMonthly))])
plot(months[1:(0.75*length(TimeSeriesTestCTByReqDateMonthly))],TimeSeriesTestCTByReqDateMonthlyTrain,type="l",xlab="Year",ylab="Tests requested",main="Monthly time series for all CT tests across all hospitals",lwd=1)
abline(TimeSeriesTestCTAllHospitalsMonthlyTrainSimpReg,lwd=3,col="red")
legend("topleft", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

TimeSeriesTestPETAllHospitalsMonthlyTrainSimpReg<-lm(TimeSeriesTestPETByReqDateMonthlyTrain~months[1:(0.75*length(TimeSeriesTestPETByReqDateMonthly))])
plot(months[1:(0.75*length(TimeSeriesTestPETByReqDateMonthly))],TimeSeriesTestPETByReqDateMonthlyTrain,type="l",xlab="Year",ylab="Tests requested",main="Monthly time series for all PET tests across all hospitals",lwd=1)
abline(TimeSeriesTestPETAllHospitalsMonthlyTrainSimpReg,lwd=3,col="red")
legend("topleft", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

TimeSeriesTestOtherAllHospitalsMonthlyTrainSimpReg<-lm(TimeSeriesTestOtherByReqDateMonthlyTrain~months[1:(0.75*length(TimeSeriesTestOtherByReqDateMonthly))])
plot(months[1:(0.75*length(TimeSeriesTestOtherByReqDateMonthly))],TimeSeriesTestOtherByReqDateMonthlyTrain,type="l",xlab="Year",ylab="Tests requested",main="Monthly time series for all Other tests across all hospitals",lwd=1)
abline(TimeSeriesTestOtherAllHospitalsMonthlyTrainSimpReg,lwd=3,col="red")
legend("topleft", legend=("Simple linear regression line"),
       col="red", lty=1:1,bty = "n", cex=1,lwd=2)

#Multiple Linear regression

write.xlsx(cbind(TimeSeriesAllTestsByReqDateDailyTrain,as.Date(days[1:(0.75*length(TimeSeriesAllTestsByReqDateDaily))])), "DailyteststrainingdataforexcelMultReg.xlsx")

write.xlsx(cbind(TimeSeriesAllTestsByReqDateDaily,as.Date(days)), "DailyteststrainingdataforexcelMultRegFull.xlsx")

TimeSeriesAllTestsAllHospitalsDailyTrainMultRegExcelFull<-read_excel("Daily tests training data for excel Multi Regression Full.xlsx")


TimeSeriesAllTestsAllHospitalsDailyTrainMultRegData<-as.data.frame(TimeSeriesAllTestsAllHospitalsDailyTrainMultRegExcelFull)

TimeSeriesAllTestsAllHospitalsDailyTrainMultRegDataFull<-as.data.frame(TimeSeriesAllTestsAllHospitalsDailyTrainMultRegExcelFull)

TimeSeriesAllTestsAllHospitalsDailyTrainMultReg<-lm(Tests~Day+Month+Year,data=TimeSeriesAllTestsAllHospitalsDailyTrainMultRegData)

TimeSeriesAllTestsAllHospitalsDailyTrainMultRegFull<-lm(Tests~Day+Month+Year,data=TimeSeriesAllTestsAllHospitalsDailyTrainMultRegDataFull)

summary(TimeSeriesAllTestsAllHospitalsDailyTrainMultReg)
summary(TimeSeriesAllTestsAllHospitalsDailyTrainMultRegFull)

#ARIMAs

acf(TimeSeriesAllTestsByReqDateDailyTrain,main="ACF for daily tests data, all hospitals")
acf(TimeSeriesAllTestsByReqDateWeeklyTrain,main="ACF for weekly tests data, all hospitals")
acf(TimeSeriesAllTestsByReqDateMonthlyTrain,main="ACF for monthly tests data, all hospitals")

acf(TimeSeriesTestCTByReqDateWeeklyTrain,main="ACF for weekly CT tests data, all hospitals")
acf(TimeSeriesTestCTByReqDateMonthlyTrain,main="ACF for monthly CT tests data, all hospitals")

acf(TimeSeriesTestPETByReqDateWeeklyTrain,main="ACF for weekly PET tests data, all hospitals")
acf(TimeSeriesTestPETByReqDateMonthlyTrain,main="ACF for monthly PET tests data, all hospitals")

pacf(TimeSeriesAllTestsByReqDateDailyTrain,main="PACF for daily tests data, all hospitals")
pacf(TimeSeriesAllTestsByReqDateWeeklyTrain,main="PACF for weekly tests data, all hospitals")
pacf(TimeSeriesAllTestsByReqDateMonthlyTrain,main="PACF for monthly tests data, all hospitals")

pacf(TimeSeriesTestCTByReqDateWeeklyTrain,main="PACF for weekly CT tests data, all hospitals")
pacf(TimeSeriesTestCTByReqDateMonthlyTrain,main="PACF for monthly CT tests data, all hospitals")

pacf(TimeSeriesTestPETByReqDateWeeklyTrain,main="PACF for weekly PET tests data, all hospitals")
pacf(TimeSeriesTestPETByReqDateMonthlyTrain,main="PACF for monthly PET tests data, all hospitals")

pacf(TimeSeriesTestOtherByReqDateWeeklyTrain,main="PACF for weekly Other tests data, all hospitals")
pacf(TimeSeriesTestOtherByReqDateMonthlyTrain,main="PACF for monthly Other tests data, all hospitals")

TimeSeriesAllTestsByReqDateDailyTrainARIMA<-arima(TimeSeriesAllTestsByReqDateDailyTrain,order=c(7,0,7))
tsdiag(TimeSeriesAllTestsByReqDateDailyTrainARIMA)
plot(forecast(TimeSeriesAllTestsByReqDateDailyTrainARIMA,h=10),main="ARIMA on daily tests data, all tests")

TimeSeriesAllTestsByReqDateWeeklyTrainARIMA<-arima(TimeSeriesAllTestsByReqDateWeeklyTrain,order=c(1,1,1))
tsdiag(TimeSeriesAllTestsByReqDateWeeklyTrainARIMA)
plot(forecast(TimeSeriesAllTestsByReqDateWeeklyTrainARIMA,h=10),main="ARIMA on daily tests data, all tests")

TimeSeriesTestCTByReqDateWeeklyTrainARIMA<-arima(TimeSeriesTestCTByReqDateWeeklyTrain,order=c(0,1,1))
tsdiag(TimeSeriesTestCTByReqDateWeeklyTrainARIMA)
plot(forecast(TimeSeriesTestCTByReqDateWeeklyTrainARIMA,h=10),main="ARIMA on daily tests data, CT tests")

TimeSeriesTestPETByReqDateWeeklyTrainARIMA<-arima(TimeSeriesTestPETByReqDateWeeklyTrain,order=c(1,1,1))
tsdiag(TimeSeriesTestPETByReqDateWeeklyTrainARIMA)
plot(forecast(TimeSeriesTestPETByReqDateWeeklyTrainARIMA,h=10),main="ARIMA on daily tests data, PET tests")

TimeSeriesTestOtherByReqDateWeeklyTrainARIMA<-arima(TimeSeriesTestOtherByReqDateWeeklyTrain,order=c(0,1,1))
tsdiag(TimeSeriesTestOtherByReqDateWeeklyTrainARIMA)
plot(forecast(TimeSeriesTestOtherByReqDateWeeklyTrainARIMA,h=10),main="ARIMA on daily tests data, Other tests")

TimeSeriesAllTestsByReqDateMonthlyTrainARIMA<-arima(TimeSeriesAllTestsByReqDateMonthlyTrain,order=c(0,0,1))
tsdiag(TimeSeriesAllTestsByReqDateMonthlyTrainARIMA)
plot(forecast(TimeSeriesAllTestsByReqDateMonthlyTrainARIMA,h=10),main="ARIMA on daily tests data, all tests")

TimeSeriesTestCTByReqDateMonthlyTrainARIMA<-arima(TimeSeriesTestCTByReqDateMonthlyTrain,order=c(0,0,1))
tsdiag(TimeSeriesTestCTByReqDateMonthlyTrainARIMA)
plot(forecast(TimeSeriesTestCTByReqDateMonthlyTrainARIMA,h=10),main="ARIMA on daily tests data, CT tests")

TimeSeriesTestPETByReqDateMonthlyTrainARIMA<-arima(TimeSeriesTestPETByReqDateMonthlyTrain,order=c(0,0,1))
tsdiag(TimeSeriesTestPETByReqDateMonthlyTrainARIMA)
plot(forecast(TimeSeriesTestPETByReqDateMonthlyTrainARIMA,h=10),main="ARIMA on daily tests data, PET tests")

TimeSeriesTestOtherByReqDateMonthlyTrainARIMA<-arima(TimeSeriesTestOtherByReqDateMonthlyTrain,order=c(1,0,0))
tsdiag(TimeSeriesTestOtherByReqDateMonthlyTrainARIMA)
plot(forecast(TimeSeriesTestOtherByReqDateMonthlyTrainARIMA,h=10),main="ARIMA on daily tests data, all tests")

library(Rssa)

#SSA Experimentation
str(TimeSeriesAllTestsByReqDateDailyTrain)
SSAAllTestsByReqDateDaily <- ssa(TimeSeriesAllTestsByReqDateDailyTrain)
rforDaily <- rforecast(SSAAllTestsByReqDateDaily, groups = list(c(1,4), 1:4), len = 100, only.new=FALSE)
rforDaily
matplot(data.frame(c(TimeSeriesAllTestsByReqDateDailyTrain, rep(NA, 100)), rforDaily), type = "l",,xlab="Day",ylab="Tests requested",main="Weekly SSA predictions for tests across all hospitals")
rforDaily
plot(SSAAllTestsByReqDateDaily,type="series")
plot(SSAAllTestsByReqDateDaily,type="vectors")

SSAtw<-forecast(SSAAllTestsByReqDateDaily,groups=list(c(1,4)),h=10)
plot(SSAtw,main="SSA method on all daily tests across all hospitals",xlab="Year",ylab="Test requests")

SSAAllTestsByReqDateWeekly <- ssa(TimeSeriesAllTestsByReqDateWeeklyTrain)
rforWeekly <- rforecast(SSAAllTestsByReqDateWeekly, groups = list(c(1,4), 1:4), len = 100, only.new=FALSE)
matplot(data.frame(c(TimeSeriesAllTestsByReqDateWeeklyTrain, rep(NA, 100)), rforWeekly), type = "l",xlab="Week",ylab="Tests requested",main="Weekly SSA predictions for tests across all hospitals")
rforWeekly
plot(SSAAllTestsByReqDateWeekly,type="series")
plot(SSAAllTestsByReqDateWeekly,type="vectors")
SSAtw<-forecast(SSAAllTestsByReqDateWeekly,groups=list(c(1,4)),h=10)
plot(SSAtw,main="SSA method on all weekly tests across all hospitals",xlab="Year",ylab="Test requests")

SSATestCTByReqDateWeekly <- ssa(TimeSeriesTestCTByReqDateWeeklyTrain)
rforWeekly <- rforecast(SSATestCTByReqDateWeekly, groups = list(c(1,4), 1:4), len = 100, only.new=FALSE)
matplot(data.frame(c(TimeSeriesTestCTByReqDateWeeklyTrain, rep(NA, 100)), rforWeekly), type = "l",xlab="Week",ylab="Tests requested",main="Weekly SSA predictions for CT tests across all hospitals")
rforWeekly
plot(SSATestCTByReqDateWeekly,type="series")
plot(SSATestCTByReqDateWeekly,type="vectors")
SSAtwCT<-forecast(SSATestCTByReqDateWeekly,groups=list(c(1,4)),h=10)
plot(SSAtwCT,main="SSA method on all weekly CT tests across all hospitals",xlab="Year",ylab="Test requests")

SSATestPETByReqDateWeekly <- ssa(TimeSeriesTestPETByReqDateWeeklyTrain)
rforWeekly <- rforecast(SSATestPETByReqDateWeekly, groups = list(c(1,4), 1:4), len = 100, only.new=FALSE)
matplot(data.frame(c(TimeSeriesTestPETByReqDateWeeklyTrain, rep(NA, 100)), rforWeekly), type = "l",xlab="Week",ylab="Tests requested",main="Weekly SSA predictions for PET tests across all hospitals")
rforWeekly
plot(SSATestPETByReqDateWeekly,type="series")
plot(SSATestPETByReqDateWeekly,type="vectors")
SSAtwPET<-forecast(SSATestPETByReqDateWeekly,groups=list(c(1,4)),h=10)
plot(SSAtwPET,main="SSA method on all weekly Other tests across all hospitals",xlab="Year",ylab="Test requests")

SSATestOtherByReqDateWeekly <- ssa(TimeSeriesTestOtherByReqDateWeeklyTrain)
rforWeekly <- rforecast(SSATestOtherByReqDateWeekly, groups = list(c(1,4), 1:4), len = 100, only.new=FALSE)
matplot(data.frame(c(TimeSeriesTestOtherByReqDateWeeklyTrain, rep(NA, 100)), rforWeekly), type = "l",xlab="Week",ylab="Tests requested",main="Weekly SSA predictions for Other tests across all hospitals")
rforWeekly
plot(SSATestOtherByReqDateWeekly,type="series")
plot(SSATestOtherByReqDateWeekly,type="vectors")
SSAtwOther<-forecast(SSATestOtherByReqDateWeekly,groups=list(c(1,4)),h=10)
plot(SSAtwOther,main="SSA method on all weekly Other tests across all hospitals",xlab="Year",ylab="Test requests")

SSAAllTestsByReqDateMonthly <- ssa(TimeSeriesAllTestsByReqDateMonthlyTrain)
rforMonthly <- rforecast(SSAAllTestsByReqDateMonthly, groups = list(c(1,4), 1:4), len = 10, only.new=FALSE)
matplot(data.frame(c(TimeSeriesAllTestsByReqDateMonthlyTrain, rep(NA, 10)), rforMonthly), type = "l",xlab="Month",ylab="Tests requested",main="Monthly SSA predictions for tests across all hospitals")
rforMonthly
plot(SSAAllTestsByReqDateMonthly,type="series")
plot(SSAAllTestsByReqDateMonthly,type="vectors")
SSAtm<-forecast(SSAAllTestsByReqDateMonthly,groups=list(c(1,4)),h=10)
plot(SSAtm,main="SSA method on all monthly tests across all hospitals",xlab="Year",ylab="Test requests")

SSATestCTByReqDateMonthly <- ssa(TimeSeriesTestCTByReqDateMonthlyTrain)
rforMonthly <- rforecast(SSATestCTByReqDateMonthly, groups = list(c(1,4), 1:4), len = 10, only.new=FALSE)
matplot(data.frame(c(TimeSeriesTestCTByReqDateMonthlyTrain, rep(NA, 10)), rforMonthly), type = "l",xlab="Month",ylab="Tests requested",main="Monthly SSA predictions for CT tests across all hospitals")

plot(SSATestCTByReqDateMonthly,type="series")
plot(SSATestCTByReqDateMonthly,type="vectors")
SSAtmCT<-forecast(SSATestCTByReqDateMonthly,groups=list(c(1,4)),h=10)
plot(SSAtmCT,main="SSA method on all CT monthly tests across all hospitals",xlab="Year",ylab="Test requests")

SSATestPETByReqDateMonthly <- ssa(TimeSeriesTestPETByReqDateMonthlyTrain)
rforMonthly <- rforecast(SSATestPETByReqDateMonthly, groups = list(c(1,4), 1:4), len = 10, only.new=FALSE)
matplot(data.frame(c(TimeSeriesTestPETByReqDateMonthlyTrain, rep(NA, 10)), rforMonthly), type = "l",xlab="Month",ylab="Tests requested",main="Monthly SSA predictions for PET tests across all hospitals")

plot(SSATestPETByReqDateMonthly,type="series")
plot(SSATestPETByReqDateMonthly,type="vectors")
SSAtmPET<-forecast(SSATestPETByReqDateMonthly,groups=list(c(1,4)),h=10)
plot(SSAtmPET,main="SSA method on all PET monthly tests across all hospitals",xlab="Year",ylab="Test requests")

SSATestOtherByReqDateMonthly <- ssa(TimeSeriesTestOtherByReqDateMonthlyTrain)
rforMonthly <- rforecast(SSATestOtherByReqDateMonthly, groups = list(c(1,4), 1:4), len = 10, only.new=FALSE)
matplot(data.frame(c(TimeSeriesTestOtherByReqDateMonthlyTrain, rep(NA, 10)), rforMonthly), type = "l",xlab="Month",ylab="Tests requested",main="Monthly SSA predictions for Other tests across all hospitals")

plot(SSATestOtherByReqDateMonthly,type="series")
plot(SSATestOtherByReqDateMonthly,type="vectors")
SSAtmOther<-forecast(SSATestOtherByReqDateMonthly,groups=list(c(1,4)),h=10)
plot(SSAtmOther,main="SSA method on all Other monthly tests across all hospitals",xlab="Year",ylab="Test requests")

library(openxlsx)
write.xlsx(TimeSeriesAllTestsByReqDateDailyTrain,file = "Dailytesttrainingdataforexcel.xlsx")
write.xlsx(TimeSeriesAllTestsByReqDateWeeklyTrain,file = "Weeklytesttrainingdataforexcel.xlsx")
write.xlsx(TimeSeriesAllTestsByReqDateMonthlyTrain,file = "Monthlytesttrainingdataforexcel.xlsx")

#Artificial Neural NeSSAtworks (Machine learning)

TimeSeriesAllTestsByReqDateDailyTrainNNet<-nnetar(TimeSeriesAllTestsByReqDateDailyTrain)
TimeSeriesAllTestsByReqDateDailyTrainNNetPlot<-forecast(TimeSeriesAllTestsByReqDateDailyTrainNNet,PI=T,h=10)
plot(TimeSeriesAllTestsByReqDateDailyTrainNNetPlot,main="Daily ANN predictions for test requests across all hospitals",xlab="Year",ylab="Test requests")

TimeSeriesAllTestsByReqDateWeeklyTrainNNet<-nnetar(TimeSeriesAllTestsByReqDateWeeklyTrain)
TimeSeriesAllTestsByReqDateWeeklyTrainNNetPlot<-forecast(TimeSeriesAllTestsByReqDateWeeklyTrainNNet,PI=T,h=10)
plot(TimeSeriesAllTestsByReqDateWeeklyTrainNNetPlot,main="Weekly ANN predictions for test requests across all hospitals",xlab="Year",ylab="Test requests")

TimeSeriesTestCTByReqDateWeeklyTrainNNet<-nnetar(TimeSeriesTestCTByReqDateWeeklyTrain)
TimeSeriesTestCTByReqDateWeeklyTrainNNetPlot<-forecast(TimeSeriesTestCTByReqDateWeeklyTrainNNet,PI=T,h=10)
plot(TimeSeriesTestCTByReqDateWeeklyTrainNNetPlot,main="Weekly ANN predictions for CT test requests across all hospitals",xlab="Year",ylab="Test requests")

TimeSeriesTestPETByReqDateWeeklyTrainNNet<-nnetar(TimeSeriesTestPETByReqDateWeeklyTrain)
TimeSeriesTestPETByReqDateWeeklyTrainNNetPlot<-forecast(TimeSeriesTestPETByReqDateWeeklyTrainNNet,PI=T,h=10)
plot(TimeSeriesTestPETByReqDateWeeklyTrainNNetPlot,main="Weekly ANN predictions for PET test requests across all hospitals",xlab="Year",ylab="Test requests")

TimeSeriesTestOtherByReqDateWeeklyTrainNNet<-nnetar(TimeSeriesTestOtherByReqDateWeeklyTrain)
TimeSeriesTestOtherByReqDateWeeklyTrainNNetPlot<-forecast(TimeSeriesTestOtherByReqDateWeeklyTrainNNet,PI=T,h=10)
plot(TimeSeriesTestOtherByReqDateWeeklyTrainNNetPlot,main="Weekly ANN predictions for Other test requests across all hospitals",xlab="Year",ylab="Test requests")

TimeSeriesAllTestsByReqDateMonthlyTrainNNet<-nnetar(TimeSeriesAllTestsByReqDateMonthlyTrain)
TimeSeriesAllTestsByReqDateMonthlyTrainNNetPlot<-forecast(TimeSeriesAllTestsByReqDateMonthlyTrainNNet,PI=T,h=5)
plot(TimeSeriesAllTestsByReqDateMonthlyTrainNNetPlot,,main="Monthly ANN predictions for test requests across all hospitals",xlab="Year",ylab="Test requests")

TimeSeriesTestCTByReqDateMonthlyTrainNNet<-nnetar(TimeSeriesTestCTByReqDateMonthlyTrain)
TimeSeriesTestCTByReqDateMonthlyTrainNNetPlot<-forecast(TimeSeriesTestCTByReqDateMonthlyTrainNNet,PI=T,h=5)
plot(TimeSeriesTestCTByReqDateMonthlyTrainNNetPlot,,main="Monthly ANN predictions for CT test requests across all hospitals",xlab="Year",ylab="Test requests")

TimeSeriesTestPETByReqDateMonthlyTrainNNet<-nnetar(TimeSeriesTestPETByReqDateMonthlyTrain)
TimeSeriesTestPETByReqDateMonthlyTrainNNetPlot<-forecast(TimeSeriesTestPETByReqDateMonthlyTrainNNet,PI=T,h=5)
plot(TimeSeriesTestPETByReqDateMonthlyTrainNNetPlot,,main="Monthly ANN predictions for PET test requests across all hospitals",xlab="Year",ylab="Test requests")

TimeSeriesTestOtherByReqDateMonthlyTrainNNet<-nnetar(TimeSeriesTestOtherByReqDateMonthlyTrain)
TimeSeriesTestOtherByReqDateMonthlyTrainNNetPlot<-forecast(TimeSeriesTestOtherByReqDateMonthlyTrainNNet,PI=T,h=5)
plot(TimeSeriesTestOtherByReqDateMonthlyTrainNNetPlot,,main="Monthly ANN predictions for Other test requests across all hospitals",xlab="Year",ylab="Test requests")


#Error statistics- Training data

accuracy(TimeSeriesAllTestsByReqDateDailyNaive)
accuracy(TimeSeriesAllTestsByReqDateDailySNaive)
accuracy(TimeSeriesAllTestsByReqDateDailySES)
accuracy(TimeSeriesAllTestsByReqDateDailyHolt)
accuracy(TimeSeriesAllTestsByReqDateForHoltWintersDailyTrain)
accuracy(TimeSeriesAllTestsAllHospitalsDailyTrainSimpReg)
accuracy(TimeSeriesAllTestsAllHospitalsDailyTrainMultReg)
accuracy(TimeSeriesAllTestsByReqDateDailyTrainARIMA)
accuracy(SSAtw)
accuracy(TimeSeriesAllTestsByReqDateDailyTrainNNet)

accuracy(TimeSeriesAllTestsByReqDateWeeklyNaive)
accuracy(TimeSeriesAllTestsByReqDateWeeklySNaive)
accuracy(TimeSeriesAllTestsByReqDateWeeklySES)
accuracy(TimeSeriesAllTestsByReqDateWeeklyHolt)
accuracy(TimeSeriesAllTestsByReqDateForHoltWintersWeeklyTrain)
accuracy(TimeSeriesAllTestsAllHospitalsWeeklyTrainSimpReg)
accuracy(TimeSeriesAllTestsByReqDateWeeklyTrainARIMA)
accuracy(SSAtw)
accuracy(TimeSeriesAllTestsByReqDateWeeklyTrainNNet)

accuracy(TimeSeriesTestCTByReqDateWeeklyNaive)
accuracy(TimeSeriesTestCTByReqDateWeeklySNaive)
accuracy(TimeSeriesTestCTByReqDateWeeklySES)
accuracy(TimeSeriesTestCTByReqDateWeeklyHolt)
accuracy(TimeSeriesTestCTByReqDateForHoltWintersWeeklyTrain)
accuracy(TimeSeriesTestCTAllHospitalsWeeklyTrainSimpReg)
accuracy(TimeSeriesTestCTByReqDateWeeklyTrainARIMA)
accuracy(SSAtwCT)
accuracy(TimeSeriesTestCTByReqDateWeeklyTrainNNet)

accuracy(TimeSeriesTestPETByReqDateWeeklyNaive)
accuracy(TimeSeriesTestPETByReqDateWeeklySNaive)
accuracy(TimeSeriesTestPETByReqDateWeeklySES)
accuracy(TimeSeriesTestPETByReqDateWeeklyHolt)
accuracy(TimeSeriesTestPETByReqDateForHoltWintersWeeklyTrain)
accuracy(TimeSeriesTestPETAllHospitalsWeeklyTrainSimpReg)
accuracy(TimeSeriesTestPETByReqDateWeeklyTrainARIMA)
accuracy(SSAtwPET)
accuracy(TimeSeriesTestPETByReqDateWeeklyTrainNNet)

accuracy(TimeSeriesTestOtherByReqDateWeeklyNaive)
accuracy(TimeSeriesTestOtherByReqDateWeeklySNaive)
accuracy(TimeSeriesTestOtherByReqDateWeeklySES)
accuracy(TimeSeriesTestOtherByReqDateWeeklyHolt)
accuracy(TimeSeriesTestOtherByReqDateForHoltWintersWeeklyTrain)
accuracy(TimeSeriesTestOtherAllHospitalsWeeklyTrainSimpReg)
accuracy(TimeSeriesTestOtherByReqDateWeeklyTrainARIMA)
accuracy(SSAtwOther)
accuracy(TimeSeriesTestOtherByReqDateWeeklyTrainNNet)

accuracy(TimeSeriesAllTestsByReqDateMonthlyNaive)
accuracy(TimeSeriesAllTestsByReqDateMonthlySNaive)
accuracy(TimeSeriesAllTestsByReqDateMonthlySES)
accuracy(TimeSeriesAllTestsByReqDateMonthlyHolt)
accuracy(TimeSeriesAllTestsByReqDateForHoltWintersMonthlyTrain)
accuracy(TimeSeriesAllTestsAllHospitalsMonthlyTrainSimpReg)
accuracy(TimeSeriesAllTestsByReqDateMonthlyTrainARIMA)
accuracy(SSAtm)
accuracy(TimeSeriesAllTestsByReqDateMonthlyTrainNNet)

accuracy(TimeSeriesTestCTByReqDateMonthlyNaive)
accuracy(TimeSeriesTestCTByReqDateMonthlySNaive)
accuracy(TimeSeriesTestCTByReqDateMonthlySES)
accuracy(TimeSeriesTestCTByReqDateMonthlyHolt)
accuracy(TimeSeriesTestCTByReqDateForHoltWintersWeeklyTrain)
accuracy(TimeSeriesTestCTAllHospitalsMonthlyTrainSimpReg)
accuracy(TimeSeriesTestCTByReqDateMonthlyTrainARIMA)
accuracy(SSAtmCT)
accuracy(TimeSeriesTestCTByReqDateMonthlyTrainNNet)

accuracy(TimeSeriesTestPETByReqDateMonthlyNaive)
accuracy(TimeSeriesTestPETByReqDateMonthlySNaive)
accuracy(TimeSeriesTestPETByReqDateMonthlySES)
accuracy(TimeSeriesTestPETByReqDateMonthlyHolt)
accuracy(TimeSeriesTestPETByReqDateForHoltWintersMonthlyTrain)
accuracy(TimeSeriesTestPETAllHospitalsMonthlyTrainSimpReg)
accuracy(TimeSeriesTestPETByReqDateMonthlyTrainARIMA)
accuracy(SSAtmPET)
accuracy(TimeSeriesTestPETByReqDateMonthlyTrainNNet)

accuracy(TimeSeriesTestOtherByReqDateMonthlyNaive)
accuracy(TimeSeriesTestOtherByReqDateMonthlySNaive)
accuracy(TimeSeriesTestOtherByReqDateMonthlySES)
accuracy(TimeSeriesTestOtherByReqDateMonthlyHolt)
accuracy(TimeSeriesTestOtherAllHospitalsMonthlyTrainSimpReg)
accuracy(TimeSeriesTestOtherByReqDateMonthlyTrainARIMA)
accuracy(SSAtmOther)
accuracy(TimeSeriesTestOtherByReqDateMonthlyTrainNNet)

accuracy(TimeSeriesTestPETByReqDateMonthlyTrainNNet)
as.data.frame(predict(TimeSeriesTestPETByReqDateMonthlyTrainNNet,h=10))

# Error statistics Test data


accuracy(TimeSeriesAllTestsByReqDateDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateDailyNaive,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateDailySNaive,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateDailySES,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateDailyHolt,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateForHoltWintersDailyTrain,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateDailyTest[0:10],as.data.frame(predict(TimeSeriesAllTestsAllHospitalsDailyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateDailyTest[0:10],as.data.frame(predict(TimeSeriesAllTestsAllHospitalsDailyTrainMultReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateDailyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateDailyTest[0:10],as.data.frame(forecast(SSAtw,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateDailyTrainNNet,h=10))[,1])



accuracy(TimeSeriesAllTestsByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateWeeklyNaive,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateWeeklySNaive,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateWeeklySES,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateWeeklyHolt,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateForHoltWintersWeeklyTrain,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateWeeklyTest[0:10],as.data.frame(predict(TimeSeriesAllTestsAllHospitalsWeeklyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateWeeklyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateWeeklyTest[0:10],as.data.frame(forecast(SSAtw,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateWeeklyTrainNNet,h=10))[,1])


accuracy(TimeSeriesTestCTByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestCTByReqDateWeeklyNaive,h=10))[,1])

accuracy(TimeSeriesTestCTByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestCTByReqDateWeeklySNaive,h=10))[,1])

accuracy(TimeSeriesTestCTByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestCTByReqDateWeeklySES,h=10))[,1])

accuracy(TimeSeriesTestCTByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestCTByReqDateWeeklyHolt,h=10))[,1])

accuracy(TimeSeriesTestCTByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestCTByReqDateForHoltWintersWeeklyTrain,h=10))[,1])

accuracy(TimeSeriesTestCTByReqDateWeeklyTest[0:10],as.data.frame(predict(TimeSeriesTestCTAllHospitalsWeeklyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesTestCTByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestCTByReqDateWeeklyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesTestCTByReqDateWeeklyTest[0:10],as.data.frame(forecast(SSAtwCT,h=10))[,1])

accuracy(TimeSeriesTestCTByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestCTByReqDateWeeklyTrainNNet,h=10))[,1])


accuracy(TimeSeriesTestPETByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestPETByReqDateWeeklyNaive,h=10))[,1])

accuracy(TimeSeriesTestPETByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestPETByReqDateWeeklySNaive,h=10))[,1])

accuracy(TimeSeriesTestPETByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestPETByReqDateWeeklySES,h=10))[,1])

accuracy(TimeSeriesTestPETByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestPETByReqDateWeeklyHolt,h=10))[,1])

accuracy(TimeSeriesTestPETByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestPETByReqDateForHoltWintersWeeklyTrain,h=10))[,1])

accuracy(TimeSeriesTestPETByReqDateWeeklyTest[0:10],as.data.frame(predict(TimeSeriesTestPETAllHospitalsWeeklyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesTestPETByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestPETByReqDateWeeklyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesTestPETByReqDateWeeklyTest[0:10],as.data.frame(forecast(SSAtwPET,h=10))[,1])

accuracy(TimeSeriesTestPETByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestPETByReqDateWeeklyTrainNNet,h=10))[,1])


accuracy(TimeSeriesTestOtherByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestOtherByReqDateWeeklyNaive,h=10))[,1])

accuracy(TimeSeriesTestOtherByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestOtherByReqDateWeeklySNaive,h=10))[,1])

accuracy(TimeSeriesTestOtherByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestOtherByReqDateWeeklySES,h=10))[,1])

accuracy(TimeSeriesTestOtherByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestOtherByReqDateWeeklyHolt,h=10))[,1])

accuracy(TimeSeriesTestOtherByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestOtherByReqDateForHoltWintersWeeklyTrain,h=10))[,1])

accuracy(TimeSeriesTestOtherByReqDateWeeklyTest[0:10],as.data.frame(predict(TimeSeriesTestOtherAllHospitalsWeeklyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesTestOtherByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestOtherByReqDateWeeklyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesTestOtherByReqDateWeeklyTest[0:10],as.data.frame(forecast(SSAtwOther,h=10))[,1])

accuracy(TimeSeriesTestOtherByReqDateWeeklyTest[0:10],as.data.frame(forecast(TimeSeriesTestOtherByReqDateWeeklyTrainNNet,h=10))[,1])



accuracy(TimeSeriesAllTestsByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateMonthlyNaive,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateMonthlySNaive,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateMonthlySES,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateMonthlyHolt,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateForHoltWintersMonthlyTrain,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateMonthlyTest[0:10],as.data.frame(predict(TimeSeriesAllTestsAllHospitalsMonthlyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateMonthlyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesAllTestsByReqDateMonthlyTest[0:10],as.vector(SSAtm$mean))

accuracy(TimeSeriesAllTestsByReqDateMonthlyTest[0:10],as.vector(TimeSeriesAllTestsByReqDateMonthlyTrainNNetPlot$mean))




accuracy(TimeSeriesTestCTByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestCTByReqDateMonthlyNaive,h=10))[,1])

accuracy(TimeSeriesTestCTByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestCTByReqDateMonthlySNaive,h=10))[,1])

accuracy(TimeSeriesTestCTByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestCTByReqDateMonthlySES,h=10))[,1])

accuracy(TimeSeriesTestCTByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestCTByReqDateMonthlyHolt,h=10))[,1])

accuracy(TimeSeriesTestCTByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestCTByReqDateForHoltWintersMonthlyTrain,h=10))[,1])

accuracy(TimeSeriesTestCTByReqDateMonthlyTest[0:10],as.data.frame(predict(TimeSeriesTestCTAllHospitalsMonthlyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesTestCTByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestCTByReqDateMonthlyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesTestCTByReqDateMonthlyTest[0:10],as.data.frame(forecast(SSAtwCT,h=10))[,1])

accuracy(TimeSeriesTestCTByReqDateMonthlyTest[0:10],as.vector(SSAtmCT$mean))

accuracy(TimeSeriesTestCTByReqDateMonthlyTest[0:10],as.vector(TimeSeriesTestCTByReqDateMonthlyTrainNNetPlot$mean))



accuracy(TimeSeriesTestPETByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestPETByReqDateMonthlyNaive,h=10))[,1])

accuracy(TimeSeriesTestPETByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestPETByReqDateMonthlySNaive,h=10))[,1])

accuracy(TimeSeriesTestPETByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestPETByReqDateMonthlySES,h=10))[,1])

accuracy(TimeSeriesTestPETByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestPETByReqDateMonthlyHolt,h=10))[,1])

accuracy(TimeSeriesTestPETByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestPETByReqDateForHoltWintersMonthlyTrain,h=10))[,1])

accuracy(TimeSeriesTestPETByReqDateMonthlyTest[0:10],as.data.frame(predict(TimeSeriesTestPETAllHospitalsMonthlyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesTestPETByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestPETByReqDateMonthlyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesTestPETByReqDateMonthlyTest[0:10],as.vector(SSAtmPET$mean))

accuracy(TimeSeriesTestPETByReqDateMonthlyTest[0:10],as.vector(TimeSeriesTestPETByReqDateMonthlyTrainNNetPlot$mean))


accuracy(TimeSeriesTestOtherByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestOtherByReqDateMonthlyNaive,h=10))[,1])

accuracy(TimeSeriesTestOtherByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestOtherByReqDateMonthlySNaive,h=10))[,1])

accuracy(TimeSeriesTestOtherByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestOtherByReqDateMonthlySES,h=10))[,1])

accuracy(TimeSeriesTestOtherByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestOtherByReqDateMonthlyHolt,h=10))[,1])

accuracy(TimeSeriesTestOtherByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestOtherByReqDateForHoltWintersMonthlyTrain,h=10))[,1])

accuracy(TimeSeriesTestOtherByReqDateMonthlyTest[0:10],as.data.frame(predict(TimeSeriesTestOtherAllHospitalsMonthlyTrainSimpReg$fitted.values,h=10))[,1])

accuracy(TimeSeriesTestOtherByReqDateMonthlyTest[0:10],as.data.frame(forecast(TimeSeriesTestOtherByReqDateMonthlyTrainARIMA,h=10))[,1])

accuracy(TimeSeriesTestOtherByReqDateMonthlyTest[0:10],as.vector(SSAtmOther$mean))

accuracy(TimeSeriesTestOtherByReqDateMonthlyTest[0:10],as.vector(TimeSeriesTestOtherByReqDateMonthlyTrainNNetPlot$mean))











#Consider hybrid model- as text in lit review suggested.

TimeSeriesAllTestsAllHospitalsDailyHybrid<-forecastHybrid::hybridModel(TimeSeriesAllTestsByReqDateDailyTrain,verbose = T)
TimeSeriesAllTestsByReqDateDailyTrainHybrid<-forecastHybrid::hybridModel(TimeSeriesAllTestsByReqDateDailyTrain)
accuracy(TimeSeriesAllTestsAllHospitalsDailyHybrid)
forecast(TimeSeriesAllTestsAllHospitalsDailyHybrid)

accuracy(TimeSeriesAllTestsByReqDateDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsAllHospitalsDailyHybrid,h=10))[,1])

dev.off()
plot(weeks[50:93],TimeSeriesAllTestsByReqDateWeekly[50:93],type="l",lwd=2,xlab="2019",ylab="Tests",main="All tests, all hospitals weekly forecasts",ylim=c(0,50))
abline(v = weeks[84], col="red", lwd=3)
points(weeks[84:93],as.data.frame(forecast(TimeSeriesAllTestsAllHospitalsDailyHybrid,h=10))[,1],type="l",col="purple",lwd=2,lty=2)
points(weeks[84:93],as.data.frame(forecast(TimeSeriesAllTestsByReqDateWeeklyTrainNNet,h=10))[,1],type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","Hybrid","ANN"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

plot(weeks[50:93],TimeSeriesTestCTByReqDateWeekly[50:93],type="l",lwd=2,xlab="2019",ylab="Tests",main="CT Tests, all hospitals weekly forecasts",ylim=c(0,50))
abline(v = weeks[84], col="red", lwd=3)
points(weeks[84:93],as.data.frame(forecast(SSAtwCT,h=10))[,1],type="l",col="purple",lwd=2,lty=2)
points(weeks[84:93],as.data.frame(forecast(TimeSeriesTestCTByReqDateWeeklyTrainNNet,h=10))[,1],type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","SSA","ANN"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

plot(weeks[50:93],TimeSeriesTestPETByReqDateWeekly[50:93],type="l",lwd=2,xlab="2019",ylab="Tests",main="PET Tests, all hospitals weekly forecasts",ylim=c(0,20))
abline(v = weeks[84], col="red", lwd=3)
points(weeks[84:93],as.data.frame(forecast(TimeSeriesTestPETByReqDateWeeklyTrainNNet,h=10))[,1],type="l",col="red",lwd=2,lty=3)
points(weeks[84:93],as.data.frame(forecast(TimeSeriesTestPETByReqDateForHoltWintersWeeklyTrain,h=10))[,1],type="l",col="purple",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","Holt-Winters","ANN"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

plot(weeks[50:93],TimeSeriesTestOtherByReqDateWeekly[50:93],type="l",lwd=2,xlab="2019",ylab="Tests",main="Other Tests, all hospitals weekly forecasts",ylim=c(0,50))
abline(v = weeks[84], col="red", lwd=3)
points(weeks[84:93],as.data.frame(forecast(TimeSeriesTestOtherByReqDateForHoltWintersWeeklyTrain,h=10))[,1],type="l",col="purple",lwd=2,lty=2)
points(weeks[84:93],as.data.frame(forecast(TimeSeriesTestOtherByReqDateWeeklySES,h=10))[,1],type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","Holt-Winters","SES"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

plot(months[0:21],TimeSeriesTestCTByReqDateMonthly[0:21],type="l",lwd=2,xlab="2019",ylab="Tests",main="CT tests, all hospitals monthly forecasts")
abline(v = months[19], col="red", lwd=3)
points(months[19:21],as.data.frame(forecast(TimeSeriesTestCTByReqDateMonthlySES,h=3))[,1],type="l",col="green",lwd=2,lty=2)
TimeSeriesTestCTByReqDateMonthlyTrainNNetPoints<-c(29.46943, 29.96307, 30.07645)
points(months[19:21],TimeSeriesTestCTByReqDateMonthlyTrainNNetPoints,type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","SES","ANN"),
       col=c("red","green","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)


plot(months[0:21],TimeSeriesTestPETByReqDateMonthly[0:21],type="l",lwd=2,xlab="2019",ylab="Tests",main="PET tests, all hospitals monthly forecasts")
abline(v = months[19], col="red", lwd=3)
points(months[19:21],as.data.frame(forecast(TimeSeriesTestPETByReqDateMonthlySES,h=3))[,1],type="l",col="green",lwd=2,lty=2)
TimeSeriesTestPETByReqDateMonthlyTrainNNetPoints<-c(17.46793, 11.70647, 16.66569)
points(months[19:21],TimeSeriesTestPETByReqDateMonthlyTrainNNetPoints,type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","SES","ANN"),
       col=c("red","green","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)


plot(months[0:21],TimeSeriesTestOtherByReqDateMonthly[0:21],type="l",lwd=2,xlab="2019",ylab="Tests",main="Other tests, all hospitals monthly forecasts")
abline(v = months[19], col="red", lwd=3)
points(months[19:21],as.data.frame(forecast(TimeSeriesTestOtherByReqDateMonthlySES,h=3))[,1],type="l",col="green",lwd=2,lty=2)
TimeSeriesTestOtherByReqDateMonthlyTrainNNetPoints<-c(18.98243, 19.01553, 20.00725)
points(months[19:21],TimeSeriesTestOtherByReqDateMonthlyTrainNNetPoints,type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","SES","ANN"),
       col=c("red","green","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)


TimeSeriesAllTestsByReqDateWeeklyTrainHybrid<-forecastHybrid::hybridModel(TimeSeriesAllTestsByReqDateWeeklyTrain)
accuracy(TimeSeriesAllTestsByReqDateWeeklyTrainHybrid)

accuracy(TimeSeriesAllTestsByReqDateWeeklyTest,as.data.frame(forecast(TimeSeriesAllTestsByReqDateWeeklyTrainHybrid,h=10))[,1])


TimeSeriesAllTestsByReqDateMonthlyTrainHybrid<-forecastHybrid::hybridModel(TimeSeriesAllTestsByReqDateMonthlyTrain)
accuracy(TimeSeriesAllTestsByReqDateMonthlyTrainHybrid)

accuracy(TimeSeriesAllTestsByReqDateMonthlyTest,as.data.frame(forecast(TimeSeriesAllTestsByReqDateMonthlyTrainHybrid,h=7))[,1])


plot(months[0:21],TimeSeriesAllTestsByReqDateMonthly[0:21],type="l",lwd=2,xlab="2019",ylab="Tests",main="All tests, all hospitals monthly forecasts")
abline(v = months[19], col="red", lwd=3)
points(months[19:21],as.data.frame(forecast(TimeSeriesAllTestsByReqDateMonthlySES,h=3))[,1],type="l",col="green",lwd=2,lty=2)
points(months[19:21],as.data.frame(forecast(TimeSeriesAllTestsByReqDateMonthlyTrainHybrid,h=3))[,1],type="l",col="purple",lwd=2,lty=2)
legend("topleft", legend=c("Training/test partition","Hybrid","SES"),
       col=c("red","purple","green"), lty=c(1,2,2),bty = "n", cex=0.8,lwd=2)

predict(TimeSeriesAllTestsAllHospitalsDailyTrainMultReg$fitted.values,h=10)

TimeSeriesAllTestsByReqDateDailyTest[0:10]
TimeSeriesAllTestsByReqDateDailyTest[0:10]
accuracy(TimeSeriesAllTestsByReqDateDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateDailyTrainARIMA,h=10))[,1])
accuracy(TimeSeriesAllTestsByReqDateDailyTest[0:10],as.data.frame(forecast(TimeSeriesAllTestsByReqDateDailyNaive,h=10))[,1])

plot(days[550:596],TimeSeriesAllTestsByReqDateDaily[550:596],type="l",ylim=c(0,20),lwd=2,xlab="2019",ylab="Tests",main="All tests, all hospitals daily forecasts")
abline(v = days[587], col="red", lwd=3)
points(days[587:596],as.data.frame(predict(TimeSeriesAllTestsAllHospitalsDailyTrainMultReg$fitted.values))[,1],type="l",col="green",lwd=2,lty=2)
points(days[587:596],as.data.frame(forecast(TimeSeriesAllTestsByReqDateDailyTrainNNet,h=10))[,1],type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","Multiple linear regression","ANN"),
       col=c("red","green","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

as.data.frame(predict(TimeSeriesAllTestsAllHospitalsDailyTrainMultRegFull$fitted.values))[,1]




plot(weeks[50:93],TimeSeriesAllTestsByReqDateWeekly[50:93],type="l",lwd=2,xlab="2019",ylab="Tests",main="All tests, all hospitals weekly forecasts",ylim=c(0,50))
abline(v = weeks[84], col="red", lwd=3)
points(weeks[84:93],as.data.frame(forecast(TimeSeriesAllTestsByReqDateWeeklyHolt,h=10))[,1],type="l",col="green",lwd=2,lty=2)
points(weeks[84:93],as.data.frame(forecast(TimeSeriesAllTestsByReqDateWeeklyTrainNNet,h=10))[,1],type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","Holt-linear","ANN"),
       col=c("red","green","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

TimeSeriesAllTestsByReqDateWeeklyHoltFull<-holt(TimeSeriesAllTestsByReqDateWeekly)
forecast(TimeSeriesAllTestsByReqDateWeeklyHoltFull)

plot(months[0:21],TimeSeriesAllTestsByReqDateMonthly[0:21],type="l",lwd=2,xlab="Time",ylab="Tests",main="All tests, all hospitals monthly forecasts")
abline(v = months[19], col="red", lwd=3)
points(months[19:21],as.data.frame(forecast(TimeSeriesAllTestsByReqDateMonthlySES,h=3))[,1],type="l",col="green",lwd=2,lty=2)
TimeSeriesAllTestsByReqDateMonthlyTrainNNetForecast<-c(67.33,67.34,67.26) #From nnetar command
points(months[19:21],TimeSeriesAllTestsByReqDateMonthlyTrainNNetForecast,type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","SES","ANN"),
       col=c("red","green","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

TimeSeriesAllTestsByReqDateMonthlySES<-ses(TimeSeriesAllTestsByReqDateMonthly)
forecast(TimeSeriesAllTestsByReqDateMonthlyNNet,h=3)

str(TimeSeriesAllTestsByReqDateMonthlySES)


plot(days[550:596],TimeSeriesAllTestsByReqDateDaily[550:596],type="l",ylim=c(0,20),lwd=2,xlab="2019",ylab="Tests",main="All tests, all hospitals daily forecasts")
abline(v = days[587], col="red", lwd=3)
points(days[587:596],as.data.frame(predict(TimeSeriesAllTestsAllHospitalsDailyTrainMultReg$fitted.values,h=10))[,1],type="l",col="red",lwd=2,lty=3)
points(days[587:596],as.data.frame(forecast(TimeSeriesAllTestsAllHospitalsDailyHybrid,h=10))[,1],type="l",col="purple",lwd=2,lty=2)
legend("topleft", legend=c("Training/test partition","Hybrid","Multiple linear regression"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

plot(weeks[50:93],TimeSeriesAllTestsByReqDateWeekly[50:93],type="l",lwd=2,xlab="2019",ylab="Tests",main="All tests, all hospitals weekly forecasts",ylim=c(0,50))
abline(v = weeks[84], col="red", lwd=3)
points(weeks[84:93],as.data.frame(forecast(TimeSeriesAllTestsByReqDateWeeklyTrainHybrid,h=10))[,1],type="l",col="purple",lwd=2,lty=2)
points(weeks[84:93],as.data.frame(forecast(TimeSeriesAllTestsByReqDateWeeklyHolt,h=10))[,1],type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","Hybrid","Holt-linear"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)

plot(months[0:21],TimeSeriesAllTestsByReqDateMonthly[0:21],type="l",lwd=2,xlab="2019",ylab="Tests",main="All tests, all hospitals monthly forecasts",)
abline(v = months[19], col="red", lwd=3)
points(months[19:21],as.data.frame(forecast(TimeSeriesAllTestsByReqDateMonthlyTrainHybrid,h=3))[,1],type="l",col="purple",lwd=2,lty=2)
points(months[19:21],TimeSeriesAllTestsByReqDateMonthlyTrainNNetForecast,type="l",col="red",lwd=2,lty=3)
legend("topleft", legend=c("Training/test partition","Hybrid","ANN"),
       col=c("red","purple","red"), lty=c(1,2,3),bty = "n", cex=0.8,lwd=2)


plot(days[0:596],TimeSeriesAllTestsByReqDateDaily[0:596],type="l")
next10days<-seq(as.Date("2019-12-25"), as.Date("2020-01-03"), by = "day")
points(next10days,as.data.frame(predict(TimeSeriesAllTestsAllHospitalsDailyTrainMultRegFull$fitted.values,h=10))[,1],col="blue")

as.data.frame(predict(TimeSeriesAllTestsAllHospitalsDailyTrainMultRegFull$fitted.values,h=10))[,1]

plot(forecast(ses(TimeSeriesAllTestsByReqDateDaily),h=10),type="l",lwd=2,xlab="Time",ylab="Tests",main="All tests, all hospitals daily forecasts",ylim=c(0,25), xaxt="n")

plot(forecast(holt(TimeSeriesAllTestsByReqDateWeekly),h=10),type="l",lwd=2,xlab="Time",ylab="Tests",main="All tests, all hospitals weekly forecasts", xaxt="n",ylim=c(0,50))

plot(forecast(holt(TimeSeriesAllTestsByReqDateWeekly),h=10),type="l",lwd=2,xlab="Year",ylab="Tests",main="All tests, all hospitals weekly forecasts", xaxt="n",ylim=c(0,50))

plot(forecast(nnetar(TimeSeriesAllTestsByReqDateMonthly),h=3),type="l",lwd=2,xlab="Time",ylab="Tests",main="All tests, all hospitals monthly forecasts", xaxt="n")

forecast(ssa(TimeSeriesAllTestsByReqDateDaily))


plot(forecast(ssa(TimeSeriesTestCTByReqDateWeekly),groups=list(c(1,4)),h=10),,type="l",lwd=2,xlab="Time",ylab="Tests",main="CT tests, all hospitals weekly forecasts", xaxt="n")

plot(forecast(hw(TimeSeriesTestPETByReqDateWeekly),groups=list(c(1,4)),h=10),,type="l",lwd=2,xlab="Time",ylab="Tests",main="PET tests, all hospitals weekly forecasts", xaxt="n",ylim=c(-0.5,10))

forecast(hw(TimeSeriesTestPETByReqDateWeekly),groups=list(c(1,4)),h=10)

forecast(ses(TimeSeriesTestOtherByReqDateWeekly),h=10)

plot(forecast(ses(TimeSeriesTestOtherByReqDateWeekly),h=10),type="l",lwd=2,xlab="Time",ylab="Tests",main="Other tests, all hospitals weekly forecasts", xaxt="n")

forecast(nnetar(TimeSeriesTestCTByReqDateMonthly),h=3)
plot(forecast(nnetar(TimeSeriesTestCTByReqDateMonthly),h=3),type="l",lwd=2,xlab="Time",ylab="Tests",main="CT tests, all hospitals monthly forecasts", xaxt="n")

forecast(ses(TimeSeriesTestPETByReqDateMonthly),h=3)
plot(forecast(ses(TimeSeriesTestPETByReqDateMonthly),h=3),type="l",lwd=2,xlab="Time",ylab="Tests",main="PET tests, all hospitals monthly forecasts", xaxt="n")

forecast(nnetar(TimeSeriesTestOtherByReqDateMonthly),h=3)
plot(forecast(nnetar(TimeSeriesTestOtherByReqDateMonthly),h=3),type="l",lwd=2,xlab="Time",ylab="Tests",main="Other tests, all hospitals monthly forecasts", xaxt="n")
