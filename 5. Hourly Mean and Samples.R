### KTH, Department of Industrial Information and Control Systems (ICS)
### Master Thesis Project
### BUILDING PREDICTIVE MODELS FOR DYNAMIC LINE RATING USING DATA SCIENCE TECHNIQUES 
### Sponsored by Vattenfall AG
### Author: Nicolae Doban
### Stockholm, 2015

## This module is comparing the variations between hourly (sampled and mean values) and minute-based values
## Packages used: base
## INPUT DATA: The interpolated and processed dataset new.Rda
## OUTPUT: Comparison between hourly sampled and mean values


                                                #Hourly Mean and Sampling

newDir <- setwd("C:/Users/LabStudent/Desktop/Nick/GitHub") #set the directory of the dataset
load("new.Rda") #loading the dataset

NEW.interp<-new #duplicating the interpolated and processed dataset

                                    #Sampling the new dataframe to get hourly observations 

sample.Hours<-NEW.interp[NEW.interp$Time %in% c("0:00", "1:00", "2:00", "3:00", "4:00", "5:00", "6:00", "7:00", "8:00", "9:00", 
                                  "10:00", "10:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", 
                                  "19:00", "20:00", "21:00", "22:00", "23:00"), ]
sample.Hours$Hours<-factor(sample.Hours$Hours)
rownames(sample.Hours)<-NULL
#/Sampling the new dataframe to get hourly observations 

#Averaging the hourly dataframe from interpolated values
NEW.interp$DateHours<-as.factor(paste(NEW.interp$Date, NEW.interp$Hours))
SolarRadHourlyMean<-tapply(NEW.interp$interpSolarRad, NEW.interp$DateHours, mean)
WindDirHourlyMean<-tapply(NEW.interp$interpWindDir, NEW.interp$DateHours, mean)
AmbTempHourlyMean<-tapply(NEW.interp$interpAmbTemp, NEW.interp$DateHours, mean)
WindSpeedHourlyMean<-tapply(NEW.interp$interpWindSpeed, NEW.interp$DateHours, mean)
HumHourlyMean<-tapply(NEW.interp$interpHum, NEW.interp$DateHours, mean)
BarPressHourlyMean<-tapply(NEW.interp$interpBarPress, NEW.interp$DateHours, mean)
CondTempHourlyMean<-tapply(NEW.interp$interpCondTemp, NEW.interp$DateHours, mean)
CurrentHourlyMean<-tapply(NEW.interp$interpCurrent, NEW.interp$DateHours, mean)
NormalDLRHourlyMean<-tapply(NEW.interp$NormalDLR, NEW.interp$DateHours, mean)

mean.Hours<-data.frame(SolarRadHourlyMean, WindDirHourlyMean, AmbTempHourlyMean, WindSpeedHourlyMean,
                       HumHourlyMean, BarPressHourlyMean, CondTempHourlyMean, CurrentHourlyMean, 
                       NormalDLRHourlyMean)
mean.Hours$Hours<-0:23
mean.Hours$Hours<-factor(mean.Hours$Hours)

new1<-new  #Duplicating the new dataframe for caution reasons
new1$Hours<-factor(new1$Hours) #Factor the new$1Hours column

#This picture shows that there is not a big variation between minute-based, hourly sampled and averaged values of NormalDLR
pdf("Time Resolution Comparison.pdf", height=9, width=16.5)
par(mfrow=c(1,3))
boxplot(NEW.interp$NormalDLR~NEW.interp$Hours, main="Original. Interpolated", ylim=c(700,3700), ylab="NormalDLR", xlab="Hours")
plot(mean.Hours$NormalDLRHourlyMean~mean.Hours$Hours, main="Hourly Mean", ylim=c(700,3700), ylab="NormalDLR", xlab="Hours")
plot(sample.Hours$NormalDLR~sample.Hours$Hours, main="Hourly Sampled", ylim=c(700,3700), ylab="NormalDLR", xlab="Hours")
dev.off()

                      #/Averaging the hourly dataframe from interpolated values

#/Hourly Mean and Sampling