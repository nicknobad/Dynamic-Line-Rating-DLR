### KTH, Department of Industrial Information and Control Systems (ICS)
### Master Thesis Project
### BUILDING PREDICTIVE MODELS FOR DYNAMIC LINE RATING USING DATA SCIENCE TECHNIQUES 
### Sponsored by Vattenfall AG
### Author: Nicolae Doban
### Stockholm, 2015

## This module builds the hourly mean values and builds the predictive models on the historical observations
## Packages used: base, reshape, zoo
## INPUT DATA: The interpolated and processed dataset new.Rda
## OUTPUT: Building the Hourly mean and sampled observations.
## mean.Hours.csv will be used in building the predictive models (PredModelHistObs.R)

newDir <- setwd("C:/Users/LabStudent/Desktop/Nick/GitHub") #set the directory of the dataset
load("new.Rda") #loading the dataset

library(reshape)
library(zoo)

#deleting the variables that won't be used in the building the predictive models
new$Hours<-NULL
new$Days<-NULL
new$MonthYearAbb<-NULL
new$Date<-NULL
new$Time<-NULL
new$Current<-NULL

new<-new[c("DateTime", "interpSolarRad", "interpWindDir", "interpAmbTemp", "interpWindSpeed", "interpCondTemp",
           "interpCurrent", "interpHum", "interpBarPress", "NormalDLR")]        #Re-arrange by columns' names
#renaming the columns
new<-rename(new, c("interpSolarRad"="SolarRad", "interpWindDir"="WindDir", "interpAmbTemp"="AmbTemp",
                   "interpWindSpeed"="WindSpeed", "interpCondTemp"="CondTemp", "interpHum"="Humidity",
                   "interpBarPress"="BarometricPressure"))
colnames(new)[7] <- "Current"

NEW.interp<-new #duplicate the tidy dataframe                                             

DateTime<-seq.POSIXt(from=as.POSIXct("2014-09-01 00:00:00", tz="Etc/GMT+12"), 
                     to=as.POSIXct("2015-03-31 23:59:00", tz="Etc/GMT+12"), by=60)
DateTime<-as.character(DateTime)
DateTime<-as.data.frame(DateTime);colnames(DateTime)[1]<-"DateTime"
NEW.interp<-merge(NEW.interp,DateTime, by="DateTime", all=TRUE)  #merge the datasets

#interpolate NA-row
NEW.interp$SolarRad<-na.spline(NEW.interp$SolarRad)
NEW.interp$WindDir<-na.spline(NEW.interp$WindDir)
NEW.interp$WindSpeed<-na.spline(NEW.interp$WindSpeed)
NEW.interp$AmbTemp<-na.spline(NEW.interp$AmbTemp)
NEW.interp$CondTemp<-na.spline(NEW.interp$CondTemp)
NEW.interp$Current<-na.spline(NEW.interp$Current)
NEW.interp$Humidity<-na.spline(NEW.interp$Humidity)
NEW.interp$BarometricPressure<-na.spline(NEW.interp$BarometricPressure)
NEW.interp$NormalDLR<-na.spline(NEW.interp$NormalDLR)
#/interpolate NA-row
#result: a dataset With 305280 rows AS IT SHOULD BE

#Sampling the new dataframe to get hourly observations 
NEW.interp$Hours<-strftime(NEW.interp$DateTime, "%H")
NEW.interp$Time<-strftime(NEW.interp$DateTime, "%H:%M")
NEW.interp$Time<-factor(NEW.interp$Time)
sample.Hours<-NEW.interp[NEW.interp$Time %in% c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", 
                                                "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", 
                                                "19:00", "20:00", "21:00", "22:00", "23:00"), ]
sample.Hours$Hours<-factor(sample.Hours$Hours)
rownames(sample.Hours)<-NULL
sample.Hours$Date<-NULL
sample.Hours$Time<-NULL
#sample.Hours$DateTime<-NULL
sample.Hours$MonthYearAbb<-NULL
sample.Hours$Month<-NULL
sample.Hours$Season<-NULL
sample.Hours$daytype<-NULL
sample.Hours$TypeOfDay<-NULL
sample.Hours$indexDT<-NULL
sample.Hours$Days<-NULL
sample.Hours$Hours<-NULL
sample.Hours$BarometricPressure<-NULL
sample.Hours$Humidity<-NULL

write.table(sample.Hours, "sample.Hours.csv", sep=",") #save to CSV file
sample.Hours<-read.csv("sample.Hours.csv", sep=",", header=TRUE)
#/Sampling the new dataframe to get hourly observations 

NEW.interp$DateTime<-seq.POSIXt(from=as.POSIXct("2014-09-01 00:00:00", tz="Etc/GMT+12"), 
                                to=as.POSIXct("2015-03-31 23:59:00", tz="Etc/GMT+12"), by=60)

#Averaging the hourly dataframe from interpolated values
SolarRad.Hourly<-aggregate(list(SolarRad = NEW.interp$SolarRad), 
                           list(DateHour = cut(NEW.interp$DateTime, "1 hour")), mean)
WindDir.Hourly<-aggregate(list(WindDir = NEW.interp$WindDir), 
                          list(DateHour = cut(NEW.interp$DateTime, "1 hour")), mean)
AmbTemp.Hourly<-aggregate(list(AmbTemp = NEW.interp$AmbTemp), 
                          list(DateHour = cut(NEW.interp$DateTime, "1 hour")), mean)
WindSpeed.Hourly<-aggregate(list(WindSpeed = NEW.interp$WindSpeed), 
                            list(DateHour = cut(NEW.interp$DateTime, "1 hour")), mean)
CondTemp.Hourly<-aggregate(list(CondTemp = NEW.interp$CondTemp), 
                           list(DateHour = cut(NEW.interp$DateTime, "1 hour")), mean)
Current.Hourly<-aggregate(list(Current = NEW.interp$Current), 
                          list(DateHour = cut(NEW.interp$DateTime, "1 hour")), mean)
NormalDLR.Hourly<-aggregate(list(NormalDLR = NEW.interp$NormalDLR), 
                            list(DateHour = cut(NEW.interp$DateTime, "1 hour")), mean)

mean.Hours<-data.frame(DateTime=SolarRad.Hourly$DateHour, SolarRad=SolarRad.Hourly$SolarRad,
                       WindDir=WindDir.Hourly$WindDir, AmbTemp=AmbTemp.Hourly$AmbTemp, 
                       WindSpeed=WindSpeed.Hourly$WindSpeed, CondTemp=CondTemp.Hourly$CondTemp,
                       Current=Current.Hourly$Current, NormalDLR.Hourly$NormalDLR)

rownames(mean.Hours)<-NULL
colnames(mean.Hours)[8]<-"NormalDLR"
write.table(mean.Hours, "mean.Hours.csv", sep=",") #save to CSV file
mean.Hours<-read.csv("mean.Hours.csv", sep=",", header=TRUE)
#/Averaging the hourly dataframe from interpolated values