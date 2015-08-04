### KTH, Department of Industrial Information and Control Systems (ICS)
### Master Thesis Project
### BUILDING PREDICTIVE MODELS FOR DYNAMIC LINE RATING USING DATA SCIENCE TECHNIQUES 
### Sponsored by Vattenfall AG
### Author: Nicolae Doban
### Stockholm, 2015

## This module combines the past +24H and +48H predictions
## Packages used: base, histogram, reshape, zoo, splines, caret, boot, DAAG, 
## INPUT DATA: The minute-based dataset and the past forecasted ambient temperature and wind datasets (AirTempP.csv, WindP.csv)
## OUTPUT: Running the models for +24H and +48H time-horizons and compare their accuracies
## Make sure you load the correct models

## Until line 207, the script is similar to either of the "24H.R or "48H.R". After that, the dataframes are 
## built for +24H and for+48H and the comparison of predicted AmbTemp and WIndSpeed is saved to png files for 
## both time horizons. 

#+24H
library(caret);library(rminer);library(boot);library(forecast);library(splines);library(DAAG); library(histogram)
setwd("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/")
mean.Hours<-read.csv("mean.Hours.csv", header=TRUE, sep=",")#import Hourly data: mean
mean.Hours$DateTime<-seq.POSIXt(from=as.POSIXct("2014-09-01 00:00:00", tz="Etc/GMT+12"), 
                                to=as.POSIXct("2015-03-31 23:00:00",tz="Etc/GMT+12"), by=3600)
mean.Hours$Hours<-format(mean.Hours$DateTime, "%H")
mean.Hours$Days<-weekdays(mean.Hours$DateTime)
mean.Hours$Days<-factor(mean.Hours$Days, levels=c("måndag", "tisdag", "onsdag", "torsdag", "fredag",
                                                  "lördag", "söndag"))  #Swedish

mean.Hours<-mean.Hours[-c(1:336), ]; rownames(mean.Hours)<-NULL     #skip the mean values of NormalDLR.

mean.Hours$Hours<-as.numeric(mean.Hours$Hours)

#Type of Day column
x <- c(rep("Weekday",24), rep("Weekday",24), rep("Weekday",24), rep("Weekday",24), rep("Weekday",24), rep("Weekend",24), rep("Weekend",24))
xW<-rep(x,28)
rest<-rep("Weekday",48)

mean.Hours$daytype<-c(xW,rest)
mean.Hours$daytype<-factor(mean.Hours$daytype)
mean.Hours<-mean.Hours[,c(1,2,3,4,5,9,10,11,7,6,8)]
mean.Hours.24H<-mean.Hours[mean.Hours$Hours %in% c(0,6,12,18),]; rownames(mean.Hours.24H)<-NULL

setwd("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/")
#AirTemp
PAirTemp<-read.csv("AirTempP.csv", sep=";", header=TRUE, skip=5120)
#PAirTemp<-head(PAirTemp, 3680)
colnames(PAirTemp)[1]<-"DateTimeLEN"  #Renaming AirTemp columns
colnames(PAirTemp)[2]<-"latitud"
colnames(PAirTemp)[3]<-"longitud"
colnames(PAirTemp)[4]<-"AmbTempK"      #/Renaming AirTemp columns
PAirTemp$AmbTempC<-PAirTemp$AmbTempK-273.15 #Convert AirTemp from Kelvin to Celsius

PAirTemp$LEN<-rep(c(24, 30, 36, 42, 48), 796) #new column to extract the +24H and +48H later on

library(zoo)
#Strange: Oct., 25 takes an interval from 6-hour to 5-hour - it is because+
#of the DayLight Savings Time from Oct.,25th to 26th
PAirTemp24<-PAirTemp[PAirTemp$LEN %in% c(24),]; rownames(PAirTemp24)<-NULL
PAirTemp24$DateTime<-seq.POSIXt(from=as.POSIXct("2014-09-14 00:00:00", tz="Etc/GMT+12"), 
                                to=as.POSIXct("2015-03-31 18:00:00",tz="Etc/GMT+12"), by=3600*6)
PAirTemp24$AmbTempK<-NULL
PAirTemp24<-PAirTemp24[c("DateTime", "LEN", "latitud", "longitud", "AmbTempC" )]
PAirTemp24<-head(PAirTemp24, 792)

mean.Hours.24H$SolarRad=mean.Hours.24H$WindDir<-NULL

#PWind
setwd("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/")
PWind<-read.csv("WindP.csv", sep=";", header=TRUE, skip=5120)  
colnames(PWind)[1]<-"DateTimeLEN"  #Renaming PWind columns
colnames(PWind)[2]<-"latitud"
colnames(PWind)[3]<-"longitud"
colnames(PWind)[4]<-"u10m"
colnames(PWind)[5]<-"v10m"          #/Renaming PWind columns
#PWind<-head(PWind, 3720)
PWind$PWindSpeed<-sqrt(PWind$u10m^2+PWind$v10m^2)

#MUST SUBSET DATA IN +24
PWind$LEN<-rep(c(24, 30, 36, 42, 48), 796)
PWind24<-PWind[PWind$LEN %in% c(24),]; PWind24<-head(PWind24, 792); rownames(PWind24)<-NULL #only +24H
#/MUST SUBSET DATA IN +24

#+24H
PWind24$DateTime<-seq.POSIXt(from=as.POSIXct("2014-09-14 00:00:00", tz="Etc/GMT+12"), 
                             to=as.POSIXct("2015-03-30 18:00:00",tz="Etc/GMT+12"), by=3600*6)
PWind24$DateTimeLEN=PWind24$u10m=PWind24$v10m<-NULL
PWind24<-PWind24[c("DateTime", "LEN", "latitud", "longitud","PWindSpeed")] #, "PWindDir"
PWind24$DateTime<-as.character(PWind24$DateTime)
#/Wind

mean.Hours.24H$PDateTime24H<-PAirTemp24$DateTime
mean.Hours.24H$PAmbTemp24H<-PAirTemp24$AmbTempC
mean.Hours.24H<-mean.Hours.24H[c("PDateTime24H", "PAmbTemp24H", "DateTime", "AmbTemp", "WindSpeed",
                                 "Hours", "Days", "daytype", "Current", "CondTemp", "NormalDLR")]

mean.Hours.24H$PWindSpeed24H<-PWind24$PWindSpeed
mean.Hours.24H<-mean.Hours.24H[c("PDateTime24H", "PAmbTemp24H", "DateTime", "AmbTemp", "PWindSpeed24H",
                                 "WindSpeed","Hours", "Days", "daytype", "Current", "CondTemp", 
                                 "NormalDLR")]


train.mean.Hours.24H<-head(mean.Hours.24H, 668);   rownames(train.mean.Hours.24H)<-NULL
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/24H/SavedModels/model.fitAmbTemp24.lm.model")
train.mean.Hours.24H$pred.AmbTemp24.lm<-predict(model.fitAmbTemp24.lm, train.mean.Hours.24H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/24H/SavedModels/model.WindSpeed.cubist.model")
train.mean.Hours.24H$pred.WindSpeed24.cubist<-predict(model.WindSpeed.cubist, train.mean.Hours.24H)

train.mean.Hours.24H<-train.mean.Hours.24H[c("PDateTime24H", "PAmbTemp24H", "DateTime", "AmbTemp", 
                                             "pred.AmbTemp24.lm", "PWindSpeed24H","WindSpeed", 
                                             "pred.WindSpeed24.cubist","Hours", "Days", "daytype", "Current", 
                                             "CondTemp", "NormalDLR")]
train<-train.mean.Hours.24H[,c("DateTime", "pred.AmbTemp24.lm","pred.WindSpeed24.cubist", "Hours", "Days", "daytype", "Current")]

test.mean.Hours.24H<-mean.Hours.24H[c(669:792),];  rownames(test.mean.Hours.24H)<-NULL
test.mean.Hours.24H$pred.AmbTemp24.lm<-predict(model.fitAmbTemp24.lm, test.mean.Hours.24H)
test.mean.Hours.24H$pred.WindSpeed24.cubist<-predict(model.WindSpeed.cubist, test.mean.Hours.24H)
test.mean.Hours.24H<-test.mean.Hours.24H[c("PDateTime24H", "PAmbTemp24H", "DateTime", "AmbTemp", "pred.AmbTemp24.lm", 
                                           "PWindSpeed24H","WindSpeed", "pred.WindSpeed24.cubist", "Hours", 
                                           "Days", "daytype", "Current", "CondTemp", "NormalDLR")]
#/+24H


#+48H

library(caret);library(rminer);library(boot);library(forecast);library(splines);library(DAAG); library(histogram)
setwd("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/")
mean.Hours<-read.csv("mean.Hours.csv", header=TRUE, sep=",")#import Hourly data: mean
mean.Hours$DateTime<-seq.POSIXt(from=as.POSIXct("2014-09-01 00:00:00", tz="Etc/GMT+12"), 
                                to=as.POSIXct("2015-03-31 23:00:00",tz="Etc/GMT+12"), by=3600)
mean.Hours$Hours<-format(mean.Hours$DateTime, "%H")
mean.Hours$Days<-weekdays(mean.Hours$DateTime)
mean.Hours$Days<-factor(mean.Hours$Days, levels=c("måndag", "tisdag", "onsdag", "torsdag", "fredag",
                                                  "lördag", "söndag"))  #Swedish

mean.Hours<-mean.Hours[-c(1:336), ]; rownames(mean.Hours)<-NULL     #skip the mean values of NormalDLR.

mean.Hours$Hours<-as.numeric(mean.Hours$Hours)

#Type of Day column
x <- c(rep("Weekday",24), rep("Weekday",24), rep("Weekday",24), rep("Weekday",24), rep("Weekday",24), rep("Weekend",24), rep("Weekend",24))
xW<-rep(x,28)
rest<-rep("Weekday",48)

mean.Hours$daytype<-c(xW,rest)
mean.Hours$daytype<-factor(mean.Hours$daytype)
mean.Hours<-mean.Hours[,c(1,2,3,4,5,9,10,11,7,6,8)]
mean.Hours.48H<-mean.Hours[mean.Hours$Hours %in% c(0,6,12,18),]; rownames(mean.Hours.48H)<-NULL

#PAirTemp48<-PAirTemp[PAirTemp$LEN %in% c(24),]; rownames(PAirTemp48)<-NULL

setwd("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/")
#AirTemp
PAirTemp<-read.csv("AirTempP.csv", sep=";", header=TRUE, skip=5100)
#PAirTemp<-head(PAirTemp, 3680)
colnames(PAirTemp)[1]<-"DateTimeLEN"  #Renaming AirTemp columns
colnames(PAirTemp)[2]<-"latitud"
colnames(PAirTemp)[3]<-"longitud"
colnames(PAirTemp)[4]<-"AmbTempK"      #/Renaming AirTemp columns
PAirTemp$AmbTempC<-PAirTemp$AmbTempK-273.15 #Convert AirTemp from Kelvin to Celsius

PAirTemp$LEN<-rep(c(24, 30, 36, 42, 48), 800) #new column to extract the +48H and +48H later on

library(zoo)
#Strange: Oct., 25 takes an interval from 6-hour to 5-hour - it is because+
#of the DayLight Savings Time from Oct.,25th to 26th
PAirTemp48<-PAirTemp[PAirTemp$LEN %in% c(48),]; rownames(PAirTemp48)<-NULL
PAirTemp48$DateTime<-seq.POSIXt(from=as.POSIXct("2014-09-13 00:00:00", tz="Etc/GMT+12"), 
                                to=as.POSIXct("2015-03-31 18:00:00",tz="Etc/GMT+12"), by=3600*6)
PAirTemp48$AmbTempK<-NULL
PAirTemp48<-PAirTemp48[c("DateTime", "LEN", "latitud", "longitud", "AmbTempC" )]
PAirTemp48<-head(PAirTemp48, 792)

mean.Hours.48H$SolarRad=mean.Hours.48H$WindDir<-NULL

mean.Hours.48H$PDateTime48H<-PAirTemp48$DateTime
mean.Hours.48H$PAmbTemp48H<-PAirTemp48$AmbTempC
mean.Hours.48H<-mean.Hours.48H[c("PDateTime48H", "PAmbTemp48H", "DateTime", "AmbTemp", "WindSpeed",
                                 "Hours", "Days", "daytype", "Current", "CondTemp", "NormalDLR")]
plot(mean.Hours.48H$PAmbTemp48H~mean.Hours.48H$AmbTemp, ylab="AmbTemp.Prognosed [C]", 
     xlab="AmbTemp.Measured [C]", main="Ambtemp. Prognosed vs Measured. +48H")

#PWind
setwd("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/")
PWind<-read.csv("WindP.csv", sep=";", header=TRUE, skip=5100)  
colnames(PWind)[1]<-"DateTimeLEN"  #Renaming PWind columns
colnames(PWind)[2]<-"latitud"
colnames(PWind)[3]<-"longitud"
colnames(PWind)[4]<-"u10m"
colnames(PWind)[5]<-"v10m"          #/Renaming PWind columns
#PWind<-head(PWind, 3720)
PWind$PWindSpeed<-sqrt(PWind$u10m^2+PWind$v10m^2)

#MUST SUBSET DATA IN +24
PWind$LEN<-rep(c(24, 30, 36, 42, 48), 800)
PWind48<-PWind[PWind$LEN %in% c(48),]; PWind48<-head(PWind48, 792); rownames(PWind48)<-NULL #only +48H
#/MUST SUBSET DATA IN +24

#+48H
PWind48$DateTime<-seq.POSIXt(from=as.POSIXct("2014-09-14 00:00:00", tz="Etc/GMT+12"), 
                             to=as.POSIXct("2015-03-30 18:00:00",tz="Etc/GMT+12"), by=3600*6)
PWind48$DateTimeLEN=PWind48$u10m=PWind48$v10m<-NULL
PWind48<-PWind48[c("DateTime", "LEN", "latitud", "longitud","PWindSpeed")] #, "PWindDir"
PWind48$DateTime<-as.character(PWind48$DateTime)
#/Wind

mean.Hours.48H$PWindSpeed48H<-PWind48$PWindSpeed
mean.Hours.48H<-mean.Hours.48H[c("PDateTime48H", "PAmbTemp48H", "DateTime", "AmbTemp", "PWindSpeed48H",
                                 "WindSpeed","Hours", "Days", "daytype", "Current", "CondTemp", 
                                 "NormalDLR")]


#Work with predicted AmbTemp and WindSpeed

#+24H
train.mean.Hours.24H<-head(mean.Hours.24H, 668);   rownames(train.mean.Hours.24H)<-NULL
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/24H/SavedModels/model.fitAmbTemp24.lm.model")
train.mean.Hours.24H$predAmbTemp.lm<-predict(model.fitAmbTemp24.lm, train.mean.Hours.24H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/24H/SavedModels/model.WindSpeed.cubist.model")
train.mean.Hours.24H$pred.WindSpeed.cubist<-predict(model.WindSpeed.cubist, train.mean.Hours.24H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/24H/SavedModels/model.Current.svmLinear.model")
train.mean.Hours.24H$pred.Current.svmLinear<-predict(model.Current.svmLinear, train.mean.Hours.24H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/24H/SavedModels/model.CondTemp.gam.model")
train.mean.Hours.24H$pred.CondTemp.gam<-predict(model.CondTemp.gam, train.mean.Hours.24H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/24H/SavedModels/model.NormalDLR.brnn.model")
train.mean.Hours.24H$pred.NormalDLR.brnn<-predict(model.NormalDLR.brnn, train.mean.Hours.24H)

train.mean.Hours.24H<-train.mean.Hours.24H[c("PDateTime24H", "PAmbTemp24H", "DateTime", "AmbTemp", 
                                             "predAmbTemp.lm", "PWindSpeed24H","WindSpeed", 
                                             "pred.WindSpeed.cubist","Hours", "Days", "daytype", "Current", 
                                             "pred.Current.svmLinear", "CondTemp", "pred.CondTemp.gam", "NormalDLR",
                                             "pred.NormalDLR.brnn")]
train<-train.mean.Hours.24H[,c("DateTime", "predAmbTemp.lm","pred.WindSpeed.cubist", "Hours", "Days", 
                               "daytype", "pred.Current.svmLinear", "pred.CondTemp.gam", "NormalDLR",
                               "pred.NormalDLR.brnn")]

test.mean.Hours.24H<-mean.Hours.24H[c(669:792),];  rownames(test.mean.Hours.24H)<-NULL
test.mean.Hours.24H$predAmbTemp.lm<-predict(model.fitAmbTemp24.lm, test.mean.Hours.24H)
test.mean.Hours.24H$pred.WindSpeed.cubist<-predict(model.WindSpeed.cubist, test.mean.Hours.24H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/24H/SavedModels/model.Current.svmLinear.model")
test.mean.Hours.24H$pred.Current.svmLinear<-predict(model.Current.svmLinear, test.mean.Hours.24H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/24H/SavedModels/model.CondTemp.gam.model")
test.mean.Hours.24H$pred.CondTemp.gam<-predict(model.CondTemp.gam, test.mean.Hours.24H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/24H/SavedModels/model.NormalDLR.brnn.model")
test.mean.Hours.24H$pred.NormalDLR.brnn<-predict(model.NormalDLR.brnn, test.mean.Hours.24H)
test.mean.Hours.24H<-test.mean.Hours.24H[,c("PDateTime24H", "PAmbTemp24H", "DateTime", "AmbTemp", 
                                            "predAmbTemp.lm", "PWindSpeed24H","WindSpeed", 
                                            "pred.WindSpeed.cubist","Hours", "Days", "daytype", "Current", 
                                            "pred.Current.svmLinear", "CondTemp", "pred.CondTemp.gam", "NormalDLR",
                                            "pred.NormalDLR.brnn")]
#/+24H


#+48H
train.mean.Hours.48H<-head(mean.Hours.48H, 668);   rownames(train.mean.Hours.48H)<-NULL
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.AmbTemp48.lm.model")
train.mean.Hours.48H$pred.AmbTemp.lm<-predict(model.AmbTemp48.lm, train.mean.Hours.48H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.WindSpeed.cubist.model")
train.mean.Hours.48H$pred.WindSpeed.cubist<-predict(model.WindSpeed.cubist, train.mean.Hours.48H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.Current.brnn.model")
train.mean.Hours.48H$pred.Current.brnn<-predict(model.Current.brnn, train.mean.Hours.48H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.CondTemp.lm.model")
train.mean.Hours.48H$pred.CondTemp.lm<-predict(model.CondTemp.lm, train.mean.Hours.48H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.NormalDLR.svmLinear.model")
train.mean.Hours.48H$pred.NormalDLR.svmLinear<-predict(model.NormalDLR.svmLinear, train.mean.Hours.48H)

train.mean.Hours.48H<-train.mean.Hours.48H[c("PDateTime48H", "PAmbTemp48H", "DateTime", "AmbTemp", 
                                             "pred.AmbTemp.lm", "PWindSpeed48H","WindSpeed", 
                                             "pred.WindSpeed.cubist","Hours", "Days", "daytype", "Current", 
                                             "pred.Current.brnn", "CondTemp", "pred.CondTemp.lm", "NormalDLR")]
train<-train.mean.Hours.48H[,c("DateTime", "pred.AmbTemp.lm","pred.WindSpeed.cubist", "Hours", "Days", 
                               "daytype", "pred.Current.brnn", "pred.CondTemp.lm", "NormalDLR")]

test.mean.Hours.48H<-mean.Hours.48H[c(669:792),];  rownames(test.mean.Hours.48H)<-NULL
test.mean.Hours.48H$pred.AmbTemp.lm<-predict(model.AmbTemp48.lm, test.mean.Hours.48H)
test.mean.Hours.48H$pred.WindSpeed.cubist<-predict(model.WindSpeed.cubist, test.mean.Hours.48H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.Current.svmLinear.model")
test.mean.Hours.48H$pred.Current.brnn<-predict(model.Current.svmLinear, test.mean.Hours.48H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.CondTemp.lm.model")
test.mean.Hours.48H$pred.CondTemp.lm<-predict(model.CondTemp.lm, test.mean.Hours.48H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.NormalDLR.svmLinear.model")
test.mean.Hours.48H$pred.NormalDLR.svmLinear<-predict(model.NormalDLR.svmLinear, test.mean.Hours.48H)

test.mean.Hours.48H<-test.mean.Hours.48H[,c("PDateTime48H", "PAmbTemp48H", "DateTime", "AmbTemp", 
                                            "pred.AmbTemp.lm", "PWindSpeed48H","WindSpeed", 
                                            "pred.WindSpeed.cubist","Hours", "Days", "daytype", "Current", 
                                            "pred.Current.brnn", "CondTemp", "pred.CondTemp.lm", "NormalDLR",
                                            "pred.NormalDLR.svmLinear")]

#+/48H

setwd("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/24H+48H")
#Plotting AmbTemp
#nonetheless, the g-plots can be still built
png("AmbTemp24&48.png", width=1300, height=650)
plot(test.mean.Hours.24H$AmbTemp~test.mean.Hours.24H$DateTime, cex.axis=1.5, cex.lab=1.7, bg='black', pch=21, cex=1.5,
     type="l", main="Measured and Predicted values of the Ambient Temperature +24H and +48H", xlab="Time", ylab="Ambient Temperature [C]")
lines(test.mean.Hours.24H$predAmbTemp.lm~test.mean.Hours.24H$DateTime, col="blue")
lines(test.mean.Hours.48H$pred.AmbTemp.lm~test.mean.Hours.48H$DateTime, col="green")
legend("topleft", 
       c("Measured","Predicted +24H", "Predicted +48H"), # puts text in the legend 
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5, 2.5, 2.5),col=c("black", "blue", "green"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1.5)
dev.off()

png("AmbTemp24&48 MeasuredVSPredicted.png", width=1300, height=650)
plot(test.mean.Hours.24H$AmbTemp~test.mean.Hours.48H$AmbTemp, type="l",  #verify that AmbTemp in both time-horizons is the same
     main="AmbTemp. Predicted vs Measured", ylab="AmbTemp. Measured [C]", xlab="AmbTemp. Predicted [C]")
#points(test.mean.Hours.24H$pred.AmbTemp24.lm~test.mean.Hours.24H$AmbTemp, col="blue")
abline(model.fitAmbTemp24.lm, col="blue")
#points(test.mean.Hours.48H$pred.AmbTemp.lm~test.mean.Hours.48H$AmbTemp, col="green")
abline(model.AmbTemp48.lm, col="green")
legend("topleft", 
       c("Measured","Predicted +24H", "Predicted +48H"), # puts text in the legend 
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5, 2.5, 2.5),col=c("black", "blue", "green"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1.5)
dev.off()
#/Plotting AmbTemp


#Plotting WindSpeed

png("WindSpeed24&48.png", width=1300, height=650)
plot(test.mean.Hours.24H$WindSpeed~test.mean.Hours.24H$DateTime, cex.axis=1.5, cex.lab=1.7, bg='black', pch=21, cex=1.5,
     type="l",main="Measured and Predicted values of the Wind Speed +24H and +48H", xlab="Time", 
     ylab="Wind Speed [m/s]")
lines(test.mean.Hours.24H$pred.WindSpeed24.cubist~test.mean.Hours.24H$DateTime, col="blue")
lines(test.mean.Hours.48H$pred.WindSpeed.cubist~test.mean.Hours.48H$DateTime, col="green")
legend("topleft", 
       c("Measured","Predicted +24H", "Predicted +48H"), # puts text in the legend 
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5, 2.5, 2.5),col=c("black", "blue", "green"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1.5)
dev.off()

#Save to png file WindSpeed +24H and +48H MeasuredVSPredicted
png("WindSpeed24&48 MeasuredVSPredicted.png", width=1300, height=650)
par(bg="gray92")
plot(test.mean.Hours.24H$WindSpeed~test.mean.Hours.48H$WindSpeed, type="l",  #verify that measured WindSpeed in both time-horizons is the same
     main="WindSpeed. Predicted vs Measured", ylab="WindSpeed. Measured [m/s]", xlab="WindSpeed. Predicted [m/s]")
points(test.mean.Hours.24H$pred.WindSpeed.cubist~test.mean.Hours.24H$WindSpeed, col="blue")
points(test.mean.Hours.48H$pred.WindSpeed.cubist~test.mean.Hours.48H$WindSpeed, col="green")
legend("topleft", 
       c("Measured","Predicted +24H", "Predicted +48H"), # puts text in the legend 
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5, 2.5, 2.5),col=c("black", "blue", "green"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1.5)
dev.off()
#/Plotting WindSpeed

#Preparing the dataframes for building the plots for the rest of the parameters

#Current
png("Current24&48.png", width=1300, height=650)
plot(test.mean.Hours.24H$Current~test.mean.Hours.24H$DateTime, cex.axis=1.5, cex.lab=1.7, bg='black', pch=21, cex=1.5,
     main="Measured and Predicted values of the Current +24H and +48H", xlab="Time", ylab="Current [A]", type="l")
lines(test.mean.Hours.24H$pred.Current.svmLinear~test.mean.Hours.24H$DateTime, col="blue")
lines(test.mean.Hours.48H$pred.Current.brnn~test.mean.Hours.48H$DateTime, col="green")
legend("topleft", 
       c("Measured","Predicted +24H", "Predicted +48H"), # puts text in the legend 
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5, 2.5, 2.5),col=c("black", "blue", "green"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1.5)
dev.off()

#Save to png file Current +24H and +48H MeasuredVSPredicted
png("Current24&48 MeasuredVSPredicted.png", width=1300, height=650)
par(bg="gray92")
plot(test.mean.Hours.24H$Current~test.mean.Hours.48H$Current, type="l",  #verify that measured Current in both time-horizons is the same
     main="Current. Predicted vs Measured", ylab="Current. Measured [A]", xlab="Current. Predicted [A]")
points(test.mean.Hours.24H$pred.Current.svmLinear~test.mean.Hours.24H$Current, col="blue")
points(test.mean.Hours.48H$pred.Current.brnn~test.mean.Hours.48H$Current, col="green")
legend("topleft", 
       c("Measured","Predicted +24H", "Predicted +48H"), # puts text in the legend 
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5, 2.5, 2.5),col=c("black", "blue", "green"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1.5)
dev.off()

#CondTemp
png("CondTemp24&48.png", width=1300, height=650)
plot(test.mean.Hours.24H$CondTemp~test.mean.Hours.24H$DateTime, cex.axis=1.5, cex.lab=1.7, bg='black', pch=21, cex=1.5,
     type="l",main="Measured and Predicted values of the Conductor Temperature +24H and +48H", xlab="Time", 
     ylab="CondTemp [C]")
lines(test.mean.Hours.24H$pred.CondTemp.gam~test.mean.Hours.24H$DateTime, col="blue")
lines(test.mean.Hours.48H$pred.CondTemp.lm~test.mean.Hours.48H$DateTime, col="green")
legend("topleft", 
       c("Measured","Predicted +24H", "Predicted +48H"), # puts text in the legend 
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5, 2.5, 2.5),col=c("black", "blue", "green"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1.5)
dev.off()

#Save to png file CondTemp +24H and +48H MeasuredVSPredicted
png("CondTemp24&48 MeasuredVSPredicted.png", width=1300, height=650)
par(bg="gray92")
plot(test.mean.Hours.24H$CondTemp~test.mean.Hours.48H$CondTemp, type="l",  #verify that measured CondTemp in both time-horizons is the same
     main="CondTemp. Predicted vs Measured", ylab="CondTemp. Measured [C]", xlab="CondTemp. Predicted [C]")
points(test.mean.Hours.24H$pred.CondTemp.gam~test.mean.Hours.24H$CondTemp, col="blue")
points(test.mean.Hours.48H$pred.CondTemp.lm~test.mean.Hours.48H$CondTemp, col="green")
legend("topleft", 
       c("Measured","Predicted +24H", "Predicted +48H"), # puts text in the legend 
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5, 2.5, 2.5),col=c("black", "blue", "green"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1.5)
dev.off()


#NormalDLR
png("NormalDLR24&48.png", width=1300, height=650)
plot(test.mean.Hours.24H$NormalDLR~test.mean.Hours.24H$DateTime, cex.axis=1.5, cex.lab=1.7, bg='black', pch=21, cex=1.5,
     main="Measured and Predicted values of the NormalDLR +24H and +48H", xlab="Time", ylab="NormalDLR [A]", 
     type="l")
lines(test.mean.Hours.24H$pred.NormalDLR.brnn~test.mean.Hours.24H$DateTime, col="blue")
lines(test.mean.Hours.48H$pred.NormalDLR.svmLinear~test.mean.Hours.48H$DateTime, col="green")
legend("topleft", 
       c("Measured","Predicted +24H", "Predicted +48H"), # puts text in the legend 
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5, 2.5, 2.5),col=c("black", "blue", "green"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1.5)
dev.off()

#Save to png file NormalDLR +24H and +48H MeasuredVSPredicted
png("NormalDLR24&48 MeasuredVSPredicted.png", width=1300, height=650)
par(bg="gray92")
plot(test.mean.Hours.24H$NormalDLR~test.mean.Hours.48H$NormalDLR, type="l",  #verify that measured NormalDLR in both time-horizons is the same
     main="NormalDLR. Predicted vs Measured", ylab="NormalDLR. Measured [A]", xlab="NormalDLR. Predicted [A]")
points(test.mean.Hours.24H$pred.NormalDLR.brnn~test.mean.Hours.24H$NormalDLR, col="blue")
points(test.mean.Hours.48H$pred.NormalDLR.svmLinear~test.mean.Hours.48H$NormalDLR, col="green")
legend("topleft", 
       c("Measured","Predicted +24H", "Predicted +48H"), # puts text in the legend 
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5, 2.5, 2.5),col=c("black", "blue", "green"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1.5)
dev.off()