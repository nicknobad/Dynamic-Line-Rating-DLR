### KTH, Department of Industrial Information and Control Systems (ICS)
### Master Thesis Project
### BUILDING PREDICTIVE MODELS FOR DYNAMIC LINE RATING USING DATA SCIENCE TECHNIQUES 
### Sponsored by Vattenfall AG
### Author: Nicolae Doban
### Stockholm, 2015

## This module combines the past forecasted dataset of Ambient Temperature and Wind Speed with the 
## "mean.Hours" dataset and finally, builds the predictive models for the +24H time-horizon
## Packages used: base, histogram, reshape, zoo, splines, caret, boot, DAAG, 
## INPUT DATA: The minute-based dataset and the past forecasted ambient temperature and wind datasets (AirTempP.csv, WindP.csv)
## OUTPUT: Building, training and testing the predictive models for the +48H time-horizon

##The order of building the predictive models: Ambient Temperature (AmbTemp), WindSpeed, Current, Conductor Temperature (CondTemp), NormalDLR
##The next predictive models are built on the previous predicted parameteres except the AmbTemp and WindSpeed
##The same approach and logic is applied when building the predictive models for the +24H time-horizon, in  "24H.R"
##The comments from "24H.R" are applicable in this script

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

setwd("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/") #making sure we are in the correct directory

#AirTemp
PAirTemp<-read.csv("AirTempP.csv", sep=";", header=TRUE, skip=5100)
#PAirTemp<-head(PAirTemp, 3680)
colnames(PAirTemp)[1]<-"DateTimeLEN"  #Renaming AirTemp columns
colnames(PAirTemp)[2]<-"latitud"
colnames(PAirTemp)[3]<-"longitud"
colnames(PAirTemp)[4]<-"AmbTempK"      #/Renaming AirTemp columns
PAirTemp$AmbTempC<-PAirTemp$AmbTempK-273.15 #Convert AirTemp from Kelvin to Celsius

PAirTemp$LEN<-rep(c(24, 30, 36, 42, 48), 800) #new column to extract the +48H and +48H later on

#+48H
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
setwd("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/") #making sure we are in the correct directory
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
#mean.Hours.48H$PWindSpeed48H<-mean.Hours.48H$PWindSpeed48H

plot(mean.Hours.48H$WindSpeed~mean.Hours.48H$PWindSpeed48H, main="Wind Speed. Prognosed vs Measured. 48H", 
     xlab="WindSpeed.Prognosed [m/s]", ylab="WindSpeed.Measured [m/s]")

train.mean.Hours.48H<-head(mean.Hours.48H, 668);   rownames(train.mean.Hours.48H)<-NULL
test.mean.Hours.48H<-mean.Hours.48H[c(669:792),];  rownames(test.mean.Hours.48H)<-NULL

model.AmbTemp48.lm<-lm(AmbTemp~PAmbTemp48H+I(PAmbTemp48H^2)+I(PAmbTemp48H^3),
                          data=train.mean.Hours.48H); accuracy(f=model.AmbTemp48.lm)
save(model.AmbTemp.lm, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                       'model.AmbTemp48.lm.model',sep=''))

train.mean.Hours.48H$pred.AmbTemp.lm<-predict(model.AmbTemp48.lm, train.mean.Hours.48H)
train.mean.Hours.48H$err.AmbTemp.lm<-abs(100*(train.mean.Hours.48H$AmbTemp-train.mean.Hours.48H$pred.AmbTemp.lm)/
                                        train.mean.Hours.48H$AmbTemp)

test.mean.Hours.48H$pred.AmbTemp.lm<-predict(model.AmbTemp48.lm, test.mean.Hours.48H)
test.mean.Hours.48H$err.AmbTemp<-abs(100*(test.mean.Hours.48H$AmbTemp-test.mean.Hours.48H$pred.AmbTemp.lm)/
                                       test.mean.Hours.48H$AmbTemp)

plot(test.mean.Hours.48H$AmbTemp~test.mean.Hours.48H$pred.AmbTemp.lm, main="AmbTemp. Measured vs Predicted",
     ylab="Measured AmbTemp [C]", xlab="Predicted AmbTemp [C]")
setwd("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels")
png("AmbTempTest.png", width=1300, height=650)
plot(test.mean.Hours.48H$AmbTemp~test.mean.Hours.48H$DateTime, main="AmbTemp. Measured and Predicted",
     ylab="AmbTemp [C]", xlab="Time", cex.axis=1.5, cex.lab=1.7)
lines(test.mean.Hours.48H$AmbTemp~test.mean.Hours.48H$DateTime, col="green")
lines(test.mean.Hours.48H$pred.AmbTemp.lm~test.mean.Hours.48H$DateTime, col="red")
legend("topleft", 
       c("Measured","Predicted"), # puts text in the legend 
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5, 2.5),col=c("green", "red"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1)
dev.off()

train<-train.mean.Hours.48H[,c("PDateTime48H", "PWindSpeed48H", "Hours", "Days", "daytype", "WindSpeed")]

plot(train.mean.Hours.48H$PWindSpeed48H~train.mean.Hours.48H$WindSpeed,  main="Wind Speed. Train DataSet. Prognosed vs Measured. 48H", 
     xlab="WindSpeed.Prognosed [m/s]", ylab="WindSpeed.Measured [m/s]")

model.WindSpeed.lm<-lm(WindSpeed~poly(PWindSpeed48H,2)+Hours+Days+daytype,
                            data=train)

train.mean.Hours.48H$pred.WindSpeed.lm<-predict(model.WindSpeed.lm, train.mean.Hours.48H)
train.mean.Hours.48H$err.WindSpeed.lm<-abs(100*(train.mean.Hours.48H$WindSpeed-train.mean.Hours.48H$pred.WindSpeed.lm)/
                                             train.mean.Hours.48H$WindSpeed)

summary(train.mean.Hours.48H); summary(model.WindSpeed.lm)

#bagEarthGCV
train_control.bagEarthGCV <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
bagEarthGCV.Grid<-expand.grid(degree=100)

time.WindSpeed.bagEarthGCV <- proc.time()
model.WindSpeed.bagEarthGCV <- train(WindSpeed~.,
                                     data=train, trControl=train_control.bagEarthGCV,
                                     method="bagEarthGCV", tuneGrid = bagEarthGCV.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.WindSpeed.bagEarthGCV

train.mean.Hours.48H$pred.WindSpeed.bagEarthGCV<-predict(model.WindSpeed.bagEarthGCV, train[,1:5])
#record the errors
train.mean.Hours.48H$err.WindSpeed.bagEarthGCV<-abs(100*(train.mean.Hours.48H$WindSpeed-train.mean.Hours.48H$pred.WindSpeed.bagEarthGCV)/
                                                      train.mean.Hours.48H$WindSpeed)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.WindSpeed.bagEarthGCV)
#show error metric
errMetric.bagEarthGCV<-mmetric(y=train.mean.Hours.48H$WindSpeed, x = train.mean.Hours.48H$pred.WindSpeed.bagEarthGCV, 
                               metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                         "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.bagEarthGCV)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$WindSpeed, x = train.mean.Hours.48H$pred.WindSpeed.bagEarthGCV, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$WindSpeed~train.mean.Hours.48H$pred.WindSpeed.bagEarthGCV, 
     main="WindSpeed. Predicted vs Measured. bagEarthGCV", xlab="Predicted WindSpeed [m/s]", ylab="Measured WindSpeed [m/s]")
#/bagEarthGCV

#svmLinear
train_control.svmLinear <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 

time.WindSpeed.svmLinear <- proc.time()
model.WindSpeed.svmLinear <- train(WindSpeed~.,
                                   data=train,trControl=train_control.svmLinear,
                                   method="svmLinear", tuneLength = 8) #tuneLength does not matter
proc.time() - time.WindSpeed.svmLinear

train.mean.Hours.48H$pred.WindSpeed.svmLinear<-predict(model.WindSpeed.svmLinear, train[,1:5])
#record the errors
train.mean.Hours.48H$err.WindSpeed.svmLinear<-abs(100*(train.mean.Hours.48H$WindSpeed-train.mean.Hours.48H$pred.WindSpeed.svmLinear)/
                                                    train.mean.Hours.48H$WindSpeed)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.WindSpeed.svmLinear)
#show error metric
errMetric.svmLinear<-mmetric(y=train.mean.Hours.48H$WindSpeed, x = train.mean.Hours.48H$pred.WindSpeed.svmLinear, 
                             metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                       "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.svmLinear)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$WindSpeed, x = train.mean.Hours.48H$pred.WindSpeed.svmLinear, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$WindSpeed~train.mean.Hours.48H$pred.WindSpeed.svmLinear, 
     main="WindSpeed. Predicted vs Measured. svmLinear", xlab="Predicted WindSpeed [m/s]", ylab="Measured WindSpeed [m/s]")
#/svmLinear

#gcvEarth
train_control.gcvEarth <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
gcvEarth.Grid<-expand.grid(degree=10)
time.WindSpeed.gcvEarth <- proc.time()
model.WindSpeed.gcvEarth <- train(WindSpeed~.,
                                  data=train, trControl=train_control.gcvEarth,
                                  method="gcvEarth", tuneLength = 8, tuneGrid=gcvEarth.Grid) #tuneLength does not matter
proc.time() - time.WindSpeed.gcvEarth

train.mean.Hours.48H$pred.WindSpeed.gcvEarth<-predict(model.WindSpeed.gcvEarth, train[,1:5])
#record the errors
train.mean.Hours.48H$err.WindSpeed.gcvEarth<-abs(100*(train.mean.Hours.48H$WindSpeed-train.mean.Hours.48H$pred.WindSpeed.gcvEarth)/
                                                   train.mean.Hours.48H$WindSpeed)
#show error metric
errMetric.gcvEarth<-mmetric(y=train.mean.Hours.48H$WindSpeed, x = train.mean.Hours.48H$pred.WindSpeed.gcvEarth, 
                            metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                      "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.gcvEarth)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$WindSpeed, x = train.mean.Hours.48H$pred.WindSpeed.gcvEarth, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$WindSpeed~train.mean.Hours.48H$pred.WindSpeed.gcvEarth, 
     main="WindSpeed. Predicted vs Measured. gcvEarth", xlab="Predicted WindSpeed [m/s]", ylab="Measured WindSpeed [m/s]")
summary(train.mean.Hours.48H)
#/gcvEarth

#cubist
train_control.cubist <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
cubist.Grid<-expand.grid(committees = 1, neighbors = 1) #highest accuracy for committees = 1, neighbors = 1
time.WindSpeed.cubist <- proc.time()
model.WindSpeed.cubist <- train(WindSpeed~.,
                                data=train, trControl=train_control.cubist,
                                method="cubist", tuneLength = 8, tuneGrid=cubist.Grid) #tuneLength does not matter
proc.time() - time.WindSpeed.cubist

train.mean.Hours.48H$pred.WindSpeed.cubist<-predict(model.WindSpeed.cubist, train[,1:5])
#record the errors
train.mean.Hours.48H$err.WindSpeed.cubist<-abs(100*(train.mean.Hours.48H$WindSpeed-train.mean.Hours.48H$pred.WindSpeed.cubist)/
                                                 train.mean.Hours.48H$WindSpeed)
#show error metric
errMetric.cubist<-mmetric(y=train.mean.Hours.48H$WindSpeed, x = train.mean.Hours.48H$pred.WindSpeed.cubist, 
                          metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                    "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.cubist)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$WindSpeed, x = train.mean.Hours.48H$pred.WindSpeed.cubist, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$WindSpeed~train.mean.Hours.48H$pred.WindSpeed.cubist, 
     main="WindSpeed. Predicted vs Measured. cubist", xlab="Predicted WindSpeed [m/s]", ylab="Measured WindSpeed [m/s]")
summary(train.mean.Hours.48H)
#/cubist

#gam
train_control.gam <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
gam.Grid<-expand.grid(select = TRUE, method = "GCV.Cp")
time.WindSpeed.gam <- proc.time()
model.WindSpeed.gam <- train(WindSpeed~.,
                             data=train, trControl=train_control.gam,
                             method="gam", tuneLength = 8, tuneGrid=gam.Grid) #tuneLength does not matter
proc.time() - time.WindSpeed.gam

train.mean.Hours.48H$pred.WindSpeed.gam<-predict(model.WindSpeed.gam, train[,1:5])
#record the errors
train.mean.Hours.48H$err.WindSpeed.gam<-abs(100*(train.mean.Hours.48H$WindSpeed-train.mean.Hours.48H$pred.WindSpeed.gam)/
                                              train.mean.Hours.48H$WindSpeed)
#show error metric
errMetric.gam<-mmetric(y=train.mean.Hours.48H$WindSpeed, x = train.mean.Hours.48H$pred.WindSpeed.gam, 
                       metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                 "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.gam)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$WindSpeed, x = train.mean.Hours.48H$pred.WindSpeed.gam, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$WindSpeed~train.mean.Hours.48H$pred.WindSpeed.gam, 
     main="WindSpeed. Predicted vs Measured. gam", xlab="Predicted WindSpeed [m/s]", ylab="Measured WindSpeed [m/s]")
summary(train.mean.Hours.48H)
#/gam

#bagEarth
train_control.bagEarth <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
bagEarth.Grid<-expand.grid(degree=10, nprune=14)
time.WindSpeed.bagEarth <- proc.time()
model.WindSpeed.bagEarth <- train(WindSpeed~.,
                                  data=train, trControl=train_control.bagEarth,
                                  method="bagEarth", tuneLength = 8, tuneGrid=bagEarth.Grid) #tuneLength does not matter
proc.time() - time.WindSpeed.bagEarth

train.mean.Hours.48H$pred.WindSpeed.bagEarth<-predict(model.WindSpeed.bagEarth, train[,1:5])
#record the errors
train.mean.Hours.48H$err.WindSpeed.bagEarth<-abs(100*(train.mean.Hours.48H$WindSpeed-train.mean.Hours.48H$pred.WindSpeed.bagEarth)/
                                                   train.mean.Hours.48H$WindSpeed)
#show error metric
errMetric.bagEarth<-mmetric(y=train.mean.Hours.48H$WindSpeed, x = train.mean.Hours.48H$pred.WindSpeed.bagEarth, 
                            metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                      "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.bagEarth)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$WindSpeed, x = train.mean.Hours.48H$pred.WindSpeed.bagEarth, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$WindSpeed~train.mean.Hours.48H$pred.WindSpeed.bagEarth, 
     main="WindSpeed. Predicted vs Measured. bagEarth", xlab="Predicted WindSpeed [m/s]", ylab="Measured WindSpeed [m/s]")
summary(train.mean.Hours.48H)
#/bagEarth

#brnn
train_control.brnn <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
brnn.Grid<-expand.grid(neurons = 20)
time.WindSpeed.brnn <- proc.time()
model.WindSpeed.brnn <- train(WindSpeed~.,
                              data=train, trControl=train_control.brnn,
                              method="brnn", tuneLength = 8, tuneGrid=brnn.Grid) #tuneLength does not matter
proc.time() - time.WindSpeed.brnn

train.mean.Hours.48H$pred.WindSpeed.brnn<-predict(model.WindSpeed.brnn, train[,1:5])
#record the errors
train.mean.Hours.48H$err.WindSpeed.brnn<-abs(100*(train.mean.Hours.48H$WindSpeed-train.mean.Hours.48H$pred.WindSpeed.brnn)/
                                               train.mean.Hours.48H$WindSpeed)
#show error metric
errMetric.brnn<-mmetric(y=train.mean.Hours.48H$WindSpeed, x = train.mean.Hours.48H$pred.WindSpeed.brnn, 
                        metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                  "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.brnn)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$WindSpeed, x = train.mean.Hours.48H$pred.WindSpeed.brnn, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$WindSpeed~train.mean.Hours.48H$pred.WindSpeed.brnn, 
     main="WindSpeed. Predicted vs Measured. brnn", xlab="Predicted WindSpeed [m/s]", ylab="Measured WindSpeed [m/s]")
summary(train.mean.Hours.48H)
#/brnn

save(model.WindSpeed.cubist, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                        'model.WindSpeed.cubist.model',sep=''))
save(model.WindSpeed.gcvEarth, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                          'model.WindSpeed.gcvEarth.model',sep=''))
save(model.WindSpeed.bagEarthGCV, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                             'model.WindSpeed.bagEarthGCV.model',sep=''))
save(model.WindSpeed.bagEarth, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                          'model.WindSpeed.bagEarth.model',sep=''))
save(model.WindSpeed.svmLinear, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                           'model.WindSpeed.svmLinear.model',sep=''))
save(model.WindSpeed.brnn, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                      'model.WindSpeed.brnn.model',sep=''))
save(model.WindSpeed.lm, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                         'model.WindSpeed.lm.model',sep=''))
save(model.WindSpeed.gam, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                     'model.WindSpeed.gam.model',sep=''))

#testing the models

test.mean.Hours.48H$pred.WindSpeed.lm<-predict(model.WindSpeed.lm, test.mean.Hours.48H)
test.mean.Hours.48H$err.WindSpeed.lm<-abs(100*(test.mean.Hours.48H$WindSpeed-test.mean.Hours.48H$pred.WindSpeed.lm)/
                                            test.mean.Hours.48H$WindSpeed)
test.mean.Hours.48H$pred.WindSpeed.cubist<-predict(model.WindSpeed.cubist, test.mean.Hours.48H)
test.mean.Hours.48H$err.WindSpeed.cubist<-abs(100*(test.mean.Hours.48H$WindSpeed-test.mean.Hours.48H$pred.WindSpeed.cubist)/
                                                test.mean.Hours.48H$WindSpeed)
test.mean.Hours.48H$pred.WindSpeed.gcvEarth<-predict(model.WindSpeed.gcvEarth, test.mean.Hours.48H)
test.mean.Hours.48H$err.WindSpeed.gcvEarth<-abs(100*(test.mean.Hours.48H$WindSpeed-test.mean.Hours.48H$pred.WindSpeed.gcvEarth)/
                                                  test.mean.Hours.48H$WindSpeed)
test.mean.Hours.48H$pred.WindSpeed.bagEarthGCV<-predict(model.WindSpeed.bagEarthGCV, test.mean.Hours.48H)
test.mean.Hours.48H$err.WindSpeed.bagEarthGCV<-abs(100*(test.mean.Hours.48H$WindSpeed-test.mean.Hours.48H$pred.WindSpeed.bagEarthGCV)/
                                                     test.mean.Hours.48H$WindSpeed)
test.mean.Hours.48H$pred.WindSpeed.bagEarth<-predict(model.WindSpeed.bagEarth, test.mean.Hours.48H)
test.mean.Hours.48H$err.WindSpeed.bagEarth<-abs(100*(test.mean.Hours.48H$WindSpeed-test.mean.Hours.48H$pred.WindSpeed.bagEarth)/
                                                  test.mean.Hours.48H$WindSpeed)
test.mean.Hours.48H$pred.WindSpeed.svmLinear<-predict(model.WindSpeed.svmLinear, test.mean.Hours.48H)
test.mean.Hours.48H$err.WindSpeed.svmLinear<-abs(100*(test.mean.Hours.48H$WindSpeed-test.mean.Hours.48H$pred.WindSpeed.svmLinear)/
                                                   test.mean.Hours.48H$WindSpeed)
test.mean.Hours.48H$pred.WindSpeed.brnn<-predict(model.WindSpeed.brnn, test.mean.Hours.48H)
test.mean.Hours.48H$err.WindSpeed.brnn<-abs(100*(test.mean.Hours.48H$WindSpeed-test.mean.Hours.48H$pred.WindSpeed.brnn)/
                                              test.mean.Hours.48H$WindSpeed)
test.mean.Hours.48H$pred.WindSpeed.gam<-predict(model.WindSpeed.gam, test.mean.Hours.48H)
test.mean.Hours.48H$err.WindSpeed.gam<-abs(100*(test.mean.Hours.48H$WindSpeed-test.mean.Hours.48H$pred.WindSpeed.gam)/
                                             test.mean.Hours.48H$WindSpeed)


#REC curves
#First REC graph
L=vector("list",4); pred=vector("list",1); test=vector("list",1);

pred[[1]]=test.mean.Hours.48H$WindSpeed; 
test[[1]]=test.mean.Hours.48H$pred.WindSpeed.lm;          L[[1]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.WindSpeed.cubist;      L[[2]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.WindSpeed.bagEarthGCV; L[[3]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.WindSpeed.bagEarth;    L[[4]]=list(pred=pred,test=test,runs=1)

WindSpeed.lm.NAREC = mmetric(y=test.mean.Hours.48H$WindSpeed, x = test.mean.Hours.48H$pred.WindSpeed.lm, 
                             metric= "NAREC", aggregate="mean", val=5)
WindSpeed.lm.NAREC.curve = paste("REC, Linear model=", round(WindSpeed.lm.NAREC, digits=4))

WindSpeed.cubist.NAREC = mmetric(y=test.mean.Hours.48H$WindSpeed, x = test.mean.Hours.48H$pred.WindSpeed.cubist, 
                                 metric= "NAREC", aggregate="mean", val=5)
WindSpeed.cubist.NAREC.curve = paste("REC, cubist=", round(WindSpeed.cubist.NAREC, digits=4))

WindSpeed.bagEarthGCV.NAREC = mmetric(y=test.mean.Hours.48H$WindSpeed, x = test.mean.Hours.48H$pred.WindSpeed.bagEarthGCV, 
                                      metric= "NAREC", aggregate="mean", val=5)
WindSpeed.bagEarthGCV.NAREC.curve = paste("REC, bagEarthGCV=", round(WindSpeed.bagEarthGCV.NAREC, digits=4))

WindSpeed.bagEarth.NAREC = mmetric(y=test.mean.Hours.48H$WindSpeed, x = test.mean.Hours.48H$pred.WindSpeed.bagEarth, 
                                   metric= "NAREC", aggregate="mean", val=5)
WindSpeed.bagEarth.NAREC.curve = paste("REC, bagEarth=", round(WindSpeed.bagEarth.NAREC, digits=4))

#Second REC graph
B=vector("list",4); pred=vector("list",1); test=vector("list",1);
pred[[1]]=test.mean.Hours.48H$WindSpeed; 

test[[1]]=test.mean.Hours.48H$pred.WindSpeed.gcvEarth;  B[[1]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.WindSpeed.svmLinear; B[[2]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.WindSpeed.brnn;      B[[3]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.WindSpeed.gam;       B[[4]]=list(pred=pred,test=test,runs=1)


WindSpeed.gcvEarth.NAREC = mmetric(y=test.mean.Hours.48H$WindSpeed, x = test.mean.Hours.48H$pred.WindSpeed.gcvEarth, 
                                   metric= "NAREC", aggregate="mean", val=5)
WindSpeed.gcvEarth.NAREC.curve = paste("REC, gcvEarth=", round(WindSpeed.gcvEarth.NAREC, digits=4))

WindSpeed.svmLinear.NAREC = mmetric(y=test.mean.Hours.48H$WindSpeed, x = test.mean.Hours.48H$pred.WindSpeed.svmLinear, 
                                    metric= "NAREC", aggregate="mean", val=5)
WindSpeed.svmLinear.NAREC.curve = paste("REC, svmLinear=", round(WindSpeed.svmLinear.NAREC, digits=4))

WindSpeed.brnn.NAREC = mmetric(y=test.mean.Hours.48H$WindSpeed, x = test.mean.Hours.48H$pred.WindSpeed.brnn, 
                               metric= "NAREC", aggregate="mean", val=5)
WindSpeed.brnn.NAREC.curve = paste("REC, brnn=", round(WindSpeed.brnn.NAREC, digits=4))

WindSpeed.gam.NAREC = mmetric(y=test.mean.Hours.48H$WindSpeed, x = test.mean.Hours.48H$pred.WindSpeed.gam, 
                              metric= "NAREC", aggregate="mean", val=5)
WindSpeed.gam.NAREC.curve = paste("REC, gam=", round(WindSpeed.gam.NAREC, digits=4))


#save to png            #png looks nicer than the pdf
png("WindSpeed. REC-curves.png", width=1110, height=550)
split.screen(c(1,2))
screen(1)
L.graph<-mgraph(L, size=c(1,10), graph="REC",
                leg=list(pos="bottomright",leg=c(WindSpeed.bagEarthGCV.NAREC.curve,
                                                 WindSpeed.cubist.NAREC.curve,
                                                 WindSpeed.lm.NAREC.curve,
                                                 WindSpeed.bagEarth.NAREC.curve)),
                col=c("blue", "gray1", "green", "red"),
                main="WindSpeed. REC curves")

screen(2)
B.graph<-mgraph(B, size=c(1,10), graph="REC",
                leg=list(pos="bottomright",leg=c(WindSpeed.svmLinear.NAREC.curve,
                                                 WindSpeed.gcvEarth.NAREC.curve,
                                                 WindSpeed.brnn.NAREC.curve,
                                                 WindSpeed.gam.NAREC.curve)),
                col=c("red", "gray1", "green", "blue"),
                main="WindSpeed. REC curves")

close.screen(all=TRUE)
dev.off()


errMetric.WindSpeed.brnn<-mmetric(y=test.mean.Hours.48H$WindSpeed, x = test.mean.Hours.48H$pred.WindSpeed.brnn, 
                                  metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                            "MAPE", "MdAPE"), aggregate="mean")
errMetric.WindSpeed.bagEarth<-mmetric(y=test.mean.Hours.48H$WindSpeed, x = test.mean.Hours.48H$pred.WindSpeed.bagEarth, 
                                      metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                                "MAPE", "MdAPE"), aggregate="mean")
errMetric.WindSpeed.bagEarthGCV<-mmetric(y=test.mean.Hours.48H$WindSpeed, x = test.mean.Hours.48H$pred.WindSpeed.bagEarthGCV, 
                                         metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                                   "MAPE", "MdAPE"), aggregate="mean")
errMetric.WindSpeed.cubist<-mmetric(y=test.mean.Hours.48H$WindSpeed, x = test.mean.Hours.48H$pred.WindSpeed.cubist, 
                                    metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                              "MAPE", "MdAPE"), aggregate="mean")
errMetric.WindSpeed.lm<-mmetric(y=test.mean.Hours.48H$WindSpeed, x = test.mean.Hours.48H$pred.WindSpeed.lm, 
                                metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                          "MAPE", "MdAPE"), aggregate="mean")
errMetric.WindSpeed.gcvEarth<-mmetric(y=test.mean.Hours.48H$WindSpeed, x = test.mean.Hours.48H$pred.WindSpeed.gcvEarth, 
                                      metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                                "MAPE", "MdAPE"), aggregate="mean")
errMetric.WindSpeed.svmLinear<-mmetric(y=test.mean.Hours.48H$WindSpeed, x = test.mean.Hours.48H$pred.WindSpeed.svmLinear, 
                                       metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                                 "MAPE", "MdAPE"), aggregate="mean")
errMetric.WindSpeed.gam<-mmetric(y=test.mean.Hours.48H$WindSpeed, x = test.mean.Hours.48H$pred.WindSpeed.gam, 
                                 metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                           "MAPE", "MdAPE"), aggregate="mean")

errMatrix.WindSpeed<-data.frame(errMetric.WindSpeed.bagEarth,errMetric.WindSpeed.brnn,errMetric.WindSpeed.cubist,
                                errMetric.WindSpeed.lm,errMetric.WindSpeed.gcvEarth,errMetric.WindSpeed.svmLinear,
                                errMetric.WindSpeed.bagEarthGCV, errMetric.WindSpeed.gam)
errMatrix.WindSpeed<-t(errMatrix.WindSpeed)
#write.table(errMatrix.WindSpeed,"errorMatrixWindSpeed.csv", sep=",")

errMatrix.WindSpeed<-errMatrix.WindSpeed[,c(7,9,10,1,2,3,4,5,6,8,11)]
errMatrix.WindSpeed.Ordered<-errMatrix.WindSpeed[ order(-errMatrix.WindSpeed[,1], -errMatrix.WindSpeed[,2], 
                                                        errMatrix.WindSpeed[,3]),]
View(errMatrix.WindSpeed.Ordered)

#/REC curves
#/testing the models

png("WindSpeedTest.png", width=1300, height=650)
plot(test.mean.Hours.48H$WindSpeed~test.mean.Hours.48H$DateTime, col="black", pch=21, type="l",
     main="Wind Speed. Predicted and Measured", ylab="WindSpeed [m/s]", xlab="Time", cex.axis=1.5, cex.lab=1.7,)
lines(test.mean.Hours.48H$WindSpeed~test.mean.Hours.48H$DateTime, bg='black', pch=21, cex=1.5)
lines(test.mean.Hours.48H$pred.WindSpeed.bagEarthGCV~test.mean.Hours.48H$DateTime,             col="blue")
lines(test.mean.Hours.48H$pred.WindSpeed.cubist~test.mean.Hours.48H$DateTime,         col="red")
lines(test.mean.Hours.48H$pred.WindSpeed.bagEarth~test.mean.Hours.48H$DateTime,            col="green")
lines(test.mean.Hours.48H$pred.WindSpeed.svmLinear~test.mean.Hours.48H$DateTime,      col="deepskyblue3")
legend("topleft", 
       c("Measured","Predicted. bagEarthGCV", "Predicted. cubist", "Predicted. bagEarth", "Predicted. svmLinear"), # puts text in the legend 
       lty=c(1,1,1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5,2.5, 2.5, 2.5),col=c("black","blue", "red", "green", "deepskyblue3"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1)
dev.off()

#Predicted versus the measured
WindSpeed<-test.mean.Hours.48H$WindSpeed #Store the measured WindSpeed as a vector
png("WindSpeedPredictedVSMeasured.png", width=1300, height=650)
par(bg="gray85") #Changing the background color of the canvas to have a greater contrast
plot(test.mean.Hours.48H$WindSpeed~WindSpeed, col="black", pch=21, type="l",
     main="Wind Speed. Predicted VS Measured", xlab="Measured WindSpeed [m/s]", ylab="Predicted WindSpeed [m/s]", cex.axis=1.5, cex.lab=1.7)
points(test.mean.Hours.48H$pred.WindSpeed.bagEarthGCV~WindSpeed,        col="blue", cex=1.5)
points(test.mean.Hours.48H$pred.WindSpeed.cubist~WindSpeed,             col="red", cex=1.5)
points(test.mean.Hours.48H$pred.WindSpeed.bagEarth~WindSpeed,           col="green", cex=1.5)
points(test.mean.Hours.48H$pred.WindSpeed.svmLinear~WindSpeed,          col="yellow", cex=1.5)
legend("topleft", 
       c("Measured","Predicted. bagEarthGCV", "Predicted. cubist", "Predicted. bagEarth", "Predicted. svmLinear"), # puts text in the legend 
       lty=c(1,1,1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5,2.5, 2.5, 2.5),col=c("black","blue", "red", "green", "yellow"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1)
dev.off()
#/Predicted versus the measured 

# The WindSpeed will be predicted with Cubist model

#Work with predicted AmbTemp and WindSpeed

#Preparing the training and test datasets for predicting the next paramters
train.mean.Hours.48H<-head(mean.Hours.48H, 668);   rownames(train.mean.Hours.48H)<-NULL
load("C:/Users/LabStudent/Desktop/Nick/WeatherDataComparison/48H/PredModels/WindSpeed/model.AmbTemp.lm.model")
train.mean.Hours.48H$pred.AmbTemp.lm<-predict(model.AmbTemp48.lm, train.mean.Hours.48H)
load("C:/Users/LabStudent/Desktop/Nick/WeatherDataComparison/48H/PredModels/WindSpeed/model.WindSpeed.cubist.model")
train.mean.Hours.48H$pred.WindSpeed.cubist<-predict(model.WindSpeed.cubist, train.mean.Hours.48H)

train.mean.Hours.48H<-train.mean.Hours.48H[c("PDateTime48H", "PAmbTemp48H", "DateTime", "AmbTemp", 
                                             "pred.AmbTemp.lm", "PWindSpeed48H","WindSpeed", 
                                             "pred.WindSpeed.cubist","Hours", "Days", "daytype", "Current", 
                                             "CondTemp", "NormalDLR")]
train<-train.mean.Hours.48H[,c("DateTime", "pred.AmbTemp.lm","pred.WindSpeed.cubist", "Hours", "Days", "daytype", "Current")]

test.mean.Hours.48H<-mean.Hours.48H[c(669:792),];  rownames(test.mean.Hours.48H)<-NULL
test.mean.Hours.48H$pred.AmbTemp.lm<-predict(model.AmbTemp48.lm, test.mean.Hours.48H)
test.mean.Hours.48H$pred.WindSpeed.cubist<-predict(model.WindSpeed.cubist, test.mean.Hours.48H)
test.mean.Hours.48H<-test.mean.Hours.48H[c("PDateTime48H", "PAmbTemp48H", "DateTime", "AmbTemp", "pred.AmbTemp.lm", 
                                           "PWindSpeed48H","WindSpeed", "pred.WindSpeed.cubist", "Hours", 
                                           "Days", "daytype", "Current", "CondTemp", "NormalDLR")]

#Predicting Current
#Linear model
time.Current.lm <- proc.time()
model.Current.lm=lm(Current~I(pred.WindSpeed.cubist^2)+I(pred.AmbTemp.lm^5)+DateTime+Days+
                      +I(Hours^1)*daytype*I(pred.WindSpeed.cubist^1),
                    data=train); accuracy(f=model.Current.lm) #MAPE 
proc.time() - time.Current.lm
#/Linear model

train.mean.Hours.48H$pred.Current.lm<-predict(model.Current.lm, train.mean.Hours.48H)
train.mean.Hours.48H$err.Current.lm<-abs(100*(train.mean.Hours.48H$Current-train.mean.Hours.48H$pred.Current.lm)
                                         /train.mean.Hours.48H$Current)

#svmLinear
train_control.svmLinear <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 

time.Current.svmLinear <- proc.time()
model.Current.svmLinear <- train(Current~.,
                                 data=train, trControl=train_control.svmLinear,
                                 method="svmLinear", tuneLength = 8) #tuneLength does not matter
proc.time() - time.Current.svmLinear

train.mean.Hours.48H$pred.Current.svmLinear<-predict(model.Current.svmLinear, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.Current.svmLinear<-abs(100*(train.mean.Hours.48H$Current-train.mean.Hours.48H$pred.Current.svmLinear)/
                                                  train.mean.Hours.48H$Current)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.Current.svmLinear)
#show error metric
errMetric.svmLinear<-mmetric(y=train.mean.Hours.48H$Current, x = train.mean.Hours.48H$pred.Current.svmLinear, 
                             metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                       "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.svmLinear)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$Current, x = train.mean.Hours.48H$pred.Current.svmLinear, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$Current~train.mean.Hours.48H$pred.Current.svmLinear, 
     main="Current. Predicted vs Measured. svmLinear", xlab="Predicted Current [A]", ylab="Measured Current [A]")
#/svmLinear

#cubist
train_control.cubist <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
cubist.Grid<-expand.grid(committees = 1, neighbors = 1) #highest accuracy for committees = 1, neighbors = 1

time.Current.cubist <- proc.time()
model.Current.cubist <- train(Current~.,
                              data=train, trControl=train_control.cubist,
                              method="cubist", tuneGrid = cubist.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.Current.cubist

train.mean.Hours.48H$pred.Current.cubist<-predict(model.Current.cubist, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.Current.cubist<-abs(100*(train.mean.Hours.48H$Current-train.mean.Hours.48H$pred.Current.cubist)/
                                               train.mean.Hours.48H$Current)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.Current.cubist)
#show error metric
errMetric.cubist<-mmetric(y=train.mean.Hours.48H$Current, x = train.mean.Hours.48H$pred.Current.cubist, 
                          metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                    "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.cubist)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$Current, x = train.mean.Hours.48H$pred.Current.cubist, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$Current~train.mean.Hours.48H$pred.Current.cubist, 
     main="Current. Predicted vs Measured. cubist", xlab="Predicted Current [A]", ylab="Measured Current [A]")
#/cubist

#bagEarthGCV
train_control.bagEarthGCV <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
bagEarthGCV.Grid<-expand.grid(degree=6)

time.Current.bagEarthGCV <- proc.time()
model.Current.bagEarthGCV <- train(Current~.,
                                   data=train, trControl=train_control.bagEarthGCV,
                                   method="bagEarthGCV", tuneGrid = bagEarthGCV.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.Current.bagEarthGCV

train.mean.Hours.48H$pred.Current.bagEarthGCV<-predict(model.Current.bagEarthGCV, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.Current.bagEarthGCV<-abs(100*(train.mean.Hours.48H$Current-train.mean.Hours.48H$pred.Current.bagEarthGCV)/
                                                    train.mean.Hours.48H$Current)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.Current.bagEarthGCV)
#show error metric
errMetric.bagEarthGCV<-mmetric(y=train.mean.Hours.48H$Current, x = train.mean.Hours.48H$pred.Current.bagEarthGCV, 
                               metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                         "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.bagEarthGCV)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$Current, x = train.mean.Hours.48H$pred.Current.bagEarthGCV, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$Current~train.mean.Hours.48H$pred.Current.bagEarthGCV, 
     main="Current. Predicted vs Measured. bagEarthGCV", xlab="Predicted Current [A]", ylab="Measured Current [A]")
#/bagEarthGCV

#gcvEarth
train_control.gcvEarth <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
gcvEarth.Grid<-expand.grid(degree=10)

time.Current.gcvEarth <- proc.time()
model.Current.gcvEarth <- train(Current~.,
                                data=train, trControl=train_control.gcvEarth,
                                method="gcvEarth", tuneGrid = gcvEarth.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.Current.gcvEarth

train.mean.Hours.48H$pred.Current.gcvEarth<-predict(model.Current.gcvEarth, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.Current.gcvEarth<-abs(100*(train.mean.Hours.48H$Current-train.mean.Hours.48H$pred.Current.gcvEarth)/
                                                 train.mean.Hours.48H$Current)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.Current.gcvEarth)
#show error metric
errMetric.gcvEarth<-mmetric(y=train.mean.Hours.48H$Current, x = train.mean.Hours.48H$pred.Current.gcvEarth, 
                            metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                      "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.gcvEarth)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$Current, x = train.mean.Hours.48H$pred.Current.gcvEarth, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$Current~train.mean.Hours.48H$pred.Current.gcvEarth, 
     main="Current. Predicted vs Measured. gcvEarth", xlab="Predicted Current [A]", ylab="Measured Current [A]")
#/gcvEarth

#gam
train_control.gam <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
gam.Grid<-expand.grid(select = TRUE, method = "GCV.Cp")

time.Current.gam <- proc.time()
model.Current.gam <- train(Current~.,
                           data=train, trControl=train_control.gam,
                           method="gam", tuneGrid = gam.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.Current.gam

train.mean.Hours.48H$pred.Current.gam<-predict(model.Current.gam, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.Current.gam<-abs(100*(train.mean.Hours.48H$Current-train.mean.Hours.48H$pred.Current.gam)/
                                            train.mean.Hours.48H$Current)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.Current.gam)
#show error metric
errMetric.gam<-mmetric(y=train.mean.Hours.48H$Current, x = train.mean.Hours.48H$pred.Current.gam, 
                       metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                 "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.gam)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$Current, x = train.mean.Hours.48H$pred.Current.gam, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$Current~train.mean.Hours.48H$pred.Current.gam, 
     main="Current. Predicted vs Measured. gam", xlab="Predicted Current [A]", ylab="Measured Current [A]")
#/gam

#bagEarth
train_control.bagEarth <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
bagEarth.Grid<-expand.grid(degree=10, nprune=14)

time.Current.bagEarth <- proc.time()
model.Current.bagEarth <- train(Current~.,
                                data=train, trControl=train_control.bagEarth,
                                method="bagEarth", tuneGrid = bagEarth.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.Current.bagEarth

train.mean.Hours.48H$pred.Current.bagEarth<-predict(model.Current.bagEarth, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.Current.bagEarth<-abs(100*(train.mean.Hours.48H$Current-train.mean.Hours.48H$pred.Current.bagEarth)/
                                                 train.mean.Hours.48H$Current)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.Current.bagEarth)
#show error metric
errMetric.bagEarth<-mmetric(y=train.mean.Hours.48H$Current, x = train.mean.Hours.48H$pred.Current.bagEarth, 
                            metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                      "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.bagEarth)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$Current, x = train.mean.Hours.48H$pred.Current.bagEarth, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$Current~train.mean.Hours.48H$pred.Current.bagEarth, 
     main="Current. Predicted vs Measured. bagEarth", xlab="Predicted Current [A", ylab="Measured Current [A]")
#/bagEarth

#brnn
train_control.brnn <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
brnn.Grid<-expand.grid(neurons = 20)

time.Current.brnn <- proc.time()
model.Current.brnn <- train(Current~.,
                            data=train, trControl=train_control.brnn,
                            method="brnn", tuneGrid = brnn.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.Current.brnn

train.mean.Hours.48H$pred.Current.brnn<-predict(model.Current.brnn, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.Current.brnn<-abs(100*(train.mean.Hours.48H$Current-train.mean.Hours.48H$pred.Current.brnn)/
                                             train.mean.Hours.48H$Current)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.Current.brnn)
#show error metric
errMetric.brnn<-mmetric(y=train.mean.Hours.48H$Current, x = train.mean.Hours.48H$pred.Current.brnn, 
                        metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                  "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.brnn)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$Current, x = train.mean.Hours.48H$pred.Current.brnn, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$Current~train.mean.Hours.48H$pred.Current.brnn, 
     main="Current. Predicted vs Measured. brnn", xlab="Predicted Current [A]", ylab="Measured Current [A]")
#/brnn

#/Predicting Current
#save the models
save(model.Current.cubist, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                      'model.Current.cubist.model',sep=''))
save(model.Current.gcvEarth, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                        'model.Current.gcvEarth.model',sep=''))
save(model.Current.bagEarthGCV, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                           'model.Current.bagEarthGCV.model',sep=''))
save(model.Current.bagEarth, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                        'model.Current.bagEarth.model',sep=''))
save(model.Current.svmLinear, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                         'model.Current.svmLinear.model',sep=''))
save(model.Current.brnn, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                    'model.Current.brnn.model',sep=''))
save(model.Current.lm, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                  'model.Current.lm.model',sep=''))
save(model.Current.gam, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                   'model.Current.gam.model',sep=''))

#/save the models

#Testing Current models

test.mean.Hours.48H$pred.Current.lm<-predict(model.Current.lm, test.mean.Hours.48H)
test.mean.Hours.48H$err.Current.lm<-abs(100*(test.mean.Hours.48H$Current-test.mean.Hours.48H$pred.Current.lm)/
                                          test.mean.Hours.48H$Current)
test.mean.Hours.48H$pred.Current.cubist<-predict(model.Current.cubist, test.mean.Hours.48H)
test.mean.Hours.48H$err.Current.cubist<-abs(100*(test.mean.Hours.48H$Current-test.mean.Hours.48H$pred.Current.cubist)/
                                              test.mean.Hours.48H$Current)
test.mean.Hours.48H$pred.Current.gcvEarth<-predict(model.Current.gcvEarth, test.mean.Hours.48H)
test.mean.Hours.48H$err.Current.gcvEarth<-abs(100*(test.mean.Hours.48H$Current-test.mean.Hours.48H$pred.Current.gcvEarth)/
                                                test.mean.Hours.48H$Current)
test.mean.Hours.48H$pred.Current.bagEarthGCV<-predict(model.Current.bagEarthGCV, test.mean.Hours.48H)
test.mean.Hours.48H$err.Current.bagEarthGCV<-abs(100*(test.mean.Hours.48H$Current-test.mean.Hours.48H$pred.Current.bagEarthGCV)/
                                                   test.mean.Hours.48H$Current)
test.mean.Hours.48H$pred.Current.bagEarth<-predict(model.Current.bagEarth, test.mean.Hours.48H)
test.mean.Hours.48H$err.Current.bagEarth<-abs(100*(test.mean.Hours.48H$Current-test.mean.Hours.48H$pred.Current.bagEarth)/
                                                test.mean.Hours.48H$Current)
test.mean.Hours.48H$pred.Current.svmLinear<-predict(model.Current.svmLinear, test.mean.Hours.48H)
test.mean.Hours.48H$err.Current.svmLinear<-abs(100*(test.mean.Hours.48H$Current-test.mean.Hours.48H$pred.Current.svmLinear)/
                                                 test.mean.Hours.48H$Current)
test.mean.Hours.48H$pred.Current.brnn<-predict(model.Current.brnn, test.mean.Hours.48H)
test.mean.Hours.48H$err.Current.brnn<-abs(100*(test.mean.Hours.48H$Current-test.mean.Hours.48H$pred.Current.brnn)/
                                            test.mean.Hours.48H$Current)
test.mean.Hours.48H$pred.Current.gam<-predict(model.Current.gam, test.mean.Hours.48H)
test.mean.Hours.48H$err.Current.gam<-abs(100*(test.mean.Hours.48H$Current-test.mean.Hours.48H$pred.Current.gam)/
                                           test.mean.Hours.48H$Current)


#REC curves
#First REC graph
L=vector("list",4); pred=vector("list",1); test=vector("list",1);

pred[[1]]=test.mean.Hours.48H$Current; 
test[[1]]=test.mean.Hours.48H$pred.Current.lm;          L[[1]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.Current.cubist;      L[[2]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.Current.bagEarthGCV; L[[3]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.Current.bagEarth;    L[[4]]=list(pred=pred,test=test,runs=1)

Current.lm.NAREC = mmetric(y=test.mean.Hours.48H$Current, x = test.mean.Hours.48H$pred.Current.lm, 
                           metric= "NAREC", aggregate="mean", val=5)
Current.lm.NAREC.curve = paste("REC, Linear model=", round(Current.lm.NAREC, digits=4))

Current.cubist.NAREC = mmetric(y=test.mean.Hours.48H$Current, x = test.mean.Hours.48H$pred.Current.cubist, 
                               metric= "NAREC", aggregate="mean", val=5)
Current.cubist.NAREC.curve = paste("REC, cubist=", round(Current.cubist.NAREC, digits=4))

Current.bagEarthGCV.NAREC = mmetric(y=test.mean.Hours.48H$Current, x = test.mean.Hours.48H$pred.Current.bagEarthGCV, 
                                    metric= "NAREC", aggregate="mean", val=5)
Current.bagEarthGCV.NAREC.curve = paste("REC, bagEarthGCV=", round(Current.bagEarthGCV.NAREC, digits=4))

Current.bagEarth.NAREC = mmetric(y=test.mean.Hours.48H$Current, x = test.mean.Hours.48H$pred.Current.bagEarth, 
                                 metric= "NAREC", aggregate="mean", val=5)
Current.bagEarth.NAREC.curve = paste("REC, bagEarth=", round(Current.bagEarth.NAREC, digits=4))

#Second REC graph
B=vector("list",4); pred=vector("list",1); test=vector("list",1);
pred[[1]]=test.mean.Hours.48H$Current; 

test[[1]]=test.mean.Hours.48H$pred.Current.gcvEarth;  B[[1]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.Current.svmLinear; B[[2]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.Current.brnn;      B[[3]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.Current.gam;       B[[4]]=list(pred=pred,test=test,runs=1)


Current.gcvEarth.NAREC = mmetric(y=test.mean.Hours.48H$Current, x = test.mean.Hours.48H$pred.Current.gcvEarth, 
                                 metric= "NAREC", aggregate="mean", val=5)
Current.gcvEarth.NAREC.curve = paste("REC, gcvEarth=", round(Current.gcvEarth.NAREC, digits=4))

Current.svmLinear.NAREC = mmetric(y=test.mean.Hours.48H$Current, x = test.mean.Hours.48H$pred.Current.svmLinear, 
                                  metric= "NAREC", aggregate="mean", val=5)
Current.svmLinear.NAREC.curve = paste("REC, svmLinear=", round(Current.svmLinear.NAREC, digits=4))

Current.brnn.NAREC = mmetric(y=test.mean.Hours.48H$Current, x = test.mean.Hours.48H$pred.Current.brnn, 
                             metric= "NAREC", aggregate="mean", val=5)
Current.brnn.NAREC.curve = paste("REC, brnn=", round(Current.brnn.NAREC, digits=4))

Current.gam.NAREC = mmetric(y=test.mean.Hours.48H$Current, x = test.mean.Hours.48H$pred.Current.gam, 
                            metric= "NAREC", aggregate="mean", val=5)
Current.gam.NAREC.curve = paste("REC, gam=", round(Current.gam.NAREC, digits=4))


#save to png            #png looks nicer than the pdf
png("Current. REC-curves.png", width=1110, height=550)
split.screen(c(1,2))
screen(1)
L.graph<-mgraph(L, size=c(1,10), graph="REC",
                leg=list(pos="bottomright",leg=c(Current.lm.NAREC.curve,
                                                 Current.cubist.NAREC.curve,
                                                 Current.bagEarthGCV.NAREC.curve,
                                                 Current.bagEarth.NAREC.curve)),
                col=c("blue", "gray1", "green", "red"),
                main="Current. REC curves")

screen(2)
B.graph<-mgraph(B, size=c(1,10), graph="REC",
                leg=list(pos="bottomright",leg=c(Current.brnn.NAREC.curve,
                                                 Current.gcvEarth.NAREC.curve,
                                                 Current.svmLinear.NAREC.curve,
                                                 Current.gam.NAREC.curve)),
                col=c("red", "gray1", "green", "blue"),
                main="Current. REC curves")

close.screen(all=TRUE)
dev.off()


errMetric.Current.brnn<-mmetric(y=test.mean.Hours.48H$Current, x = test.mean.Hours.48H$pred.Current.brnn, 
                                metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                          "MAPE", "MdAPE"), aggregate="mean")
errMetric.Current.bagEarth<-mmetric(y=test.mean.Hours.48H$Current, x = test.mean.Hours.48H$pred.Current.bagEarth, 
                                    metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                              "MAPE", "MdAPE"), aggregate="mean")
errMetric.Current.bagEarthGCV<-mmetric(y=test.mean.Hours.48H$Current, x = test.mean.Hours.48H$pred.Current.bagEarthGCV, 
                                       metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                                 "MAPE", "MdAPE"), aggregate="mean")
errMetric.Current.cubist<-mmetric(y=test.mean.Hours.48H$Current, x = test.mean.Hours.48H$pred.Current.cubist, 
                                  metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                            "MAPE", "MdAPE"), aggregate="mean")
errMetric.Current.lm<-mmetric(y=test.mean.Hours.48H$Current, x = test.mean.Hours.48H$pred.Current.lm, 
                              metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                        "MAPE", "MdAPE"), aggregate="mean")
errMetric.Current.gcvEarth<-mmetric(y=test.mean.Hours.48H$Current, x = test.mean.Hours.48H$pred.Current.gcvEarth, 
                                    metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                              "MAPE", "MdAPE"), aggregate="mean")
errMetric.Current.svmLinear<-mmetric(y=test.mean.Hours.48H$Current, x = test.mean.Hours.48H$pred.Current.svmLinear, 
                                     metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                               "MAPE", "MdAPE"), aggregate="mean")
errMetric.Current.gam<-mmetric(y=test.mean.Hours.48H$Current, x = test.mean.Hours.48H$pred.Current.gam, 
                               metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                         "MAPE", "MdAPE"), aggregate="mean")

errMatrix.Current<-data.frame(errMetric.Current.bagEarth,errMetric.Current.brnn,errMetric.Current.cubist,
                              errMetric.Current.lm,errMetric.Current.gcvEarth,errMetric.Current.svmLinear,
                              errMetric.Current.bagEarthGCV, errMetric.Current.gam)
errMatrix.Current<-t(errMatrix.Current)
#write.table(errMatrix.Current,"errorMatrixCurrent.csv", sep=",")

errMatrix.Current<-errMatrix.Current[,c(7,9,10,1,2,3,4,5,6,8,11)]
errMatrix.Current.Ordered<-errMatrix.Current[ order(-errMatrix.Current[,1], -errMatrix.Current[,2], 
                                                    errMatrix.Current[,3]),]
View(errMatrix.Current.Ordered)

#/REC curves
#/testing the models

png("CurrentTest.png", width=1300, height=650)
plot(test.mean.Hours.48H$Current~test.mean.Hours.48H$DateTime, col="black", pch=21, type="l",
     main="Current Intensity. Predicted and Measured", ylab="Current [A]", xlab="Time", cex.axis=1.5, cex.lab=1.7)
points(test.mean.Hours.48H$Current~test.mean.Hours.48H$DateTime, bg='black', pch=21, cex=1.5)
lines(test.mean.Hours.48H$pred.Current.lm~test.mean.Hours.48H$DateTime,    col="blue")
lines(test.mean.Hours.48H$pred.Current.brnn~test.mean.Hours.48H$DateTime,  col="violetred")
lines(test.mean.Hours.48H$pred.Current.gam~test.mean.Hours.48H$DateTime,   col="green")
legend("topleft", 
       c("Measured","Predicted. lm", "Predicted. brnn", "Predicted. gam"), # puts text in the legend 
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5,2.5),col=c("black","blue", "violetred", "green"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1)
dev.off()

#Predicted versus the measured
Current<-test.mean.Hours.48H$Current #Store the measured WindSpeed as a vector
png("CurrentPredictedVSMeasured.png", width=1300, height=650)
par(bg="gray85") #Changing the background color of the canvas to have a greater contrast
plot(test.mean.Hours.48H$Current~Current, col="black", pch=21, type="l",
     main="Current. Predicted VS Measured", xlab="Measured Current [A]", ylab="Predicted Current [A]", cex.axis=1.5, cex.lab=1.7)
points(test.mean.Hours.48H$pred.Current.lm~Current,        col="blue", cex=1.5)
points(test.mean.Hours.48H$pred.Current.brnn~Current,      col="green", cex=1.5)
points(test.mean.Hours.48H$pred.Current.gam~Current,       col="yellow", cex=1.5)
legend("topleft", 
       c("Measured","Predicted. lm", "Predicted. brnn", "Predicted. gam"), # puts text in the legend 
       lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5,2.5, 2.5),col=c("black","blue", "green", "yellow"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1)
dev.off()


#/Testing Current models

                                            #CondTemp
train.mean.Hours.48H<-head(mean.Hours.48H, 668);   rownames(train.mean.Hours.48H)<-NULL
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.AmbTemp48.lm.model")
train.mean.Hours.48H$pred.AmbTemp.lm<-predict(model.AmbTemp.lm, train.mean.Hours.48H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.WindSpeed.cubist.model")
train.mean.Hours.48H$pred.WindSpeed.cubist<-predict(model.WindSpeed.cubist, train.mean.Hours.48H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.Current.brnn.model")
train.mean.Hours.48H$pred.Current.brnn<-predict(model.Current.gam, train.mean.Hours.48H)

train.mean.Hours.48H<-train.mean.Hours.48H[c("PDateTime48H", "PAmbTemp48H", "DateTime", "AmbTemp", 
                                             "pred.AmbTemp.lm", "PWindSpeed48H","WindSpeed", 
                                             "pred.WindSpeed.cubist","Hours", "Days", "daytype", "Current", 
                                             "pred.Current.brnn", "CondTemp", "NormalDLR")]
train<-train.mean.Hours.48H[,c("DateTime", "pred.AmbTemp.lm","pred.WindSpeed.cubist", "Hours", "Days", 
                               "daytype", "pred.Current.brnn", "CondTemp")]

test.mean.Hours.48H<-mean.Hours.48H[c(669:792),];  rownames(test.mean.Hours.48H)<-NULL
test.mean.Hours.48H$pred.AmbTemp.lm<-predict(model.AmbTemp48.lm, test.mean.Hours.48H)
test.mean.Hours.48H$pred.WindSpeed.cubist<-predict(model.WindSpeed.cubist, test.mean.Hours.48H)
load("C:/Users/LabStudent/Desktop/Nick/WeatherDataComparison/48H/PredModels/Current/model.Current.brnn.model")
test.mean.Hours.48H$pred.Current.brnn<-predict(model.Current.svmLinear, test.mean.Hours.48H)
test.mean.Hours.48H<-test.mean.Hours.48H[,c("PDateTime48H", "PAmbTemp48H", "DateTime", "AmbTemp", 
                                            "pred.AmbTemp.lm", "PWindSpeed48H","WindSpeed", 
                                            "pred.WindSpeed.cubist","Hours", "Days", "daytype", "Current", 
                                            "pred.Current.brnn", "CondTemp", "NormalDLR")]
#Predicting CondTemp
#Linear model
time.CondTemp.lm <- proc.time()
model.CondTemp.lm=lm(CondTemp~I(pred.WindSpeed.cubist^1)+I(pred.AmbTemp.lm^1)+DateTime+Days+
                       I(pred.Current.brnn^12)+I(Hours^1)*daytype*I(pred.WindSpeed.cubist^1),
                     data=train); accuracy(f=model.CondTemp.lm) #MAPE 
#/Linear model
proc.time() - time.CondTemp.lm

train.mean.Hours.48H$pred.CondTemp.lm<-predict(model.CondTemp.lm, train.mean.Hours.48H)
train.mean.Hours.48H$err.CondTemp.lm<-abs(100*(train.mean.Hours.48H$CondTemp-train.mean.Hours.48H$pred.CondTemp.lm)
                                          /train.mean.Hours.48H$CondTemp)

                                                  #svmLinear
train_control.svmLinear <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 

time.CondTemp.svmLinear <- proc.time()
model.CondTemp.svmLinear <- train(CondTemp~.,
                                  data=train, trControl=train_control.svmLinear,
                                  method="svmLinear", tuneLength = 8) #tuneLength does not matter
proc.time() - time.CondTemp.svmLinear

train.mean.Hours.48H$pred.CondTemp.svmLinear<-predict(model.CondTemp.svmLinear, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.CondTemp.svmLinear<-abs(100*(train.mean.Hours.48H$CondTemp-train.mean.Hours.48H$pred.CondTemp.svmLinear)/
                                                   train.mean.Hours.48H$CondTemp)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.CondTemp.svmLinear)
#show error metric
errMetric.svmLinear<-mmetric(y=train.mean.Hours.48H$CondTemp, x = train.mean.Hours.48H$pred.CondTemp.svmLinear, 
                             metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                       "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.svmLinear)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$CondTemp, x = train.mean.Hours.48H$pred.CondTemp.svmLinear, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$CondTemp~train.mean.Hours.48H$pred.CondTemp.svmLinear, 
     main="CondTemp. Predicted vs Measured. svmLinear", xlab="Predicted CondTemp [C]", ylab="Measured CondTemp [C]")
                                          #/svmLinear

                                                      #cubist
train_control.cubist <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
cubist.Grid<-expand.grid(committees = 1, neighbors = 1) #highest accuracy for committees = 1, neighbors = 1

time.CondTemp.cubist <- proc.time()
model.CondTemp.cubist <- train(CondTemp~.,
                               data=train, trControl=train_control.cubist,
                               method="cubist", tuneGrid = cubist.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.CondTemp.cubist

train.mean.Hours.48H$pred.CondTemp.cubist<-predict(model.CondTemp.cubist, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.CondTemp.cubist<-abs(100*(train.mean.Hours.48H$CondTemp-train.mean.Hours.48H$pred.CondTemp.cubist)/
                                                train.mean.Hours.48H$CondTemp)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.CondTemp.cubist)
#show error metric
errMetric.cubist<-mmetric(y=train.mean.Hours.48H$CondTemp, x = train.mean.Hours.48H$pred.CondTemp.cubist, 
                          metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                    "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.cubist)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$CondTemp, x = train.mean.Hours.48H$pred.CondTemp.cubist, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$CondTemp~train.mean.Hours.48H$pred.CondTemp.cubist, 
     main="CondTemp. Predicted vs Measured. cubist", xlab="Predicted CondTemp [C]", ylab="Measured CondTemp [C]")
                                                      #/cubist

                                                        #bagEarthGCV
train_control.bagEarthGCV <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
bagEarthGCV.Grid<-expand.grid(degree=7)

time.CondTemp.bagEarthGCV <- proc.time()
model.CondTemp.bagEarthGCV <- train(CondTemp~.,
                                    data=train, trControl=train_control.bagEarthGCV,
                                    method="bagEarthGCV", tuneGrid = bagEarthGCV.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.CondTemp.bagEarthGCV

train.mean.Hours.48H$pred.CondTemp.bagEarthGCV<-predict(model.CondTemp.bagEarthGCV, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.CondTemp.bagEarthGCV<-abs(100*(train.mean.Hours.48H$CondTemp-train.mean.Hours.48H$pred.CondTemp.bagEarthGCV)/
                                                     train.mean.Hours.48H$CondTemp)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.CondTemp.bagEarthGCV)
#show error metric
errMetric.bagEarthGCV<-mmetric(y=train.mean.Hours.48H$CondTemp, x = train.mean.Hours.48H$pred.CondTemp.bagEarthGCV, 
                               metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                         "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.bagEarthGCV)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$CondTemp, x = train.mean.Hours.48H$pred.CondTemp.bagEarthGCV, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$CondTemp~train.mean.Hours.48H$pred.CondTemp.bagEarthGCV, 
     main="CondTemp. Predicted vs Measured. bagEarthGCV", xlab="Predicted CondTemp [C]", ylab="Measured CondTemp [C]")
                                                      #/bagEarthGCV

                                                                #gcvEarth
train_control.gcvEarth <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
gcvEarth.Grid<-expand.grid(degree=15)

time.CondTemp.gcvEarth <- proc.time()
model.CondTemp.gcvEarth <- train(CondTemp~.,
                                 data=train, trControl=train_control.gcvEarth,
                                 method="gcvEarth", tuneGrid = gcvEarth.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.CondTemp.gcvEarth

train.mean.Hours.48H$pred.CondTemp.gcvEarth<-predict(model.CondTemp.gcvEarth, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.CondTemp.gcvEarth<-abs(100*(train.mean.Hours.48H$CondTemp-train.mean.Hours.48H$pred.CondTemp.gcvEarth)/
                                                  train.mean.Hours.48H$CondTemp)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.CondTemp.gcvEarth)
#show error metric
errMetric.gcvEarth<-mmetric(y=train.mean.Hours.48H$CondTemp, x = train.mean.Hours.48H$pred.CondTemp.gcvEarth, 
                            metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                      "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.gcvEarth)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$CondTemp, x = train.mean.Hours.48H$pred.CondTemp.gcvEarth, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$CondTemp~train.mean.Hours.48H$pred.CondTemp.gcvEarth, 
     main="CondTemp. Predicted vs Measured. gcvEarth", xlab="Predicted CondTemp [C]", ylab="Measured CondTemp [C]")
                                            #/gcvEarth

                                              #gam
train_control.gam <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
gam.Grid<-expand.grid(select = TRUE, method = "GCV.Cp")

time.CondTemp.gam <- proc.time()
model.CondTemp.gam <- train(CondTemp~.,
                            data=train, trControl=train_control.gam,
                            method="gam", tuneGrid = gam.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.CondTemp.gam

train.mean.Hours.48H$pred.CondTemp.gam<-predict(model.CondTemp.gam, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.CondTemp.gam<-abs(100*(train.mean.Hours.48H$CondTemp-train.mean.Hours.48H$pred.CondTemp.gam)/
                                             train.mean.Hours.48H$CondTemp)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.CondTemp.gam)
#show error metric
errMetric.gam<-mmetric(y=train.mean.Hours.48H$CondTemp, x = train.mean.Hours.48H$pred.CondTemp.gam, 
                       metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                 "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.gam)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$CondTemp, x = train.mean.Hours.48H$pred.CondTemp.gam, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$CondTemp~train.mean.Hours.48H$pred.CondTemp.gam, 
     main="CondTemp. Predicted vs Measured. gam", xlab="Predicted CondTemp [C]", ylab="Measured CondTemp [C]")
                                                  #/gam

                                                        #bagEarth
train_control.bagEarth <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
bagEarth.Grid<-expand.grid(degree=10, nprune=14)

time.CondTemp.bagEarth <- proc.time()
model.CondTemp.bagEarth <- train(CondTemp~.,
                                 data=train, trControl=train_control.bagEarth,
                                 method="bagEarth", tuneGrid = bagEarth.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.CondTemp.bagEarth

train.mean.Hours.48H$pred.CondTemp.bagEarth<-predict(model.CondTemp.bagEarth, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.CondTemp.bagEarth<-abs(100*(train.mean.Hours.48H$CondTemp-train.mean.Hours.48H$pred.CondTemp.bagEarth)/
                                                  train.mean.Hours.48H$CondTemp)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.CondTemp.bagEarth)
#show error metric
errMetric.bagEarth<-mmetric(y=train.mean.Hours.48H$CondTemp, x = train.mean.Hours.48H$pred.CondTemp.bagEarth, 
                            metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                      "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.bagEarth)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$CondTemp, x = train.mean.Hours.48H$pred.CondTemp.bagEarth, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$CondTemp~train.mean.Hours.48H$pred.CondTemp.bagEarth, 
     main="CondTemp. Predicted vs Measured. bagEarth", xlab="Predicted CondTemp [C]", ylab="Measured CondTemp [C]")
                                                      #/bagEarth

                                                            #brnn
train_control.brnn <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
brnn.Grid<-expand.grid(neurons = 20)

time.CondTemp.brnn <- proc.time()
model.CondTemp.brnn <- train(CondTemp~.,
                             data=train, trControl=train_control.brnn,
                             method="brnn", tuneGrid = brnn.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.CondTemp.brnn

train.mean.Hours.48H$pred.CondTemp.brnn<-predict(model.CondTemp.brnn, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.CondTemp.brnn<-abs(100*(train.mean.Hours.48H$CondTemp-train.mean.Hours.48H$pred.CondTemp.brnn)/
                                              train.mean.Hours.48H$CondTemp)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.CondTemp.brnn)
#show error metric
errMetric.brnn<-mmetric(y=train.mean.Hours.48H$CondTemp, x = train.mean.Hours.48H$pred.CondTemp.brnn, 
                        metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                  "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.brnn)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$CondTemp, x = train.mean.Hours.48H$pred.CondTemp.brnn, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$CondTemp~train.mean.Hours.48H$pred.CondTemp.brnn, 
     main="CondTemp. Predicted vs Measured. brnn", xlab="Predicted CondTemp [C]", ylab="Measured CondTemp [C]")
                                  #/brnn

#/Predicting CondTemp
#save the models
save(model.CondTemp.cubist, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                       'model.CondTemp.cubist.model',sep=''))
save(model.CondTemp.gcvEarth, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                         'model.CondTemp.gcvEarth.model',sep=''))
save(model.CondTemp.bagEarthGCV, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                            'model.CondTemp.bagEarthGCV.model',sep=''))
save(model.CondTemp.bagEarth, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                         'model.CondTemp.bagEarth.model',sep=''))
save(model.CondTemp.svmLinear, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                          'model.CondTemp.svmLinear.model',sep=''))
save(model.CondTemp.brnn, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                     'model.CondTemp.brnn.model',sep=''))
save(model.CondTemp.lm, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                   'model.CondTemp.lm.model',sep=''))
save(model.CondTemp.gam, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                    'model.CondTemp.gam.model',sep=''))


#/save the models

#Testing CondTemp models

load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.CondTemp.svmLinear.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.CondTemp.gcvEarth.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.CondTemp.brnn.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.CondTemp.lm.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.CondTemp.gam.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.CondTemp.bagEarthGCV.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.CondTemp.bagEarth.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.CondTemp.cubist.model")

test.mean.Hours.48H$pred.CondTemp.lm<-predict(model.CondTemp.lm, test.mean.Hours.48H)
test.mean.Hours.48H$err.CondTemp.lm<-abs(100*(test.mean.Hours.48H$CondTemp-test.mean.Hours.48H$pred.CondTemp.lm)/
                                           test.mean.Hours.48H$CondTemp)
test.mean.Hours.48H$pred.CondTemp.cubist<-predict(model.CondTemp.cubist, test.mean.Hours.48H)
test.mean.Hours.48H$err.CondTemp.cubist<-abs(100*(test.mean.Hours.48H$CondTemp-test.mean.Hours.48H$pred.CondTemp.cubist)/
                                               test.mean.Hours.48H$CondTemp)
test.mean.Hours.48H$pred.CondTemp.gcvEarth<-predict(model.CondTemp.gcvEarth, test.mean.Hours.48H)
test.mean.Hours.48H$err.CondTemp.gcvEarth<-abs(100*(test.mean.Hours.48H$CondTemp-test.mean.Hours.48H$pred.CondTemp.gcvEarth)/
                                                 test.mean.Hours.48H$CondTemp)
test.mean.Hours.48H$pred.CondTemp.bagEarthGCV<-predict(model.CondTemp.bagEarthGCV, test.mean.Hours.48H)
test.mean.Hours.48H$err.CondTemp.bagEarthGCV<-abs(100*(test.mean.Hours.48H$CondTemp-test.mean.Hours.48H$pred.CondTemp.bagEarthGCV)/
                                                    test.mean.Hours.48H$CondTemp)
test.mean.Hours.48H$pred.CondTemp.bagEarth<-predict(model.CondTemp.bagEarth, test.mean.Hours.48H)
test.mean.Hours.48H$err.CondTemp.bagEarth<-abs(100*(test.mean.Hours.48H$CondTemp-test.mean.Hours.48H$pred.CondTemp.bagEarth)/
                                                 test.mean.Hours.48H$CondTemp)
test.mean.Hours.48H$pred.CondTemp.svmLinear<-predict(model.CondTemp.svmLinear, test.mean.Hours.48H)
test.mean.Hours.48H$err.CondTemp.svmLinear<-abs(100*(test.mean.Hours.48H$CondTemp-test.mean.Hours.48H$pred.CondTemp.svmLinear)/
                                                  test.mean.Hours.48H$CondTemp)
test.mean.Hours.48H$pred.CondTemp.brnn<-predict(model.CondTemp.brnn, test.mean.Hours.48H)
test.mean.Hours.48H$err.CondTemp.brnn<-abs(100*(test.mean.Hours.48H$CondTemp-test.mean.Hours.48H$pred.CondTemp.brnn)/
                                             test.mean.Hours.48H$CondTemp)
test.mean.Hours.48H$pred.CondTemp.gam<-predict(model.CondTemp.gam, test.mean.Hours.48H)
test.mean.Hours.48H$err.CondTemp.gam<-abs(100*(test.mean.Hours.48H$CondTemp-test.mean.Hours.48H$pred.CondTemp.gam)/
                                            test.mean.Hours.48H$CondTemp)

#REC curves
#First REC graph
L=vector("list",4); pred=vector("list",1); test=vector("list",1);

pred[[1]]=test.mean.Hours.48H$CondTemp; 
test[[1]]=test.mean.Hours.48H$pred.CondTemp.lm;          L[[1]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.CondTemp.cubist;      L[[2]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.CondTemp.bagEarthGCV; L[[3]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.CondTemp.bagEarth;    L[[4]]=list(pred=pred,test=test,runs=1)

CondTemp.lm.NAREC = mmetric(y=test.mean.Hours.48H$CondTemp, x = test.mean.Hours.48H$pred.CondTemp.lm, 
                            metric= "NAREC", aggregate="mean", val=5)
CondTemp.lm.NAREC.curve = paste("REC, Linear model=", round(CondTemp.lm.NAREC, digits=4))

CondTemp.cubist.NAREC = mmetric(y=test.mean.Hours.48H$CondTemp, x = test.mean.Hours.48H$pred.CondTemp.cubist, 
                                metric= "NAREC", aggregate="mean", val=5)
CondTemp.cubist.NAREC.curve = paste("REC, cubist=", round(CondTemp.cubist.NAREC, digits=4))

CondTemp.bagEarthGCV.NAREC = mmetric(y=test.mean.Hours.48H$CondTemp, x = test.mean.Hours.48H$pred.CondTemp.bagEarthGCV, 
                                     metric= "NAREC", aggregate="mean", val=5)
CondTemp.bagEarthGCV.NAREC.curve = paste("REC, bagEarthGCV=", round(CondTemp.bagEarthGCV.NAREC, digits=4))

CondTemp.bagEarth.NAREC = mmetric(y=test.mean.Hours.48H$CondTemp, x = test.mean.Hours.48H$pred.CondTemp.bagEarth, 
                                  metric= "NAREC", aggregate="mean", val=5)
CondTemp.bagEarth.NAREC.curve = paste("REC, bagEarth=", round(CondTemp.bagEarth.NAREC, digits=4))

#Second REC graph
B=vector("list",4); pred=vector("list",1); test=vector("list",1);
pred[[1]]=test.mean.Hours.48H$CondTemp; 

test[[1]]=test.mean.Hours.48H$pred.CondTemp.gcvEarth;  B[[1]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.CondTemp.svmLinear; B[[2]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.CondTemp.brnn;      B[[3]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.CondTemp.gam;       B[[4]]=list(pred=pred,test=test,runs=1)


CondTemp.gcvEarth.NAREC = mmetric(y=test.mean.Hours.48H$CondTemp, x = test.mean.Hours.48H$pred.CondTemp.gcvEarth, 
                                  metric= "NAREC", aggregate="mean", val=5)
CondTemp.gcvEarth.NAREC.curve = paste("REC, gcvEarth=", round(CondTemp.gcvEarth.NAREC, digits=4))

CondTemp.svmLinear.NAREC = mmetric(y=test.mean.Hours.48H$CondTemp, x = test.mean.Hours.48H$pred.CondTemp.svmLinear, 
                                   metric= "NAREC", aggregate="mean", val=5)
CondTemp.svmLinear.NAREC.curve = paste("REC, svmLinear=", round(CondTemp.svmLinear.NAREC, digits=4))

CondTemp.brnn.NAREC = mmetric(y=test.mean.Hours.48H$CondTemp, x = test.mean.Hours.48H$pred.CondTemp.brnn, 
                              metric= "NAREC", aggregate="mean", val=5)
CondTemp.brnn.NAREC.curve = paste("REC, brnn=", round(CondTemp.brnn.NAREC, digits=4))

CondTemp.gam.NAREC = mmetric(y=test.mean.Hours.48H$CondTemp, x = test.mean.Hours.48H$pred.CondTemp.gam, 
                             metric= "NAREC", aggregate="mean", val=5)
CondTemp.gam.NAREC.curve = paste("REC, gam=", round(CondTemp.gam.NAREC, digits=4))


#save to png            #png looks nicer than the pdf
png("CondTemp. REC-curves.png", width=1110, height=550)
split.screen(c(1,2))
screen(1)
L.graph<-mgraph(L, size=c(1,10), graph="REC",
                leg=list(pos="bottomright",leg=c(CondTemp.bagEarth.NAREC.curve,
                                                 CondTemp.cubist.NAREC.curve,
                                                 CondTemp.lm.NAREC.curve,
                                                 CondTemp.bagEarthGCV.NAREC.curve)),
                col=c("red", "gray1", "green", "blue"),
                main="CondTemp. REC curves")

screen(2)
B.graph<-mgraph(B, size=c(1,10), graph="REC",
                leg=list(pos="bottomright",leg=c(CondTemp.gcvEarth.NAREC.curve,
                                                 CondTemp.svmLinear.NAREC.curve,
                                                 CondTemp.brnn.NAREC.curve,
                                                 CondTemp.gam.NAREC.curve)),
                col=c("red", "gray1", "green", "blue"),
                main="CondTemp. REC curves")

close.screen(all=TRUE)
dev.off()

errMetric.CondTemp.brnn<-mmetric(y=test.mean.Hours.48H$CondTemp, x = test.mean.Hours.48H$pred.CondTemp.brnn, 
                                 metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                           "MAPE", "MdAPE"), aggregate="mean")
errMetric.CondTemp.bagEarth<-mmetric(y=test.mean.Hours.48H$CondTemp, x = test.mean.Hours.48H$pred.CondTemp.bagEarth, 
                                     metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                               "MAPE", "MdAPE"), aggregate="mean")
errMetric.CondTemp.bagEarthGCV<-mmetric(y=test.mean.Hours.48H$CondTemp, x = test.mean.Hours.48H$pred.CondTemp.bagEarthGCV, 
                                        metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                                  "MAPE", "MdAPE"), aggregate="mean")
errMetric.CondTemp.cubist<-mmetric(y=test.mean.Hours.48H$CondTemp, x = test.mean.Hours.48H$pred.CondTemp.cubist, 
                                   metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                             "MAPE", "MdAPE"), aggregate="mean")
errMetric.CondTemp.lm<-mmetric(y=test.mean.Hours.48H$CondTemp, x = test.mean.Hours.48H$pred.CondTemp.lm, 
                               metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                         "MAPE", "MdAPE"), aggregate="mean")
errMetric.CondTemp.gcvEarth<-mmetric(y=test.mean.Hours.48H$CondTemp, x = test.mean.Hours.48H$pred.CondTemp.gcvEarth, 
                                     metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                               "MAPE", "MdAPE"), aggregate="mean")
errMetric.CondTemp.svmLinear<-mmetric(y=test.mean.Hours.48H$CondTemp, x = test.mean.Hours.48H$pred.CondTemp.svmLinear, 
                                      metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                                "MAPE", "MdAPE"), aggregate="mean")
errMetric.CondTemp.gam<-mmetric(y=test.mean.Hours.48H$CondTemp, x = test.mean.Hours.48H$pred.CondTemp.gam, 
                                metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                          "MAPE", "MdAPE"), aggregate="mean")

errMatrix.CondTemp<-data.frame(errMetric.CondTemp.bagEarth,errMetric.CondTemp.brnn,errMetric.CondTemp.cubist,
                               errMetric.CondTemp.lm,errMetric.CondTemp.gcvEarth,errMetric.CondTemp.svmLinear,
                               errMetric.CondTemp.bagEarthGCV, errMetric.CondTemp.gam)
errMatrix.CondTemp<-t(errMatrix.CondTemp)
#write.table(errMatrix.CondTemp,"errorMatrixCondTemp.csv", sep=",")

errMatrix.CondTemp<-errMatrix.CondTemp[,c(7,9,10,1,2,3,4,5,6,8,11)]
errMatrix.CondTemp.Ordered<-errMatrix.CondTemp[ order(-errMatrix.CondTemp[,1], -errMatrix.CondTemp[,2], 
                                                      errMatrix.CondTemp[,3]),]
View(errMatrix.CondTemp.Ordered)

#/REC curves
#/testing the models

png("CondTempTest.png", width=1300, height=650)
plot(test.mean.Hours.48H$CondTemp~test.mean.Hours.48H$DateTime, col="black", pch=21, type="l",
     main="CondTemp. Predicted and Measured", ylab="CondTemp [C]", xlab="Time", cex.axis=1.5, cex.lab=1.7)
points(test.mean.Hours.48H$CondTemp~test.mean.Hours.48H$DateTime, bg='black', pch=21, cex=1.5)
lines(test.mean.Hours.48H$pred.CondTemp.lm~test.mean.Hours.48H$DateTime,        col="blue")
lines(test.mean.Hours.48H$pred.CondTemp.brnn~test.mean.Hours.48H$DateTime,      col="violetred")
lines(test.mean.Hours.48H$pred.CondTemp.gam~test.mean.Hours.48H$DateTime,    col="green") 
lines(test.mean.Hours.48H$pred.CondTemp.svmLinear~test.mean.Hours.48H$DateTime, col="red")
legend("topleft", 
       c("Measured","Predicted. lm", "Predicted. brnn", "Predicted. gam", "Predicted. svmLinear"), # puts text in the legend 
       lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5,2.5, 2.5),col=c("black","blue", "violetred", "green", "red"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1)
dev.off()

#Predicted versus the measured
CondTemp<-test.mean.Hours.48H$CondTemp #Store the measured WindSpeed as a vector
png("CondTempPredictedVSMeasured.png", width=1300, height=650)
par(bg="gray85") #Changing the background color of the canvas to have a greater contrast
plot(test.mean.Hours.48H$CondTemp~CondTemp, col="black", pch=21, type="l",
     main="CondTemp. Predicted VS Measured", xlab="Measured CondTemp [C]", ylab="Predicted CondTemp [C]", cex.axis=1.5, cex.lab=1.7)
points(test.mean.Hours.48H$pred.CondTemp.lm~CondTemp,        col="blue", cex=1.5)
points(test.mean.Hours.48H$pred.CondTemp.brnn~CondTemp,      col="red", cex=1.5)
points(test.mean.Hours.48H$pred.CondTemp.gam~CondTemp,       col="green", cex=1.5)
points(test.mean.Hours.48H$pred.CondTemp.svmLinear~CondTemp, col="yellow", cex=1.5)
legend("topleft", 
       c("Measured","Predicted. lm", "Predicted. brnn", "Predicted. gam", "Predicted. svmLinear"), # puts text in the legend 
       lty=c(1,1,1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5,2.5, 2.5, 2.5),col=c("black","blue", "red", "green", "yellow"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1)
dev.off()


#/Testing CondTemp models

#/CondTemp

#NormalDLR
train.mean.Hours.48H<-head(mean.Hours.48H, 668);   rownames(train.mean.Hours.48H)<-NULL
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.AmbTemp48.lm.model")
train.mean.Hours.48H$pred.AmbTemp.lm<-predict(model.AmbTemp.lm, train.mean.Hours.48H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.WindSpeed.cubist.model")
train.mean.Hours.48H$pred.WindSpeed.cubist<-predict(model.WindSpeed.cubist, train.mean.Hours.48H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.Current.brnn.model")
train.mean.Hours.48H$pred.Current.brnn<-predict(model.Current.brnn, train.mean.Hours.48H)
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.CondTemp.lm.model")
train.mean.Hours.48H$pred.CondTemp.lm<-predict(model.CondTemp.lm, train.mean.Hours.48H)

train.mean.Hours.48H<-train.mean.Hours.48H[c("PDateTime48H", "PAmbTemp48H", "DateTime", "AmbTemp", 
                                             "pred.AmbTemp.lm", "PWindSpeed48H","WindSpeed", 
                                             "pred.WindSpeed.cubist","Hours", "Days", "daytype", "Current", 
                                             "pred.Current.brnn", "CondTemp", "pred.CondTemp.lm", "NormalDLR")]
train<-train.mean.Hours.48H[,c("DateTime", "pred.AmbTemp.lm","pred.WindSpeed.cubist", "Hours", "Days", 
                               "daytype", "pred.Current.brnn", "pred.CondTemp.lm", "NormalDLR")]

test.mean.Hours.48H<-mean.Hours.48H[c(669:792),];  rownames(test.mean.Hours.48H)<-NULL
test.mean.Hours.48H$pred.AmbTemp.lm<-predict(model.AmbTemp48.lm, test.mean.Hours.48H)
test.mean.Hours.48H$pred.WindSpeed.cubist<-predict(model.WindSpeed.cubist, test.mean.Hours.48H)
load("C:/Users/LabStudent/Desktop/Nick/WeatherDataComparison/48H/PredModels/Current/model.Current.svmLinear.model")
test.mean.Hours.48H$pred.Current.brnn<-predict(model.Current.svmLinear, test.mean.Hours.48H)
load("C:/Users/LabStudent/Desktop/Nick/WeatherDataComparison/48H/PredModels/CondTemp/model.CondTemp.gam.model")
test.mean.Hours.48H$pred.CondTemp.lm<-predict(model.CondTemp.gam, test.mean.Hours.48H)
test.mean.Hours.48H<-test.mean.Hours.48H[,c("PDateTime48H", "PAmbTemp48H", "DateTime", "AmbTemp", 
                                            "pred.AmbTemp.lm", "PWindSpeed48H","WindSpeed", 
                                            "pred.WindSpeed.cubist","Hours", "Days", "daytype", "Current", 
                                            "pred.Current.brnn", "CondTemp", "pred.CondTemp.lm", "NormalDLR")]

#Predicting NormalDLR
#Linear model
time.NormalDLR.lm <- proc.time()
model.NormalDLR.lm=lm(NormalDLR~I(pred.WindSpeed.cubist^2)+I(pred.AmbTemp.lm^1)+DateTime*daytype*Days+
                        I(pred.CondTemp.lm^2)*I(pred.WindSpeed.cubist^5)+I(pred.Current.brnn^12)+
                        I(Hours^1)*daytype*I(pred.WindSpeed.cubist^1),
                      data=train); accuracy(f=model.NormalDLR.lm) #MAPE 
#/Linear model
proc.time() - time.NormalDLR.lm

train.mean.Hours.48H$pred.NormalDLR.lm<-predict(model.NormalDLR.lm, train.mean.Hours.48H)
train.mean.Hours.48H$err.NormalDLR.lm<-abs(100*(train.mean.Hours.48H$NormalDLR-train.mean.Hours.48H$pred.NormalDLR.lm)
                                           /train.mean.Hours.48H$NormalDLR)

#svmLinear
train_control.svmLinear <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 

time.NormalDLR.svmLinear <- proc.time()
model.NormalDLR.svmLinear <- train(NormalDLR~.,
                                   data=train, trControl=train_control.svmLinear,
                                   method="svmLinear", tuneLength = 8) #tuneLength does not matter
proc.time() - time.NormalDLR.svmLinear

train.mean.Hours.48H$pred.NormalDLR.svmLinear<-predict(model.NormalDLR.svmLinear, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.NormalDLR.svmLinear<-abs(100*(train.mean.Hours.48H$NormalDLR-train.mean.Hours.48H$pred.NormalDLR.svmLinear)/
                                                    train.mean.Hours.48H$NormalDLR)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.NormalDLR.svmLinear)
#show error metric
errMetric.svmLinear<-mmetric(y=train.mean.Hours.48H$NormalDLR, x = train.mean.Hours.48H$pred.NormalDLR.svmLinear, 
                             metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                       "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.svmLinear)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$NormalDLR, x = train.mean.Hours.48H$pred.NormalDLR.svmLinear, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$NormalDLR~train.mean.Hours.48H$pred.NormalDLR.svmLinear, 
     main="NormalDLR. Predicted vs Measured. svmLinear", xlab="Predicted NormalDLR [A]", ylab="Measured NormalDLR [A]")
#/svmLinear

#cubist
train_control.cubist <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
cubist.Grid<-expand.grid(committees = 1, neighbors = 1) #highest accuracy for committees = 1, neighbors = 1

time.NormalDLR.cubist <- proc.time()
model.NormalDLR.cubist <- train(NormalDLR~.,
                                data=train, trControl=train_control.cubist,
                                method="cubist", tuneGrid = cubist.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.NormalDLR.cubist

train.mean.Hours.48H$pred.NormalDLR.cubist<-predict(model.NormalDLR.cubist, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.NormalDLR.cubist<-abs(100*(train.mean.Hours.48H$NormalDLR-train.mean.Hours.48H$pred.NormalDLR.cubist)/
                                                 train.mean.Hours.48H$NormalDLR)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.NormalDLR.cubist)
#show error metric
errMetric.cubist<-mmetric(y=train.mean.Hours.48H$NormalDLR, x = train.mean.Hours.48H$pred.NormalDLR.cubist, 
                          metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                    "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.cubist)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$NormalDLR, x = train.mean.Hours.48H$pred.NormalDLR.cubist, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$NormalDLR~train.mean.Hours.48H$pred.NormalDLR.cubist, 
     main="NormalDLR. Predicted vs Measured. cubist", xlab="Predicted NormalDLR [A]", ylab="Measured NormalDLR [A]")
#/cubist

#bagEarthGCV
train_control.bagEarthGCV <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
bagEarthGCV.Grid<-expand.grid(degree=6)

time.NormalDLR.bagEarthGCV <- proc.time()
model.NormalDLR.bagEarthGCV <- train(NormalDLR~.,
                                     data=train, trControl=train_control.bagEarthGCV,
                                     method="bagEarthGCV", tuneGrid = bagEarthGCV.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.NormalDLR.bagEarthGCV

train.mean.Hours.48H$pred.NormalDLR.bagEarthGCV<-predict(model.NormalDLR.bagEarthGCV, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.NormalDLR.bagEarthGCV<-abs(100*(train.mean.Hours.48H$NormalDLR-train.mean.Hours.48H$pred.NormalDLR.bagEarthGCV)/
                                                      train.mean.Hours.48H$NormalDLR)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.NormalDLR.bagEarthGCV)
#show error metric
errMetric.bagEarthGCV<-mmetric(y=train.mean.Hours.48H$NormalDLR, x = train.mean.Hours.48H$pred.NormalDLR.bagEarthGCV, 
                               metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                         "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.bagEarthGCV)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$NormalDLR, x = train.mean.Hours.48H$pred.NormalDLR.bagEarthGCV, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$NormalDLR~train.mean.Hours.48H$pred.NormalDLR.bagEarthGCV, 
     main="NormalDLR. Predicted vs Measured. bagEarthGCV", xlab="Predicted NormalDLR [A]", ylab="Measured NormalDLR [A]")
#/bagEarthGCV

#gcvEarth
train_control.gcvEarth <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
gcvEarth.Grid<-expand.grid(degree=10)

time.NormalDLR.gcvEarth <- proc.time()
model.NormalDLR.gcvEarth <- train(NormalDLR~.,
                                  data=train, trControl=train_control.gcvEarth,
                                  method="gcvEarth", tuneGrid = gcvEarth.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.NormalDLR.gcvEarth

train.mean.Hours.48H$pred.NormalDLR.gcvEarth<-predict(model.NormalDLR.gcvEarth, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.NormalDLR.gcvEarth<-abs(100*(train.mean.Hours.48H$NormalDLR-train.mean.Hours.48H$pred.NormalDLR.gcvEarth)/
                                                   train.mean.Hours.48H$NormalDLR)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.NormalDLR.gcvEarth)
#show error metric
errMetric.gcvEarth<-mmetric(y=train.mean.Hours.48H$NormalDLR, x = train.mean.Hours.48H$pred.NormalDLR.gcvEarth, 
                            metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                      "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.gcvEarth)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$NormalDLR, x = train.mean.Hours.48H$pred.NormalDLR.gcvEarth, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$NormalDLR~train.mean.Hours.48H$pred.NormalDLR.gcvEarth, 
     main="NormalDLR. Predicted vs Measured. gcvEarth", xlab="Predicted NormalDLR [A]", ylab="Measured NormalDLR [A]")
#/gcvEarth

#gam
train_control.gam <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
gam.Grid<-expand.grid(select = TRUE, method = "GCV.Cp")

time.NormalDLR.gam <- proc.time()
model.NormalDLR.gam <- train(NormalDLR~.,
                             data=train, trControl=train_control.gam,
                             method="gam", tuneGrid = gam.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.NormalDLR.gam

train.mean.Hours.48H$pred.NormalDLR.gam<-predict(model.NormalDLR.gam, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.NormalDLR.gam<-abs(100*(train.mean.Hours.48H$NormalDLR-train.mean.Hours.48H$pred.NormalDLR.gam)/
                                              train.mean.Hours.48H$NormalDLR)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.NormalDLR.gam)
#show error metric
errMetric.gam<-mmetric(y=train.mean.Hours.48H$NormalDLR, x = train.mean.Hours.48H$pred.NormalDLR.gam, 
                       metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                 "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.gam)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$NormalDLR, x = train.mean.Hours.48H$pred.NormalDLR.gam, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$NormalDLR~train.mean.Hours.48H$pred.NormalDLR.gam, 
     main="NormalDLR. Predicted vs Measured. gam", xlab="Predicted NormalDLR [A]", ylab="Measured NormalDLR [A]")
#/gam

#bagEarth
train_control.bagEarth <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
bagEarth.Grid<-expand.grid(degree=10, nprune=14)

time.NormalDLR.bagEarth <- proc.time()
model.NormalDLR.bagEarth <- train(NormalDLR~.,
                                  data=train, trControl=train_control.bagEarth,
                                  method="bagEarth", tuneGrid = bagEarth.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.NormalDLR.bagEarth

train.mean.Hours.48H$pred.NormalDLR.bagEarth<-predict(model.NormalDLR.bagEarth, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.NormalDLR.bagEarth<-abs(100*(train.mean.Hours.48H$NormalDLR-train.mean.Hours.48H$pred.NormalDLR.bagEarth)/
                                                   train.mean.Hours.48H$NormalDLR)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.NormalDLR.bagEarth)
#show error metric
errMetric.bagEarth<-mmetric(y=train.mean.Hours.48H$NormalDLR, x = train.mean.Hours.48H$pred.NormalDLR.bagEarth, 
                            metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                      "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.bagEarth)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$NormalDLR, x = train.mean.Hours.48H$pred.NormalDLR.bagEarth, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$NormalDLR~train.mean.Hours.48H$pred.NormalDLR.bagEarth, 
     main="NormalDLR. Predicted vs Measured. bagEarth", xlab="Predicted NormalDLR [A]", ylab="Measured NormalDLR [A]")
#/bagEarth

#brnn
train_control.brnn <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
brnn.Grid<-expand.grid(neurons = 20)

time.NormalDLR.brnn <- proc.time()
model.NormalDLR.brnn <- train(NormalDLR~.,
                              data=train, trControl=train_control.brnn,
                              method="brnn", tuneGrid = brnn.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.NormalDLR.brnn

train.mean.Hours.48H$pred.NormalDLR.brnn<-predict(model.NormalDLR.brnn, train.mean.Hours.48H)
#record the errors
train.mean.Hours.48H$err.NormalDLR.brnn<-abs(100*(train.mean.Hours.48H$NormalDLR-train.mean.Hours.48H$pred.NormalDLR.brnn)/
                                               train.mean.Hours.48H$NormalDLR)
summary(train.mean.Hours.48H)

#histogram(train.mean.Hours.48H$err.NormalDLR.brnn)
#show error metric
errMetric.brnn<-mmetric(y=train.mean.Hours.48H$NormalDLR, x = train.mean.Hours.48H$pred.NormalDLR.brnn, 
                        metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                  "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.brnn)
#/show error metric
#plot REC curve
mgraph(y=train.mean.Hours.48H$NormalDLR, x = train.mean.Hours.48H$pred.NormalDLR.brnn, graph="REC", 
       col = "red")
#/plot REC curve
plot(train.mean.Hours.48H$NormalDLR~train.mean.Hours.48H$pred.NormalDLR.brnn, 
     main="NormalDLR. Predicted vs Measured. brnn", xlab="Predicted NormalDLR [A]", ylab="Measured NormalDLR [A]")
#/brnn

#/Predicting NormalDLR
#save the models
save(model.NormalDLR.cubist, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                        'model.NormalDLR.cubist.model',sep=''))
save(model.NormalDLR.gcvEarth, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                          'model.NormalDLR.gcvEarth.model',sep=''))
save(model.NormalDLR.bagEarthGCV, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                             'model.NormalDLR.bagEarthGCV.model',sep=''))
save(model.NormalDLR.bagEarth, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                          'model.NormalDLR.bagEarth.model',sep=''))
save(model.NormalDLR.svmLinear, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                           'model.NormalDLR.svmLinear.model',sep=''))
save(model.NormalDLR.brnn, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                      'model.NormalDLR.brnn.model',sep=''))
save(model.NormalDLR.lm, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                    'model.NormalDLR.lm.model',sep=''))
save(model.NormalDLR.gam, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/',
                                     'model.NormalDLR.gam.model',sep=''))

#/save the models

#Testing NormalDLR models

load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.NormalDLR.svmLinear.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.NormalDLR.gcvEarth.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.NormalDLR.brnn.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.NormalDLR.lm.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.NormalDLR.gam.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.NormalDLR.bagEarthGCV.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.NormalDLR.bagEarth.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/ForecastedObservations/48H/SavedModels/model.NormalDLR.cubist.model")

test.mean.Hours.48H$pred.NormalDLR.lm<-predict(model.NormalDLR.lm, test.mean.Hours.48H)
test.mean.Hours.48H$err.NormalDLR.lm<-abs(100*(test.mean.Hours.48H$NormalDLR-test.mean.Hours.48H$pred.NormalDLR.lm)/
                                            test.mean.Hours.48H$NormalDLR)
test.mean.Hours.48H$pred.NormalDLR.cubist<-predict(model.NormalDLR.cubist, test.mean.Hours.48H)
test.mean.Hours.48H$err.NormalDLR.cubist<-abs(100*(test.mean.Hours.48H$NormalDLR-test.mean.Hours.48H$pred.NormalDLR.cubist)/
                                                test.mean.Hours.48H$NormalDLR)
test.mean.Hours.48H$pred.NormalDLR.gcvEarth<-predict(model.NormalDLR.gcvEarth, test.mean.Hours.48H)
test.mean.Hours.48H$err.NormalDLR.gcvEarth<-abs(100*(test.mean.Hours.48H$NormalDLR-test.mean.Hours.48H$pred.NormalDLR.gcvEarth)/
                                                  test.mean.Hours.48H$NormalDLR)
test.mean.Hours.48H$pred.NormalDLR.bagEarthGCV<-predict(model.NormalDLR.bagEarthGCV, test.mean.Hours.48H)
test.mean.Hours.48H$err.NormalDLR.bagEarthGCV<-abs(100*(test.mean.Hours.48H$NormalDLR-test.mean.Hours.48H$pred.NormalDLR.bagEarthGCV)/
                                                     test.mean.Hours.48H$NormalDLR)
test.mean.Hours.48H$pred.NormalDLR.bagEarth<-predict(model.NormalDLR.bagEarth, test.mean.Hours.48H)
test.mean.Hours.48H$err.NormalDLR.bagEarth<-abs(100*(test.mean.Hours.48H$NormalDLR-test.mean.Hours.48H$pred.NormalDLR.bagEarth)/
                                                  test.mean.Hours.48H$NormalDLR)
test.mean.Hours.48H$pred.NormalDLR.svmLinear<-predict(model.NormalDLR.svmLinear, test.mean.Hours.48H)
test.mean.Hours.48H$err.NormalDLR.svmLinear<-abs(100*(test.mean.Hours.48H$NormalDLR-test.mean.Hours.48H$pred.NormalDLR.svmLinear)/
                                                   test.mean.Hours.48H$NormalDLR)
test.mean.Hours.48H$pred.NormalDLR.brnn<-predict(model.NormalDLR.brnn, test.mean.Hours.48H)
test.mean.Hours.48H$err.NormalDLR.brnn<-abs(100*(test.mean.Hours.48H$NormalDLR-test.mean.Hours.48H$pred.NormalDLR.brnn)/
                                              test.mean.Hours.48H$NormalDLR)
test.mean.Hours.48H$pred.NormalDLR.gam<-predict(model.NormalDLR.gam, test.mean.Hours.48H)
test.mean.Hours.48H$err.NormalDLR.gam<-abs(100*(test.mean.Hours.48H$NormalDLR-test.mean.Hours.48H$pred.NormalDLR.gam)/
                                             test.mean.Hours.48H$NormalDLR)


#REC curves
#First REC graph
L=vector("list",4); pred=vector("list",1); test=vector("list",1);

pred[[1]]=test.mean.Hours.48H$NormalDLR; 
test[[1]]=test.mean.Hours.48H$pred.NormalDLR.lm;          L[[1]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.NormalDLR.cubist;      L[[2]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.NormalDLR.bagEarthGCV; L[[3]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.NormalDLR.bagEarth;    L[[4]]=list(pred=pred,test=test,runs=1)

NormalDLR.lm.NAREC = mmetric(y=test.mean.Hours.48H$NormalDLR, x = test.mean.Hours.48H$pred.NormalDLR.lm, 
                             metric= "NAREC", aggregate="mean", val=5)
NormalDLR.lm.NAREC.curve = paste("REC, Linear model=", round(NormalDLR.lm.NAREC, digits=4))

NormalDLR.cubist.NAREC = mmetric(y=test.mean.Hours.48H$NormalDLR, x = test.mean.Hours.48H$pred.NormalDLR.cubist, 
                                 metric= "NAREC", aggregate="mean", val=5)
NormalDLR.cubist.NAREC.curve = paste("REC, cubist=", round(NormalDLR.cubist.NAREC, digits=4))

NormalDLR.bagEarthGCV.NAREC = mmetric(y=test.mean.Hours.48H$NormalDLR, x = test.mean.Hours.48H$pred.NormalDLR.bagEarthGCV, 
                                      metric= "NAREC", aggregate="mean", val=5)
NormalDLR.bagEarthGCV.NAREC.curve = paste("REC, bagEarthGCV=", round(NormalDLR.bagEarthGCV.NAREC, digits=4))

NormalDLR.bagEarth.NAREC = mmetric(y=test.mean.Hours.48H$NormalDLR, x = test.mean.Hours.48H$pred.NormalDLR.bagEarth, 
                                   metric= "NAREC", aggregate="mean", val=5)
NormalDLR.bagEarth.NAREC.curve = paste("REC, bagEarth=", round(NormalDLR.bagEarth.NAREC, digits=4))

#Second REC graph
B=vector("list",4); pred=vector("list",1); test=vector("list",1);
pred[[1]]=test.mean.Hours.48H$NormalDLR; 

test[[1]]=test.mean.Hours.48H$pred.NormalDLR.gcvEarth;  B[[1]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.NormalDLR.svmLinear; B[[2]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.NormalDLR.brnn;      B[[3]]=list(pred=pred,test=test,runs=1)
test[[1]]=test.mean.Hours.48H$pred.NormalDLR.gam;       B[[4]]=list(pred=pred,test=test,runs=1)


NormalDLR.gcvEarth.NAREC = mmetric(y=test.mean.Hours.48H$NormalDLR, x = test.mean.Hours.48H$pred.NormalDLR.gcvEarth, 
                                   metric= "NAREC", aggregate="mean", val=5)
NormalDLR.gcvEarth.NAREC.curve = paste("REC, gcvEarth=", round(NormalDLR.gcvEarth.NAREC, digits=4))

NormalDLR.svmLinear.NAREC = mmetric(y=test.mean.Hours.48H$NormalDLR, x = test.mean.Hours.48H$pred.NormalDLR.svmLinear, 
                                    metric= "NAREC", aggregate="mean", val=5)
NormalDLR.svmLinear.NAREC.curve = paste("REC, svmLinear=", round(NormalDLR.svmLinear.NAREC, digits=4))

NormalDLR.brnn.NAREC = mmetric(y=test.mean.Hours.48H$NormalDLR, x = test.mean.Hours.48H$pred.NormalDLR.brnn, 
                               metric= "NAREC", aggregate="mean", val=5)
NormalDLR.brnn.NAREC.curve = paste("REC, brnn=", round(NormalDLR.brnn.NAREC, digits=4))

NormalDLR.gam.NAREC = mmetric(y=test.mean.Hours.48H$NormalDLR, x = test.mean.Hours.48H$pred.NormalDLR.gam, 
                              metric= "NAREC", aggregate="mean", val=5)
NormalDLR.gam.NAREC.curve = paste("REC, gam=", round(NormalDLR.gam.NAREC, digits=4))


#save to png            #png looks nicer than the pdf
png("NormalDLR. REC-curves.png", width=1110, height=550)
split.screen(c(1,2))
screen(1)
L.graph<-mgraph(L, size=c(1,10), graph="REC",
                leg=list(pos="bottomright",leg=c(NormalDLR.lm.NAREC.curve,
                                                 NormalDLR.cubist.NAREC.curve,
                                                 NormalDLR.bagEarthGCV.NAREC.curve,
                                                 NormalDLR.bagEarth.NAREC.curve)),
                col=c("blue", "gray1", "green", "red"),
                main="NormalDLR. REC curves")

screen(2)
B.graph<-mgraph(B, size=c(1,10), graph="REC",
                leg=list(pos="bottomright",leg=c(NormalDLR.gcvEarth.NAREC.curve,
                                                 NormalDLR.gam.NAREC.curve,
                                                 NormalDLR.svmLinear.NAREC.curve,
                                                 NormalDLR.brnn.NAREC.curve)),
                col=c("red", "gray1", "green", "blue"),
                main="NormalDLR. REC curves")

close.screen(all=TRUE)
dev.off()

errMetric.NormalDLR.brnn<-mmetric(y=test.mean.Hours.48H$NormalDLR, x = test.mean.Hours.48H$pred.NormalDLR.brnn, 
                                  metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                            "MAPE", "MdAPE"), aggregate="mean")
errMetric.NormalDLR.bagEarth<-mmetric(y=test.mean.Hours.48H$NormalDLR, x = test.mean.Hours.48H$pred.NormalDLR.bagEarth, 
                                      metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                                "MAPE", "MdAPE"), aggregate="mean")
errMetric.NormalDLR.bagEarthGCV<-mmetric(y=test.mean.Hours.48H$NormalDLR, x = test.mean.Hours.48H$pred.NormalDLR.bagEarthGCV, 
                                         metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                                   "MAPE", "MdAPE"), aggregate="mean")
errMetric.NormalDLR.cubist<-mmetric(y=test.mean.Hours.48H$NormalDLR, x = test.mean.Hours.48H$pred.NormalDLR.cubist, 
                                    metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                              "MAPE", "MdAPE"), aggregate="mean")
errMetric.NormalDLR.lm<-mmetric(y=test.mean.Hours.48H$NormalDLR, x = test.mean.Hours.48H$pred.NormalDLR.lm, 
                                metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                          "MAPE", "MdAPE"), aggregate="mean")
errMetric.NormalDLR.gcvEarth<-mmetric(y=test.mean.Hours.48H$NormalDLR, x = test.mean.Hours.48H$pred.NormalDLR.gcvEarth, 
                                      metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                                "MAPE", "MdAPE"), aggregate="mean")
errMetric.NormalDLR.svmLinear<-mmetric(y=test.mean.Hours.48H$NormalDLR, x = test.mean.Hours.48H$pred.NormalDLR.svmLinear, 
                                       metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                                 "MAPE", "MdAPE"), aggregate="mean")
errMetric.NormalDLR.gam<-mmetric(y=test.mean.Hours.48H$NormalDLR, x = test.mean.Hours.48H$pred.NormalDLR.gam, 
                                 metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                           "MAPE", "MdAPE"), aggregate="mean")

errMatrix.NormalDLR<-data.frame(errMetric.NormalDLR.bagEarth,errMetric.NormalDLR.brnn,errMetric.NormalDLR.cubist,
                                errMetric.NormalDLR.lm,errMetric.NormalDLR.gcvEarth,errMetric.NormalDLR.svmLinear,
                                errMetric.NormalDLR.bagEarthGCV, errMetric.NormalDLR.gam)
errMatrix.NormalDLR<-t(errMatrix.NormalDLR)
#write.table(errMatrix.NormalDLR,"errorMatrixNormalDLR.csv", sep=",")

errMatrix.NormalDLR<-errMatrix.NormalDLR[,c(7,9,10,1,2,3,4,5,6,8,11)]
errMatrix.NormalDLR.Ordered<-errMatrix.NormalDLR[ order(-errMatrix.NormalDLR[,1], -errMatrix.NormalDLR[,2], 
                                                        errMatrix.NormalDLR[,3]),]
View(errMatrix.NormalDLR.Ordered)

#/REC curves
#/testing the models

png("NormalDLRTest.png", width=1300, height=650)
plot(test.mean.Hours.48H$NormalDLR~test.mean.Hours.48H$DateTime, col="black", pch=21, type="l",
     main="NormalDLR. Predicted and Measured", ylab="NormalDLR [A]", xlab="Time", cex.axis=1.5, cex.lab=1.7)
points(test.mean.Hours.48H$NormalDLR~test.mean.Hours.48H$DateTime, bg='black', pch=21, cex=1.5)
lines(test.mean.Hours.48H$pred.NormalDLR.lm~test.mean.Hours.48H$DateTime,               col="blue")
lines(test.mean.Hours.48H$pred.NormalDLR.brnn~test.mean.Hours.48H$DateTime,      col="red")
lines(test.mean.Hours.48H$pred.NormalDLR.gam~test.mean.Hours.48H$DateTime,              col="green") 
lines(test.mean.Hours.48H$pred.NormalDLR.svmLinear~test.mean.Hours.48H$DateTime,        col="violetred")
legend("topleft", 
       c("Measured","Predicted. lm", "Predicted. brnn", "Predicted. gam", "Predicted. svmLinear"), # puts text in the legend 
       lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5,2.5, 2.5),col=c("black","blue", "red", "green", "violetred"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1)
dev.off()

#Predicted versus the measured
NormalDLR<-test.mean.Hours.48H$NormalDLR #Store the measured WindSpeed as a vector
png("NormalDLRPredictedVSMeasured.png", width=1300, height=650)
par(bg="gray85") #Changing the background color of the canvas to have a greater contrast
plot(test.mean.Hours.48H$NormalDLR~NormalDLR, col="black", pch=21, type="l",
     main="NormalDLR. Predicted VS Measured", xlab="Measured NormalDLR [A]", ylab="Predicted NormalDLR [A]", cex.axis=1.5, cex.lab=1.7)
points(test.mean.Hours.48H$pred.NormalDLR.lm~NormalDLR,        col="blue", cex=1.5)
points(test.mean.Hours.48H$pred.NormalDLR.brnn~NormalDLR,    col="red", cex=1.5)
points(test.mean.Hours.48H$pred.NormalDLR.gam~NormalDLR,       col="green", cex=1.5)
points(test.mean.Hours.48H$pred.NormalDLR.svmLinear~NormalDLR, col="yellow", cex=1.5)
legend("topleft", 
       c("Measured","Predicted. lm", "Predicted. brnn", "Predicted. gam", "Predicted. svmLinear"), # puts text in the legend 
       lty=c(1,1,1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5,2.5, 2.5, 2.5),col=c("black","blue", "red", "green", "yellow"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1)
dev.off()
#/Predicted versus the measured
  
#/Testing NormalDLR models
#Benchmark the predictive models according to their error rates
#This analysis can be also be easily done for other predicted parameters
#Comment "ylim=c(0,40)," to see the real results
png("NormalDLRModelsErrors.png", width=1300, height=650)
par(bg="gray92")
boxplot(test.mean.Hours.48H$err.NormalDLR.lm, test.mean.Hours.48H$err.NormalDLR.cubist, test.mean.Hours.48H$err.NormalDLR.gcvEarth,
        test.mean.Hours.48H$err.NormalDLR.bagEarthGCV, test.mean.Hours.48H$err.NormalDLR.bagEarth, test.mean.Hours.48H$err.NormalDLR.svmLinear,
        test.mean.Hours.48H$err.NormalDLR.brnn, test.mean.Hours.48H$err.NormalDLR.gam, 
        cex.lab=1.2, cex.axis=1.2, #ylim=c(0,40),
        names=c("LinearModel", "Cubist", "bagEarthGCV", "bagEarth", "gcvEarth", "svmLinear", "brnn", "gam"),
        col=c("red","green", "orange", "blue2", "darkslateblue", "mediumvioletred", "orangered3", "yellow"),
        xlab="Predictive Models", ylab="Error rate profile [%]", main="Error rate distribution of the predictive models for NormalDLR in the +48H time-horizon")
dev.off()
#/Benchmark the predictive models according to their error rates

#/NormalDLR
#svmLinear seems to be the best model
#/Work with predicted AmbTemp and WindSpeed
