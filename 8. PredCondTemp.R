### KTH, Department of Industrial Information and Control Systems (ICS)
### Master Thesis Project
### BUILDING PREDICTIVE MODELS FOR DYNAMIC LINE RATING USING DATA SCIENCE TECHNIQUES 
### Sponsored by Vattenfall AG
### Author: Nicolae Doban
### Stockholm, 2015

## This module builds the hourly mean values and builds the predictive models on the historical observations
## Packages used: base, caret, boot, DAAG, forecast, rminer, splines, DAAG
## INPUT DATA: The Hourly mean dataset mean.Hours.csv which will serve as training set.
## OUTPUT: Building the Predictive Models for the CondTemp parameter using historical observations

##When building the models for CondTemp, the models will use the predicted Current values obtained via the "cubist" model
##The performance of the built models will be assessed in 10. AprilTest.R between the lines: 289-473

library(caret)
library(rminer)
library(boot)
library(forecast)
library(splines)
library(DAAG)

setwd("C:/Users/LabStudent/Desktop/Nick/GitHub")
mean.Hours<-read.csv("mean.Hours.csv", sep=",", header=TRUE)
#build a DataTime column because once read from the csv file, the DateTime column was not in "POSIXt" format
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
mean.HoursTop<-mean.Hours[,2:10]


load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/Current/cubist.Current.model")
mean.HoursTop$pred.Current.cubist<-predict(model.Current.cubist, mean.HoursTop) #store the predicted values of Current variable using "cubist" model
#mean.HoursTop$err.Current.cubist<-abs(100*(mean.HoursTop$Current-mean.HoursTop$pred.Current.cubist)/
#                                            mean.HoursTop$Current)
mean.HoursTop<-mean.HoursTop[,c(1,2,3,4,5,6,7,8,10,9)] #re-arrange the columns
mean.HoursTop$Current<-NULL   #delete Current column to build the predictive models for COndTemp with
                                  #the predicted Current values
mean.HoursTopPerf<-mean.HoursTop

                                                #Linear model
ptm <- proc.time()
model.CondTemp.Formula=lm(CondTemp~I(WindSpeed^4)*I(pred.Current.cubist^2)+I(WindDir^3)+
                            I(AmbTemp^1)*I(pred.Current.cubist^1)+
                    I(SolarRad^1)+I(Hours^6)*I(pred.Current.cubist^6)+daytype+
                      Days*I(pred.Current.cubist^6)+I(pred.Current.cubist^2),
                  data=mean.HoursTop); accuracy(f=model.CondTemp.Formula) #MAPE 
proc.time() - ptm
mean.HoursTopPerf$pred.CondTemp.Formula<-predict(model.CondTemp.Formula, mean.HoursTopPerf)
mean.HoursTopPerf$err.CondTemp.Formula<-abs(100*(mean.HoursTopPerf$CondTemp-mean.HoursTopPerf$pred.CondTemp.Formula)/
                                             mean.HoursTop$CondTemp);summary(mean.HoursTopPerf)
errMetric.Formula<-mmetric(y=mean.HoursTopPerf$CondTemp, x = mean.HoursTopPerf$pred.CondTemp.Formula, 
                          metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                    "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.Formula)
save(model.CondTemp.Formula, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/CondTemp/',
                                       'model.CondTemp.Formula.model',sep='')) #save the model

                                                    #/Linear model

                                                          #cubist
time.CondTemp.cubist <- proc.time()
# define training control
train_control.cubist <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
cubist.Grid<-expand.grid(committees = 1, neighbors = 1) #highest accuracy for committees = 1, neighbors = 1
model.CondTemp.cubist <- train(CondTemp~., data=mean.HoursTop, trControl=train_control.cubist,
                              method="cubist", tuneGrid = cubist.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.CondTemp.cubist
# make predictions
mean.HoursTopPerf$pred.CondTemp.cubist<-predict(model.CondTemp.cubist, mean.HoursTop[,1:8])
#record the errors
mean.HoursTopPerf$err.CondTemp.cubist<-abs(100*(mean.HoursTopPerf$CondTemp-mean.HoursTopPerf$pred.CondTemp.cubist)/
                                            mean.HoursTop$CondTemp)
histogram(mean.HoursTopPerf$err.CondTemp.cubist)
#show error metric
errMetric.cubist<-mmetric(y=mean.HoursTopPerf$CondTemp, x = mean.HoursTopPerf$pred.CondTemp.cubist, 
                          metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                    "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.cubist)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$CondTemp, x = mean.HoursTopPerf$pred.CondTemp.cubist, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.cubist)
plot(mean.HoursTopPerf$CondTemp~mean.HoursTopPerf$pred.CondTemp.cubist, main="cubist Performance", 
     xlab="CondTemp predicted by cubist", ylab="CondTemp")

save(model.CondTemp.cubist, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/CondTemp/',
                                      'cubist.CondTemp.model',sep=''))
                                                        #/cubist

                                                #bagEarthGCV
time.CondTemp.bagEarthGCV <- proc.time()
# define training control
train_control.bagEarthGCV <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
bagEarthGCV.Grid<-expand.grid(degree=6)
model.CondTemp.bagEarthGCV <- train(CondTemp~., data=mean.HoursTop, trControl=train_control.bagEarthGCV,
                               method="bagEarthGCV", tuneGrid = bagEarthGCV.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.CondTemp.bagEarthGCV
# make predictions
mean.HoursTopPerf$pred.CondTemp.bagEarthGCV<-predict(model.CondTemp.bagEarthGCV, mean.HoursTop[,1:8])
#record the errors
mean.HoursTopPerf$err.CondTemp.bagEarthGCV<-abs(100*(mean.HoursTopPerf$CondTemp-mean.HoursTopPerf$pred.CondTemp.bagEarthGCV)/
                                             mean.HoursTop$CondTemp)
histogram(mean.HoursTopPerf$err.CondTemp.bagEarthGCV)
#show error metric
errMetric.bagEarthGCV<-mmetric(y=mean.HoursTopPerf$CondTemp, x = mean.HoursTopPerf$pred.CondTemp.bagEarthGCV, 
                          metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                    "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.bagEarthGCV)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$CondTemp, x = mean.HoursTopPerf$pred.CondTemp.bagEarthGCV, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.bagEarthGCV)
plot(mean.HoursTopPerf$CondTemp~mean.HoursTopPerf$pred.CondTemp.bagEarthGCV, main="bagEarthGCV Performance", 
     xlab="CondTemp predicted by bagEarthGCV", ylab="CondTemp")

save(model.CondTemp.bagEarthGCV, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/CondTemp/',
                                       'bagEarthGCV.CondTemp.model',sep=''))
                                        #/bagEarthGCV

                                                      #gcvEarth
time.CondTemp.gcvEarth <- proc.time()
# define training control
train_control.gcvEarth <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
gcvEarth.Grid<-expand.grid(degree=1)
model.CondTemp.gcvEarth <- train(CondTemp~., data=mean.HoursTop, trControl=train_control.gcvEarth,
                                    method="gcvEarth", tuneGrid = gcvEarth.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.CondTemp.gcvEarth
# make predictions
mean.HoursTopPerf$pred.CondTemp.gcvEarth<-predict(model.CondTemp.gcvEarth, mean.HoursTop[,1:8])
#record the errors
mean.HoursTopPerf$err.CondTemp.gcvEarth<-abs(100*(mean.HoursTopPerf$CondTemp-mean.HoursTopPerf$pred.CondTemp.gcvEarth)/
                                                  mean.HoursTop$CondTemp)
histogram(mean.HoursTopPerf$err.CondTemp.gcvEarth)
#show error metric
errMetric.gcvEarth<-mmetric(y=mean.HoursTopPerf$CondTemp, x = mean.HoursTopPerf$pred.CondTemp.gcvEarth, 
                               metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                         "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.gcvEarth)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$CondTemp, x = mean.HoursTopPerf$pred.CondTemp.gcvEarth, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.gcvEarth)
plot(mean.HoursTopPerf$CondTemp~mean.HoursTopPerf$pred.CondTemp.gcvEarth, main="gcvEarth Performance", 
     xlab="CondTemp predicted by gcvEarth", ylab="CondTemp")

save(model.CondTemp.gcvEarth, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/CondTemp/',
                                            'gcvEarth.CondTemp.model',sep=''))
                                            #/gcvEarth

                                                #svmLinear
time.CondTemp.svmLinear <- proc.time()
# define training control
train_control.svmLinear <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
model.CondTemp.svmLinear <- train(CondTemp~., data=mean.HoursTop, trControl=train_control.svmLinear,
                                 method="svmLinear", tuneLength = 8)#, tuneGrid = svmLinear.Grid) #tuneLength does not matter
proc.time() - time.CondTemp.svmLinear
# make predictions
mean.HoursTopPerf$pred.CondTemp.svmLinear<-predict(model.CondTemp.svmLinear, mean.HoursTop[,1:8])
#record the errors
mean.HoursTopPerf$err.CondTemp.svmLinear<-abs(100*(mean.HoursTopPerf$CondTemp-mean.HoursTopPerf$pred.CondTemp.svmLinear)/
                                               mean.HoursTop$CondTemp)
histogram(mean.HoursTopPerf$err.CondTemp.svmLinear)
#show error metric
errMetric.svmLinear<-mmetric(y=mean.HoursTopPerf$CondTemp, x = mean.HoursTopPerf$pred.CondTemp.svmLinear, 
                            metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                      "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.svmLinear)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$CondTemp, x = mean.HoursTopPerf$pred.CondTemp.svmLinear, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.svmLinear)
plot(mean.HoursTopPerf$CondTemp~mean.HoursTopPerf$pred.CondTemp.svmLinear, main="svmLinear Performance", 
     xlab="CondTemp predicted by svmLinear", ylab="CondTemp")

save(model.CondTemp.svmLinear, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/CondTemp/',
                                         'svmLinear.CondTemp.model',sep=''))
                                                #/svmLinear

                                                  #bagEarth
time.CondTemp.bagEarth <- proc.time()
# define training control
train_control.bagEarth <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
bagEarth.Grid<-expand.grid(degree=10, nprune=14)
# train the model 
model.CondTemp.bagEarth <- train(CondTemp~., data=mean.HoursTop, trControl=train_control.bagEarth,
                                  method="bagEarth", tuneLength = 8)#, tuneGrid = bagEarth.Grid) #tuneLength does not matter
proc.time() - time.CondTemp.bagEarth
# make predictions
mean.HoursTopPerf$pred.CondTemp.bagEarth<-predict(model.CondTemp.bagEarth, mean.HoursTop[,1:8])
#record the errors
mean.HoursTopPerf$err.CondTemp.bagEarth<-abs(100*(mean.HoursTopPerf$CondTemp-mean.HoursTopPerf$pred.CondTemp.bagEarth)/
                                                mean.HoursTop$CondTemp)
histogram(mean.HoursTopPerf$err.CondTemp.bagEarth)
#show error metric
errMetric.bagEarth<-mmetric(y=mean.HoursTopPerf$CondTemp, x = mean.HoursTopPerf$pred.CondTemp.bagEarth, 
                             metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                       "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.bagEarth)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$CondTemp, x = mean.HoursTopPerf$pred.CondTemp.bagEarth, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.bagEarth)
plot(mean.HoursTopPerf$CondTemp~mean.HoursTopPerf$pred.CondTemp.bagEarth, main="bagEarth Performance", 
     xlab="CondTemp predicted by bagEarth", ylab="CondTemp")

save(model.CondTemp.bagEarth, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/CondTemp/',
                                          'bagEarth.CondTemp.model',sep=''))
                                            #/bagEarth

                                                  #brnn
time.CondTemp.brnn <- proc.time()
# define training control
train_control.brnn <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
brnn.Grid<-expand.grid(neurons = 20)
# train the model 
model.CondTemp.brnn <- train(CondTemp~., data=mean.HoursTop, trControl=train_control.brnn,
                                 method="brnn", tuneLength = 8)#, tuneGrid = brnn.Grid) #tuneLength does not matter
proc.time() - time.CondTemp.brnn
# make predictions
mean.HoursTopPerf$pred.CondTemp.brnn<-predict(model.CondTemp.brnn, mean.HoursTop[,1:8])
#record the errors
mean.HoursTopPerf$err.CondTemp.brnn<-abs(100*(mean.HoursTopPerf$CondTemp-mean.HoursTopPerf$pred.CondTemp.brnn)/
                                               mean.HoursTop$CondTemp)
histogram(mean.HoursTopPerf$err.CondTemp.brnn)
#show error metric
errMetric.brnn<-mmetric(y=mean.HoursTopPerf$CondTemp, x = mean.HoursTopPerf$pred.CondTemp.brnn, 
                            metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                      "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.brnn)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$CondTemp, x = mean.HoursTopPerf$pred.CondTemp.brnn, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.brnn)
plot(mean.HoursTopPerf$CondTemp~mean.HoursTopPerf$pred.CondTemp.brnn, main="brnn Performance", 
     xlab="CondTemp predicted by brnn", ylab="CondTemp")

save(model.CondTemp.brnn, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/CondTemp/',
                                         'brnn.CondTemp.model',sep=''))
                                              #/brnn

