### KTH, Department of Industrial Information and Control Systems (ICS)
### Master Thesis Project
### BUILDING PREDICTIVE MODELS FOR DYNAMIC LINE RATING USING DATA SCIENCE TECHNIQUES 
### Sponsored by Vattenfall AG
### Author: Nicolae Doban
### Stockholm, 2015

## This module builds the hourly mean values and builds the predictive models on the historical observations
## Packages used: base, caret, boot, DAAG, forecast, rminer, splines, DAAG
## INPUT DATA: The Hourly mean dataset mean.Hours.csv which will serve as training set
## OUTPUT: Building the Predictive Models for the NormalDLR parameter using historical observations

##When building the models for NormalDLR, the models will use the predicted Current and predicted CondTemp values
##The performance of the built models will be assessed in 10. AprilTest.R between the lines: 476-683

library(caret);library(rminer);library(boot);library(forecast);library(splines);library(DAAG); library(histogram)

setwd("C:/Users/LabStudent/Desktop/Nick/GitHub")
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
mean.HoursTop<-mean.Hours[,2:11]
#Predicitng Current
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/Current/cubist.Current.model")
mean.HoursTop$pred.Current.cubist<-predict(model.Current.cubist, mean.HoursTop)
#Store the error rates of the Current parameter on the test dataset
#mean.HoursTop$err.Current.cubist<-abs(100*(mean.HoursTop$Current-mean.HoursTop$pred.Current.cubist)/
#                                            mean.HoursTop$Current)
#Predicting CondTemp
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/CondTemp/bagEarthGCV.CondTemp.model")
mean.HoursTop$pred.CondTemp.bagEarthGCV<-predict(model.CondTemp.bagEarthGCV, mean.HoursTop)
#Store the error rates of the CondTemp parameter on the test dataset
#mean.HoursTop$err.CondTemp.bagEarthGCV<-abs(100*(mean.HoursTop$CondTemp-mean.HoursTop$pred.CondTemp.bagEarthGCV)/
#                                            mean.HoursTop$CondTemp)
mean.HoursTop$Current=mean.HoursTop$CondTemp<-NULL #the models will be built on the already predicted Current and CondTemp values
mean.HoursTop<-mean.HoursTop[,c(1,2,3,4,5,6,7,9,10,8)]
mean.HoursTopPerf<-mean.HoursTop

                                                #Linear model
ptm <- proc.time()
model.NormalDLR.Formula=lm(NormalDLR~Days*I(WindSpeed^1)*I(pred.Current.cubist^1)+
                                     Days*I(WindDir^1)+
                                     Days*I(AmbTemp^1)*I(pred.Current.cubist^1)+
                                     Days*I(pred.CondTemp.bagEarthGCV^1)+
                                     Days*I(SolarRad^1)+
                                     Days*I(Hours^1)*I(pred.Current.cubist^11)+
                                     daytype+ #lm doesn't improve with Days*daytype
                                     Days*I(pred.Current.cubist^1)+
                                     Days*I(pred.Current.cubist^2),
                          data=mean.HoursTop); accuracy(f=model.NormalDLR.Formula) #MAPE = 4.8500%
proc.time() - ptm
mean.HoursTopPerf$pred.NormalDLR.Formula<-predict(model.NormalDLR.Formula, mean.HoursTopPerf)
mean.HoursTopPerf$err.NormalDLR.Formula<-abs(100*(mean.HoursTopPerf$NormalDLR-mean.HoursTopPerf$pred.NormalDLR.Formula)/
                                              mean.HoursTop$NormalDLR);summary(mean.HoursTopPerf)

errMetric.Formula<-mmetric(y=mean.HoursTopPerf$NormalDLR, x = mean.HoursTopPerf$pred.NormalDLR.Formula, 
                           metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                     "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.Formula)
save(model.NormalDLR.Formula, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/',
                                        'model.NormalDLR.Formula.model',sep=''))

                                        #/Linear model

                                                      #cubist
time.NormalDLR.cubist <- proc.time()
# define training control
train_control.cubist <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
cubist.Grid<-expand.grid(committees = 1, neighbors = 1) #highest accuracy for committees = 1, neighbors = 1
model.NormalDLR.cubist <- train(NormalDLR~., data=mean.HoursTop, trControl=train_control.cubist,
                               method="cubist", tuneGrid = cubist.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.NormalDLR.cubist
# make predictions
mean.HoursTopPerf$pred.NormalDLR.cubist<-predict(model.NormalDLR.cubist, mean.HoursTop[,1:9])
#record the errors
mean.HoursTopPerf$err.NormalDLR.cubist<-abs(100*(mean.HoursTopPerf$NormalDLR-mean.HoursTopPerf$pred.NormalDLR.cubist)/
                                             mean.HoursTop$NormalDLR)
histogram(mean.HoursTopPerf$err.NormalDLR.cubist)
#show error metric
errMetric.cubist<-mmetric(y=mean.HoursTopPerf$NormalDLR, x = mean.HoursTopPerf$pred.NormalDLR.cubist, 
                          metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                    "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.cubist)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$NormalDLR, x = mean.HoursTopPerf$pred.NormalDLR.cubist, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.cubist)
plot(mean.HoursTopPerf$NormalDLR~mean.HoursTopPerf$pred.NormalDLR.cubist, main="cubist Performance", 
     xlab="NormalDLR predicted by cubist", ylab="NormalDLR")

save(model.NormalDLR.cubist, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/',
                                       'cubist.NormalDLR.model',sep=''))
                                            #/cubist

                                                  #bagEarthGCV
time.NormalDLR.bagEarthGCV <- proc.time()
# define training control
train_control.bagEarthGCV <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
bagEarthGCV.Grid<-expand.grid(degree=6)
model.NormalDLR.bagEarthGCV <- train(NormalDLR~., data=mean.HoursTop, trControl=train_control.bagEarthGCV,
                                    method="bagEarthGCV", tuneGrid = bagEarthGCV.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.NormalDLR.bagEarthGCV
# make predictions
mean.HoursTopPerf$pred.NormalDLR.bagEarthGCV<-predict(model.NormalDLR.bagEarthGCV, mean.HoursTop[,1:9])
#record the errors
mean.HoursTopPerf$err.NormalDLR.bagEarthGCV<-abs(100*(mean.HoursTopPerf$NormalDLR-mean.HoursTopPerf$pred.NormalDLR.bagEarthGCV)/
                                                  mean.HoursTop$NormalDLR)
histogram(mean.HoursTopPerf$err.NormalDLR.bagEarthGCV)
#show error metric
errMetric.bagEarthGCV<-mmetric(y=mean.HoursTopPerf$NormalDLR, x = mean.HoursTopPerf$pred.NormalDLR.bagEarthGCV, 
                               metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                         "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.bagEarthGCV)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$NormalDLR, x = mean.HoursTopPerf$pred.NormalDLR.bagEarthGCV, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.bagEarthGCV)
plot(mean.HoursTopPerf$NormalDLR~mean.HoursTopPerf$pred.NormalDLR.bagEarthGCV, main="bagEarthGCV Performance", 
     xlab="NormalDLR predicted by bagEarthGCV", ylab="NormalDLR")

save(model.NormalDLR.bagEarthGCV, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/',
                                            'bagEarthGCV.NormalDLR.model',sep=''))
                              #/bagEarthGCV

                                              #gcvEarth
time.NormalDLR.gcvEarth <- proc.time()
# define training control
train_control.gcvEarth <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
gcvEarth.Grid<-expand.grid(degree=1)
model.NormalDLR.gcvEarth <- train(NormalDLR~., data=mean.HoursTop, trControl=train_control.gcvEarth,
                                 method="gcvEarth", tuneGrid = gcvEarth.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.NormalDLR.gcvEarth
# make predictions
mean.HoursTopPerf$pred.NormalDLR.gcvEarth<-predict(model.NormalDLR.gcvEarth, mean.HoursTop[,1:9])
#record the errors
mean.HoursTopPerf$err.NormalDLR.gcvEarth<-abs(100*(mean.HoursTopPerf$NormalDLR-mean.HoursTopPerf$pred.NormalDLR.gcvEarth)/
                                               mean.HoursTop$NormalDLR)
histogram(mean.HoursTopPerf$err.NormalDLR.gcvEarth)
#show error metric
errMetric.gcvEarth<-mmetric(y=mean.HoursTopPerf$NormalDLR, x = mean.HoursTopPerf$pred.NormalDLR.gcvEarth, 
                            metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                      "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.gcvEarth)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$NormalDLR, x = mean.HoursTopPerf$pred.NormalDLR.gcvEarth, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.gcvEarth)
plot(mean.HoursTopPerf$NormalDLR~mean.HoursTopPerf$pred.NormalDLR.gcvEarth, main="gcvEarth Performance", 
     xlab="NormalDLR predicted by gcvEarth", ylab="NormalDLR")

save(model.NormalDLR.gcvEarth, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/',
                                         'gcvEarth.NormalDLR.model',sep=''))
                                                  #/gcvEarth

                                            #svmLinear
time.NormalDLR.svmLinear <- proc.time()
# define training control
train_control.svmLinear <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
model.NormalDLR.svmLinear <- train(NormalDLR~., data=mean.HoursTop, trControl=train_control.svmLinear,
                                  method="svmLinear", tuneLength = 8)#, tuneGrid = svmLinear.Grid) #tuneLength does not matter
proc.time() - time.NormalDLR.svmLinear
# make predictions
mean.HoursTopPerf$pred.NormalDLR.svmLinear<-predict(model.NormalDLR.svmLinear, mean.HoursTop[,1:9])
#record the errors
mean.HoursTopPerf$err.NormalDLR.svmLinear<-abs(100*(mean.HoursTopPerf$NormalDLR-mean.HoursTopPerf$pred.NormalDLR.svmLinear)/
                                                mean.HoursTop$NormalDLR)
histogram(mean.HoursTopPerf$err.NormalDLR.svmLinear)
#show error metric
errMetric.svmLinear<-mmetric(y=mean.HoursTopPerf$NormalDLR, x = mean.HoursTopPerf$pred.NormalDLR.svmLinear, 
                             metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                       "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.svmLinear)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$NormalDLR, x = mean.HoursTopPerf$pred.NormalDLR.svmLinear, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.svmLinear)
plot(mean.HoursTopPerf$NormalDLR~mean.HoursTopPerf$pred.NormalDLR.svmLinear, main="svmLinear Performance", 
     xlab="NormalDLR predicted by svmLinear", ylab="NormalDLR")

save(model.NormalDLR.svmLinear, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/',
                                          'svmLinear.NormalDLR.model',sep=''))
                                #/svmLinear

                                  #bagEarth
time.NormalDLR.bagEarth <- proc.time()
# define training control
train_control.bagEarth <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
bagEarth.Grid<-expand.grid(degree=10, nprune=14)
# train the model 
model.NormalDLR.bagEarth <- train(NormalDLR~., data=mean.HoursTop, trControl=train_control.bagEarth,
                                 method="bagEarth", tuneLength = 8)#, tuneGrid = bagEarth.Grid) #tuneLength does not matter
proc.time() - time.NormalDLR.bagEarth
# make predictions
mean.HoursTopPerf$pred.NormalDLR.bagEarth<-predict(model.NormalDLR.bagEarth, mean.HoursTop[,1:9])
#record the errors
mean.HoursTopPerf$err.NormalDLR.bagEarth<-abs(100*(mean.HoursTopPerf$NormalDLR-mean.HoursTopPerf$pred.NormalDLR.bagEarth)/
                                               mean.HoursTop$NormalDLR)
histogram(mean.HoursTopPerf$err.NormalDLR.bagEarth)
#show error metric
errMetric.bagEarth<-mmetric(y=mean.HoursTopPerf$NormalDLR, x = mean.HoursTopPerf$pred.NormalDLR.bagEarth, 
                            metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                      "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.bagEarth)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$NormalDLR, x = mean.HoursTopPerf$pred.NormalDLR.bagEarth, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.bagEarth)
plot(mean.HoursTopPerf$NormalDLR~mean.HoursTopPerf$pred.NormalDLR.bagEarth, main="bagEarth Performance", 
     xlab="NormalDLR predicted by bagEarth", ylab="NormalDLR")

save(model.NormalDLR.bagEarth, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/',
                                         'bagEarth.NormalDLR.model',sep=''))
                                          #/bagEarth

                                              #brnn
time.NormalDLR.brnn <- proc.time()
# define training control
train_control.brnn <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
brnn.Grid<-expand.grid(neurons = 20)
# train the model 
model.NormalDLR.brnn <- train(NormalDLR~., data=mean.HoursTop, trControl=train_control.brnn,
                             method="brnn", tuneLength = 8)#, tuneGrid = brnn.Grid) #tuneLength does not matter
proc.time() - time.NormalDLR.brnn
# make predictions
mean.HoursTopPerf$pred.NormalDLR.brnn<-predict(model.NormalDLR.brnn, mean.HoursTop[,1:9])
#record the errors
mean.HoursTopPerf$err.NormalDLR.brnn<-abs(100*(mean.HoursTopPerf$NormalDLR-mean.HoursTopPerf$pred.NormalDLR.brnn)/
                                           mean.HoursTop$NormalDLR)
histogram(mean.HoursTopPerf$err.NormalDLR.brnn)
#show error metric
errMetric.brnn<-mmetric(y=mean.HoursTopPerf$NormalDLR, x = mean.HoursTopPerf$pred.NormalDLR.brnn, 
                        metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                  "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.brnn)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$NormalDLR, x = mean.HoursTopPerf$pred.NormalDLR.brnn, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.brnn)
plot(mean.HoursTopPerf$NormalDLR~mean.HoursTopPerf$pred.NormalDLR.brnn, main="brnn Performance", 
     xlab="NormalDLR predicted by brnn", ylab="NormalDLR")

save(model.NormalDLR.brnn, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/',
                                     'brnn.NormalDLR.model',sep=''))
                                        #/brnn

load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/cubist.NormalDLR.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/bagEarth.NormalDLR.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/bagEarthGCV.NormalDLR.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/brnn.NormalDLR.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/gcvEarth.NormalDLR.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/model.NormalDLR.Formula.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/svmLinear.NormalDLR.model")











