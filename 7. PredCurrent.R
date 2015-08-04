### KTH, Department of Industrial Information and Control Systems (ICS)
### Master Thesis Project
### BUILDING PREDICTIVE MODELS FOR DYNAMIC LINE RATING USING DATA SCIENCE TECHNIQUES 
### Sponsored by Vattenfall AG
### Author: Nicolae Doban
### Stockholm, 2015

## This module builds the hourly mean values and builds the predictive models on the historical observations
## Packages used: base, caret, boot, DAAG, forecast, rminer, splines, DAAG
## INPUT DATA: The Hourly mean dataset mean.Hours.csv which will serve as training set
## OUTPUT: Building the Predictive Models for the Current parameter using historical observations

#The predictive models will be built using the mean.Hours dataset
#Firstly, the predictive models are built for Current parameter
#The folllowing code trains the models. Later the models will be tested on April dataset.
#mean.Hours.TopPerf will be used for comparing the predictive models' performance whereas
#the mean.Hours will be kept intact and will be used for building the models from its observations
#The comments from the first predictive model (Linear Model) apply also to the other predictive models
#There were built more than 30 predictive models out of which only a few were chosen for usage.

          #Explanation of rationales on building predictive models for Current parameter
          
          #However, in order to predict the Current given as input the weather parameters, 
          #there were built several predictive models which took into account not only the weather 
          #parameters but also the hour of the day, type of day (week-/weekend) and day of the week. 
          #Out of eleven predictive models there was chosen only one which presented higher accuracy 
          #on the test dataset (April dataset)- cubist model. There is no benchmarking done between 
          #other models because the other models (10 models) had a very poor performance (MAPE of 
          #orders of 10^54%) within the training dataset and therefore, are not analyzed furthemore. 
          #The reason of such low accuracy is that the Current is very loosely dependant on weather 
          #and temporal parameters. The chosen model for testing is cubist because of its lower error rate.
          #In "10. AprilTest.R" the Current cubist model is tested


setwd("C:/Users/LabStudent/Desktop/Nick/GitHub")
mean.Hours<-read.csv("mean.Hours.csv", sep=",", header=TRUE)

library(caret)
library(rminer)
library(boot)
library(forecast)
library(splines)
library(DAAG)

#build a DataTime column because once read from the csv file, the DateTime column was not in "POSIXt" format
mean.Hours$DateTime<-seq.POSIXt(from=as.POSIXct("2014-09-01 00:00:00", tz="Etc/GMT+12"), 
                                to=as.POSIXct("2015-03-31 23:00:00",tz="Etc/GMT+12"), by=3600)
mean.Hours$Hours<-format(mean.Hours$DateTime, "%H")
mean.Hours$Days<-weekdays(mean.Hours$DateTime)
mean.Hours$Days<-factor(mean.Hours$Days, levels=c("måndag", "tisdag", "onsdag", "torsdag", "fredag",
                                    "lördag", "söndag"))  #Swedish

#There were identified inconclusive data. 
#This block investigates the situation and omits the inconclusive observations
mean.Hours<-mean.Hours[-c(1:336), ]; rownames(mean.Hours)<-NULL     #skip the mean values of NormalDLR.
#/There were identified inconclusive data. 
#/This block investigates the situation and omits the inconclusive observations
mean.Hours$Hours<-as.numeric(mean.Hours$Hours)


#Create type of Day column
x <- c(rep("Weekday",24), rep("Weekday",24), rep("Weekday",24), rep("Weekday",24), rep("Weekday",24), rep("Weekend",24), rep("Weekend",24))
xW<-rep(x,28)
rest<-rep("Weekday",48)

mean.Hours$daytype<-c(xW,rest)
mean.Hours$daytype<-factor(mean.Hours$daytype)
#/Create type of Day column

mean.Hours<-mean.Hours[,c(1,2,3,4,5,9,10,11,7,6,8)] #re-arrange the columns
mean.HoursTop<-mean.Hours[,2:9] #mean.HoursTop will be used for benchmarking models' performances


mean.HoursTopPerf<-mean.HoursTop #Build "mean.HoursTopPerf" to compare the predicted and real values

                                                #Linear model
fit.Current.lm=lm(Current~I(WindSpeed^1)+I(WindDir^1)+I(AmbTemp^1)+
                    I(SolarRad^1)+I(Hours^1)*daytype*I(WindSpeed^1),
                  data=mean.HoursTop); accuracy(f=fit.Current.lm) #MAPE 2.064317e+54 too large  error- there is no reason to compare it furthermore
                                                  #/Linear model
                                  #cubist
time.Current.cubist <- proc.time()
# define training control
train_control.cubist <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
cubist.Grid<-expand.grid(committees = 1, neighbors = 1) #highest accuracy for committees = 1, neighbors = 1
#train the model
model.Current.cubist <- train(Current~., data=mean.HoursTop, trControl=train_control.cubist,
                               method="cubist", tuneGrid = cubist.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.Current.cubist
# make predictions
mean.HoursTopPerf$pred.Current.cubist<-predict(model.Current.cubist, mean.HoursTop[,1:7]) #store the predictions
#record the errors
mean.HoursTopPerf$err.Current.cubist<-abs(100*(mean.HoursTopPerf$Current-mean.HoursTopPerf$pred.Current.cubist)/
                                            mean.HoursTop$Current) #calculate the MAPE
histogram(mean.HoursTopPerf$err.Current.cubist) #investigate the performance of the model on the training set
#show error metric
errMetric.cubist<-mmetric(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.cubist, 
                          metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                    "MAPE", "MdAPE"), aggregate="mean") #build an error metric
print(errMetric.cubist)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.cubist, graph="REC", 
       col = "red") #Build REC curve
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.cubist)
plot(mean.HoursTopPerf$Current~mean.HoursTopPerf$pred.Current.cubist, main="cubist Performance", 
     xlab="Current predicted by cubist", ylab="Current") #see the correlation between real and predicted values

#save the model
save(model.Current.cubist, file=paste('C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/Current/',
                                 'cubist.Current.model',sep=''))
                                              #/cubist


##As written in the header of the current script, the Cubist model has the lowest error rate on the training set
##whereas the other models behaved more poorly. A simple validation of this can be done running the syntax 
##"summary(mean.HoursTopPerf)" after all the following models are ran. In the summary there will be seen the huge
##error rates of the other predictive models. So, the chosen model for Current parameter is "cubist". 

                                                #bagEarthGCV
time.Current.bagEarthGCV <- proc.time()
# define training control
train_control.bagEarthGCV <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
bagEarthGCV.Grid<-expand.grid(degree=6)
model.Current.bagEarthGCV <- train(Current~., data=mean.HoursTop, trControl=train_control.bagEarthGCV,
                              method="bagEarthGCV", tuneGrid = bagEarthGCV.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.Current.bagEarthGCV
# make predictions
mean.HoursTopPerf$pred.Current.bagEarthGCV<-predict(model.Current.bagEarthGCV, mean.HoursTop[,1:7])
#record the errors
mean.HoursTopPerf$err.Current.bagEarthGCV<-abs(100*(mean.HoursTopPerf$Current-mean.HoursTopPerf$pred.Current.bagEarthGCV)/
                                            mean.HoursTop$Current)
histogram(mean.HoursTopPerf$err.Current.bagEarthGCV)
#show error metric
errMetric.bagEarthGCV<-mmetric(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.bagEarthGCV, 
                          metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                    "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.bagEarthGCV)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.bagEarthGCV, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.bagEarthGCV)
plot(mean.HoursTopPerf$Current~mean.HoursTopPerf$pred.Current.bagEarthGCV, main="bagEarthGCV Performance", 
     xlab="Current predicted by bagEarthGCV", ylab="Current")
                                  #/bagEarthGCV

                                                      #gcvEarth
time.Current.gcvEarth <- proc.time()
# define training control
train_control.gcvEarth <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
gcvEarth.Grid<-expand.grid(degree=10)
model.Current.gcvEarth <- train(Current~., data=mean.HoursTop, trControl=train_control.gcvEarth,
                                   method="gcvEarth", tuneGrid = gcvEarth.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.Current.gcvEarth
# make predictions
mean.HoursTopPerf$pred.Current.gcvEarth<-predict(model.Current.gcvEarth, mean.HoursTop[,1:7])
#record the errors
mean.HoursTopPerf$err.Current.gcvEarth<-abs(100*(mean.HoursTopPerf$Current-mean.HoursTopPerf$pred.Current.gcvEarth)/
                                                 mean.HoursTop$Current)
histogram(mean.HoursTopPerf$err.Current.gcvEarth)
#show error metric
errMetric.gcvEarth<-mmetric(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.gcvEarth, 
                               metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                         "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.gcvEarth)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.gcvEarth, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.gcvEarth)
plot(mean.HoursTopPerf$Current~mean.HoursTopPerf$pred.Current.gcvEarth, main="gcvEarth Performance", 
     xlab="Current predicted by gcvEarth", ylab="Current")
                                                #/gcvEarth

                                                    #gam
time.Current.gam <- proc.time()
# define training control
train_control.gam <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
gam.Grid<-expand.grid(select = TRUE, method = "GCV.Cp")
model.Current.gam <- train(Current~., data=mean.HoursTop, trControl=train_control.gam,
                                method="gam", tuneGrid = gam.Grid, tuneLength = 8) #tuneLength does not matter
proc.time() - time.Current.gam
# make predictions
mean.HoursTopPerf$pred.Current.gam<-predict(model.Current.gam, mean.HoursTop[,1:7])
#record the errors
mean.HoursTopPerf$err.Current.gam<-abs(100*(mean.HoursTopPerf$Current-mean.HoursTopPerf$pred.Current.gam)/
                                              mean.HoursTop$Current)
histogram(mean.HoursTopPerf$err.Current.gam)
#show error metric
errMetric.gam<-mmetric(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.gam, 
                            metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                      "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.gam)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.gam, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.gam)
plot(mean.HoursTopPerf$Current~mean.HoursTopPerf$pred.Current.gam, main="gam Performance", 
     xlab="Current predicted by gam", ylab="Current")
                                  #/gam

                                                        #svmLinear
time.Current.svmLinear <- proc.time()
# define training control
train_control.svmLinear <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
# train the model 
model.Current.svmLinear <- train(Current~., data=mean.HoursTop, trControl=train_control.svmLinear,
                           method="svmLinear", tuneLength = 8)#,tuneGrid = svmLinear.Grid) #tuneLength does not matter
proc.time() - time.Current.svmLinear
# make predictions
mean.HoursTopPerf$pred.Current.svmLinear<-predict(model.Current.svmLinear, mean.HoursTop[,1:7])
#record the errors
mean.HoursTopPerf$err.Current.svmLinear<-abs(100*(mean.HoursTopPerf$Current-mean.HoursTopPerf$pred.Current.svmLinear)/
                                         mean.HoursTop$Current)
histogram(mean.HoursTopPerf$err.Current.svmLinear)
#show error metric
errMetric.svmLinear<-mmetric(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.svmLinear, 
                       metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                 "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.svmLinear)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.svmLinear, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.svmLinear)
plot(mean.HoursTopPerf$Current~mean.HoursTopPerf$pred.Current.svmLinear, main="svmLinear Performance", 
     xlab="Current predicted by svmLinear", ylab="Current")
                                                #/svmLinear

                                        #bagEarth
time.Current.bagEarth <- proc.time()
# define training control
train_control.bagEarth <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
bagEarth.Grid<-expand.grid(degree=10, nprune=14)
# train the model 
model.Current.bagEarth <- train(Current~., data=mean.HoursTop, trControl=train_control.bagEarth,
                                 method="bagEarth", tuneLength = 8,tuneGrid = bagEarth.Grid) #tuneLength does not matter
proc.time() - time.Current.bagEarth
# make predictions
mean.HoursTopPerf$pred.Current.bagEarth<-predict(model.Current.bagEarth, mean.HoursTop[,1:7])
#record the errors
mean.HoursTopPerf$err.Current.bagEarth<-abs(100*(mean.HoursTopPerf$Current-mean.HoursTopPerf$pred.Current.bagEarth)/
                                               mean.HoursTop$Current)
histogram(mean.HoursTopPerf$err.Current.bagEarth)
#show error metric
errMetric.bagEarth<-mmetric(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.bagEarth, 
                             metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                       "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.bagEarth)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.bagEarth, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.bagEarth)
plot(mean.HoursTopPerf$Current~mean.HoursTopPerf$pred.Current.bagEarth, main="bagEarth Performance", 
     xlab="Current predicted by bagEarth", ylab="Current")
                                      #/bagEarth

                                                      #brnn
time.Current.brnn <- proc.time()
# define training control
train_control.brnn <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
brnn.Grid<-expand.grid(neurons = 20)
# train the model 
model.Current.brnn <- train(Current~., data=mean.HoursTop, trControl=train_control.brnn,
                                method="brnn", tuneLength = 8,tuneGrid = brnn.Grid) #tuneLength does not matter
proc.time() - time.Current.brnn
# make predictions
mean.HoursTopPerf$pred.Current.brnn<-predict(model.Current.brnn, mean.HoursTop[,1:7])
#record the errors
mean.HoursTopPerf$err.Current.brnn<-abs(100*(mean.HoursTopPerf$Current-mean.HoursTopPerf$pred.Current.brnn)/
                                             mean.HoursTopPerf$Current)
histogram(mean.HoursTopPerf$err.Current.brnn)
#show error metric
errMetric.brnn<-mmetric(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.brnn, 
                            metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                      "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.brnn)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.brnn, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.brnn)
plot(mean.HoursTopPerf$Current~mean.HoursTopPerf$pred.Current.brnn, main="brnn Performance", 
     xlab="Current predicted by brnn", ylab="Current")
                                                    #/brnn

                                              #cforest
time.Current.cforest <- proc.time()
# define training control
train_control.cforest <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
cforest.Grid<-expand.grid(mtry = 7)
# train the model 
model.Current.cforest <- train(Current~., data=mean.HoursTop, trControl=train_control.cforest,
                            method="cforest", tuneLength = 8,tuneGrid = cforest.Grid) #tuneLength does not matter
proc.time() - time.Current.cforest
# make predictions
mean.HoursTopPerf$pred.Current.cforest<-predict(model.Current.cforest, mean.HoursTop[,1:7])
#record the errors
mean.HoursTopPerf$err.Current.cforest<-abs(100*(mean.HoursTopPerf$Current-mean.HoursTopPerf$pred.Current.cforest)/
                                          mean.HoursTopPerf$Current)
histogram(mean.HoursTopPerf$err.Current.cforest)
#show error metric
errMetric.cforest<-mmetric(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.cforest, 
                        metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                  "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.cforest)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.cforest, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.cforest)
plot(mean.HoursTopPerf$Current~mean.HoursTopPerf$pred.Current.cforest, main="cforest Performance", 
     xlab="Current predicted by cforest", ylab="Current")
                                    #/cforest
                                                      #qrf
  time.Current.qrf <- proc.time()
  # define training control
  train_control.qrf <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
  qrf.Grid<-expand.grid(mtry = 7)
  # train the model 
  model.Current.qrf <- train(Current~., data=mean.HoursTop, trControl=train_control.qrf,
                               method="qrf", tuneLength = 8,tuneGrid = qrf.Grid) #tuneLength does not matter
  proc.time() - time.Current.qrf
  # make predictions
  mean.HoursTopPerf$pred.Current.qrf<-predict(model.Current.qrf, mean.HoursTop[,1:7])
  #record the errors
  mean.HoursTopPerf$err.Current.qrf<-abs(100*(mean.HoursTopPerf$Current-mean.HoursTopPerf$pred.Current.qrf)/
                                             mean.HoursTopPerf$Current)
  histogram(mean.HoursTopPerf$err.Current.qrf)
  #show error metric
  errMetric.qrf<-mmetric(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.qrf, 
                           metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                     "MAPE", "MdAPE"), aggregate="mean")
  print(errMetric.qrf)
  #/show error metric
  #plot REC curve
  mgraph(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.qrf, graph="REC", 
         col = "red")
  #/plot REC curve
  #plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.qrf)
  plot(mean.HoursTopPerf$Current~mean.HoursTopPerf$pred.Current.qrf, main="qrf Performance", 
       xlab="Current predicted by qrf", ylab="Current")
                                      #/qrf

                                #RRF
time.Current.RRF <- proc.time()
# define training control
train_control.RRF <- trainControl(method="repeatedcv", number=10, repeats=3, p = 0.75)
RRF.Grid<-expand.grid(mtry = 7, coefReg = 1, coefImp = 10000000)
# train the model 
model.Current.RRF <- train(Current~., data=mean.HoursTop, trControl=train_control.RRF,
                           method="RRF", tuneLength = 8,tuneGrid = RRF.Grid) #tuneLength does not matter
proc.time() - time.Current.RRF
# make predictions
mean.HoursTopPerf$pred.Current.RRF<-predict(model.Current.RRF, mean.HoursTop[,1:7])
#record the errors
mean.HoursTopPerf$err.Current.RRF<-abs(100*(mean.HoursTopPerf$Current-mean.HoursTopPerf$pred.Current.RRF)/
                                         mean.HoursTopPerf$Current)
histogram(mean.HoursTopPerf$err.Current.RRF)
#show error metric
errMetric.RRF<-mmetric(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.RRF, 
                       metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                 "MAPE", "MdAPE"), aggregate="mean")
print(errMetric.RRF)
#/show error metric
#plot REC curve
mgraph(y=mean.HoursTopPerf$Current, x = mean.HoursTopPerf$pred.Current.RRF, graph="REC", 
       col = "red")
#/plot REC curve
#plot(mean.HoursTopTop5$NormalDLR~mean.HoursTopTop5$pred.RRF)
plot(mean.HoursTopPerf$Current~mean.HoursTopPerf$pred.Current.RRF, main="RRF Performance", 
     xlab="Current predicted by RRF", ylab="Current")
                                  #/RRF