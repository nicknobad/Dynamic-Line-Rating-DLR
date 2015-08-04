### KTH, Department of Industrial Information and Control Systems (ICS)
### Master Thesis Project
### BUILDING PREDICTIVE MODELS FOR DYNAMIC LINE RATING USING DATA SCIENCE TECHNIQUES 
### Sponsored by Vattenfall AG
### Author: Nicolae Doban
### Stockholm, 2015

## This module builds the hourly mean values and builds the predictive models on the historical observations
## Packages used: base, histogram, reshape, zoo, splines, caret
## INPUT DATA: The minute-based dataset of April, 2015
## OUTPUT: Benchmarking the predictive models for Current, CondTemp and NormalDLR

##The script-block from 16th to 200th line is similar to "1. Processing.R"- it processes and uses similar
##logic conditions on the April dataset. Later until the 242nd line, the hourly mean values of the April dataset
##are built. The following script lines runs the models (which were built on training dataset) on the test 
##dataset i.e. April dataset

library(histogram)
setwd("C:/Users/LabStudent/Desktop/Nick/GitHub")
April <- read.csv("Datafile_Doban_3April.csv", header=TRUE, na.strings=c("No Data"))
NrOFDays.April<-30
April <- head(April, NrOFDays.April*24*60)
#Deleting the unnecessary columns
April$CTM_DLR=April$X.1=April$Donut2_Tension=April$X=
  April$Donut2_Sag=April$Donut2_Inclinometer=
  April$Donut2_Low_Point=April$X.2=
  April$ZL8WSHumidity=April$ZL8WSBarometricPressure<-NULL
#/Deleting the unnecessary columns
histogram(April$NormalDLR)

#Build the temporal variables
April$Date<-as.Date(April$Date, "%m/%d/%Y")
April$Time<-as.character(April$Time)
April$DateTime<-paste(April$Date, April$Time, sep=" ")
April$DateTime<-strptime(April$DateTime, "%Y-%m-%d %H:%M")
April$Hours<-strftime(April$DateTime, "%H")

#April$Time<-as.character(April$Time)
April$Month<-months(April$Date)
#Create and re-arrange the DateTime column    used for plotting versus hours

April$Days<-weekdays(as.Date(April$Date, format="%Y-%m-%d"))
#/Build the temporal variables
library(zoo)
library(splines)
interp<-data.frame()
interp<-data.frame(Date=April$Date, Time=April$Time, #SolarRad=April$SolarRad, 
                   interpSolarRad=na.approx(April$SolarRad), 
                   #ErrorSolarRad=abs(((na.approx(April$SolarRad)-April$SolarRad)/April$SolarRad)*100),
                   #WindDir=April$WindDir, 
                   interpWindDir=na.approx(April$WindDir), 
                   #ErrorWindDir=abs(((na.approx(April$WindDir)-April$WindDir)/April$WindDir)*100),
                   #AmbTemp=April$AmbTemp, 
                   interpAmbTemp=na.spline(April$AmbTemp), 
                   #ErrorAmbTemp=abs(((na.spline(April$AmbTemp)-April$AmbTemp)/April$AmbTemp)*100),
                   #WindSpeed=April$WindSpeed, 
                   interpWindSpeed=na.approx(April$WindSpeed), 
                   #ErrorWindSpeed=abs(((na.approx(April$WindSpeed)-April$WindSpeed)/April$WindSpeed)*100),
                   #CondTemp=April$CondTemp, 
                   interpCondTemp=na.spline(April$CondTemp), 
                   #ErrorCondTemp=abs(((na.spline(April$CondTemp)-April$CondTemp)/April$CondTemp)*100),
                   #Current=April$Current, 
                   interpCurrent=na.approx(April$Current), 
                   #ErrorCurrent=abs(((na.approx(April$Current)-April$Current)/April$Current)*100),
                   #NormalDLR=April$NormalDLR, 
                   interpNormalDLR=na.spline(April$NormalDLR) 
                   #ErrorNormalDLR=abs(((na.spline(April$NormalDLR)-April$NormalDLR)/April$NormalDLR)*100),
                   ) 

interp$Date<-as.character(interp$Date)
interp$DateTime<-paste(interp$Date, interp$Time, sep=" ")
interp$DateTime<-strptime(interp$DateTime, "%Y-%m-%d %H:%M")
interp$Hours<-strftime(interp$DateTime, "%H")

#interp$Hours<-as.character(interp$Hours)
interp$indexDT<-as.factor(as.character(interp$DateTime))
#interp$DateTime<-as.character(interp$DateTime)
interp$Days<-weekdays(as.Date(interp$Date, format="%Y-%m-%d"))

#Dealing with strange data

#Condition on WindSpeed
ptm <- proc.time()
interp$indexDT<-as.numeric(interp$indexDT)
for(i in 1:length(interp$interpWindSpeed)){
  if(interp$interpWindSpeed[i]< 0.1){
    interp$interpWindSpeed[i]<-interp$interpWindSpeed[i+1]+((interp$indexDT[i]-interp$indexDT[i+1])*(interp$interpWindSpeed[i-1]-interp$interpWindSpeed[i+1]))/(interp$indexDT[i-1]-interp$indexDT[i+1])
  }
  if(interp$interpWindSpeed[i]>10){
    interp$interpWindSpeed[i]<-interp$interpWindSpeed[i+1]+((interp$indexDT[i]-interp$indexDT[i+1])*(interp$interpWindSpeed[i-1]-interp$interpWindSpeed[i+1]))/(interp$indexDT[i-1]-interp$indexDT[i+1])
  }
}
proc.time() - ptm
#k<-data.frame(indexDT=interp$indexDT[interp$interpWindSpeed<0], WindSpeed=interp$interpWindSpeed[interp$interpWindSpeed<0])
#/Condition on WindSpeed

#Condition on AmbTemp
ptm <- proc.time()
interp$indexDT<-as.numeric(interp$indexDT)
for(i in 1:length(interp$interpAmbTemp)){
  if(interp$interpAmbTemp[i]< -45){
    interp$interpAmbTemp[i]<-interp$interpAmbTemp[i+1]+((interp$indexDT[i]-interp$indexDT[i+1])*(interp$interpAmbTemp[i-1]-interp$interpAmbTemp[i+1]))/(interp$indexDT[i-1]-interp$indexDT[i+1])
  }
  if(interp$interpAmbTemp[i]>40){
    interp$interpAmbTemp[i]<-interp$interpAmbTemp[i+1]+((interp$indexDT[i]-interp$indexDT[i+1])*(interp$interpAmbTemp[i-1]-interp$interpAmbTemp[i+1]))/(interp$indexDT[i-1]-interp$indexDT[i+1])
  }
}
proc.time() - ptm
#k<-data.frame(DateTime=interp$DateTime[interp$interpAmbTemp==NA], AmbTemp=interp$interpAmbTemp[interp$interpAmbTemp==NA])
#/Condition on AmbTemp

#Condition on SolarRad
# Start the clock!
ptm <- proc.time()
for(i in 1:length(interp$interpSolarRad)){
  if(interp$interpSolarRad[i]<0.01){
    interp$interpSolarRad[i]<-0
  }
  if(interp$interpSolarRad[i]>1400){
    #some kind of alarm ? 
    # measuring device malfunction
    #interp$interpSolarRad[i]<-0
  }
}
proc.time() - ptm
#/Condition on SolarRad

#Condition on WindDir
ptm <- proc.time()
for(i in 1:length(interp$interpWindDir)){
  if(interp$interpWindDir[i]<0.1){
    interp$interpWindDir[i]<-0
  }
  if(interp$interpWindDir[i]>360){
    interp$interpWindDir[i]<-360
  }
}
proc.time() - ptm
#/Condition on WindDir

#Condition on CondTemp
ptm <- proc.time()
for(i in 1:length(interp$interpCondTemp)){
  if(interp$interpCondTemp[i]< -20){
    interp$interpCondTemp[i]<-interp$interpCondTemp[i+1]+((interp$indexDT[i]-interp$indexDT[i+1])*(interp$interpCondTemp[i-1]-interp$interpCondTemp[i+1]))/(interp$indexDT[i-1]-interp$indexDT[i+1])
  }
  if(interp$interpCondTemp[i]>45){
    #send an signal
    #interp$interpCondTemp[i]<-interp$interpCondTemp[i+1]+((interp$indexDT[i]-interp$indexDT[i+1])*(interp$interpCondTemp[i-1]-interp$interpCondTemp[i+1]))/(interp$indexDT[i-1]-interp$indexDT[i+1])
  }
  if(interp$interpCondTemp[i]>48){
    #send an alarm
  }
}
proc.time() - ptm
#/Condition on CondTemp

interp$MonthYearAbb<-strftime(as.character(interp$Date), "%b, %Y")
interp$MonthYearAbb<-as.character(interp$MonthYearAbb)
#new <- interp[interp$MonthYearAbb %in% c("Sep, 2014", "Oct, 2014", "Nov, 2014", "Dec, 2014", "Jan, 2015", 
#                                         "Feb, 2015", "Mar, 2015"), ]  #English Language

interp$MonthYearAbb<-as.character(interp$MonthYearAbb)
new.April <- interp[interp$MonthYearAbb %in% c("apr, 2015"), ]   #Swedish language
rownames(new.April)<-NULL
new.April$Date<-as.character(new.April$Date)
new.April$DateTime<-paste(new.April$Date, new.April$Time, sep=" ")
new.April$DateTime<-strptime(new.April$DateTime, "%Y-%m-%d %H:%M")
new.April$Hours<-strftime(new.April$DateTime, "%H")

new.April$interpCTMDLR<-NULL
new.April$indexDT<-NULL

#Replacing NAs in NormalDLR with Mean NormalDLR of that day
ptm <- proc.time()
for(i in 1:length(new.April$interpNormalDLR)){
  if(is.na(new.April$interpNormalDLR[i])){
    new.April$interpNormalDLR[i]<-mean(April$interpNormalDLR[April$Days[i]==April$Days[i]], na.rm=TRUE)
  }
}
proc.time() - ptm
#/Replacing NAs in interpNormalDLR with Mean interpNormalDLR of that day

#Replacing NAs in Current with Mean Current of that day
ptm <- proc.time()
for(i in 1:length(new.April$interpCurrent)){
  if(is.na(new.April$interpCurrent[i])){
    new.April$interpCurrent[i]<-mean(April$interpCurrent[April$Days[i]==April$Days[i]], na.rm=TRUE)
  }
}
proc.time() - ptm
#/Replacing NAs in Current with Mean Current of that day
#Factor the Days
new.April$Days<-as.character(new.April$Days)
#new.April$Days<-factor(new.April$Days, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
#                                    "Saturday", "Sunday"))    #English
new.April$Days<-as.character(new.April$Days)
new.April$Days<-factor(new.April$Days, levels=c("måndag", "tisdag", "onsdag", "torsdag", "fredag",
                                    "lördag", "söndag"))  #Swedish

#Make MonthYearAbb a factor
new.April$MonthYearAbb<-as.character(new.April$MonthYearAbb)
new.April$MonthYearAbb<-factor(new.April$MonthYearAbb, levels=unique(new.April$MonthYearAbb))
#/Make MonthYearAbb a factor
                                                                  #/Processing
new.April$Hours<-NULL
new.April$Days<-NULL
new.April$MonthYearAbb<-NULL
new.April$Date<-NULL
new.April$Time<-NULL
new.April$Current<-NULL

new.April<-new.April[c("DateTime", "interpSolarRad", "interpWindDir", "interpAmbTemp", "interpWindSpeed", "interpCondTemp",
           "interpCurrent", "interpNormalDLR")]        #Re-arrange by columns' names
library(reshape)                         
new.April<-rename(new.April, c("interpSolarRad"="SolarRad", "interpWindDir"="WindDir", "interpAmbTemp"="AmbTemp",
                   "interpWindSpeed"="WindSpeed", "interpCondTemp"="CondTemp", "interpCurrent"="Current",
                   "interpNormalDLR"="NormalDLR"))

NEW.April.interp<-new.April #duplicate the tidy dataframe


#Better way to average the hourly values 
#because the above method does not give me a dataframe. The below method gives me a dataframe
SolarRad.Hourly.April<-aggregate(list(SolarRad = NEW.April.interp$SolarRad), 
                           list(DateHour = cut(NEW.April.interp$DateTime, "1 hour")), mean)
WindDir.Hourly.April<-aggregate(list(WindDir = NEW.April.interp$WindDir), 
                          list(DateHour = cut(NEW.April.interp$DateTime, "1 hour")), mean)
AmbTemp.Hourly.April<-aggregate(list(AmbTemp = NEW.April.interp$AmbTemp), 
                          list(DateHour = cut(NEW.April.interp$DateTime, "1 hour")), mean)
WindSpeed.Hourly.April<-aggregate(list(WindSpeed = NEW.April.interp$WindSpeed), 
                            list(DateHour = cut(NEW.April.interp$DateTime, "1 hour")), mean)
CondTemp.Hourly.April<-aggregate(list(CondTemp = NEW.April.interp$CondTemp), 
                           list(DateHour = cut(NEW.April.interp$DateTime, "1 hour")), mean)
Current.Hourly.April<-aggregate(list(Current = NEW.April.interp$Current), 
                          list(DateHour = cut(NEW.April.interp$DateTime, "1 hour")), mean)
NormalDLR.Hourly.April<-aggregate(list(NormalDLR = NEW.April.interp$NormalDLR), 
                            list(DateHour = cut(NEW.April.interp$DateTime, "1 hour")), mean)

mean.Hours.April<-data.frame(DateTime=SolarRad.Hourly.April$DateHour, SolarRad=SolarRad.Hourly.April$SolarRad,
                       WindDir=WindDir.Hourly.April$WindDir, AmbTemp=AmbTemp.Hourly.April$AmbTemp, 
                       WindSpeed=WindSpeed.Hourly.April$WindSpeed, CondTemp=CondTemp.Hourly.April$CondTemp,
                       Current=Current.Hourly.April$Current, NormalDLR=NormalDLR.Hourly.April$NormalDLR)
rownames(mean.Hours.April)<-NULL
#write.table(mean.Hours.April, "mean.Hours.April.csv", sep=",") #save to CSV file
#/Averaging the hourly dataframe from interpolated values

April.test<-mean.Hours.April
April.test$DateTime<-as.POSIXct(April.test$DateTime)
April.test$Days<-weekdays(April.test$DateTime)
April.test$Hours<-format(April.test$DateTime, "%H"); April.test$Hours<-as.numeric(April.test$Hours)
xApril <- c(rep("Weekday",24), rep("Weekday",24), rep("Weekday",24), rep("Weekend",24), rep("Weekend",24), rep("Weekday",24), rep("Weekday",24))
xW<-rep(xApril,4)
rest<-rep("Weekday",48)
April.test$daytype<-c(xW,rest)
April.test$daytype<-factor(April.test$daytype)

#re-arange columns so that (Current, CondTemp and NormalDLR) are the last ones
April.test<-April.test[c("DateTime", "SolarRad", "WindDir", "AmbTemp", "WindSpeed", "Days", "Hours", "daytype",  
                         "Current","CondTemp", "NormalDLR")] #re-arrange columns to match the ones in the training set

April.test$DateTime<-NULL
NormalDLR.April<-April.test$NormalDLR
#April.test$NormalDLR<-NULL
#testing Current's model on April
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/Current/cubist.Current.model")
April.test$pred.Current.cubist<-predict(model.Current.cubist, April.test)
April.test$err.Current.cubist<-abs(100*(April.test$Current-April.test$pred.Current.cubist)/
                                       April.test$Current)
#plot the real versus the predicted values
plot(April.test$Current~April.test$pred.Current.cubist, main="Measured vs Predicted Current Values. April Test Dataset", xlab="Predicted Current Values [A]", ylab="Measured Current Values [A]")

April.test$DateTime<-seq.POSIXt(from=as.POSIXct("2015-04-01 00:00:00", tz="Etc/GMT+12"), 
                                to=as.POSIXct("2015-04-30 23:00:00", tz="Etc/GMT+12"), by=3600)

#Compare the real and predicted values on a timescale
pdf("CurrentAprilTest.pdf", width=18, height=10)
plot(April.test$Current~April.test$DateTime, main="Predicted and real Current values",
     xlab="Time", ylab="Current [A]", cex.lab=1.7, cex.axis=1.7)
lines(col="red", April.test$pred.Current.cubist~April.test$DateTime)
legend("topleft", 
       c("Measured","Predicted. cubist"), # puts text in the legend 
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5, 2.5),col=c("black","red"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1)
dev.off()
#/Compare the real and predicted values on a timescale
#/testing Current's model on April
                                              #CondTemp
#load models for CondTemp
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/CondTemp/model.CondTemp.Formula.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/CondTemp/gcvEarth.CondTemp.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/CondTemp/brnn.CondTemp.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/CondTemp/bagEarth.CondTemp.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/CondTemp/svmLinear.CondTemp.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/CondTemp/bagEarthGCV.CondTemp.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/CondTemp/cubist.CondTemp.model")
#/load models for CondTemp

#testing CondTemp's models on April
April.test$pred.CondTemp.Formula <- predict(model.CondTemp.Formula, April.test)
April.test$err.CondTemp.Formula<-abs(100*(April.test$CondTemp-April.test$pred.CondTemp.Formula)/
                                       April.test$CondTemp)
April.test$pred.CondTemp.cubist <- predict(model.CondTemp.cubist, April.test)
April.test$err.CondTemp.cubist<-abs(100*(April.test$CondTemp-April.test$pred.CondTemp.cubist)/
                                       April.test$CondTemp)
April.test$pred.CondTemp.bagEarthGCV <- predict(model.CondTemp.bagEarthGCV, April.test)
April.test$err.CondTemp.bagEarthGCV<-abs(100*(April.test$CondTemp-April.test$pred.CondTemp.bagEarthGCV)/
                                      April.test$CondTemp)
April.test$pred.CondTemp.bagEarth <- predict(model.CondTemp.bagEarth, April.test)
April.test$err.CondTemp.bagEarth<-abs(100*(April.test$CondTemp-April.test$pred.CondTemp.bagEarth)/
                                           April.test$CondTemp)
April.test$pred.CondTemp.gcvEarth <- predict(model.CondTemp.gcvEarth, April.test)
April.test$err.CondTemp.gcvEarth<-abs(100*(April.test$CondTemp-April.test$pred.CondTemp.gcvEarth)/
                                        April.test$CondTemp)
April.test$pred.CondTemp.svmLinear <- predict(model.CondTemp.svmLinear, April.test)
April.test$err.CondTemp.svmLinear<-abs(100*(April.test$CondTemp-April.test$pred.CondTemp.svmLinear)/
                                        April.test$CondTemp)
April.test$pred.CondTemp.brnn <- predict(model.CondTemp.brnn, April.test)
April.test$err.CondTemp.brnn<-abs(100*(April.test$CondTemp-April.test$pred.CondTemp.brnn)/
                                         April.test$CondTemp)

summary(April.test) #analyze the performances of the models
#/testing CondTemp's models on April

                                                      #REC curves
#REC curves represent an indicator for model performance

#First REC graph
L=vector("list",4); pred=vector("list",1); test=vector("list",1);

pred[[1]]=April.test$CondTemp; 
test[[1]]=April.test$pred.CondTemp.Formula;     L[[1]]=list(pred=pred,test=test,runs=1)
test[[1]]=April.test$pred.CondTemp.cubist;      L[[2]]=list(pred=pred,test=test,runs=1)
test[[1]]=April.test$pred.CondTemp.bagEarthGCV; L[[3]]=list(pred=pred,test=test,runs=1)
test[[1]]=April.test$pred.CondTemp.bagEarth;    L[[4]]=list(pred=pred,test=test,runs=1)

CondTemp.Formula.NAREC = mmetric(y=April.test$CondTemp, x = April.test$pred.CondTemp.Formula, 
                                metric= "NAREC", aggregate="mean", val=5)
CondTemp.Formula.NAREC.curve = paste("REC, Formula=", round(CondTemp.Formula.NAREC, digits=5))

CondTemp.cubist.NAREC = mmetric(y=April.test$CondTemp, x = April.test$pred.CondTemp.cubist, 
                                metric= "NAREC", aggregate="mean", val=5)
CondTemp.cubist.NAREC.curve = paste("REC, cubist=", round(CondTemp.cubist.NAREC, digits=4))

CondTemp.bagEarthGCV.NAREC = mmetric(y=April.test$CondTemp, x = April.test$pred.CondTemp.bagEarthGCV, 
                                    metric= "NAREC", aggregate="mean", val=5)
CondTemp.bagEarthGCV.NAREC.curve = paste("REC, bagEarthGCV=", round(CondTemp.bagEarthGCV.NAREC, digits=4))

CondTemp.bagEarth.NAREC = mmetric(y=April.test$CondTemp, x = April.test$pred.CondTemp.bagEarth, 
                                  metric= "NAREC", aggregate="mean", val=5)
CondTemp.bagEarth.NAREC.curve = paste("REC, bagEarth=", round(CondTemp.bagEarth.NAREC, digits=4))

#Second REC graph
B=vector("list",3); pred=vector("list",1); test=vector("list",1);
pred[[1]]=April.test$CondTemp; 

test[[1]]=April.test$pred.CondTemp.gcvEarth;  B[[1]]=list(pred=pred,test=test,runs=1)
test[[1]]=April.test$pred.CondTemp.svmLinear; B[[2]]=list(pred=pred,test=test,runs=1)
test[[1]]=April.test$pred.CondTemp.brnn;      B[[3]]=list(pred=pred,test=test,runs=1)


CondTemp.gcvEarth.NAREC = mmetric(y=April.test$CondTemp, x = April.test$pred.CondTemp.gcvEarth, 
                         metric= "NAREC", aggregate="mean", val=5)
CondTemp.gcvEarth.NAREC.curve = paste("REC, gcvEarth=", round(CondTemp.gcvEarth.NAREC, digits=4))

CondTemp.svmLinear.NAREC = mmetric(y=April.test$CondTemp, x = April.test$pred.CondTemp.svmLinear, 
                          metric= "NAREC", aggregate="mean", val=5)
CondTemp.svmLinear.NAREC.curve = paste("REC, svmLinear=", round(CondTemp.svmLinear.NAREC, digits=4))

CondTemp.brnn.NAREC = mmetric(y=April.test$CondTemp, x = April.test$pred.CondTemp.brnn, 
                     metric= "NAREC", aggregate="mean", val=5)
CondTemp.brnn.NAREC.curve = paste("REC, brnn=", round(CondTemp.brnn.NAREC, digits=4))


#save to png            #png looks nicer than the pdf
png("CondTemp. REC-curves.png", width=1110, height=550)
split.screen(c(1,2))
screen(1)
L.graph<-mgraph(size=c(1,10), L, graph="REC", 
                leg=list(pos="bottomright",leg=c(CondTemp.gcvEarth.NAREC.curve,
                                                 CondTemp.svmLinear.NAREC.curve,
                                                 CondTemp.brnn.NAREC.curve,
                                                 CondTemp.bagEarth.NAREC.curve)),
                col=c("red", "blue", "gray1", "green"),
                main="CondTemp. REC curves")
screen(2)
B.graph<-mgraph(B, size=c(1,10), graph="REC", 
                leg=list(pos="bottomright",leg=c(CondTemp.Formula.NAREC.curve,
                                                 CondTemp.cubist.NAREC.curve,
                                                 CondTemp.bagEarthGCV.NAREC.curve)),
                col=c("gray1", "blue", "green", "red"),
                main="CondTemp. REC curves")

close.screen(all=TRUE)
dev.off()
#store the error metrics of the models on the test dataset
errMetric.CondTemp.brnn<-mmetric(y=April.test$CondTemp, x = April.test$pred.CondTemp.brnn, 
                        metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                  "MAPE", "MdAPE"), aggregate="mean")
errMetric.CondTemp.bagEarth<-mmetric(y=April.test$CondTemp, x = April.test$pred.CondTemp.bagEarth, 
                        metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                  "MAPE", "MdAPE"), aggregate="mean")
errMetric.CondTemp.bagEarthGCV<-mmetric(y=April.test$CondTemp, x = April.test$pred.CondTemp.bagEarthGCV, 
                            metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                      "MAPE", "MdAPE"), aggregate="mean")
errMetric.CondTemp.cubist<-mmetric(y=April.test$CondTemp, x = April.test$pred.CondTemp.cubist, 
                               metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                         "MAPE", "MdAPE"), aggregate="mean")
errMetric.CondTemp.Formula<-mmetric(y=April.test$CondTemp, x = April.test$pred.CondTemp.Formula, 
                          metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                    "MAPE", "MdAPE"), aggregate="mean")
errMetric.CondTemp.gcvEarth<-mmetric(y=April.test$CondTemp, x = April.test$pred.CondTemp.gcvEarth, 
                       metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                 "MAPE", "MdAPE"), aggregate="mean")
errMetric.CondTemp.svmLinear<-mmetric(y=April.test$CondTemp, x = April.test$pred.CondTemp.svmLinear, 
                        metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                  "MAPE", "MdAPE"), aggregate="mean")

errMatrix.CondTemp<-data.frame(errMetric.CondTemp.bagEarth,errMetric.CondTemp.brnn,errMetric.CondTemp.cubist,
                               errMetric.CondTemp.Formula,errMetric.CondTemp.gcvEarth,errMetric.CondTemp.svmLinear,
                               errMetric.CondTemp.bagEarthGCV)
errMatrix.CondTemp<-t(errMatrix.CondTemp)
#write.table(errMatrix.CondTemp,"errorMatrixCondTemp.csv", sep=",") #optional. Save the error metric table

errMatrix.CondTemp<-errMatrix.CondTemp[,c(7,9,10,1,2,3,4,5,6,8,11)] #re-arrange the error metric table with the desired columns more to the left
#order the error metric matrix
errMatrix.CondTemp.Ordered<-errMatrix.CondTemp[ order(-errMatrix.CondTemp[,1], errMatrix.CondTemp[,2], 
                                                errMatrix.CondTemp[,3]),]
View(errMatrix.CondTemp.Ordered) #View the ordered error metric matrix. 

                                            #/REC curves

pdf("ScatterplotsCondTempAprilTest.pdf", width=18, height=10)
par(mfrow=c(3,3))
plot(April.test$CondTemp~April.test$pred.CondTemp.Formula, main="CondTemp. Measured vs Predicted by the linear model",
     ylab="Measured values [C]", xlab="Predicted values [C]", cex.lab=1.7, cex.axis=1.7)
plot(April.test$CondTemp~April.test$pred.CondTemp.cubist, main="CondTemp. Measured vs Predicted by the cubist model",
     ylab="Measured values [C]", xlab="Predicted values [C]", cex.lab=1.7, cex.axis=1.7)
plot(April.test$CondTemp~April.test$pred.CondTemp.bagEarthGCV, main="CondTemp. Measured vs Predicted by the bagEarthGCV model",
     ylab="Measured values [C]", xlab="Predicted values [C]", cex.lab=1.7, cex.axis=1.7)
plot(April.test$CondTemp~April.test$pred.CondTemp.bagEarth, main="CondTemp. Measured vs Predicted by the bagEarth model",
     ylab="Measured values [C]", xlab="Predicted values [C]", cex.lab=1.7, cex.axis=1.7)
plot(April.test$CondTemp~April.test$pred.CondTemp.brnn, main="CondTemp. Measured vs Predicted by the brnn model",
     ylab="Measured values [C]", xlab="Predicted values [C]", cex.lab=1.7, cex.axis=1.7)
plot(April.test$CondTemp~April.test$pred.CondTemp.gcvEarth, main="CondTemp. Measured vs Predicted by the gcvEarth model",
     ylab="Measured values [C]", xlab="Predicted values [C]", cex.lab=1.7, cex.axis=1.7)
plot(April.test$CondTemp~April.test$pred.CondTemp.svmLinear, main="CondTemp. Measured vs Predicted by the svmLinear model",
     ylab="Measured values [C]", xlab="Predicted values [C]", cex.lab=1.7, cex.axis=1.7)
dev.off()

#bagEarthGCV the best model; must plot pred.CondTemp.bagEarthGCV and COndTemp against time  
April.test$DateTime<-seq.POSIXt(from=as.POSIXct("2015-04-01 00:00:00", tz="Etc/GMT+12"), 
                                to=as.POSIXct("2015-04-30 23:00:00", tz="Etc/GMT+12"), by=3600)

pdf("CondTempAprilTest.pdf", width=18, height=10)
#plot the most accurate models vs measured 
plot(col="black", April.test$CondTemp~April.test$DateTime, main="Predicted and measured Conductor Temperature",
     xlab="Time", ylab="Conductor Temperature [C]", cex.lab=1.7, cex.axis=1.7)
lines(col="blue", April.test$pred.CondTemp.bagEarthGCV~April.test$DateTime)
lines(col="green", April.test$pred.CondTemp.brnn~April.test$DateTime)
lines(col="red", April.test$pred.CondTemp.svmLinear~April.test$DateTime)
legend("topleft", 
       c("Measured","Predicted. bagEarthGCV", "Predicted. brnn", "Predicted. svmLinear"), # puts text in the legend 
       lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5,2.5, 2.5),col=c("black","red", "green", "blue"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1)
#/plot the most accurate models vs measured 
#/bagEarthGCV the best model; must plot pred.CondTemp.cubist and COndTemp against time  
dev.off()

                                        #bagEarthGCV the best for CondTemp
                            #/CondTemp


                                              #NormalDLR

                    #load models for NormalDLR

load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/cubist.NormalDLR.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/bagEarth.NormalDLR.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/bagEarthGCV.NormalDLR.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/brnn.NormalDLR.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/gcvEarth.NormalDLR.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/model.NormalDLR.Formula.model")
load("C:/Users/LabStudent/Desktop/Nick/GitHub/SavedModels/PredModHistoricalObs/NormalDLR/svmLinear.NormalDLR.model")

                        #/load models for NormalDLR

                            #testing NormalDLR's models on April
April.test$pred.NormalDLR.Formula <- predict(model.NormalDLR.Formula, April.test)
April.test$err.NormalDLR.Formula<-abs(100*(April.test$NormalDLR-April.test$pred.NormalDLR.Formula)/
                                       April.test$NormalDLR)
April.test$pred.NormalDLR.cubist <- predict(model.NormalDLR.cubist, April.test)
April.test$err.NormalDLR.cubist<-abs(100*(April.test$NormalDLR-April.test$pred.NormalDLR.cubist)/
                                      April.test$NormalDLR)
April.test$pred.NormalDLR.bagEarthGCV <- predict(model.NormalDLR.bagEarthGCV, April.test)
April.test$err.NormalDLR.bagEarthGCV<-abs(100*(April.test$NormalDLR-April.test$pred.NormalDLR.bagEarthGCV)/
                                           April.test$NormalDLR)
April.test$pred.NormalDLR.bagEarth <- predict(model.NormalDLR.bagEarth, April.test)
April.test$err.NormalDLR.bagEarth<-abs(100*(April.test$NormalDLR-April.test$pred.NormalDLR.bagEarth)/
                                        April.test$NormalDLR)
April.test$pred.NormalDLR.gcvEarth <- predict(model.NormalDLR.gcvEarth, April.test)
April.test$err.NormalDLR.gcvEarth<-abs(100*(April.test$NormalDLR-April.test$pred.NormalDLR.gcvEarth)/
                                        April.test$NormalDLR)
April.test$pred.NormalDLR.svmLinear <- predict(model.NormalDLR.svmLinear, April.test)
April.test$err.NormalDLR.svmLinear<-abs(100*(April.test$NormalDLR-April.test$pred.NormalDLR.svmLinear)/
                                         April.test$NormalDLR)
April.test$pred.NormalDLR.brnn <- predict(model.NormalDLR.brnn, April.test)
April.test$err.NormalDLR.brnn<-abs(100*(April.test$NormalDLR-April.test$pred.NormalDLR.brnn)/
                                    April.test$NormalDLR)

summary(April.test)
                              #/testing NormalDLR's models on April

                                    #REC curves
#First REC graph
L=vector("list",4); pred=vector("list",1); test=vector("list",1);

pred[[1]]=April.test$NormalDLR; 
test[[1]]=April.test$pred.NormalDLR.Formula;     L[[1]]=list(pred=pred,test=test,runs=1)
test[[1]]=April.test$pred.NormalDLR.cubist;      L[[2]]=list(pred=pred,test=test,runs=1)
test[[1]]=April.test$pred.NormalDLR.bagEarthGCV; L[[3]]=list(pred=pred,test=test,runs=1)
test[[1]]=April.test$pred.NormalDLR.bagEarth;    L[[4]]=list(pred=pred,test=test,runs=1)

NormalDLR.Formula.NAREC = mmetric(y=April.test$NormalDLR, x = April.test$pred.NormalDLR.Formula, 
                                 metric= "NAREC", aggregate="mean", val=5)
NormalDLR.Formula.NAREC.curve = paste("REC, Formula=", round(NormalDLR.Formula.NAREC, digits=5))

NormalDLR.cubist.NAREC = mmetric(y=April.test$NormalDLR, x = April.test$pred.NormalDLR.cubist, 
                                metric= "NAREC", aggregate="mean", val=5)
NormalDLR.cubist.NAREC.curve = paste("REC, cubist=", round(NormalDLR.cubist.NAREC, digits=4))

NormalDLR.bagEarthGCV.NAREC = mmetric(y=April.test$NormalDLR, x = April.test$pred.NormalDLR.bagEarthGCV, 
                                     metric= "NAREC", aggregate="mean", val=5)
NormalDLR.bagEarthGCV.NAREC.curve = paste("REC, bagEarthGCV=", round(NormalDLR.bagEarthGCV.NAREC, digits=4))

NormalDLR.bagEarth.NAREC = mmetric(y=April.test$NormalDLR, x = April.test$pred.NormalDLR.bagEarth, 
                                  metric= "NAREC", aggregate="mean", val=5)
NormalDLR.bagEarth.NAREC.curve = paste("REC, bagEarth=", round(NormalDLR.bagEarth.NAREC, digits=4))

#Second REC graph
B=vector("list",3); pred=vector("list",1); test=vector("list",1);
pred[[1]]=April.test$NormalDLR; 

test[[1]]=April.test$pred.NormalDLR.gcvEarth;  B[[1]]=list(pred=pred,test=test,runs=1)
test[[1]]=April.test$pred.NormalDLR.svmLinear; B[[2]]=list(pred=pred,test=test,runs=1)
test[[1]]=April.test$pred.NormalDLR.brnn;      B[[3]]=list(pred=pred,test=test,runs=1)


NormalDLR.gcvEarth.NAREC = mmetric(y=April.test$NormalDLR, x = April.test$pred.NormalDLR.gcvEarth, 
                                  metric= "NAREC", aggregate="mean", val=5)
NormalDLR.gcvEarth.NAREC.curve = paste("REC, gcvEarth=", round(NormalDLR.gcvEarth.NAREC, digits=4))

NormalDLR.svmLinear.NAREC = mmetric(y=April.test$NormalDLR, x = April.test$pred.NormalDLR.svmLinear, 
                                   metric= "NAREC", aggregate="mean", val=5)
NormalDLR.svmLinear.NAREC.curve = paste("REC, svmLinear=", round(NormalDLR.svmLinear.NAREC, digits=4))

NormalDLR.brnn.NAREC = mmetric(y=April.test$NormalDLR, x = April.test$pred.NormalDLR.brnn, 
                              metric= "NAREC", aggregate="mean", val=5)
NormalDLR.brnn.NAREC.curve = paste("REC, brnn=", round(NormalDLR.brnn.NAREC, digits=4))


#save to png            #png looks nicer than the pdf
png("NormalDLR. REC-curves.png", width=1110, height=550)
split.screen(c(1,2))
screen(1)
L.graph<-mgraph(size=c(1,10), L, graph="REC", 
                leg=list(pos="bottomright",leg=c(NormalDLR.gcvEarth.NAREC.curve,
                                                 NormalDLR.svmLinear.NAREC.curve,
                                                 NormalDLR.brnn.NAREC.curve,
                                                 NormalDLR.bagEarth.NAREC.curve)),
                col=c("red", "blue", "gray1", "green"),
                main="NormalDLR. REC curves")
screen(2)
B.graph<-mgraph(B, size=c(1,10), graph="REC", 
                leg=list(pos="bottomright",leg=c(NormalDLR.Formula.NAREC.curve,
                                                 NormalDLR.cubist.NAREC.curve,
                                                 NormalDLR.bagEarthGCV.NAREC.curve)),
                col=c("gray1", "blue", "green", "red"),
                main="NormalDLR. REC curves")

close.screen(all=TRUE)
dev.off()

errMetric.NormalDLR.brnn<-mmetric(y=April.test$NormalDLR, x = April.test$pred.NormalDLR.brnn, 
                                 metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                           "MAPE", "MdAPE"), aggregate="mean")
errMetric.NormalDLR.bagEarth<-mmetric(y=April.test$NormalDLR, x = April.test$pred.NormalDLR.bagEarth, 
                                     metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                               "MAPE", "MdAPE"), aggregate="mean")
errMetric.NormalDLR.bagEarthGCV<-mmetric(y=April.test$NormalDLR, x = April.test$pred.NormalDLR.bagEarthGCV, 
                                        metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                                  "MAPE", "MdAPE"), aggregate="mean")
errMetric.NormalDLR.cubist<-mmetric(y=April.test$NormalDLR, x = April.test$pred.NormalDLR.cubist, 
                                   metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                             "MAPE", "MdAPE"), aggregate="mean")
errMetric.NormalDLR.Formula<-mmetric(y=April.test$NormalDLR, x = April.test$pred.NormalDLR.Formula, 
                                    metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                              "MAPE", "MdAPE"), aggregate="mean")
errMetric.NormalDLR.gcvEarth<-mmetric(y=April.test$NormalDLR, x = April.test$pred.NormalDLR.gcvEarth, 
                                     metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                               "MAPE", "MdAPE"), aggregate="mean")
errMetric.NormalDLR.svmLinear<-mmetric(y=April.test$NormalDLR, x = April.test$pred.NormalDLR.svmLinear, 
                                      metric= c("RAE", "MSE", "ME","RMSE", "RSE", "RRSE", "R2", "Q2", "NAREC",
                                                "MAPE", "MdAPE"), aggregate="mean")

errMatrix.NormalDLR<-data.frame(errMetric.NormalDLR.bagEarth,errMetric.NormalDLR.brnn,errMetric.NormalDLR.cubist,
                               errMetric.NormalDLR.Formula,errMetric.NormalDLR.gcvEarth,errMetric.NormalDLR.svmLinear,
                               errMetric.NormalDLR.bagEarthGCV)
errMatrix.NormalDLR<-t(errMatrix.NormalDLR)
write.table(errMatrix.NormalDLR,"errorMatrixNormalDLR.csv", sep=",")

errMatrix.NormalDLR<-errMatrix.NormalDLR[,c(7,9,10,1,2,3,4,5,6,8,11)]
errMatrix.NormalDLR.Ordered<-errMatrix.NormalDLR[ order(-errMatrix.NormalDLR[,1], errMatrix.NormalDLR[,2], 
                                                      errMatrix.NormalDLR[,3]),]
View(errMatrix.NormalDLR.Ordered)
#/REC curves

pdf("ScatterplotsNormalDLRAprilTest.pdf", width=18, height=10)
par(mfrow=c(3,3))
plot(April.test$NormalDLR~April.test$pred.NormalDLR.Formula, main="NormalDLR. Calculated vs Predicted by the linear model",
     ylab="Calculated values [A]", xlab="Predicted values [A]", cex.lab=1.7, cex.axis=2.5, cex.main=1.79)
plot(April.test$NormalDLR~April.test$pred.NormalDLR.cubist, main="NormalDLR. Calculated vs Predicted by the cubist model",
     ylab="Calculated values [A]", xlab="Predicted values [A]", cex.lab=1.7, cex.axis=2.5, cex.main=1.79)
plot(April.test$NormalDLR~April.test$pred.NormalDLR.bagEarthGCV, main="NormalDLR. Calculated vs Predicted by the bagEarthGCV model",
     ylab="Calculated values [A]", xlab="Predicted values [A]", cex.lab=1.7, cex.axis=2.5, cex.main=1.79)
plot(April.test$NormalDLR~April.test$pred.NormalDLR.bagEarth, main="NormalDLR. Calculated vs Predicted by the bagEarth model",
     ylab="Calculated values [A]", xlab="Predicted values [A]", cex.lab=1.7, cex.axis=2.5, cex.main=1.79)
plot(April.test$NormalDLR~April.test$pred.NormalDLR.brnn, main="NormalDLR. Calculated vs Predicted by the brnn model",
     ylab="Calculated values [A]", xlab="Predicted values [A]", cex.lab=1.7, cex.axis=2.5, cex.main=1.79)
plot(April.test$NormalDLR~April.test$pred.NormalDLR.gcvEarth, main="NormalDLR. Calculated vs Predicted by the gcvEarth model",
     ylab="Calculated values [A]", xlab="Predicted values [A]", cex.lab=1.7, cex.axis=2.5, cex.main=1.79)
plot(April.test$NormalDLR~April.test$pred.NormalDLR.svmLinear, main="NormalDLR. Calculated vs Predicted by the svmLinear model",
     ylab="Calculated values [A]", xlab="Predicted values [A]", cex.lab=1.7, cex.axis=2.5, cex.main=1.79)
dev.off()

#bagEarthGCV the best model; must plot NormalDLR against time  
April.test$DateTime<-seq.POSIXt(from=as.POSIXct("2015-04-01 00:00:00", tz="Etc/GMT+12"), 
                                to=as.POSIXct("2015-04-30 23:00:00", tz="Etc/GMT+12"), by=3600)

#png is better than pdf!!!
png("NormalDLRAprilTest.png", width=1100, height=550)
#plot the most accurate models vs measured 
plot(col="black", April.test$NormalDLR~April.test$DateTime, main="Predicted and Calculated NormalDLR",
     xlab="Time", ylab="DLR [A]", cex.lab=1.3, cex.axis=1.7, type="l")
lines(April.test$pred.NormalDLR.bagEarthGCV~April.test$DateTime,col="blue")
lines(April.test$pred.NormalDLR.bagEarth~April.test$DateTime,   col="green")
lines(April.test$pred.NormalDLR.brnn~April.test$DateTime,       col="red")
legend("topleft", 
       c("Measured","Predicted. bagEarthGCV", "Predicted. bagEarth", "Predicted. brnn"), # puts text in the legend 
       lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5,2.5, 2.5),col=c("black","red", "green", "blue"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1)
dev.off()
#/png is better than pdf!!!

pdf("NormalDLRAprilTest.pdf", width=18, height=10)
#plot the most accurate models vs measured 
plot(col="black", April.test$NormalDLR~April.test$DateTime, main="Predicted and Calculated NormalDLR",
     xlab="Time", ylab="DLR [A]", cex.lab=1.7, cex.axis=1.7)
lines(col="blue", April.test$pred.NormalDLR.bagEarth~April.test$DateTime)
lines(col="green", April.test$pred.NormalDLR.brnn~April.test$DateTime)
lines(col="red", April.test$pred.NormalDLR.gcvEarth~April.test$DateTime)
legend("topleft", 
       c("Measured","Predicted. gcvEarth", "Predicted. brnn", "Predicted. svmLinear"), # puts text in the legend 
       lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5,2.5, 2.5),col=c("black","red", "green", "blue"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.5, text.font=1)
dev.off()
#/plot the most accurate models vs measured 

png("ErrorRateDLR.png", width=1100, height=550)
histogram( ~ err.NormalDLR.Formula + err.NormalDLR.cubist +
           err.NormalDLR.bagEarthGCV + err.NormalDLR.bagEarth +
             err.NormalDLR.gcvEarth + err.NormalDLR.svmLinear +
             err.NormalDLR.brnn, data=April.test, cex.axis = 1.5, cexlab = 1.7, xlab="Error rate", main="Models' error rates on the test dataset")
dev.off()

                                                    #/NormalDLR

#Benchmark the predictive models according to their error rates
#This analysis can be also be easily done for other predicted parameters
par(bg="gray92")
boxplot(April.test$err.NormalDLR.Formula, April.test$err.NormalDLR.cubist, April.test$err.NormalDLR.bagEarthGCV,
        April.test$err.NormalDLR.bagEarth, April.test$err.NormalDLR.gcvEarth, April.test$err.NormalDLR.svmLinear,
        April.test$err.NormalDLR.brnn, cex.lab=1.2, cex.axis=1.2, #ylim=c(0,20),
        names=c("LinearModel", "Cubist", "bagEarthGCV", "bagEarth", "gcvEarth", "svmLinear", "brnn"),
        col=c("red","green", "orange", "blue2", "darkslateblue", "mediumvioletred", "orangered3"),
        xlab="Predictive Models", ylab="Error rate profile [%]", main="The distribution of the error rate of the predictive models for NormalDLR")
#/Benchmark the predictive models according to their error rates

#The above figure can be validated simply by running "summary(April.test)" and also by looking at 
#"errMatrix.NormalDLR.Ordered" by running "View(errMatrix.NormalDLR.Ordered)"


#Plotting real values against the predicted ones
png("NormalDLRvsPredicted.png", width=1100, height=550)
par(bg="gray92")
plot(April.test$NormalDLR,April.test$NormalDLR, col="black", type="l", main="NormalDLR. Real observations VS the predicted ones",
     xlab="The real values of NormalDLR [A]", ylab="The predicted values of NormalDLR [A]",
     cex.lab=1.5, cex.axis=1.5)
points(April.test$NormalDLR,April.test$pred.NormalDLR.bagEarthGCV, col="blue")
points(April.test$NormalDLR,April.test$pred.NormalDLR.brnn, col="green")
points(April.test$NormalDLR,April.test$pred.NormalDLR.svmLinear, col="red")
legend("topleft", 
       c("Real","Predicted. bagEarthGCV", "Predicted. brnn", "Predicted. svmLinear"), # puts text in the legend 
       lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(3, 3, 3, 3),col=c("black", "blue", "green", "red"),# gives the legend lines the correct color and width
       cex=1.55, pt.cex=1.55, text.font=1.4)
dev.off()


#/Plotting real values against the predicted ones




