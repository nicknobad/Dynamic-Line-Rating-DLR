### KTH, Department of Industrial Information and Control Systems (ICS)
### Master Thesis Project
### BUILDING PREDICTIVE MODELS FOR DYNAMIC LINE RATING USING DATA SCIENCE TECHNIQUES 
### Sponsored by Vattenfall AG
### Author: Nicolae Doban
### Stockholm, 2015

## This module is processing the entire dataset
## Packages used lattice, gridExtra, lubridate
## INPUT DATA: The entire dataset
## OUTPUT: Rda files for later use for building the Boxplots, 

                                                            #Processing
library(lattice)
library(gridExtra)
library(lubridate)

#setwd("D:/R/Practice/ThesisDuplicateModeling/Data/ChangedColumnNames/FullDataSet") #when working locally
setwd("C:/Users/LabStudent/Desktop/Nick/GitHub")
FullDataSetNA <- read.csv("FullDataSet.csv", header=TRUE, na.strings=c("No Data", "")) #assign blanks and "No Data" to NA

#preparing the dataframe
#deleting some parameters that won't be used in the future analysis
FullDataSetNA$Tension<-NULL
FullDataSetNA$Sag<-NULL
FullDataSetNA$Incli<-NULL
FullDataSetNA$LowPoint<-NULL
FullDataSetNA$X<-NULL
#/deleting some parameters that won't be used in the future analysis
#transform the strange values into their absolute values
FullDataSetNA$Hum<-abs(FullDataSetNA$Hum)
FullDataSetNA$BarPress<-abs(FullDataSetNA$BarPress)
#/transform the strange values into their absolute values
#/preparing the dataframe


FullDataSetNA$Date <- as.Date(FullDataSetNA$Date, "%m/%d/%Y") #Add Month Name to FullDataSetNA dataframe
#FullDataSetNA$Time<-as.character(FullDataSetNA$Time)
FullDataSetNA$Month<-months(FullDataSetNA$Date)
#Create and re-arrange the DateTime column    used for plotting versus hours

FullDataSetNA$DateTime<-paste(FullDataSetNA$Date, FullDataSetNA$Time, sep=" ")
FullDataSetNA$DateTime<-strptime(FullDataSetNA$DateTime, "%Y-%m-%d %H:%M")
FullDataSetNA$Hours<-strftime(FullDataSetNA$DateTime, "%H")

#FullDataSetNA<-FullDataSetNA[,c(1,2,14,3,4,5,6,7,8,9,10,11,12,13)] #re-arrange the columns
FullDataSetNA$Days<-weekdays(as.Date(FullDataSetNA$Date, format="%Y-%m-%d"))

#Merge Month and Year columns in one column
FullDataSetNA$MonthYearAbb <- strftime(FullDataSetNA$Date, "%b, %Y")

#Interpolation of NAs from all the variables
#the commented rows were used to assess the MAPE (mean absolute percentage error) of each observation
#interp is the dataframe with some of the interpolated observations
library(zoo)
library(splines)
interp<-data.frame()
interp<-data.frame(Date=FullDataSetNA$Date, Time=FullDataSetNA$Time, #SolarRad=FullDataSetNA$SolarRad, 
                   interpSolarRad=na.approx(FullDataSetNA$SolarRad), 
                   #ErrorSolarRad=abs(((na.approx(FullDataSetNA$SolarRad)-FullDataSetNA$SolarRad)/FullDataSetNA$SolarRad)*100),
                   #WindDir=FullDataSetNA$WindDir, 
                   interpWindDir=na.approx(FullDataSetNA$WindDir), 
                   #ErrorWindDir=abs(((na.approx(FullDataSetNA$WindDir)-FullDataSetNA$WindDir)/FullDataSetNA$WindDir)*100),
                   #AmbTemp=FullDataSetNA$AmbTemp, 
                   interpAmbTemp=na.spline(FullDataSetNA$AmbTemp), 
                   #ErrorAmbTemp=abs(((na.spline(FullDataSetNA$AmbTemp)-FullDataSetNA$AmbTemp)/FullDataSetNA$AmbTemp)*100),
                   #WindSpeed=FullDataSetNA$WindSpeed, 
                   interpWindSpeed=na.approx(FullDataSetNA$WindSpeed), 
                   #ErrorWindSpeed=abs(((na.approx(FullDataSetNA$WindSpeed)-FullDataSetNA$WindSpeed)/FullDataSetNA$WindSpeed)*100),
                   #CondTemp=FullDataSetNA$CondTemp, 
                   interpCondTemp=na.spline(FullDataSetNA$CondTemp), 
                   #ErrorCondTemp=abs(((na.spline(FullDataSetNA$CondTemp)-FullDataSetNA$CondTemp)/FullDataSetNA$CondTemp)*100),
                   #Current=FullDataSetNA$Current, 
                   interpCurrent=na.approx(FullDataSetNA$Current), 
                   #ErrorCurrent=abs(((na.approx(FullDataSetNA$Current)-FullDataSetNA$Current)/FullDataSetNA$Current)*100),
                   #CTMDLR=FullDataSetNA$CTMDLR, 
                   interpCTMDLR=na.spline(FullDataSetNA$CTMDLR), 
                   #ErrorCTMDLR=abs(((na.spline(FullDataSetNA$CTMDLR)-FullDataSetNA$CTMDLR)/FullDataSetNA$CTMDLR)*100),
                   #NormalDLR=FullDataSetNA$NormalDLR, 
                   interpNormalDLR=na.spline(FullDataSetNA$NormalDLR), 
                   #ErrorNormalDLR=abs(((na.spline(FullDataSetNA$NormalDLR)-FullDataSetNA$NormalDLR)/FullDataSetNA$NormalDLR)*100),
                   #Hum=FullDataSetNA$Hum, 
                   interpHum=na.spline(FullDataSetNA$Hum), 
                   #ErrorHum=abs(((na.spline(FullDataSetNA$Hum)-FullDataSetNA$Hum)/FullDataSetNA$Hum)*100),
                   #BarPress=FullDataSetNA$BarPress, 
                   interpBarPress=na.spline(FullDataSetNA$BarPress)) 

#Copying NormalDLR and CUrrent to interp DF
interp$NormalDLR<-FullDataSetNA$NormalDLR
interp$Current<-FullDataSetNA$Current
#/Copying NormalDLR and CUrrent to interp DF

#adding the DateTime column to the dataframe with the interpolated parameters
interp$DateTime<-paste(interp$Date, interp$Time, sep=" ")
interp$DateTime<-strptime(interp$DateTime, "%Y-%m-%d %H:%M")
interp$Hours<-strftime(interp$DateTime, "%H")

#adding an auxiliary DT column to count the timestamps
interp$indexDT<-as.factor(as.character(interp$DateTime))
#interp$DateTime<-as.character(interp$DateTime)
interp$Days<-weekdays(as.Date(interp$Date, format="%Y-%m-%d"))

#Dealing with strange data
#Adding logical conditions to parameters

#Condition on BarPress
interp$indexDT<-as.numeric(interp$indexDT)
ptm <- proc.time()
for(i in 1:length(interp$interpBarPress)){
  if(interp$interpBarPress[i]< 900){
    #signal something is wrong with Barometric Pressure measuring device. Barometric Pressure is negative
    interp$interpBarPress[i]<-mean(FullDataSetNA$BarPress[FullDataSetNA$Days[i]==FullDataSetNA$Days[i]], na.rm=TRUE)
  }
  
  
  if(interp$interpBarPress[i]>1520){
    #signal something is wrong with Barometric Pressure measuring device. Barometric Pressure is too high
    #interp$interpBarPress[i]<-interp$interpBarPress[i+1]+((interp$indexDT[i]-interp$indexDT[i+1])*(interp$interpBarPress[i-1]-interp$interpBarPress[i+1]))/(interp$indexDT[i-1]-interp$indexDT[i+1])
    interp$interpBarPress[i]<-mean(FullDataSetNA$BarPress[FullDataSetNA$Days==FullDataSetNA$Days[i]], na.rm=TRUE)
  }
}
proc.time() - ptm
#See where the changed column has strange values
#n<-data.frame(Date=interp$Date[interp$interpBarPress<870], Time=interp$Time[interp$interpBarPress<870], 
#              BarPress=interp$interpBarPress[interp$interpBarPress<870])
#Summary(n); View(n)
#/See where the changed column has strange values

#/Condition on BarPress

#Condition on Hum
#interpolating the Humidity column
interp$indexDT<-as.numeric(interp$indexDT)
ptm <- proc.time()
for(i in 1:length(interp$interpHum)){
  if(interp$interpHum[i]< 0){
    #signal something is wrong with Humidity measuring device. Humidity is negative
    interp$interpHum[i]<-interp$interpHum[i+1]+((interp$indexDT[i]-interp$indexDT[i+1])*(interp$interpHum[i-1]-interp$interpHum[i+1]))/(interp$indexDT[i-1]-interp$indexDT[i+1])
  }
  if(interp$interpHum[i]>100){
    #signal something is wrong with Humidity measuring device. Humidity too high
    interp$interpHum[i]<-interp$interpHum[i+1]+((interp$indexDT[i]-interp$indexDT[i+1])*(interp$interpHum[i-1]-interp$interpHum[i+1]))/(interp$indexDT[i-1]-interp$indexDT[i+1])
  }
}
proc.time() - ptm
#/Condition on Hum

#Condition on WindSpeed
#interpolating the WindSpeed column
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
#See where the changed column has strange values
#k<-data.frame(indexDT=interp$indexDT[interp$interpWindSpeed<0], WindSpeed=interp$interpWindSpeed[interp$interpWindSpeed<0])
#/See where the changed column has strange values
#/Condition on WindSpeed

#Condition on AmbTemp
#interpolating the AmbTemp column
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
#See where the changed column has strange values
#k<-data.frame(DateTime=interp$DateTime[interp$interpAmbTemp==NA], AmbTemp=interp$interpAmbTemp[interp$interpAmbTemp==NA])
#/See where the changed column has strange values
#/Condition on AmbTemp

#Condition on SolarRad
#interpolating the SolarRad columnS
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
#interpolating the WindDir column
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
#interpolating the CondTemp column
#Set Possible signals
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

#Adding the column of MonthyearAbb for when building the appropriate boxplots
interp$MonthYearAbb<-strftime(as.character(interp$Date), "%b, %Y")
interp$MonthYearAbb<-as.character(interp$MonthYearAbb)

#new <- interp[interp$MonthYearAbb %in% c("Sep, 2014", "Oct, 2014", "Nov, 2014", "Dec, 2014", "Jan, 2015", 
#                                         "Feb, 2015", "Mar, 2015"), ]  #in English Language

interp$MonthYearAbb<-as.character(interp$MonthYearAbb)
#Create a "new" dataframe with only the parameters of interest 
new <- interp[interp$MonthYearAbb %in% c("sep, 2014", "okt, 2014", "nov, 2014", "dec, 2014", "jan, 2015", 
                                         "feb, 2015", "mar, 2015"), ]   #in Swedish language

new$DateTime<-paste(new$Date, new$Time, sep=" ")
new$DateTime<-strptime(new$DateTime, "%Y-%m-%d %H:%M")
new$Hours<-strftime(new$DateTime, "%H")

new$interpCTMDLR<-NULL
new$interpNormalDLR<-NULL

#Replacing NAs in NormalDLR with Mean NormalDLR of that day
ptm <- proc.time()
for(i in 1:length(new$NormalDLR)){
  if(is.na(new$NormalDLR[i])){
    new$NormalDLR[i]<-mean(FullDataSetNA$NormalDLR[FullDataSetNA$Days[i]==FullDataSetNA$Days[i]], na.rm=TRUE)
  }
}
proc.time() - ptm
#/Replacing NAs in NormalDLR with Mean NormalDLR of that day

#Replacing NAs in Current with Mean Current of that day
ptm <- proc.time()
for(i in 1:length(new$Current)){
  if(is.na(new$Current[i])){
    new$Current[i]<-mean(FullDataSetNA$Current[FullDataSetNA$Days[i]==FullDataSetNA$Days[i]], na.rm=TRUE)
  }
}
proc.time() - ptm
#/Replacing NAs in Current with Mean Current of that day

#Factor the Days
new$Days<-as.character(new$Days)
#new$Days<-factor(new$Days, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
#                                    "Saturday", "Sunday"))    #English
new$Days<-as.character(new$Days)
new$Days<-factor(new$Days, levels=c("måndag", "tisdag", "onsdag", "torsdag", "fredag",
                                    "lördag", "söndag"))  #Swedish

new$MonthYearAbb<-as.character(new$MonthYearAbb)
new$MonthYearAbb<-factor(new$MonthYearAbb, levels=unique(new$MonthYearAbb))

#Monthly Boxplots
#Make MonthYearAbb a factor
new$MonthYearAbb<-as.character(new$MonthYearAbb)
new$MonthYearAbb<-factor(new$MonthYearAbb, levels=unique(new$MonthYearAbb))
#/Make MonthYearAbb a factor
row.names(new)<-NULL
                                                #/Processing

newDir <- setwd("C:/Users/LabStudent/Desktop/Nick/GitHub")
save(FullDataSetNA, file= "FullDataSetNA.Rda")
save(interp, file= "interp.Rda")
save(new, file= "new.Rda")
