### KTH, Department of Industrial Information and Control Systems (ICS)
### Master Thesis Project
### BUILDING PREDICTIVE MODELS FOR DYNAMIC LINE RATING USING DATA SCIENCE TECHNIQUES 
### Sponsored by Vattenfall AG
### Author: Nicolae Doban
### Stockholm, 2015

## This module builds the correlograms and the correlation tables for the entire dataset and for each month
## Packages used lattice, gridExtra, lubridate, corrgram
## INPUT DATA: The interpolated and processed dataset new.Rda
## OUTPUT: Correlogram and correlation tables

##The NA values are chosen to be omitted. An interpolation of the NAs could be done like in 1. Boxplots.R
##but it would not have shown the REAL correlations between parameters

newDir <- setwd("C:/Users/LabStudent/Desktop/Nick/GitHub") #set the directory of the dataset
load("new.Rda") #loading the dataset
load("FullDataSetNA.Rda") #loading the entire dataset for building the REAL correlograms and correlation tables
                                                        #Correlations

#Building corelograms and correlation tables for the entire dataset and for each month

#Building corelograms and correlation tables for the entire dataset
pdf("new.pdf", height=9, width=16.5)
#<this line is not necessary- it is just to view the cor-dataframe>
newcor <- round(cor(new[3:12]),6) #building the correlation table. leaving out the time parameters and rounding the correlation coefficients
#</this line is not necessary>
grid.table(round(cor(new[3:12]),6)) #building the correlation table. leaving out the time parameters and rounding the correlation coefficients
library(corrgram)
corrgram(new[3:12], order=TRUE, lower.panel=panel.shade, #building the correlogram. Applied also to the latter blocks
         upper.panel=panel.pie, text.panel=panel.txt,
         main="newMinute")

View(newcor)
sink("newSummary.txt")
summary(new)
sink(NULL)
dev.off()
#/Building corelograms and correlation tables for the entire dataset

#Building corelograms and correlation tables for each month

#JulyNA  #juli
juliNA<-data.frame(Date=FullDataSetNA$Date[FullDataSetNA$Month=="juli"],   
                     Time=FullDataSetNA$Time[FullDataSetNA$Month=="juli"],
                     SolarRad=FullDataSetNA$SolarRad[FullDataSetNA$Month=="juli"],
                     WindDir=FullDataSetNA$WindDir[FullDataSetNA$Month=="juli"],
                     AmbTemp=FullDataSetNA$AmbTemp[FullDataSetNA$Month=="juli"],
                     WindSpeed=FullDataSetNA$WindSpeed[FullDataSetNA$Month=="juli"],
                     CondTemp=FullDataSetNA$CondTemp[FullDataSetNA$Month=="juli"],
                     Current=FullDataSetNA$Current[FullDataSetNA$Month=="juli"],
                     CTMDLR=FullDataSetNA$CTMDLR[FullDataSetNA$Month=="juli"],
                     NormalDLR=FullDataSetNA$NormalDLR[FullDataSetNA$Month=="juli"],
                     Hum=FullDataSetNA$Hum[FullDataSetNA$Month=="juli"],
                     BarPress=FullDataSetNA$BarPress[FullDataSetNA$Month=="juli"],
                     Month=FullDataSetNA$Month[FullDataSetNA$Month=="juli"]
)
juliNA$CTMDLR<-NULL       #because No Data
juliNA$NormalDLR<-NULL    #because No Data
juli<-na.omit(juliNA) #omit the NA values, JUST in case there are any remaining NAs. 
pdf("juliCor.pdf", height=9, width=16.5)
#<this line is not necessary- it is just to view the cor-dataframe>
julicor <- round(cor(juli[3:10]),6)
#</this line is not necessary>
grid.table(round(cor(juli[3:10]),6))
library(corrgram)
corrgram(juli[3:10], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="juliMinute")

View(julicor)
sink("juliSummary.txt") #save the summary of the juli-dataframe. Applied also to the latter blocks
summary(juli)
sink(NULL)
dev.off()
#/juli

#AugustNA  #augusti
augustiNA<-data.frame(Date=FullDataSetNA$Date[FullDataSetNA$Month=="augusti"],
                        Time=FullDataSetNA$Time[FullDataSetNA$Month=="augusti"],
                        SolarRad=FullDataSetNA$SolarRad[FullDataSetNA$Month=="augusti"],
                        WindDir=FullDataSetNA$WindDir[FullDataSetNA$Month=="augusti"],
                        AmbTemp=FullDataSetNA$AmbTemp[FullDataSetNA$Month=="augusti"],
                        WindSpeed=FullDataSetNA$WindSpeed[FullDataSetNA$Month=="augusti"],
                        CondTemp=FullDataSetNA$CondTemp[FullDataSetNA$Month=="augusti"],
                        Current=FullDataSetNA$Current[FullDataSetNA$Month=="augusti"],
                        CTMDLR=FullDataSetNA$CTMDLR[FullDataSetNA$Month=="augusti"],
                        NormalDLR=FullDataSetNA$NormalDLR[FullDataSetNA$Month=="augusti"],
                        Hum=FullDataSetNA$Hum[FullDataSetNA$Month=="augusti"],
                        BarPress=FullDataSetNA$BarPress[FullDataSetNA$Month=="augusti"],
                        Month=FullDataSetNA$Month[FullDataSetNA$Month=="augusti"]
)
augustiNA$CTMDLR<-NULL        #because No Data
augustiNA$NormalDLR<-NULL     #because No Data
augusti<-na.omit(augustiNA)
pdf("augustiCor.pdf", height=9, width=16.5)
#<this line is not necessary- it is just to view the cor-dataframe>
augusticor <- round(cor(augusti[3:10]),6)
#</this line is not necessary>
grid.table(round(cor(augusti[3:10]),6))
library(corrgram)
corrgram(augusti[3:10], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="augustiMinute")

View(augustiNAcor)
sink("augustiSummary.txt")
summary(augusti)
sink(NULL)
dev.off()
#/augusti

#SeptemberNA   #september
septemberNA<-data.frame(Date=FullDataSetNA$Date[FullDataSetNA$Month=="september"],
                        Time=FullDataSetNA$Time[FullDataSetNA$Month=="september"],
                        SolarRad=FullDataSetNA$SolarRad[FullDataSetNA$Month=="september"],
                        WindDir=FullDataSetNA$WindDir[FullDataSetNA$Month=="september"],
                        AmbTemp=FullDataSetNA$AmbTemp[FullDataSetNA$Month=="september"],
                        WindSpeed=FullDataSetNA$WindSpeed[FullDataSetNA$Month=="september"],
                        CondTemp=FullDataSetNA$CondTemp[FullDataSetNA$Month=="september"],
                        Current=FullDataSetNA$Current[FullDataSetNA$Month=="september"],
                        CTMDLR=FullDataSetNA$CTMDLR[FullDataSetNA$Month=="september"],
                        NormalDLR=FullDataSetNA$NormalDLR[FullDataSetNA$Month=="september"],
                        Hum=FullDataSetNA$Hum[FullDataSetNA$Month=="september"],
                        BarPress=FullDataSetNA$BarPress[FullDataSetNA$Month=="september"],
                        Month=FullDataSetNA$Month[FullDataSetNA$Month=="september"]
)
september<-na.omit(septemberNA)
pdf("septemberCor.pdf", height=9, width=16.5)
#<this line is not necessary- it is just to view the cor-dataframe>
septembercor <- round(cor(september[3:12]),6)
#</this line is not necessary>
grid.table(round(cor(september[3:12]),6))
library(corrgram)
corrgram(september[3:12], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="septemberMinute")

View(septemberNAcor)
sink("septemberSummary.txt")
summary(september)
sink(NULL)
dev.off()
#/september

#OctoberNA  #oktober
oktoberNA<-data.frame(Date=FullDataSetNA$Date[FullDataSetNA$Month=="oktober"],
                        Time=FullDataSetNA$Time[FullDataSetNA$Month=="oktober"],
                        SolarRad=FullDataSetNA$SolarRad[FullDataSetNA$Month=="oktober"],
                        WindDir=FullDataSetNA$WindDir[FullDataSetNA$Month=="oktober"],
                        AmbTemp=FullDataSetNA$AmbTemp[FullDataSetNA$Month=="oktober"],
                        WindSpeed=FullDataSetNA$WindSpeed[FullDataSetNA$Month=="oktober"],
                        CondTemp=FullDataSetNA$CondTemp[FullDataSetNA$Month=="oktober"],
                        Current=FullDataSetNA$Current[FullDataSetNA$Month=="oktober"],
                        CTMDLR=FullDataSetNA$CTMDLR[FullDataSetNA$Month=="oktober"],
                        NormalDLR=FullDataSetNA$NormalDLR[FullDataSetNA$Month=="oktober"],
                        Hum=FullDataSetNA$Hum[FullDataSetNA$Month=="oktober"],
                        BarPress=FullDataSetNA$BarPress[FullDataSetNA$Month=="oktober"],
                        Month=FullDataSetNA$Month[FullDataSetNA$Month=="oktober"]
)
oktober<-na.omit(oktoberNA)
pdf("oktoberCor.pdf", height=9, width=16.5)
#<this line is not necessary- it is just to view the cor-dataframe>
oktobercor <- round(cor(oktober[3:12]),6)
#</this line is not necessary>
grid.table(round(cor(oktober[3:12]),6))
library(corrgram)
corrgram(oktober[3:12], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="oktoberMinute")

View(oktoberNAcor)
sink("oktoberSummary.txt")
summary(oktober)
sink(NULL)
dev.off()
#/oktober

#novemberNA   #november
novemberNA<-data.frame(Date=FullDataSetNA$Date[FullDataSetNA$Month=="november"],
                      Time=FullDataSetNA$Time[FullDataSetNA$Month=="november"],
                      SolarRad=FullDataSetNA$SolarRad[FullDataSetNA$Month=="november"],
                      WindDir=FullDataSetNA$WindDir[FullDataSetNA$Month=="november"],
                      AmbTemp=FullDataSetNA$AmbTemp[FullDataSetNA$Month=="november"],
                      WindSpeed=FullDataSetNA$WindSpeed[FullDataSetNA$Month=="november"],
                      CondTemp=FullDataSetNA$CondTemp[FullDataSetNA$Month=="november"],
                      Current=FullDataSetNA$Current[FullDataSetNA$Month=="november"],
                      CTMDLR=FullDataSetNA$CTMDLR[FullDataSetNA$Month=="november"],
                      NormalDLR=FullDataSetNA$NormalDLR[FullDataSetNA$Month=="november"],
                      Hum=FullDataSetNA$Hum[FullDataSetNA$Month=="november"],
                      BarPress=FullDataSetNA$BarPress[FullDataSetNA$Month=="november"],
                      Month=FullDataSetNA$Month[FullDataSetNA$Month=="november"]
)
november<-na.omit(novemberNA)
pdf("novemberCor.pdf", height=9, width=16.5)
#<this line is not necessary- it is just to view the cor-dataframe>
novembercor <- round(cor(november[3:12]),6)
#</this line is not necessary>
grid.table(round(cor(november[3:12]),6))
library(corrgram)
corrgram(november[3:12], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="novemberMinute")

View(novemberNAcor)
sink("novemberSummary.txt")
summary(november)
sink(NULL)
dev.off()
#/november

#decemberNA  #december
decemberNA<-data.frame(Date=FullDataSetNA$Date[FullDataSetNA$Month=="december"],
                       Time=FullDataSetNA$Time[FullDataSetNA$Month=="december"],
                       SolarRad=FullDataSetNA$SolarRad[FullDataSetNA$Month=="december"],
                       WindDir=FullDataSetNA$WindDir[FullDataSetNA$Month=="december"],
                       AmbTemp=FullDataSetNA$AmbTemp[FullDataSetNA$Month=="december"],
                       WindSpeed=FullDataSetNA$WindSpeed[FullDataSetNA$Month=="december"],
                       CondTemp=FullDataSetNA$CondTemp[FullDataSetNA$Month=="december"],
                       Current=FullDataSetNA$Current[FullDataSetNA$Month=="december"],
                       CTMDLR=FullDataSetNA$CTMDLR[FullDataSetNA$Month=="december"],
                       NormalDLR=FullDataSetNA$NormalDLR[FullDataSetNA$Month=="december"],
                       Hum=FullDataSetNA$Hum[FullDataSetNA$Month=="december"],
                       BarPress=FullDataSetNA$BarPress[FullDataSetNA$Month=="december"],
                       Month=FullDataSetNA$Month[FullDataSetNA$Month=="december"]
)
december<-na.omit(decemberNA)
pdf("decemberCor.pdf", height=9, width=16.5)
#<this line is not necessary- it is just to view the cor-dataframe>
decembercor <- round(cor(december[3:12]),6)
#</this line is not necessary>
grid.table(round(cor(december[3:12]),6))
library(corrgram)
corrgram(december[3:12], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="decemberMinute")

View(decemberNAcor)
sink("decemberSummary.txt")
summary(december)
sink(NULL)
dev.off()
#/december

#januariNA  #januari
januariNA<-data.frame(Date=FullDataSetNA$Date[FullDataSetNA$Month=="januari"],
                       Time=FullDataSetNA$Time[FullDataSetNA$Month=="januari"],
                       SolarRad=FullDataSetNA$SolarRad[FullDataSetNA$Month=="januari"],
                       WindDir=FullDataSetNA$WindDir[FullDataSetNA$Month=="januari"],
                       AmbTemp=FullDataSetNA$AmbTemp[FullDataSetNA$Month=="januari"],
                       WindSpeed=FullDataSetNA$WindSpeed[FullDataSetNA$Month=="januari"],
                       CondTemp=FullDataSetNA$CondTemp[FullDataSetNA$Month=="januari"],
                       Current=FullDataSetNA$Current[FullDataSetNA$Month=="januari"],
                       CTMDLR=FullDataSetNA$CTMDLR[FullDataSetNA$Month=="januari"],
                       NormalDLR=FullDataSetNA$NormalDLR[FullDataSetNA$Month=="januari"],
                       Hum=FullDataSetNA$Hum[FullDataSetNA$Month=="januari"],
                       BarPress=FullDataSetNA$BarPress[FullDataSetNA$Month=="januari"],
                       Month=FullDataSetNA$Month[FullDataSetNA$Month=="januari"]
)
januari<-na.omit(januariNA)
pdf("januariCor.pdf", height=9, width=16.5)
#<this line is not necessary- it is just to view the cor-dataframe>
januaricor <- round(cor(januari[3:12]),6)
#</this line is not necessary>
grid.table(round(cor(januari[3:12]),6))
library(corrgram)
corrgram(januari[3:12], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="januariMinute")

View(januariNAcor)
sink("januariSummary.txt")
summary(januari)
sink(NULL)
dev.off()
#/januari

#februari  #februari
februariNA<-data.frame(Date=FullDataSetNA$Date[FullDataSetNA$Month=="februari"],
                      Time=FullDataSetNA$Time[FullDataSetNA$Month=="februari"],
                      SolarRad=FullDataSetNA$SolarRad[FullDataSetNA$Month=="februari"],
                      WindDir=FullDataSetNA$WindDir[FullDataSetNA$Month=="februari"],
                      AmbTemp=FullDataSetNA$AmbTemp[FullDataSetNA$Month=="februari"],
                      WindSpeed=FullDataSetNA$WindSpeed[FullDataSetNA$Month=="februari"],
                      CondTemp=FullDataSetNA$CondTemp[FullDataSetNA$Month=="februari"],
                      Current=FullDataSetNA$Current[FullDataSetNA$Month=="februari"],
                      CTMDLR=FullDataSetNA$CTMDLR[FullDataSetNA$Month=="februari"],
                      NormalDLR=FullDataSetNA$NormalDLR[FullDataSetNA$Month=="februari"],
                      Hum=FullDataSetNA$Hum[FullDataSetNA$Month=="februari"],
                      BarPress=FullDataSetNA$BarPress[FullDataSetNA$Month=="februari"],
                      Month=FullDataSetNA$Month[FullDataSetNA$Month=="februari"]
)
februari<-na.omit(februariNA)
pdf("februariCor.pdf", height=9, width=16.5)
#<this line is not necessary- it is just to view the cor-dataframe>
februaricor <- round(cor(februari[3:12]),6)
#</this line is not necessary>
grid.table(round(cor(februari[3:12]),6))
library(corrgram)
corrgram(februari[3:12], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="februariMinute")

View(februariNAcor)
sink("februariSummary.txt")
summary(februari)
sink(NULL)
dev.off()
#/februari

#mars  #mars
marsNA<-data.frame(Date=FullDataSetNA$Date[FullDataSetNA$Month=="mars"],
                       Time=FullDataSetNA$Time[FullDataSetNA$Month=="mars"],
                       SolarRad=FullDataSetNA$SolarRad[FullDataSetNA$Month=="mars"],
                       WindDir=FullDataSetNA$WindDir[FullDataSetNA$Month=="mars"],
                       AmbTemp=FullDataSetNA$AmbTemp[FullDataSetNA$Month=="mars"],
                       WindSpeed=FullDataSetNA$WindSpeed[FullDataSetNA$Month=="mars"],
                       CondTemp=FullDataSetNA$CondTemp[FullDataSetNA$Month=="mars"],
                       Current=FullDataSetNA$Current[FullDataSetNA$Month=="mars"],
                       CTMDLR=FullDataSetNA$CTMDLR[FullDataSetNA$Month=="mars"],
                       NormalDLR=FullDataSetNA$NormalDLR[FullDataSetNA$Month=="mars"],
                       Hum=FullDataSetNA$Hum[FullDataSetNA$Month=="mars"],
                       BarPress=FullDataSetNA$BarPress[FullDataSetNA$Month=="mars"],
                       Month=FullDataSetNA$Month[FullDataSetNA$Month=="mars"]
)
mars<-na.omit(marsNA)
pdf("marsCor.pdf", height=9, width=16.5)
#<this line is not necessary- it is just to view the cor-dataframe>
marscor <- round(cor(mars[3:12]),6)
#</this line is not necessary>
grid.table(round(cor(mars[3:12]),6))
library(corrgram)
corrgram(mars[3:12], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="marsMinute")

View(marsNAcor)
sink("marsSummary.txt")
summary(mars)
sink(NULL)
dev.off()
#/mars
