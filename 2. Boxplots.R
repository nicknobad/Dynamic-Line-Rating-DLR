### KTH, Department of Industrial Information and Control Systems (ICS)
### Master Thesis Project
### BUILDING PREDICTIVE MODELS FOR DYNAMIC LINE RATING USING DATA SCIENCE TECHNIQUES 
### Sponsored by Vattenfall AG
### Author: Nicolae Doban
### Stockholm, 2015

## This module builds the monthly, seasonal, daily, hourly, type of day Boxplots
## Packages used gridExtra, ggplot2, graphics, grid, histogram
## INPUT DATA: The interpolated and processed dataset new.Rda
## OUTPUT: Various types of Boxplots


newDir <- setwd("C:/Users/LabStudent/Desktop/Nick/GitHub") #set the directory of the dataset
load("new.Rda") #loading the dataset

                            #Monthly Boxplots
library(histogram)
library(ggplot2)
library(graphics)
library(grid)
library(gridExtra)

#Boxplots of different parameters versus Month and Year variable
#Figure 2 in the report
par(mfrow=c(2,3))
plot(NormalDLR~MonthYearAbb, data=new, xlab="Month, year", ylab="NormalDLR, [A]", main="NormalDLR", col="blue")
plot(interpWindSpeed~MonthYearAbb, data=new, xlab="Month, year", ylab="WindSpeed, [m/s]",main="WindSpeed", col="blue")
plot(interpSolarRad~MonthYearAbb, data=new, xlab="Month, year", ylab="SolarRad, [W/m^2]",main="SolarRad", col="blue")
plot(interpAmbTemp~MonthYearAbb, data=new, xlab="Month, year", ylab="AmbTemp, [Celsius]",main="AmbTemp", col="blue")
plot(interpWindDir~MonthYearAbb, data=new, xlab="Month, year", ylab="WindDir, [degrees]",main="WindDir", col="blue")
plot(interpCondTemp~MonthYearAbb, data=new, xlab="Month, year", ylab="CondTemp, [Celsius]",main="CondTemp", col="blue")
#/Figure 2 in the report
#/Boxplots of different parameters versus Month and Year variable

#More detailed Boxplots that will be used in the Annex
#NormalDLR
ptm <- proc.time()
NormalDLRtitle = ggtitle('Monthly NormalDLR Profile')
NormalDLRBP<-ggplot(new, aes(MonthYearAbb, NormalDLR, color=factor(MonthYearAbb)))+geom_point()+
  geom_boxplot()+
  NormalDLRtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="NormalDLR per \nMonth")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(NormalDLR~MonthYearAbb, new, mean), aes(label = NormalDLR, y = NormalDLR + 95), col="black")
#/NormalDLR
proc.time() - ptm
print(NormalDLRBP)

#Current
ptm <- proc.time()
Currenttitle = ggtitle('Monthly Current Profile')
CurrentBP<-ggplot(new, aes(MonthYearAbb, Current, color=factor(MonthYearAbb)))+geom_point()+
  geom_boxplot()+
  Currenttitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="Current per \nMonth")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(Current~MonthYearAbb, new, mean), aes(label = Current, y = Current + 95), col="black")
#/Current
proc.time() - ptm
print(CurrentBP)

#interpSolarRad
interpSolarRadtitle = ggtitle('Monthly interpSolarRad Profile')
interpSolarRadBP<-ggplot(new, aes(MonthYearAbb, interpSolarRad, color=factor(MonthYearAbb)))+geom_point()+
  geom_boxplot()+
  interpSolarRadtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpSolarRad per \nMonth")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpSolarRad~MonthYearAbb, new, mean), aes(label = interpSolarRad, y = interpSolarRad + 15), col="black")
#/interpSolarRad
#interpSolarRadBP

#interpWindSpeed
interpWindSpeedtitle = ggtitle('Monthly interpWindSpeed Profile')
interpWindSpeedBP <- ggplot(new, aes(MonthYearAbb, interpWindSpeed, color=factor(MonthYearAbb)))+geom_point()+
  geom_boxplot()+
  interpWindSpeedtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpWindSpeed per \nMonth")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpWindSpeed~MonthYearAbb, new, mean), aes(label = interpWindSpeed, y = interpWindSpeed + 5), col="black")
#/interpWindSpeed

#interpWindDir
interpWindDirtitle = ggtitle('Monthly interpWindDir Profile')
interpWindDirBP<-ggplot(new, aes(MonthYearAbb, interpWindDir, color=factor(MonthYearAbb)))+geom_point()+
  geom_boxplot()+
  interpWindDirtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpWindDir per \nMonth")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpWindDir~MonthYearAbb, new, mean), aes(label = interpWindDir, y = interpWindDir + 15), col="black")
#/interpWindDir

#interpAmbTemp
interpAmbTemptitle = ggtitle('Monthly interpAmbTemp Profile')
interpAmbTempBP<-ggplot(new, aes(MonthYearAbb, interpAmbTemp, color=factor(MonthYearAbb)))+geom_point()+
  geom_boxplot()+
  interpAmbTemptitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpAmbTemp per \nMonth")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpAmbTemp~MonthYearAbb, new, mean), aes(label = interpAmbTemp, y = interpAmbTemp + 5), col="black")
#/interpAmbTemp

#interpCondTemp
interpCondTemptitle = ggtitle('Monthly interpCondTemp Profile')
interpCondTempBP<-ggplot(new, aes(MonthYearAbb, interpCondTemp, color=factor(MonthYearAbb)))+geom_point()+
  geom_boxplot()+
  interpCondTemptitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpCondTemp per \nMonth")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpCondTemp~MonthYearAbb, new, mean), aes(label = interpCondTemp, y = interpCondTemp + 5), col="black")
#/interpCondTemp
#/Monthly Boxplots

#Save to a pdf
pdf("MonthlyProfile.pdf", height=10, width=20)
print(NormalDLRBP)
print(CurrentBP)
interpWindSpeedBP
interpAmbTempBP
interpSolarRadBP
interpWindDirBP
interpCondTempBP
print(grid.arrange(NormalDLRBP, CurrentBP, interpWindSpeedBP, interpAmbTempBP, 
                   interpSolarRadBP, interpWindDirBP, interpCondTempBP, ncol=3))
dev.off()
#/Save to a pdf                                                              #/Monthly Boxplots
                                                
                                              #Seasonal Boxplots
#Create the Season  column
Season.Time<-proc.time()
new$Month<-months(as.Date(new$Date))
for(i in 1:length(new$DateTime)){
  if(new$Month[i]=="september" | new$Month[i]=="oktober" | new$Month[i]=="november"){
    new$Season[i]<-"höst/Fall"
  }
}
for(i in 1:length(new$DateTime)){
  if(new$Month[i]=="december" | new$Month[i]=="januari" | new$Month[i]=="februari"){
    new$Season[i]<-"vinter/Winter"
  }
}
for(i in 1:length(new$DateTime)){
  if(new$Month[i]=="mars" | new$Month[i]=="april" | new$Month[i]=="maj"){
    new$Season[i]<-"vår/Spring"
  }
}
for(i in 1:length(new$DateTime)){
  if(new$Month[i]=="juni" | new$Month[i]=="juli" | new$Month[i]=="augusti"){
    new$Season[i]<-"sommar/Summer"
  }
}
proc.time() - Season.Time

#Make Season a factor
new$Season<-as.character(new$Season)
new$Season<-factor(new$Season, levels=unique(new$Season))
#/Make Season a factor

#Annex. Summarized Boxplots
pdf("SeasonBP.pdf", width=15)
par(mfrow=c(2,3))
#NormalDLR
boxplot(new$NormalDLR~new$Season, col="dodgerblue2", ylab="NormalDLR, [A]", xlab="Season", main="NormalDLR")
#means <- tapply(new$NormalDLR,new$Season,mean) #means don't coincide with the mean line
#points(means,col="black",pch=18)
means <- by(new$NormalDLR, new$Season, mean)
points(means, pch = 23, cex = 0.75, bg = "black")
text(means, labels = formatC(means, format = "f", digits = 1),
     pos = 3, cex = 1.2, col = "black", font=2)
#WindSpeed
boxplot(new$interpWindSpeed~new$Season, col="dodgerblue2", ylab="WindSpeed, [m/s]", xlab="Season", main="WindSpeed, [m/s]")
means <- by(new$interpWindSpeed, new$Season, mean)
points(means, pch = 23, cex = 0.75, bg = "black")
text(means, labels = formatC(means, format = "f", digits = 1),
     pos = 3, cex = 1.2, col = "black", font=2)
#SolarRad
boxplot(new$interpSolarRad~new$Season, col="dodgerblue2", ylab="SolarRad, [W/m^2]", xlab="Season", main="SolarRad")
means <- by(new$interpSolarRad, new$Season, mean)
points(means, pch = 23, cex = 0.75, bg = "black")
text(means, labels = formatC(means, format = "f", digits = 1),
     pos = 3, cex = 1.2, col = "black", font=2)
#AmbTemp
boxplot(new$interpAmbTemp~new$Season, col="dodgerblue2", ylab="AmbTemp, [Celsius]", xlab="Season", main="AmbTemp")
means <- by(new$interpAmbTemp, new$Season, mean)
points(means, pch = 23, cex = 0.75, bg = "black")
text(means, labels = formatC(means, format = "f", digits = 1),
     pos = 3, cex = 1.2, col = "black", font=2)
#WindDir
boxplot(new$interpWindDir~new$Season, col="dodgerblue2", ylab="WindDir, [degrees]", xlab="Season", main="WindDir")
means <- by(new$interpWindDir, new$Season, mean)
points(means, pch = 23, cex = 0.75, bg = "black")
text(means, labels = formatC(means, format = "f", digits = 1),
     pos = 3, cex = 1.2, col = "black", font=2)
#CondTemp
bpt<-boxplot(new$interpCondTemp~new$Season, col="dodgerblue2", ylab="CondTemp, [Celsius]", xlab="Season", main="CondTemp")
means <- by(new$interpCondTemp, new$Season, mean)
points(means, pch = 23, cex = 0.75, bg = "black")
text(means, labels = formatC(means, format = "f", digits = 1),
     pos = 3, cex = 1.2, col = "black", font=2)
dev.off()
#/Annex. Summarized Boxplots

#More detailed Boxplots
library(ggplot2)
library(graphics)
library(grid)
library(gridExtra)
#NormalDLR
#"S" stands for "Seasonal" 
ptm <- proc.time()
SNormalDLRtitle = ggtitle('Seasonal NormalDLR Profile')
SNormalDLRBP<-ggplot(new, aes(Season, NormalDLR, color=factor(Season)))+geom_point()+
  geom_boxplot()+
  SNormalDLRtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="NormalDLR per \nSeason")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(NormalDLR~Season, new, mean), aes(label = NormalDLR, y = NormalDLR + 95), col="black")
#/NormalDLR
proc.time() - ptm
#print(SNormalDLRBP)

#Current
ptm <- proc.time()
SCurrenttitle = ggtitle('Seasonal Current Profile')
SCurrentBP<-ggplot(new, aes(Season, Current, color=factor(Season)))+geom_point()+
  geom_boxplot()+
  SCurrenttitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="Current per \nSeason")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(Current~Season, new, mean), aes(label = Current, y = Current + 95), col="black")
#/Current
proc.time() - ptm
#print(SCurrentBP)
#/per month and year

#interpSolarRad
SinterpSolarRadtitle = ggtitle('Seasonal interpSolarRad Profile')
SinterpSolarRadBP<-ggplot(new, aes(Season, interpSolarRad, color=factor(Season)))+geom_point()+
  geom_boxplot()+
  SinterpSolarRadtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpSolarRad per \nMonth")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpSolarRad~Season, new, mean), aes(label = interpSolarRad, y = interpSolarRad + 15), col="black")
#/interpSolarRad
#SinterpSolarRadBP

#interpWindSpeed
SinterpWindSpeedtitle = ggtitle('Seasonal interpWindSpeed Profile')
SinterpWindSpeedBP <- ggplot(new, aes(Season, interpWindSpeed, color=factor(Season)))+geom_point()+
  geom_boxplot()+
  SinterpWindSpeedtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpWindSpeed per \nMonth")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpWindSpeed~Season, new, mean), aes(label = interpWindSpeed, y = interpWindSpeed + 5), col="black")
#/interpWindSpeed
  SinterpWindSpeedBP

#interpWindDir
SinterpWindDirtitle = ggtitle('Seasonal interpWindDir Profile')
SinterpWindDirBP<-ggplot(new, aes(Season, interpWindDir, color=factor(Season)))+geom_point()+
  geom_boxplot()+
  SinterpWindDirtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpWindDir per \nMonth")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpWindDir~Season, new, mean), aes(label = interpWindDir, y = interpWindDir + 15), col="black")
#/interpWindDir
SinterpWindDirBP

#interpAmbTemp
SinterpAmbTemptitle = ggtitle('Seasonal interpAmbTemp Profile')
SinterpAmbTempBP<-ggplot(new, aes(Season, interpAmbTemp, color=factor(Season)))+geom_point()+
  geom_boxplot()+
  SinterpAmbTemptitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpAmbTemp per \nMonth")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpAmbTemp~Season, new, mean), aes(label = interpAmbTemp, y = interpAmbTemp + 5), col="black")
#/interpAmbTemp
SinterpAmbTempBP

#interpCondTemp
SinterpCondTemptitle = ggtitle('Seasonal interpCondTemp Profile')
SinterpCondTempBP<-ggplot(new, aes(Season, interpCondTemp, color=factor(Season)))+geom_point()+
  geom_boxplot()+
  SinterpCondTemptitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpCondTemp per \nMonth")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpCondTemp~Season, new, mean), aes(label = interpCondTemp, y = interpCondTemp + 5), col="black")
#/interpCondTemp
SinterpCondTempBP
#/More detailed Boxplots

#/Seasonal Boxplots

library(gridExtra)
#grid.arrange(SNormalDLRBP, SCurrentBP, SinterpWindSpeedBP, SinterpAmbTempBP, SinterpSolarRadBP, SinterpWindDirBP, SinterpCondTempBP)

#Save to a pdf
pdf("SeasonalProfile.pdf", height=10, width=20)
print(SNormalDLRBP)
print(SCurrentBP)
SinterpWindSpeedBP
SinterpAmbTempBP
SinterpSolarRadBP
SinterpWindDirBP
SinterpCondTempBP
print(grid.arrange(SNormalDLRBP, SCurrentBP, SinterpWindSpeedBP, SinterpAmbTempBP, 
                   SinterpSolarRadBP, SinterpWindDirBP, SinterpCondTempBP, ncol=3))
dev.off()
#/Save to a pdf


                                          #Daily Boxplots Profile
new$Date<-as.Date(new$Date)
new$Days<-weekdays(as.Date(new$Date, format="%m/%d/%Y"))  #Get Day names from dataframe
new$Days<-as.character(new$Days)  

#new$Days<-factor(new$Days, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
#                                    "Friday","Saturday", "Sunday"))  #Factor the days
new$Days<-factor(new$Days, levels=c("måndag", "tisdag", "onsdag", "torsdag",     #Factor the days
                                    "fredag","lördag", "söndag"))  #Factor the days


#Annex. Summarized boxplot
pdf("DailyBP.pdf")
par(mfrow=c(2,3))
#NormalDLR
boxplot(new$NormalDLR~new$Days, col="dodgerblue2", ylab="NormalDLR, [A]", xlab="Days", main="NormalDLR")
#means <- tapply(new$NormalDLR,new$Days,mean) #means don't coincide with the mean line
#points(means,col="red",pch=18)
#WindSpeed
boxplot(new$interpWindSpeed~new$Days, col="dodgerblue2", ylab="WindSpeed, [m/s]", xlab="Days", main="WindSpeed, [m/s]")
#SolarRad
boxplot(new$interpSolarRad~new$Days, col="dodgerblue2", ylab="SolarRad, [W/m^2]", xlab="Days", main="SolarRad")
#AmbTemp
boxplot(new$interpAmbTemp~new$Days, col="dodgerblue2", ylab="AmbTemp, [Celsius]", xlab="Days", main="AmbTemp")
#WindDir
boxplot(new$interpWindDir~new$Days, col="dodgerblue2", ylab="WindDir, [degrees]", xlab="Days", main="WindDir")
#CondTemp
boxplot(new$interpCondTemp~new$Days, col="dodgerblue2", ylab="CondTemp, [Celsius]", xlab="Days", main="CondTemp")
dev.off()
#/Annex. Summarized boxplot

#More detailed boxplot
#"D" stands for "day"
library(ggplot2)
library(graphics)
library(grid)
library(gridExtra)
#NormalDLR
DNormalDLRtitle = ggtitle('NormalDLR during week days. Average')
DNormalDLRBP<-ggplot(new, aes(Days, NormalDLR, color=factor(Days)))+geom_point()+
  geom_boxplot()+
  DNormalDLRtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="NormalDLR per \nDay of the Week ")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(NormalDLR~Days, new, mean), aes(label = NormalDLR, y = NormalDLR + 55), col="black")
#/NormalDLR

#Current
DCurrenttitle = ggtitle('Current during week days. Average')
DCurrentBP<-ggplot(new, aes(Days, Current, color=factor(Days)))+geom_point()+
  geom_boxplot()+
  DCurrenttitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="Current per \nDay of the Week ")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(Current~Days, new, mean), aes(label = Current, y = Current + 55), col="black")
#/Current

#interpWindSpeed
DinterpWindSpeedtitle = ggtitle('interpWindSpeed during week days. Average')
DinterpWindSpeedBP<-ggplot(new, aes(Days, interpWindSpeed, color=factor(Days)))+geom_point()+
  geom_boxplot()+
  DinterpWindSpeedtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpWindSpeed per \nDay of the Week ")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpWindSpeed~Days, new, mean), aes(label = interpWindSpeed, y = interpWindSpeed + 0.5), col="black")
#/interpWindSpeed

#interpAmbTemp
DinterpAmbTemptitle = ggtitle('interpAmbTemp during week days. Average')
DinterpAmbTempBP<-ggplot(new, aes(Days, interpAmbTemp, color=factor(Days)))+geom_point()+
  geom_boxplot()+
  DinterpAmbTemptitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpAmbTemp per \nDay of the Week ")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpAmbTemp~Days, new, mean), aes(label = interpAmbTemp, y = interpAmbTemp + 0.5), col="black")
#/interpAmbTemp

#interpSolarRad
DinterpSolarRadtitle = ggtitle('interpSolarRad during week days. Average')
DinterpSolarRadBP<-ggplot(new, aes(Days, interpSolarRad, color=factor(Days)))+geom_point()+
  geom_boxplot()+
  DinterpSolarRadtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpSolarRad per \nDay of the Week ")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpSolarRad~Days, new, mean), aes(label = interpSolarRad, y = interpSolarRad + 10), col="black")
#/interpSolarRad

#interpWindDir
DinterpWindDirtitle = ggtitle('interpWindDir during week days. Average')
DinterpWindDirBP<-ggplot(new, aes(Days, interpWindDir, color=factor(Days)))+geom_point()+
  geom_boxplot()+
  DinterpWindDirtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpWindDir per \nDay of the Week ")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpWindDir~Days, new, mean), aes(label = interpWindDir, y = interpWindDir + 15), col="black")
#/interpWindDir

#interpCondTemp
DinterpCondTemptitle = ggtitle('interpCondTemp during week days. Average')
DinterpCondTempBP<-ggplot(new, aes(Days, interpCondTemp, color=factor(Days)))+geom_point()+
  geom_boxplot()+
  DinterpCondTemptitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpCondTemp per \nDay of the Week ")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpCondTemp~Days, new, mean), aes(label = interpCondTemp, y = interpCondTemp + 1), col="black")
#/interpCondTemp

#Save to a pdf
pdf("DailyProfile.pdf", height=10, width=20)
print(DNormalDLRBP)
print(DCurrentBP)
DinterpWindSpeedBP
DinterpAmbTempBP
DinterpSolarRadBP
DinterpWindDirBP
DinterpCondTempBP
print(grid.arrange(DNormalDLRBP, DCurrentBP, DinterpWindSpeedBP, DinterpAmbTempBP, 
                   DinterpSolarRadBP, DinterpWindDirBP, DinterpCondTempBP, ncol=3))
dev.off()
#/Save to a pdf
#/More detailed Boxplots
                                          #/Daily Boxplots  Profile


                                                      #Hourly Boxplots
new$DateTime<-paste(new$Date, new$Time, sep=" ")
new$DateTime<-strptime(new$DateTime, "%Y-%m-%d %H:%M")
new$Hours<-strftime(new$DateTime, "%H")
new$Hours<-as.character(new$Hours)
new$Hours<-factor(new$Hours, levels=unique(new$Hours))



#Annex. Summarized boxplot
pdf("HourlyBP.pdf", width=15)
par(mfrow=c(2,3))
#NormalDLR
boxplot(new$NormalDLR~new$Hours, col="dodgerblue2", ylab="NormalDLR, [A]", xlab="Hours", main="NormalDLR")
#means <- tapply(new$NormalDLR,new$Hours,mean) #means don't coincide with the mean line
#points(means,col="red",pch=18)
#WindSpeed
boxplot(new$interpWindSpeed~new$Hours, col="dodgerblue2", ylab="WindSpeed, [m/s]", xlab="Hours", main="WindSpeed, [m/s]")
#SolarRad
boxplot(new$interpSolarRad~new$Hours, col="dodgerblue2", ylab="SolarRad, [W/m^2]", xlab="Hours", main="SolarRad")
#AmbTemp
boxplot(new$interpAmbTemp~new$Hours, col="dodgerblue2", ylab="AmbTemp, [Celsius]", xlab="Hours", main="AmbTemp")
#WindDir
boxplot(new$interpWindDir~new$Hours, col="dodgerblue2", ylab="WindDir, [degrees]", xlab="Hours", main="WindDir")
#CondTemp
boxplot(new$interpCondTemp~new$Hours, col="dodgerblue2", ylab="CondTemp, [Celsius]", xlab="Hours", main="CondTemp")
dev.off()
#/Annex. Summarized boxplot

#More detailed Boxplots
#"H" stands for "hour"
new<-na.omit(new)
library(ggplot2)
library(graphics)
library(grid)
library(gridExtra)
#NormalDLR
HNormalDLRtitle = ggtitle('NormalDLR during the day. Average')
HNormalDLRBP<-ggplot(new, aes(Hours, NormalDLR, color=factor(Hours)))+geom_point()+
  geom_boxplot()+
  HNormalDLRtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="NormalDLR per \n hour of day ")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))#+ # x-axis label size and color
#geom_text(data = aggregate(NormalDLR~Hours, new, mean), aes(label = NormalDLR, y = NormalDLR + 55), col="black") #writes the mean value above the mean-point
#/NormalDLR

#Current
HCurrenttitle = ggtitle('Current during the day. Average')
HCurrentBP<-ggplot(new, aes(Hours, Current, color=factor(Hours)))+geom_point()+
  geom_boxplot()+
  HCurrenttitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="Current per \n hour of day ")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))#+ # x-axis label size and color
#geom_text(data = aggregate(Current~Hours, new, mean), aes(label = Current, y = Current + 55), col="black") #writes the mean value above the mean-point
#/Current

#interpWindSpeed
HinterpWindSpeedtitle = ggtitle('interpWindSpeed during the day. Average')
HinterpWindSpeedBP<-ggplot(new, aes(Hours, interpWindSpeed, color=factor(Hours)))+geom_point()+
  geom_boxplot()+
  HinterpWindSpeedtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpWindSpeed per \n hour of day ")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))#+ # x-axis label size and color
#geom_text(data = aggregate(interpWindSpeed~Hours, new, mean), aes(label = interpWindSpeed, y = interpWindSpeed + 0.5), col="black") #writes the mean value above the mean-point
#/interpWindSpeed

#interpAmbTemp
HinterpAmbTemptitle = ggtitle('interpAmbTemp during the day. Average')
HinterpAmbTempBP<-ggplot(new, aes(Hours, interpAmbTemp, color=factor(Hours)))+geom_point()+
  geom_boxplot()+
  HinterpAmbTemptitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpAmbTemp per \n hour of day ")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))#+ # x-axis label size and color
#geom_text(data = aggregate(interpAmbTemp~Hours, new, mean), aes(label = interpAmbTemp, y = interpAmbTemp + 0.5), col="black") #writes the mean value above the mean-point
#/interpAmbTemp

#interpSolarRad
HinterpSolarRadtitle = ggtitle('interpSolarRad during the day. Average')
HinterpSolarRadBP<-ggplot(new, aes(Hours, interpSolarRad, color=factor(Hours)))+geom_point()+
  geom_boxplot()+
  HinterpSolarRadtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpSolarRad per \n hour of day ")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))#+ # x-axis label size and color
#geom_text(data = aggregate(interpSolarRad~Hours, new, mean), aes(label = interpSolarRad, y = interpSolarRad + 10), col="black") #writes the mean value above the mean-point
#/interpSolarRad

#interpWindDir
HinterpWindDirtitle = ggtitle('interpWindDir during the day. Average')
HinterpWindDirBP<-ggplot(new, aes(Hours, interpWindDir, color=factor(Hours)))+geom_point()+
  geom_boxplot()+
  HinterpWindDirtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpWindDir per \n hour of day ")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))#+ # x-axis label size and color
#geom_text(data = aggregate(interpWindDir~Hours, new, mean), aes(label = interpWindDir, y = interpWindDir + 15), col="black") #writes the mean value above the mean-point
#/interpWindDir

#interpCondTemp
HinterpCondTemptitle = ggtitle('interpCondTemp during the day. Average')
HinterpCondTempBP<-ggplot(new, aes(Hours, interpCondTemp, color=factor(Hours)))+geom_point()+
  geom_boxplot()+
  HinterpCondTemptitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpCondTemp per \n hour of day ")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))#+ # x-axis label size and color
#geom_text(data = aggregate(interpCondTemp~Hours, new, mean), aes(label = interpCondTemp, y = interpCondTemp + 1), col="black") #writes the mean value above the mean-point
#/interpCondTemp

#Save to a pdf
pdf("HourlyProfile.pdf", height=10, width=20)
print(HNormalDLRBP)
print(HCurrentBP)
HinterpWindSpeedBP
HinterpAmbTempBP
HinterpSolarRadBP
HinterpWindDirBP
HinterpCondTempBP
print(grid.arrange(HNormalDLRBP, HCurrentBP, HinterpWindSpeedBP, HinterpAmbTempBP, 
                   HinterpSolarRadBP, HinterpWindDirBP, HinterpCondTempBP, ncol=3))
dev.off()
#/Save to a pdf
#/More detailed Boxplots

                                                          #/Hourly Boxplots


                                    #Type of Day Boxplots
#Type of Day column
x <- c(rep(1,60*24), rep(1,60*24), rep(1,60*24), rep(1,60*24), rep(1,60*24), rep(2,60*24), rep(2,60*24))
xW<-rep(x,30)
march<-rep(1,2879)
new$daytype<-c(xW,march)
ptm<-proc.time()
for(i in 1:length(new$daytype)){
  if(new$daytype[i]==1){
    new$TypeOfDay[i]<-"Weekday"
  }
  if(new$daytype[i]==2){
    new$TypeOfDay[i]<-"Weekend"
  }
}
proc.time() - ptm
#/Type of Day column

#Annex. Summarized Boxplots
pdf("TypeOfDayBP.pdf", width=15)
#png("TypeOfDayBP.png") #too low quality with png and jpeg
par(mfrow=c(2,3))
#NormalDLR
boxplot(new$NormalDLR~new$TypeOfDay, col="dodgerblue2", ylab="NormalDLR, [A]", xlab="TypeOfDay", main="NormalDLR")
#means <- tapply(new$NormalDLR,new$TypeOfDay,mean) #means don't coincide with the mean line
#points(means,col="red",pch=18)
#WindSpeed
boxplot(new$interpWindSpeed~new$TypeOfDay, col="dodgerblue2", ylab="WindSpeed, [m/s]", xlab="TypeOfDay", main="WindSpeed, [m/s]")
#SolarRad
boxplot(new$interpSolarRad~new$TypeOfDay, col="dodgerblue2", ylab="SolarRad, [W/m^2]", xlab="TypeOfDay", main="SolarRad")
#AmbTemp
boxplot(new$interpAmbTemp~new$TypeOfDay, col="dodgerblue2", ylab="AmbTemp, [Celsius]", xlab="TypeOfDay", main="AmbTemp")
#WindDir
boxplot(new$interpWindDir~new$TypeOfDay, col="dodgerblue2", ylab="WindDir, [degrees]", xlab="TypeOfDay", main="WindDir")
#CondTemp
boxplot(new$interpCondTemp~new$TypeOfDay, col="dodgerblue2", ylab="CondTemp, [Celsius]", xlab="TypeOfDay", main="CondTemp")
dev.off()
#/Annex. Summarized Boxplots

#More detailed Boxplots
#"TD" stands for "type of day"
library(ggplot2)
library(graphics)
library(grid)
library(gridExtra)
#NormalDLR
ptm <- proc.time()
TDNormalDLRtitle = ggtitle('TypeOfDay NormalDLR Profile')
TDNormalDLRBP<-ggplot(new, aes(TypeOfDay, NormalDLR, color=factor(TypeOfDay)))+geom_point()+
  geom_boxplot()+
  TDNormalDLRtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="NormalDLR per \nTypeOfDay")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(NormalDLR~TypeOfDay, new, mean), aes(label = NormalDLR, y = NormalDLR + 95), col="black")
#/NormalDLR
proc.time() - ptm
#print(TDNormalDLRBP)


#Current
ptm <- proc.time()
TDCurrenttitle = ggtitle('TypeOfDay Current Profile')
TDCurrentBP<-ggplot(new, aes(TypeOfDay, Current, color=factor(TypeOfDay)))+geom_point()+
  geom_boxplot()+
  TDCurrenttitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="Current per \nTypeOfDay")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(Current~TypeOfDay, new, mean), aes(label = Current, y = Current + 95), col="black")
#/Current
proc.time() - ptm
#print(TDCurrentBP)

#interpWindSpeed
ptm <- proc.time()
TDinterpWindSpeedtitle = ggtitle('TypeOfDay interpWindSpeed Profile')
TDinterpWindSpeedBP<-ggplot(new, aes(TypeOfDay, interpWindSpeed, color=factor(TypeOfDay)))+geom_point()+
  geom_boxplot()+
  TDinterpWindSpeedtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpWindSpeed per \nTypeOfDay")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpWindSpeed~TypeOfDay, new, mean), aes(label = interpWindSpeed, y = interpWindSpeed + 0.5), col="black")
#/interpWindSpeed
proc.time() - ptm
#print(TDinterpWindSpeedBP)

#interpAmbTemp
ptm <- proc.time()
TDinterpAmbTemptitle = ggtitle('TypeOfDay interpAmbTemp Profile')
TDinterpAmbTempBP<-ggplot(new, aes(TypeOfDay, interpAmbTemp, color=factor(TypeOfDay)))+geom_point()+
  geom_boxplot()+
  TDinterpAmbTemptitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpAmbTemp per \nTypeOfDay")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpAmbTemp~TypeOfDay, new, mean), aes(label = interpAmbTemp, y = interpAmbTemp + 0.5), col="black")
#/interpAmbTemp
proc.time() - ptm
#print(TDinterpAmbTempBP)

#interpSolarRad
ptm <- proc.time()
TDinterpSolarRadtitle = ggtitle('TypeOfDay interpSolarRad Profile')
TDinterpSolarRadBP<-ggplot(new, aes(TypeOfDay, interpSolarRad, color=factor(TypeOfDay)))+geom_point()+
  geom_boxplot()+
  TDinterpSolarRadtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpSolarRad per \nTypeOfDay")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpSolarRad~TypeOfDay, new, mean), aes(label = interpSolarRad, y = interpSolarRad + 10), col="black")
#/interpSolarRad
proc.time() - ptm
#print(TDinterpSolarRadBP)

#interpWindDir
ptm <- proc.time()
TDinterpWindDirtitle = ggtitle('TypeOfDay interpWindDir Profile')
TDinterpWindDirBP<-ggplot(new, aes(TypeOfDay, interpWindDir, color=factor(TypeOfDay)))+geom_point()+
  geom_boxplot()+
  TDinterpWindDirtitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpWindDir per \nTypeOfDay")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpWindDir~TypeOfDay, new, mean), aes(label = interpWindDir, y = interpWindDir + 15), col="black")
#/interpWindDir
proc.time() - ptm
#print(TDinterpWindDirBP)

#interpCondTemp
ptm <- proc.time()
TDinterpCondTemptitle = ggtitle('TypeOfDay interpCondTemp Profile')
TDinterpCondTempBP<-ggplot(new, aes(TypeOfDay, interpCondTemp, color=factor(TypeOfDay)))+geom_point()+
  geom_boxplot()+
  TDinterpCondTemptitle + theme(plot.title = element_text(size=20, face="bold", vjust=2))+
  theme(panel.grid.minor = element_line(colour="black", size=1.75)) + 
  #lines connecting the means of the boxes
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=2, color="black")+
  stat_summary(fun.y=mean, geom="point")+
  #/lines connecting the means of the boxes
  theme(panel.background = element_rect(fill = 'grey75'), #panel color
        panel.grid.major = element_line(colour = "white", size=2), #vertical grid
        panel.grid.minor = element_line(colour = "white"), #horizontal grid
        plot.background = element_rect(fill = 'white'))+ #background color
  theme(legend.key.size = unit(2.5, "cm"),
        legend.title = element_text(colour="chocolate", size=16, face="bold"))+ #legend
  scale_color_discrete(name="interpCondTemp per \nTypeOfDay")+ #/legend
  theme(legend.text = element_text(size = 15, face="bold"))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=1),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  theme(axis.text=element_text(size=15), #y-axis size
        axis.title=element_text(size=20,face="bold"))+ #y-axis label size
  theme(axis.text.x = element_text(size=19,color="black", face="bold"))+ # x-axis label size and color
  geom_text(data = aggregate(interpCondTemp~TypeOfDay, new, mean), aes(label = interpCondTemp, y = interpCondTemp + 1), col="black")
#/interpCondTemp
proc.time() - ptm
#print(TDinterpCondTempBP)

#Save to a pdf
pdf("TypeOfDayProfile.pdf", height=10, width=20)
print(TDNormalDLRBP)
print(TDCurrentBP)
TDinterpWindSpeedBP
TDinterpAmbTempBP
TDinterpSolarRadBP
TDinterpWindDirBP
TDinterpCondTempBP
print(grid.arrange(TDNormalDLRBP, TDCurrentBP, TDinterpWindSpeedBP, TDinterpAmbTempBP, 
                   TDinterpSolarRadBP, TDinterpWindDirBP, TDinterpCondTempBP, ncol=3))
dev.off()
#/Save to a pdf
#/More detailed boxplots