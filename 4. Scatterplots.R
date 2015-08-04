### KTH, Department of Industrial Information and Control Systems (ICS)
### Master Thesis Project
### BUILDING PREDICTIVE MODELS FOR DYNAMIC LINE RATING USING DATA SCIENCE TECHNIQUES 
### Sponsored by Vattenfall AG
### Author: Nicolae Doban
### Stockholm, 2015

## This module is processing and builds the scatterplots
## Packages used: base
## INPUT DATA: The interpolated and processed dataset new.Rda
## OUTPUT: Scatterplots

newDir <- setwd("C:/Users/LabStudent/Desktop/Nick/GitHub") #set the directory of the dataset
load("new.Rda") #loading the dataset

                                                    #Scatterplots
colors <- ifelse(new$NormalDLR>new$Current, "green", "red") #preparing distinct colors for NormalDLR and Current parameters
pdf("Scatterplots.pdf", height=20, width=20)


#Scatterplots used in the report
#without the constant NormalDLR values
new<-new[c(20161:305279),];  rownames(new)<-NULL #omit the mean values on the plot. These values are present 
                                                 #because the NormalDLR started to be calculated from Sept., 15th 2014

png("Scatterplots.png", width=1300, height=650)
par(mfrow=c(1,2))
plot(new$NormalDLR~new$interpWindSpeed, xlab="WindSpeed [m/s]", cex.lab=1.5, ylab="Line Current Intensity [A]", main="NormalDLR and Load Current VS WindSpeed", las=1, col="dodgerblue3")
points(new$Current~new$interpWindSpeed, col=colors)
legend("topright", legend=c("NormalDLR", "Load Current"), fill=c("dodgerblue3", "green"), bty="n")

plot(new$NormalDLR~new$interpCondTemp, cex.lab=1.5, xlab="CondTemp [C]", ylab="Line Current Intensity [A]", main="NormalDLR and Load Current VS CondTemp", las=1, col="dodgerblue3")
points(new$Current~new$interpCondTemp, col=colors)
legend("topright", legend=c("NormalDLR", "Load Current"), fill=c("dodgerblue3", "green"), bty="n")
dev.off()
#/Scatterplots used in the report
#/without the constant NormalDLR values


#More detailed Scatterplots
#new$interpWindSpeed
par(mfrow=c(1,2))
plot(new$NormalDLR~new$interpWindSpeed, xlab="new$interpWindSpeed (m/s)", ylab="Current Intensity (A)", main="new. NormalDLR and Current VS WindSpeed", las=1, col="dodgerblue3")
points(new$Current~new$interpWindSpeed, col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")
#Logarithmic scale
plot(log(new$NormalDLR)~log(new$interpWindSpeed), xlab="new$interpWindSpeed (m/s)", ylab="Current Intensity (A)", main="new. NormalDLR and Current VS WindSpeed Log Scale", las=1, col="dodgerblue3")
points(log(new$Current)~log(new$interpWindSpeed), col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")

#interpAmbTemp
par(mfrow=c(1,2))
plot(new$NormalDLR~new$interpAmbTemp, xlab="interpAmbTemp (C)", ylab="Current Intensity (A)", main="new. NormalDLR and Current VS interpAmbTemp", las=1, col="dodgerblue3")
points(new$Current~new$interpAmbTemp, col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")
#Logarithmic scale
plot(log(new$NormalDLR)~log(new$interpAmbTemp), xlab="interpAmbTemp (C)", ylab="Current Intensity (A)", main="new. NormalDLR and Current VS interpAmbTemp. Log Scale", las=1, col="dodgerblue3")
points(log(new$Current)~log(new$interpAmbTemp), col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")

#interpCondTemp
par(mfrow=c(1,2))
plot(new$NormalDLR~new$interpCondTemp, xlab="CondTemp (C)", ylab="Line Current Intensity (A)", main="NormalDLR and Current VS CondTemp", las=1, col="dodgerblue3")
points(new$Current~new$interpCondTemp, col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")
#Logarithmic scale
plot(log(new$NormalDLR)~log(new$interpCondTemp), xlab="interpCondTemp (C)", ylab="Current Intensity (A)", main="new. NormalDLR and Current VS interpCondTemp Log Scale", las=1, col="dodgerblue3")
points(log(new$Current)~log(new$interpCondTemp), col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")

#interpSolarRad
par(mfrow=c(1,2))
plot(new$NormalDLR~new$interpSolarRad, xlab="interpSolarRad (W/m^2)", ylab="Current Intensity (A)", main="new. NormalDLR and Current VS interpSolarRad", las=1, col="dodgerblue3")
points(new$Current~new$interpSolarRad, col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")
#Logarithmic scale
plot(log(new$NormalDLR)~log(new$interpSolarRad), xlab="interpSolarRad (W/m^2)", ylab="Current Intensity (A)", main="new. NormalDLR and Current VS interpSolarRad Log Scale", las=1, col="dodgerblue3")
points(log(new$Current)~log(new$interpSolarRad), col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")

#interpWindDir
par(mfrow=c(1,2))
plot(new$NormalDLR~new$interpWindDir, xlab="interpWindDir (degrees)", ylab="Current Intensity (A)", main="new. NormalDLR and Current VS interpWindDir", las=1, col="dodgerblue3")
points(new$Current~new$interpWindDir, col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")
#Logarithmic scale
plot(log(new$NormalDLR)~log(new$interpWindDir), xlab="interpWindDir (degrees)", ylab="Current Intensity (A)", main="new. NormalDLR and Current VS interpWindDir Log Scale", las=1, col="dodgerblue3")
points(log(new$Current)~log(new$interpWindDir), col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")

                                                #Scatterplots On One Page without log graphs
par(mfrow=c(2,3))
colors <- ifelse(new$NormalDLR>new$Current, "green", "red")
#windSpeed
plot(new$NormalDLR~new$interpWindSpeed, xlab="new$interpWindSpeed (m/s)", ylab="Current Intensity (A)", main="new. NormalDLR and Current VS WindSpeed", las=1, col="dodgerblue3")
points(new$Current~new$interpWindSpeed, col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")

#interpAmbTemp
plot(new$NormalDLR~new$interpAmbTemp, cex.lab=1.5, xlab="interpAmbTemp (C)", ylab="Current Intensity (A)", main="new. NormalDLR and Current VS interpAmbTemp", las=1, col="dodgerblue3")
points(new$Current~new$interpAmbTemp, col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")

#interpCondTemp
plot(new$NormalDLR~new$interpCondTemp, cex.lab=1.5, xlab="interpCondTemp (C)", ylab="Current Intensity (A)", main="new. NormalDLR and Current VS interpCondTemp", las=1, col="dodgerblue3")
points(new$Current~new$interpCondTemp, col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")

#interpSolarRad
plot(new$NormalDLR~new$interpSolarRad, cex.lab=1.5, xlab="interpSolarRad (W/m^2)", ylab="Current Intensity (A)", main="new. NormalDLR and Current VS interpSolarRad", las=1, col="dodgerblue3")
points(new$Current~new$interpSolarRad, col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")

#interpWindDir
plot(new$NormalDLR~new$interpWindDir, cex.lab=1.5, xlab="interpWindDir (degrees)", ylab="Current Intensity (A)", main="new. NormalDLR and Current VS interpWindDir", las=1, col="dodgerblue3")
points(new$Current~new$interpWindDir, col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")
dev.off()
                                                  #On One Page
pdf("ScatterplotsOnePage.pdf", width=15)
par(mfrow=c(2,3))
colors <- ifelse(new$NormalDLR>new$Current, "green", "red")
#windSpeed
plot(new$NormalDLR~new$interpWindSpeed, xlab="WindSpeed [m/s]", ylab="Current Intensity [A]", main="NormalDLR and Current VS WindSpeed", las=1, col="dodgerblue3")
points(new$Current~new$interpWindSpeed, col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")

#interpAmbTemp
plot(new$NormalDLR~new$interpAmbTemp, cex.lab=1.5, xlab="AmbTemp [C]", ylab="Current Intensity [A]", main="NormalDLR and Current VS AmbTemp", las=1, col="dodgerblue3")
points(new$Current~new$interpAmbTemp, col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")

#interpCondTemp
plot(new$NormalDLR~new$interpCondTemp, cex.lab=1.5, xlab="CondTemp [C]", ylab="Current Intensity [A]", main="NormalDLR and Current VS CondTemp", las=1, col="dodgerblue3")
points(new$Current~new$interpCondTemp, col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")

#interpSolarRad
plot(new$NormalDLR~new$interpSolarRad, cex.lab=1.5, xlab="SolarRad [W/m^2]", ylab="Current Intensity [A]", main="NormalDLR and Current VS SolarRad", las=1, col="dodgerblue3")
points(new$Current~new$interpSolarRad, col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")

#interpWindDir
plot(new$NormalDLR~new$interpWindDir, cex.lab=1.5, xlab="WindDir [degrees]", ylab="Current Intensity [A]", main="NormalDLR and Current VS WindDir", las=1, col="dodgerblue3")
points(new$Current~new$interpWindDir, col=colors)
legend("topright", legend=c("NormalDLR", "Current"), fill=c("dodgerblue3", "green"), bty="n")
dev.off()
                                                  #/Scatterplots
