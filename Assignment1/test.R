## Set-up
getwd()
setwd("~/Desktop/sfu/CMPT-318/Assignment1")


Datadf <- read.table("Group_Assignment_1_Dataset.txt", header = TRUE, sep = ",")
Datadf$Date <- as.POSIXlt(Datadf$Date, format = "%d/%m/%Y")
Datadf$Time <- as.POSIXlt(Datadf$Time, format = "%H:%M:%S")
ourData <- subset(Datadf, Datadf$Date >= as.POSIXlt("29/1/2007", format = "%d/%m/%Y") & Datadf$Date <= as.POSIXlt("4/2/2007", format = "%d/%m/%Y"))##week 5 data (starts on Jan 29, 2007 - Feb 4 2007 inclusive)

#AB (Global_active_power, Global_reactive_power)
AB <- cor(ourData$Global_active_power, ourData$Global_reactive_power, method = "pearson")

#AC (Global_active_power, Voltage)
AC <- cor(ourData$Global_active_power, ourData$Voltage, method = "pearson")

#AD (Global_active_power, Global_intensity)
AD <- cor(ourData$Global_active_power, ourData$Global_intensity, method = "pearson")

#AE (Global_active_power, Submetering 1)
AE <- cor(ourData$Global_active_power, ourData$Sub_metering_1, method = "pearson")

#AF (Global_active_power, Submetering 2)
AF <- cor(ourData$Global_active_power, ourData$Sub_metering_2, method = "pearson")

#AG (Global_active_power, Submetering 3)
AG <- cor(ourData$Global_active_power, ourData$Sub_metering_3, method = "pearson")

#BC (Global_reactive_power, Voltage)
BC <- cor(ourData$Global_reactive_power, ourData$Voltage, method = "pearson")

#BD (Global_reactive_power, Global_intensity)
BD <- cor(ourData$Global_reactive_power, ourData$Global_intensity, method = "pearson")

#BE (Global_reactive_power, Submetering 1)
BE <- cor(ourData$Global_reactive_power, ourData$Sub_metering_1, method = "pearson")

#BF (Global_reactive_power, Submetering 2)
BF <- cor(ourData$Global_reactive_power, ourData$Sub_metering_2, method = "pearson")

#BG (Global_reactive_power, Submetering 3)
BG <- cor(ourData$Global_reactive_power, ourData$Sub_metering_3, method = "pearson")

#CD (Voltage, Global_intensity)
CD <- cor(ourData$Voltage, ourData$Global_intensity, method = "pearson")

#CE (Voltage, Submetering 1)
CE <- cor(ourData$Voltage, ourData$Sub_metering_1, method = "pearson")

#CF (Voltage, Submetering 2)
CF <- cor(ourData$Voltage, ourData$Sub_metering_2, method = "pearson")

#CG (Voltage, SUbmetering 3)
CG <- cor(ourData$Voltage, ourData$Sub_metering_3, method = "pearson")

#DE (Global_intensity, Submetering 1)
DE <- cor(ourData$Global_intensity, ourData$Sub_metering_1, method = "pearson")

#DF (Global_intensity, Submetering 2)
DF <- cor(ourData$Global_intensity, ourData$Sub_metering_2, method = "pearson")

#DG (Global_intensity, Submetering 3)
DG <- cor(ourData$Global_intensity, ourData$Sub_metering_3, method = "pearson")

#EF (Submetering 1, Submetering 2)
EF <- cor(ourData$Sub_metering_1, ourData$Sub_metering_2, method = "pearson")

#EG (Submetering 1, Submetering 3)
EG <- cor(ourData$Sub_metering_1, ourData$Sub_metering_3, method = "pearson")

#FG (Submetering 2, Submetering 3)
FG <- cor(ourData$Sub_metering_2, ourData$Sub_metering_3, method = "pearson")

cat(sprintf("Correlation between AB: %0.2f
            Correlation between AC: %0.2f
            Correlation between AD: %0.2f
            Correlation between AE: %0.2f
            Correlation between AF: %0.2f
            Correlation between AG: %0.2f
            Correlation between BC: %0.2f
            Correlation between BD: %0.2f
            Correlation between BE: %0.2f
            Correlation between BF: %0.2f
            Correlation between BG: %0.2f
            Correlation between CD: %0.2f
            Correlation between CE: %0.2f
            Correlation between CF: %0.2f
            Correlation between CG: %0.2f
            Correlation between DE: %0.2f
            Correlation between DF: %0.2f
            Correlation between DG: %0.2f
            Correlation between EF: %0.2f
            Correlation between EG: %0.2f
            Correlation between FG: %0.2f", AB,AC,AD,AE,AF,AG,BC,BD,BE,BF,BG,CD,CE,CF,CG,DE,DF,DG,EF,EG,FG))
