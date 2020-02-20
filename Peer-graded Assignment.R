                                   ###  1. Loading the data###

getwd()
Electric_Power_Consumption<-read.table("C:/Users/roberto.feraboli/Documents/Personal doc/R/COURSERA/DATA SCIENCE-COURSERA/MODULE 4 - Exploratory Data Analysis/Week 1/Dataset/household_power_consumption.txt",sep=";",header = TRUE,na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
str(Electric_Power_Consumption)

### Format date to Type Date

Electric_Power_Consumption$Date <- as.Date(Electric_Power_Consumption$Date, "%d/%m/%Y")
str(Electric_Power_Consumption)

###Filter data from the  2007-02-01 and 2007-02-02

Electric_Power_Consumption <- subset(Electric_Power_Consumption,Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))

###Remove NAs

# complete.cases(Electric_Power_Consumption)
Electric_Power_Consumption <- Electric_Power_Consumption[complete.cases(Electric_Power_Consumption),]

## Combine Date and Time column
dateTime <- paste(Electric_Power_Consumption$Date, Electric_Power_Consumption$Time)

## Remove Date and Time column

Electric_Power_Consumption <-Electric_Power_Consumption[,-c(1,2)]

## Add DateTime column
Electric_Power_Consumption <- cbind(dateTime, Electric_Power_Consumption)

## Format dateTime Column

library(lubridate)
Electric_Power_Consumption$dateTime <- ymd_hms(dateTime)



                                         ###2. Making Plots###

# Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
# Name each of the plot files as \color{red}{\verb|plot1.png|}plot1.png, \color{red}{\verb|plot2.png|}plot2.png, etc.


#First graph

hist(Electric_Power_Consumption$Global_active_power,
     xlab="Global Active Power (Kilowatts)", main="Global Active Power",col="red")

dev.copy(png,"plot1.png", width=480, height=480)
dev.off()


#Second graph
with(Electric_Power_Consumption, plot(dateTime,Global_active_power,xlab="DateTime",ylab="Global Active Power (Kilowatts)",type = "l"))
dev.copy(png,"plot2.png", width=480, height=480)
dev.off()


#Third graph

with(Electric_Power_Consumption, {
    plot(Sub_metering_1~dateTime, type="l",
         ylab="Energy sub metering", xlab="")
    lines(Sub_metering_2~dateTime,col='Red')
    lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"),lwd=c(1,1,1),
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

dev.copy(png,"plot3.png", width=480, height=480)
dev.off()


#Fourth graph

par(mfrow=c(2,2),mar=c(4,4,2,2))

with(Electric_Power_Consumption, {
     plot(dateTime,Global_active_power,ylab="Global Active Power",type = "l")
     plot(dateTime,Voltage,ylab="Voltage",type = "l")
     plot(Sub_metering_1~dateTime, type="l",
          ylab="Energy sub metering", xlab="")
     lines(Sub_metering_2~dateTime,col='Red')
     lines(Sub_metering_3~dateTime,col='Blue')
     plot(dateTime,Global_reactive_power,ylab="Global Reactive Power",type = "l")
     
} )

dev.copy(png,"plot4.png", width=480, height=480)
dev.off()





