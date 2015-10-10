# generates plot1.png

library(lubridate) # has hour, minute functions
require(ggplot2)
require(scales)

setwd("C:/Users/wubbummery/Documents/coursera - exploratory data analysis 2015")
#read in whole file in raw format
mainDF <- read.csv("household_power_consumption.txt", sep=";", colClasses = "character")

mainDF<- mainDF[grepl("1/2/2007|2/2/2007",mainDF$Date),] # subset by date
for(x in 3:9) mainDF[,x] <- as.numeric(mainDF[,x] )# convert numeric columns
mainDF$Date <- as.Date(mainDF$Date, format="%d/%m/%Y" )# convert Date column
mainDF$DateTime <- as.POSIXct(paste(mainDF$Date, mainDF$Time), format="%Y-%m-%d %H:%M:%S") # add combined column
mainDF$DateTime <- as.Date( mainDF$DateTime )
mainDF$Weekday <- format(mainDF$Date, format="%A")# extract weekday, make new column


#plot 1: frequency histogram of global active power
hist(mainDF$Global_active_power, col="red", main ="Global active power", xlab="Global active power (KW)")
#data in example seems to be scaled, since graph shapes are identical except for maximum value

# save to file
dev.copy(png, file ="plot1.png")
dev.off()


