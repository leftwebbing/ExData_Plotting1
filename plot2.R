# global active power vs weekday

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

###### plot 2: time plot of global active power vs weekday
ggplot( data = mainDF[1:2880,], aes(DateTime, Global_active_power)) +
theme_bw()+ # sets black & white theme
ylab("Global active power (KW)")+ # adds y label
xlab("")+  # adds x label
geom_line() +
scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%A\n%d %b\n%Y") ) # adds scale
# followed examples from:
# http://stackoverflow.com/questions/24569628/date-format-for-subset-of-ticks-on-time-axis
# http://stackoverflow.com/questions/10438752/adding-x-and-y-axis-labels-in-ggplot2

# save to file
dev.copy(png, file ="plot2.png")
dev.off()
