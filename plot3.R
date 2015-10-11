# generates plot3.png

#read file into data frame
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


#plot energy sub metering vs weekday for 3 sub_meterings

ggplot(data = mainDF[1:2880,], aes(x = DateTime)) +
  geom_line(aes(y = Sub_metering_1, colour = "Sub_metering_1")) +  #draw subs 1,2,3
  geom_line(aes(y = Sub_metering_2, colour = "Sub_metering_2")) +
  geom_line(aes(y = Sub_metering_3, colour = "Sub_metering_3")) +

  ylab('Energy Sub Metering') +
  xlab("") +
  labs(color="variable") + # set legend title
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%A\n%d %b\n%Y") ) + # adds scale and date labels
  theme_bw()+ # sets black & white theme
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + # places legend upper right corner
  scale_colour_manual(values=c("black","red","blue"))

# save to file
dev.copy(png, file ="plot3.png")
dev.off()

# followed examples from:
# http://stackoverflow.com/questions/19921842/plotting-multiple-time-series-on-the-same-plot-using-ggplot
# http://stackoverflow.com/questions/10349206/add-legend-to-ggplot2-line-plot
# http://www.cookbook-r.com/Graphs/Legends_%28ggplot2%29/
