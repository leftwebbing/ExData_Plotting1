# generates plot4.png

#read file into data frame

#plot 4: multi 2x2 plot 
#can recycle previous plots 2 & 3

# follow example from cookbook-r, not actually my code 
# from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/

# Multiple plot function
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


############### MY CODE BELOW ############### 

setwd("C:/Users/wubbummery/Documents/coursera - exploratory data analysis 2015")
#read in whole file in raw format
mainDF <- read.csv("household_power_consumption.txt", sep=";", colClasses = "character")

mainDF<- mainDF[grepl("1/2/2007|2/2/2007",mainDF$Date),] # subset by date
for(x in 3:9) mainDF[,x] <- as.numeric(mainDF[,x] )# convert numeric columns
mainDF$Date <- as.Date(mainDF$Date, format="%d/%m/%Y" )# convert Date column
mainDF$DateTime <- as.POSIXct(paste(mainDF$Date, mainDF$Time), format="%Y-%m-%d %H:%M:%S") # add combined column
mainDF$DateTime <- as.Date( mainDF$DateTime )
mainDF$Weekday <- format(mainDF$Date, format="%A")# extract weekday, make new column

# recycle old plot 2 as new plot 1
p1 <- ggplot( data = mainDF[1:2880,], aes(DateTime, Global_active_power)) +
  theme_bw()+ # sets black & white theme
  ylab("Global active power (KW)")+ # adds y label
  xlab("")+  # adds x label
  geom_line() +
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%A\n%d %b\n%Y") ) # adds scale

# recycle old plot 3 as new plot 2, plus some tweaks to legend
p2 <- ggplot(data = mainDF[1:2880,], aes(x = DateTime)) +
  geom_line(aes(y = Sub_metering_1, colour = "Sub_metering_1")) +  #draw subs 1,2,3
  geom_line(aes(y = Sub_metering_2, colour = "Sub_metering_2")) +
  geom_line(aes(y = Sub_metering_3, colour = "Sub_metering_3")) +

  ylab('Energy Sub Metering') +
  xlab("") +
  labs(color="") + # set legend title
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%A\n%d %b\n%Y") ) + # adds scale and date labels
  theme_bw()+ # sets black & white theme
  theme(legend.justification=c(1,1), 
    legend.position=c(1,1),
    legend.text = element_text(size=6),
    legend.title = element_blank(),
    legend.margin=unit(0, "lines") ) + # places legend upper right corner
  scale_colour_manual(values=c("black","red","blue"))

# make new plot 3, voltage vs datetime
p3 <- ggplot( data = mainDF[1:2880,], aes(DateTime, Voltage)) +
  geom_line() +
  theme_bw()+ # sets black & white theme
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%A\n%d %b\n%Y") ) # adds scale and date labels

# make new plot 4, global reactive power vs datetime
p4 <- ggplot( data = mainDF[1:2880,], aes(DateTime, Global_reactive_power)) +
  geom_line() +
  theme_bw()+ # sets black & white theme
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%A\n%d %b\n%Y") ) # adds scale and date labels

multiplot(p1, p2, p3, p4, cols=2)  #actually displays multi plot 2x2

# save to file
dev.copy(png, file ="plot4.png")
dev.off()


# followed example from
# http://stackoverflow.com/questions/17073772/ggplot2-legend-on-top-and-margin
