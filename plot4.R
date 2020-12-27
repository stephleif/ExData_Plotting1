plot4 <- function(datafile=
                      "C:/Users/steph/data/power/household_power_consumption.txt"){
    
    ## only using data from the dates
    ## 2007-02-01 and
    ## 2007-02-02
    ## na = "?"
    
    ## libraries
    library(dplyr)
    library(lubridate)
    
    ## makes a 2 by 2 stack of 4 plots
    
    alldata <- read.table(file = datafile,
                          header = TRUE,
                          sep = ";",
                          na.strings = "?")
    ## to convert dates, we have 16/12/2006
    ## default format is "%Y-%m-%d %H:%M:%S" 
    
    
    mydata <- alldata %>% ## convert the dates
        mutate(theDate = strptime(x =Date, 
                                  format ="%d/%m/%Y")) %>%
        subset(theDate == as.Date("2007-02-01") |
                   theDate == as.Date("2007-02-02")) %>%
        mutate(theDatetime = as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M")) 
    
    ## PNG file with a width of 480 pixels and a height of 480 pixels.
    ## file name plot3.png
    
    ##print(names(mydata))
    
    png(filename = "plot4.png", width = 480, height = 480)
    
    ## do the layout
    par(mfrow=c(2,2))
    ##1 is same as plot 2 assignment
    plot(mydata$Global_active_power~mydata$theDatetime, type="l",
         ylab="Global Active Power (kilowatts)", xlab="")
    ##2 is y Voltage x datetime
    plot(mydata$Voltage ~mydata$theDatetime, type = "l",
         ylab = "Voltage", xlab = "datetime")
    ##3 is plot 3
    plot(mydata$Sub_metering_1
         ~mydata$theDatetime, type="l", col = "black",ylab="Energy sub metering", xlab="")
    lines(mydata$Sub_metering_2
          ~mydata$theDatetime,  col = "red")
    lines(mydata$Sub_metering_3
          ~mydata$theDatetime,  col = "blue")
    
    legend(x = "topright",
           legend =c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           col=c("black", "red","blue"),lty= c(1,1,1))
    
    ##4 y is Global_reactive_power, x is datetime
    plot(mydata$Global_reactive_power ~ mydata$theDatetime, type = "l",
         ylab = "Global reactive power", xlab = "datetime")
  
    
    dev.off() ## close the png file
}