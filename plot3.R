plot3 <- function(datafile=
                      "C:/Users/steph/data/power/household_power_consumption.txt"){
    
    ## only using data from the dates
    ## 2007-02-01 and
    ## 2007-02-02
    ## na = "?"
    
    ## libraries
    library(dplyr)
    library(lubridate)
 
    
    
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
    
    png(filename = "plot3.png", width = 480, height = 480)
   
    ## y axis Energy sub metering
    ## x axis is date like in plot 2
    ## black Sub_metering_1
    ## red Sub_metering_2
    ## blue Sub_metering_3
    
    
     plot(mydata$Sub_metering_1
              ~mydata$theDatetime, type="l", col = "black",ylab="Energy sub metering", xlab="")
     lines(mydata$Sub_metering_2
          ~mydata$theDatetime,  col = "red")
    lines(mydata$Sub_metering_3
          ~mydata$theDatetime,  col = "blue")
  
     legend(x = "topright",
            legend =c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
                            col=c("black", "red","blue"),lty= c(1,1,1))
    
    
    
    dev.off() ## close the png file
}