##plot1.R

plot1 <- function(datafile=
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
                     theDate == as.Date("2007-02-02")
                     )
    
    ## PNG file with a width of 480 pixels and a height of 480 pixels.
    ## file name plot1.png
    
    png(filename = "plot1.png",
       width = 480,
       height = 480)
    ## y = frequency, x = Global Active Power (kilowatts),
    ## color is red
    hist(mydata$Global_active_power, col = "red",
         main = "Global Active Power",
         xlab = "Global Active Power (kilowatts)", ylab = "Frequency")
    dev.off() ## close the png file
    
    
}