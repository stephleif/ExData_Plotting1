plot2 <- function(datafile=
                      "C:/Users/steph/data/power/household_power_consumption.txt"){
    
    ## only using data from the dates
    ## 2007-02-01 and
    ## 2007-02-02
    ## na = "?"
    
    ## libraries
    library(dplyr)
    library(lubridate)
    library(lattice)

    
    alldata <- read.table(file = datafile,
                          header = TRUE,
                          sep = ";",
                          na.strings = "?")
    ## to convert dates, we have 16/12/2006
    ## default format is "%Y-%m-%d %H:%M:%S" 
    
    print(names(alldata))
    
    
    mydata <- alldata %>% ## convert the dates
        mutate(theDate = strptime(x =Date, 
                                  format ="%d/%m/%Y")) %>%
        subset(theDate == as.Date("2007-02-01") |
                   theDate == as.Date("2007-02-02")) %>%
        mutate(theDatetime = as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M")) 

    ## PNG file with a width of 480 pixels and a height of 480 pixels.
    ## file name plot2.png
    
    png(filename = "plot2.png", width = 480, height = 480)
     plot(mydata$Global_active_power~mydata$theDatetime, type="l",
          ylab="Global Active Power (kilowatts)", xlab="")


  
    dev.off() ## close the png file
}