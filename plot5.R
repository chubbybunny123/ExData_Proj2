## Coursera - Exploring Data
## Project 2

## function: plot5
## Emissions from motor vehicle sources changed from 1999–2008 in
## Baltimore City?

## going to assume that this is just the "raod" type source

## Plot description: 
##     4 panel plot (source), Baltimore only
##     y = sum(Emissions by year), x = year

## Inputs:  none
## Outputs: PNG file ./plot5.png

## run using plot5()


plot5 <- function() {

    ## Download file if necessary. 
    localname <- "NEI_data.zip"
    
    if (!file.exists(localname)) {
        myurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        download.file(myurl, destfile = localname, method = "wget")
        unzip(localname)
    }
    
    ## This first line will likely take a few seconds. Be patient!
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")

    
    ## Make the plot
    require(plyr)
    require(ggplot2)
    BaltimoreCars <- subset(NEI, ((NEI$fips == "24510") & (NEI$type == "ON-ROAD")), )
    BalCars <- ddply(BaltimoreCars, .(year), summarise, sum=sum(Emissions))
    # head(BalCars, n=4)
    
    png("plot5.png") # default is width=480, height=480
    print(qplot(year, sum, data = BalCars,
          , ylab = "PM[2.5] Emissions" 
          , xlab = "Year" 
          , main = "Baltimore City, MD PM2.5 Emissions from Cars"
    ))
    dev.off()
}