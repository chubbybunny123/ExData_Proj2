## Coursera - Exploring Data
## Project 2

## function: plot6
## Emissions from motor vehicle sources changed from 1999–2008 in
## Baltimore City vs LA?

## going to assume that this is just the "raod" type source

## Plot description: 
##     2 panel plot (Baltimore vs LA)
##     y = diff from 1999 of sum(Emissions by year), x = year

## Inputs:  none
## Outputs: PNG file ./plot6.png

## run using plot6()


plot6 <- function() {

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
    Cars <- subset(NEI, (((NEI$fips == "24510") | (NEI$fips == "06037"))
                         & (NEI$type == "ON-ROAD")), )
    SumCars <- ddply(Cars, .(year, fips), summarise, sum=sum(Emissions))
    
    fipsCode <- data.frame(list("fips" = c("06037", "24510")
                                , "area" = c("Los Angeles", "Baltimore City"))
    )
    
    SumCars <- merge(x=SumCars, y=fipsCode, by.x="fips", by.y="fips"
                     , all.x = TRUE, sort = FALSE)
    
## ----- Basic plot of total(LA) vs total(Baltimore)    
#     png("plot6.png") # default is width=480, height=480
#     print(qplot(year, sum, data = SumCars,
#                 , ylab = "PM[2.5] Emissions" 
#                 , xlab = "Year" 
#                 , main = "Baltimore City vs Los Angeles County PM2.5 Emissions from Cars"
#                 , facets = .~area
#     ))
#     dev.off()

## ------ calculate difference from 1999 emission total ----------
    LA1999 <- SumCars$sum[which(SumCars$area == "Los Angeles" 
                        & SumCars$year == 1999)]
    BC1999 <- SumCars$sum[which(SumCars$area == "Baltimore City" 
                        & SumCars$year == 1999)]

    SumCars$diff[SumCars$area == "Los Angeles"] <- 
        (SumCars$sum[SumCars$area == "Los Angeles"] - LA1999)
    SumCars$diff[SumCars$area == "Baltimore City"] <- 
        (SumCars$sum[SumCars$area == "Baltimore City"] - BC1999)

    png("plot6.png", width=720) # default is width=480, height=480
    print(qplot(year, diff, data = SumCars,
            , ylab = "Change from 1999 PM2.5 \n Emissions" 
            , xlab = "Year" 
            , main = paste("Change in PM2.5 Emissions from Cars",
                           "\n Relative to 1999 for Baltimore City vs",
                           " Los Angeles County")
            , facets = .~area
    ))
    dev.off()

}