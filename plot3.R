## Coursera - Exploring Data
## Project 2

## function: plot3
## Plot description: 
##     4 panel plot (source), Baltimore only
##     y = sum(Emissions by year), x = year

## Inputs:  none
## Outputs: PNG file ./plot3.png

## run using plot3()


plot3 <- function() {

    ## Download file if necessary. 
    localname <- "NEI_data.zip"
    
    if (!file.exists(localname)) {
        myurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        download.file(myurl, destfile = localname, method = "wget")
        unzip(localname)
    }
    
    ## This first line will likely take a few seconds. Be patient!
#     NEI <- readRDS("summarySCC_PM25.rds")
#     SCC <- readRDS("Source_Classification_Code.rds")

    
    ## Make the plot
    require(plyr)
    require(ggplot2)
    Baltimore <- subset(NEI, NEI$fips == "24510", )
    BalSources <- ddply(Baltimore, .(year, type), summarise, sum=sum(Emissions))
    #head(BalSources, n=3)
    # year     type      sum
    # 1 1999 NONPOINT 2107.625
    # 2 1999 NON-ROAD  522.940
    # 3 1999  ON-ROAD  346.820
    
    png("plot3.png", width=1280, height=480) # default is width=480, height=480
    print(qplot(year, sum, data = BalSources, facets = .~type
          , ylab = "PM[2.5]" 
          , xlab = "Year" 
          , main = "Baltimore City, MD PM2.5 Emission by Type"
    ))
    dev.off()
}