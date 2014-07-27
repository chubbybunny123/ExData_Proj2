## Coursera - Exploring Data
## Project 2

## function: plot1
## Plot description: 
##     y = sum(Emissions by year), x = year

## Inputs:  none
## Outputs: PNG file ./plot1.png

## run using plot1()


plot1 <- function() {
    ## Load base plotting package if necessary
    require(graphics)
    par(bg = NA)

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
    emissionsByYear <- ddply(NEI, . (year), summarise, sum=sum(Emissions))

    png("./plot1.png") # default is width=480, height=480
        with(emissionsByYear, plot(sum ~ year, type = "b"
                               , main = "Total U.S. PM2.5 Emissions by Year"
                               , xlab = "Year"
                               , ylab = "Total PM2.5 Emissions (tons)"))
    dev.off()
}