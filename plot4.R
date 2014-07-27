## Coursera - Exploring Data
## Project 2

## function: plot4
# Across the United States change in emissions from coal combustion-related
# sources from 1999–2008?

## Plot description: 
##     4 panel plot (source), Baltimore only
##     y = sum(Emissions by year), x = year

## Strategy
## 1. find which SCC codes in SCC correspond to Coal and Comb
## 2. use those SCC codes to subset rows in NEI
## 3. sum the total emissions grouped by year
## 4. plot!

## Inputs:  none
## Outputs: PNG file ./plot4.png

## run using plot4()


plot4 <- function() {

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

    SCCmask3Comb <- grepl("Comb", SCC$Short.Name)
    # sum(SCCmask3Comb)
    # #593

    SCCmask3Coal <- grepl("Coal", SCC$Short.Name)
    # sum(SCCmask3Coal)   
    # #230

    SCCmask <- SCCmask3Comb & SCCmask3Coal
    # sum(SCCmask)
    # 91

    CoalCombSCC <- SCC[SCCmask, 1]
    CoalCombNEI <- subset(NEI, NEI$SCC %in% CoalCombSCC, )

    CCemissByYear <- ddply(CoalCombNEI, . (year), summarise, sum=sum(Emissions))
    # CCemissByYear
    # year      sum
    # 1 1999 575206.5
    # 2 2002 547380.1
    # 3 2005 553549.4
    # 4 2008 343979.3

    png("plot4.png") # default is width=480, height=480
    with(CCemissByYear, plot(sum ~ year, type = "b"
                         , main = "Total U.S. Coal PM2.5 Emissions by Year"
                         , xlab = "Year"
                         , ylab = "Coal PM2.5 Emissions (tons)"))
    dev.off()
}