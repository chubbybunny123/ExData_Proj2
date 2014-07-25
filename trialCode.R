setwd("~/Desktop/ubuntu-share/coursera-exploreData/ExData_Proj2")

myurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
localname <- "NEI_data.zip"

if (!file.exists(localname)) {
    download.file(myurl, destfile = localname, method = "wget")
    unzip(localname)
}

## This first line will likely take a few seconds. Be patient!
if (!exists("NEI")) {
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
}

# contents:

# PM2.5 Emissions Data (summarySCC_PM25.rds): This file contains a data frame
# with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. For each
# year, the table contains number of tons of PM2.5 emitted from a specific type
# of source for the entire year. 
# 
# fips: A five-digit number (represented as a string) indicating the U.S. county
# SCC: The name of the source as indicated by a digit string 
#  (see source code classification table)
# Pollutant: A string indicating the pollutant
# Emissions: Amount of PM2.5 emitted, in tons
# type: The type of source (point, non-point, on-road, or non-road)
# year: The year of emissions recorded

# Source Classification Code Table (Source_Classification_Code.rds): This table
# provides a mapping from the SCC digit strings in the Emissions table to the
# actual name of the PM2.5 source. The sources are categorized in a few
# different ways from more general to more specific and you may choose to
# explore whatever categories you think are most useful. For example,
# source “10100101” is known as
# “Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal”.

# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to
# 2008? Using the base plotting system, make a plot showing the total PM2.5
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.

# Want y = sum(Emissions by year), x = year
require(plyr)
emissionsByYear <- ddply(NEI, . (year), summarise, sum=sum(Emissions))
emissionsByYear
#  bleh <- tapply(NEI$Emissions, NEI$year, sum)
# year     sum
# 1 1999 7332967
# 2 2002 5635780
# 3 2005 5454703
# 4 2008 3464206

with(emissionsByYear, plot(sum ~ year, type = "b"
                           , main = "Total U.S. PM2.5 Emissions by Year"
                           , xlab = "Year"
                           , ylab = "Total PM2.5 Emissions (tons)"))

# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot
# answering this question.

Baltimore <- subset(NEI, NEI$fips == "24510", )
require(plyr)
BalEmissions <- ddply(Baltimore, .(year), summarise, sum=sum(Emissions))
BalEmissions

with(BalEmissions, plot(sum ~ year, type = "b"
                           , main = "Total Baltimore City, MD PM2.5 Emissions by Year"
                           , xlab = "Year"
                           , ylab = "Total PM2.5 Emissions (tons)"))

# 3. Of the four types of sources indicated by the type (point, nonpoint, onroad,
# nonroad) variable, which of these four sources have seen decreases in emissions
# from 1999–2008 for Baltimore City? Which have seen increases in emissions from
# 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

# install.packages("ggplot2")   ## had to do this in R terminal
Baltimore <- subset(NEI, NEI$fips == "24510", )
require(plyr)
require(ggplot2)
BalSources <- ddply(Baltimore, .(year, type), summarise, sum=sum(Emissions))
head(BalSources, n=3)
# year     type      sum
# 1 1999 NONPOINT 2107.625
# 2 1999 NON-ROAD  522.940
# 3 1999  ON-ROAD  346.820

qplot(year, sum, data = BalSources, facets = .~type
      , ylab = "PM[2.5]" 
      , xlab = "Year" 
      , main = "Baltimore City, MD PM2.5 Emission by Type"
)
   

# ## Setup ggplot with data frame
# g <- ggplot(data = BalSources, aes(x = year, y = sum))
#     
# ## Add layers
# finalg <- (g + geom_point()
#            + facet_wrap( ~ .type, nrow = 1, ncol = 4)  ## panels
#            + labs(y = "PM[2.5]") 
#            + labs(x = "Year") 
#            + labs(title = "Baltimore City, MD PM2.5 Emission by Type"))
# 
# print(finalg)
# ##Error in layout_base(data, vars, drop = drop) : 
# ##    At least one layer must contain all variables used for facetting
#
# g+ geom_point()

# 4. Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?

# 3 Short.Name has comb and Coal
# 4 EI.Sector has Comb and Coal
# 9 SCC.Level.Three Coal   .... gives type of coal
# 10 SCC.Level.Four Coal  ... tells how fired?

# grepl or grep (word, data)
## 1. find which SCC codes in SCC correspond to Coal and Comb
## 2. use those SCC codes to subset rows in NEI
## 3. sum the total emissions grouped by year
## 4. plot!
require(plyr)
require(ggplot2)
SCCmask3Comb <- grepl("Comb", SCC$Short.Name)
# sum(SCCmask3Comb)
# #593

SCCmask3Coal <- grepl("Coal", SCC$Short.Name)
# sum(SCCmask3Coal)   
# #230
# c3 <- grep("Coal", SCC$Short.Name)
# c9 <-grep("Coal", SCC$SCC.Level.Three)
# #172
# c10 <- grep("Coal", SCC$SCC.Level.Four)
# #126

SCCmask <- SCCmask3Comb & SCCmask3Coal
sum(SCCmask)
# 91

# tail(SCC[SCCmask3Comb, 3])
# tail(SCC[SCCmask3Coal, 3])
# tail(SCC[SCCmask, 3])

CoalCombSCC <- SCC[SCCmask, 1]
CoalCombNEI <- subset(NEI, NEI$SCC %in% CoalCombSCC, )
# which(CoalCombSCC == CoalCombNEI$SCC[20000])

CCemissByYear <- ddply(CoalCombNEI, . (year), summarise, sum=sum(Emissions))
CCemissByYear
# year      sum
# 1 1999 575206.5
# 2 2002 547380.1
# 3 2005 553549.4
# 4 2008 343979.3

with(CCemissByYear, plot(sum ~ year, type = "b"
                           , main = "Total U.S. Coal PM2.5 Emissions by Year"
                           , xlab = "Year"
                           , ylab = "Coal PM2.5 Emissions (tons)"))

# #######################
# 5. How have emissions from motor vehicle sources changed from 1999–2008 in
# Baltimore City?
## going to assume that this is just the "raod" type source

BaltimoreCars <- subset(NEI, ((NEI$fips == "24510") & (NEI$type == "ON-ROAD")), )
require(plyr)
require(ggplot2)
BalCars <- ddply(BaltimoreCars, .(year), summarise, sum=sum(Emissions))
# head(BalCars, n=4)

qplot(year, sum, data = BalCars,
      , ylab = "PM[2.5] Emissions" 
      , xlab = "Year" 
      , main = "Baltimore City, MD PM2.5 Emissions from Cars"
)

# #######################
# 6. Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?

Cars <- subset(NEI, (((NEI$fips == "24510") | (NEI$fips == "06037"))
                     & (NEI$type == "ON-ROAD")), )
require(plyr)
require(ggplot2)
SumCars <- ddply(Cars, .(year, fips), summarise, sum=sum(Emissions))
SumCars

fipsCode <- data.frame(list("fips" = c("06037", "24510")
                            , "area" = c("Los Angeles", "Baltimore City"))
)

SumCars <- merge(x=SumCars, y=fipsCode, by.x="fips", by.y="fips"
                      , all.x = TRUE, sort = FALSE)

qplot(year, sum, data = SumCars,
      , ylab = "PM[2.5] Emissions" 
      , xlab = "Year" 
      , main = "Baltimore City vs Los Angeles County PM2.5 Emissions from Cars"
      , facets = .~area
)

## calculate difference from 1999 emission total
LA1999 <- SumCars$sum[which(SumCars$area == "Los Angeles" 
                            & SumCars$year == 1999)]
BC1999 <- SumCars$sum[which(SumCars$area == "Baltimore City" 
                            & SumCars$year == 1999)]

SumCars$diff[SumCars$area == "Los Angeles"] <- 
    -(SumCars$sum[SumCars$area == "Los Angeles"] - LA1999)
# SumCars$diff[which(SumCars$area == "Los Angeles" & SumCars$year == 1999)]
# 0

SumCars$diff[SumCars$area == "Baltimore City"] <- 
    -(SumCars$sum[SumCars$area == "Baltimore City"] - BC1999)
# SumCars$diff[which(SumCars$area == "Baltimore City" & SumCars$year == 1999)]
# 0

qplot(year, diff, data = SumCars,
      , ylab = "Decrease from 1999 PM2.5 \n Emissions" 
      , xlab = "Year" 
      , main = paste("Decrease in Baltimore City vs Los Angeles County",
                     "PM2.5 Emissions from Cars \n Relative to 1999")
      , facets = .~area
)