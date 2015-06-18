library(ggplot2)

# Loading provided datasets
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot 1
sumEmission <- tapply(NEI$Emissions, NEI$year, sum)
png(filename='plot1.png')
barplot(sumEmission, main='Total Emission of PM2.5', xlab='Year', ylab='PM2.5 in Kilotons')
dev.off()

# Plot 2
NEI_Baltimore <- NEI[NEI$fips == "24510",]
sumEmissionBaltimore <- tapply(NEI_Baltimore$Emissions, NEI_Baltimore$year, sum)
png(filename='plot2.png')
barplot(sumEmissionBaltimore, main='Total Emission of PM2.5 in Baltimore', xlab='Year', ylab='PM2.5 in Kilotons')
dev.off()

# Plot 3
# NEI_Baltimore <- NEI[NEI$fips == "24510",]
NEI_Baltimore$year <- factor(NEI_Baltimore$year, levels=c('1999', '2002', '2005', '2008'))
png(filename='plot3.png')
ggplot(data=NEI_Baltimore, aes(x=year, y=Emissions, fill=type)) + facet_grid(. ~ type) +
    geom_bar(stat='identity') +
    ylab('Log of PM2.5 Emissions') + xlab('Year') + 
    ggtitle('Emissions per Type in Baltimore City, Maryland') +
    guides(fill=FALSE)
dev.off()

# Plot 4
SCC_coal = SCC[grepl("coal", SCC$Short.Name, ignore.case=TRUE),]
NEI_coal <- merge(NEI, SCC_coal, by='SCC')
sumEmissionCoal <- tapply(NEI_coal$Emissions, NEI_coal$year, sum)
png(filename='plot4.png')
barplot(sumEmissionCoal, main='Total Emission of PM2.5 due to coal', xlab='Year', ylab='PM2.5 in Kilotons')
dev.off()

# Plot 5
NEI_Baltimore_Road <- NEI[NEI$fips == "24510" & NEI$type == 'ON-ROAD',]
sumEmissionRoadBaltimore <- tapply(NEI_Baltimore_Road$Emissions, NEI_Baltimore_Road$year, sum)
png(filename='plot5.png')
barplot(sumEmissionRoadBaltimore, main='Total Emission of PM2.5 due to MV in Baltimore', xlab='Year', ylab='PM2.5 in Kilotons')
dev.off()

# Plot 6
NEI_LA_Road <- NEI[NEI$fips == "06037" & NEI$type == 'ON-ROAD',]
sumEmissionRoadLA <- tapply(NEI_LA_Road$Emissions, NEI_LA_Road$year, sum)
png(filename='plot6.png')
par(mfrow=c(1,2))
barplot(sumEmissionRoadBaltimore, main='Total Emission of PM2.5 due to MV in Baltimore', xlab='Year', ylab='PM2.5 in Kilotons')
barplot(sumEmissionRoadLA, main='Total Emission of PM2.5 due to MV in LA', xlab='Year', ylab='PM2.5 in Kilotons')
dev.off()