plot6 <- function()
{
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  NEI_BCity <- subset(NEI, fips == "24510")
  NEI_LA <- subset(NEI, fips == "06037")
  
  #subset on baltimore city and LA
  motor_scc <- SCC[grepl("mobile", SCC$EI.Sector, ignore.case = TRUE), 1]
  NEI_subset_BCity <- NEI_BCity[NEI_BCity$SCC %in% motor_scc,]
  NEI_subset_LA <- NEI_LA[NEI_LA$SCC %in% motor_scc,]
  
  # merge
  m_BCity <- merge(NEI_subset_BCity, SCC, by.x = "SCC", by.y = "SCC")[,c(1:9)]
  m_LA <- merge(NEI_subset_LA, SCC, by.x = "SCC", by.y = "SCC")[,c(1:9)]
  
  # add city names and row bind them
  m_BCity$city <- "Baltimore City"
  m_LA$city <- "Los Angeles County"
  bothCity <- rbind(m_BCity, m_LA)
  
  # plot for both cities on all the motor types.
  # different motor types not shows as was did for plot5. 
  # all the motor types are taken together.
  plot <- qplot(year, Emissions, data = bothCity, 
                stat = "summary", fun.y = "mean", 
                geom = "line", col = city, facets = .~city,
                xlab = "years", ylab = "mean Emission (in Tons)",
                main = "Motor vehicle sources, Baltimore City & LA (1999-2008)")
  print(plot)
  dev.copy(png, file = "plot6.png")
  dev.off()
  
  
}
