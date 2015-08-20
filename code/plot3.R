plot3 <- function()
{
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  # subset on baltimore city
  NEI_BCity <- subset(NEI, fips == "24510")
  
  # plot different types (total of 4) for sum of emissions vs years
  plot <- qplot(year, Emissions, data = NEI_BCity, 
                stat="summary", fun.y="sum", col = type, 
                geom = "line", xlab = "years (factor)", 
                ylab = "Emission (sum)", 
                main = "Emissions in Baltimore city for different 'types' during 1999 - 2008")
  print(plot)
  dev.copy(png, file = "plot3.png")
  dev.off()
}
