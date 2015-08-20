plot4 <- function()
{
  # read the data files
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  # Take out those rows which has coal as fuel type
  coal_scc <- SCC[grep("Coal", SCC$EI.Sector), 1]
  NEI_subset <- NEI[NEI$SCC %in% coal_scc,]
  
  # merge the file to get SCC names
  m <- merge(NEI_subset, SCC, by.x = "SCC", by.y = "SCC")[,c(1:9)]
  
  # draw graphs
  plot <- qplot(year, Emissions, data = m, stat="summary", 
                fun.y="sum", col = EI.Sector, geom = "line", 
                xlab = "years", ylab = "Emission (in Tons)", 
                main = "emissions from coal related sources (1999-2008)") +
                theme(legend.position="bottom", legend.direction="vertical")
  print(plot)
  dev.copy(png, file = "plot4.png")
  dev.off()
  
                
}
