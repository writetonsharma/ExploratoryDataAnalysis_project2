plot5 <- function()
{
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  NEI_BCity <- subset(NEI, fips == "24510")
  
  # Take out those rows which has coal as fuel type
  motor_scc <- SCC[grepl("mobile", SCC$EI.Sector, ignore.case = TRUE), 1]
  NEI_subset <- NEI_BCity[NEI_BCity$SCC %in% motor_scc,]
  
  # merge the file to get SCC names
  m <- merge(NEI_subset, SCC, by.x = "SCC", by.y = "SCC")[,c(1:9)]
  
  #plot seperately for every motor type
  par(mfrow = c(3,4))
  #title(main = "emissions from motor vehicle sources (1999-2008)")
  counter <- 0
  for(i in unique(factor(m$EI.Sector)))
  {
    s <- subset(m, m$EI.Sector == i)
    df <- ddply(s, .(year), summarize, mean = mean(Emissions))
    with(df, plot(year, mean, pch=19, col="red", type = "o", 
                  xlab="Years", ylab="mean (Emissions)"))

    title(sub = i)
  }
  # draw graphs
  #   plot <- qplot(year, Emissions, data = m, stat="summary", 
  #                 fun.y="mean", col = EI.Sector, geom = "line", 
  #                 xlab = "years", ylab = "mean Emission (in Tons)", 
  #                 main = "emissions from motor vehicle sources (1999-2008)") +
  #                 theme(legend.position="right", legend.direction="vertical")
  #print(plot)
  dev.copy(png, file = "plot5.png")
  dev.off()
  
}
