plot1 <- function()
{
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  png("plot1.png")
  par(mfrow = c(2,1))
  
  # factor on year and sum of emissions
  df <- ddply(NEI, .(year), summarize, sum = sum(Emissions))
  with(df, plot(year, sum, pch=19, col="red", type = "o", 
                xlab="Years", ylab="sum (Emissions)"))
  title(sub = "Graph of sum of emission vs years")
  title(main = "Graphs showing emissions for United states from 1999 - 2008")
  
  # factor on year and mean of emissions
  df <- ddply(NEI, .(year), summarize, mean = mean(Emissions))
  with(df, plot(year, mean, pch=19, col="blue", 
                type = "o", xlab="Years", ylab="mean (Emissions)"))
  title(sub = "Graph of mean of emission vs years")
  
  # plots two graphs, one sum of emissions vs years
  # other mean of emissions vs years
  
  dev.off()
}