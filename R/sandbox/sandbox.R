
# Miscellenous analysis and reporting functions
# Anything here for a while should be moved somewhere else


## Generates various forms of time series
annual_total_costs_of_disasters_in_australia_time_series <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEvent()
  # Just for normalised data
  totalCostsByYear <- with(totalCosts, aggregate(Reported.cost.normalised, by=list(Year.financial), FUN=safeSum))
  
  # Taken from http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
  
  costSeries <- ts(totalCostsByYear$x, c(1967))
  plot.ts(costSeries)
  lines(lowess(time(costSeries), costSeries), col="blue", lwd=2)
  
  library("TTR")
  costSeriesSMA <- SMA(costSeries,n=8)
  plot.ts(costSeriesSMA)

  # Forecast from the first point forwards
  forecastedCosts <- HoltWinters(costSeries, beta=FALSE, gamma=FALSE, l.start=18927975956)
  #forecastedCosts <- HoltWinters(costSeries, beta=FALSE, gamma=FALSE)
  forecastedCosts
  plot(forecastedCosts)
  forecastedCostsNoGamma <- HoltWinters(costSeries, gamma=FALSE, l.start=18927975956)
  
  # https://www.google.com.au/url?sa=t&rct=j&q=&esrc=s&source=web&cd=9&cad=rja&uact=8&ved=0CFMQFjAI&url=http%3A%2F%2Fwww.ghement.ca%2FMann-Kendall%2520Trend%2520Test%2520in%2520R.doc&ei=Un_6VOTOIobW8gXYzYGwBA&usg=AFQjCNEO0yK7QxSgQFIwzs0WsObjQMCDNg&sig2=niAp_DilG2nPWaaBa_MsXw
  install.packages("Kendall")
  require(Kendall) 
  par(mfrow=c(2,1))
  # Autocorrelation
  acf(costSeries)
  # Partial Autocorrelation
  pacf(costSeries)
  res <- MannKendall(costSeries)
  print(res)
  summary(res)
  
  # Also see: 
  # http://pubs.usgs.gov/twri/twri4a3/pdf/chapter12.pdf
  # https://www.otexts.org/fpp/4/8
  
  library("forecast")
  forecastedCostsInFuture <- forecast.HoltWinters(forecastedCosts, h=50)
  plot.forecast(forecastedCostsInFuture)
  
  # http://www.itl.nist.gov/div898/handbook/eda/section3/eda35d.htm
  library(lawstat)
  runs.test(totalCostsByYear$x,alternative="two.sided")
  qnorm(.975)
  
  data <- data.frame(x = totalCostsByYear$Group.1, y = totalCostsByYear$x)
  ggplot(data, aes(x = x, y = y)) + geom_point() +
    stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
    #stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE) +
    #stat_smooth(method = 'nls', formula = y ~ a * log(x) +b, aes(colour = 'logarithmic'), se = FALSE, start = list(a=1,b=1)) +
    #stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), aes(colour = 'Exponential'), se = FALSE, start = list(a=1,b=1)) +
    theme_bw() +
    scale_colour_brewer(name = 'Trendline', palette = 'Set2')
}

function <- compareInsuredToTotalCosts() {
  events <- totalCostForEvent()
  dd <- as.data.frame(events[c("Year.financial", "resourceType", "directCost.normalised", "indirectCost.normalised", "intangibleCost.normalised", "total.normalised", "Insured.Costs.normalised", "Insured.Costs.indexed")])
  with(dd, aggregate(total.normalised, by=list(Year.financial), FUN=safeMean))
  dd <- subset(dd, !is.na(Insured.Costs.normalised) & Insured.Costs.normalised > 0)
  dd <- subset(dd, !is.na(Insured.Costs.indexed) & Insured.Costs.indexed > 0)
  a <- subset(mydata, !is.na(Insured.Costs.indexed) & Insured.Costs.indexed > 0)
  
  dd$nondirectCost.normalised <- (dd$intangibleCost.normalised + dd$indirectCost.normalised)
  
  mean(dd$nondirectCost.normalised / (dd$nondirectCost.normalised + dd$Insured.Costs.normalised))
  
  nd <- with(dd, aggregate(nondirectCost.normalised, by=list(resourceType), FUN=mean))
  ic <- with(dd, aggregate(Insured.Costs.indexed, by=list(resourceType), FUN=mean))
  tc <- with(dd, aggregate(total.normalised, by=list(resourceType), FUN=mean))
  data <- as.data.frame(c(nd, ic, tc))
  data$ratio <- data$x / data$x.1
  data$total <- data$x + data$x.1
  data$proportion <- data$x.1 / data$total
  data$proportion.2 <- data$x.1 / data$x.2
  
  
  subset(dd, resourceType == 'Landslide')
  data
  events <- (events[order (events[,1]), ])
  mydata <- (mydata[order (mydata[,9]), ])
  dd <- (dd[order (dd[,1]), ])
  
  write.table(data, file = "./output/summary.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
}

function doAllData() {
  
  initialise()
  
  # Repeats logic from totalCostForEvent(), getEvents()
  mydata$Deaths <- as.numeric(mydata$Deaths)
  mydata$Injuries <- as.numeric(mydata$Injuries)
  mydata$Deaths.normalised <- as.numeric(mydata$Deaths.normalised)
  mydata$Injuries.normalised <- as.numeric(mydata$Injuries.normalised)
  # xsub <- mydata[,6:24] 
  # xsub[is.na(xsub)] <- 0 
  # mydata[,6:24]<-xsub
  
  mydata <- computedDirectCosts(mydata)
  # mydata <- directCosts(mydata)
  mydata <- indirectCosts(mydata)
  mydata <- intangibleCosts(mydata)
  mydata$total <- rowSums(subset(mydata, select = c(directCost, indirectCost, intangibleCost)), na.rm = TRUE)
  mydata$total.normalised <- rowSums(subset(mydata, select = c(directCost.normalised, indirectCost.normalised, intangibleCost.normalised)), na.rm = TRUE)
    
}

