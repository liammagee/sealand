
# Miscellenous analysis and reporting functions
# Anything here for a while should be moved somewhere else

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

