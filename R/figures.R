
# NOTE: We use 3 approaches to deriving costs:
# 1. Reported costs
# 2. Insured costs multiplied by factors based on event type (following Joy 1991)
# 3. Synthetic costs, where specific direct, indirect and intangible components
# are collected, missing values interpollated, indexed to June 2013 dollar values
# and normalised to June 2013 wealth and population levels.
# The graphs below can be modified for each of these approaches by
# replacing the computed total variable names as follows:
# 1. Reported.Cost.normalised
# 1. Insured.Cost.multiplied.normalised
# 3. Synthetic.Cost.normalised


# Libraries
library(ggplot2)
library(scales)
library(reshape2)

# Sources
source("R/functions.r", TRUE)

# Global variables
title.size <- 0.8
character.size <- 0.6

# Functions for generating figures

yearBreaks <- function(years) {
  # Every third year
  # years <-years[(years-1967) %% 3 == 0];
  return (years)
}

yearLabels <- function(years) {
  years <- yearBreaks(years)
  output <- paste(formatC((years - 1) %% 100, width = 2, format = "d", flag = "0"), 
                  "/", 
                  formatC(years %% 100, width = 2, format = "d", flag = "0"), sep = "")
  return (output)
}


## Provides a single function for generating bar charts
standardBarChart <- function(data, file_name, title, x_label, y_label, useYears=TRUE) {
  
  # Set colours
  background <- '#F0D2AF'
  foreground <- '#D08728'
  textColor <- '#888888'
  
  # Ensure the order remains the same
  if (useYears==FALSE) {
    data$Group.1 <- factor(data$Group.1, as.character(data$Group.1))  
  }
  # Calculate range from 0 to max value of costs
  p <- ggplot(data, aes(x=Group.1, y = x)) + geom_bar(width=0.5, stat="identity", fill=foreground, colour=foreground)
  p
  if (useYears==TRUE) {
    x_scale <- scale_x_continuous(name = x_label, breaks = yearBreaks(data$Group.1), labels = yearLabels(data$Group.1))
  } else {
    x_scale <- xlab(x_label)
  }
  p + ggtitle(title) + x_scale + scale_y_continuous(name=y_label, labels=comma) + 
    theme(plot.title = element_text(colour = foreground, lineheight=1.0, face="bold"),
          panel.grid.minor.y=element_blank(), 
          panel.grid.major.y=element_line(colour = foreground),
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.x=element_blank(),
          panel.background = element_rect(fill = background, colour = foreground),
          axis.title=element_text(color=textColor, lineheight=1.0, size = 12),
          axis.text.x=element_text(angle=45, vjust=1.0, hjust=1.0, size = 8)
      )
  
  ggsave(file=paste("./figs/", file_name, ".png", sep=""))
  return (p)
}

## Provides a single function for generating bar charts
standardBarChartClustered <- function(data, file_name, title, x_label, y_label, useYears=TRUE) {
  
  # Set colours
  background <- '#F0D2AF'
  background2 <- '#888888'
  foreground <- '#D08728'
  textColor <- '#888888'
  
  # Ensure the order remains the same
  if (useYears==FALSE) {
    data$Group.1 <- factor(data$Group.1, as.character(data$Group.1))  
  }
  
  # Calculate range from 0 to max value of costs
  p <- ggplot(data, aes(x=Group.1, y = value)) + 
        geom_bar(aes(fill=variable), width=0.75,position = "dodge", stat="identity") +  
        scale_fill_manual(name="", values=c(foreground, background2))
  p
  if (useYears==TRUE) {
    x_scale <- scale_x_continuous(name=x_label, breaks=yearBreaks(data$Group.1), labels = yearLabels(data$Group.1))
  } else {
    x_scale <- xlab(x_label)
  }
  # Note: title height is 0.9, due to presence of legend on clustered charts
  p + ggtitle(title) + x_scale + scale_y_continuous(name=y_label, labels=comma) + 
    theme(plot.title = element_text(colour = foreground, lineheight=0.9, face="bold"),
          panel.grid.minor.y=element_blank(), 
          panel.grid.major.y=element_line(colour = foreground),
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.x=element_blank(),
          panel.background = element_rect(fill = background, colour = foreground),
          axis.title=element_text(color=textColor, lineheight=1.0, size = 12),
          axis.text.x=element_text(angle=45, vjust=1.0, hjust=1.0, size = 8),
          legend.position="bottom")
  
  ggsave(file=paste("./figs/", file_name, ".png", sep=""))
}


## Provides a single function for generating pie charts (for 3.12)
standardPieChart <- function(data, file_name, title) {

  # Set colours
  background <- '#F0D2AF'
  background2 <- '#888888'
  foreground <- '#D08728'
  textColor <- '#888888'
  
  # Calculate range from 0 to max value of costs
  p = ggplot(data = data, aes(x = factor(1), y = percentage, fill = factor(Group.2)))
  p = p + geom_bar(width = 1, stat = "identity") 
  p = p + coord_polar(theta="y") 
  p = p + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
  p = p + xlab('') + ylab('') + labs(fill = 'Disaster Type') 
  p = p + ggtitle(title)
  p
  
  ggsave(file=paste("./figs/", file_name, ".png", sep=""))
}


# Merges a sequence of all years to ensure graphs are complete
# Assumes a two-column data frame with columns named "Group.1" (years) and "x" (values)
includeAllYears <- function(data) {
  # Merge with a sequence, to ensure years with zero events are represented
  allYears <- data.frame(seq(1967, 2013))
  names(allYears)[1] <- "Group.1"
  data <- merge(allYears, data, by = "Group.1", all.x = TRUE)
  if (length(data[is.na(data$x),]$x) > 0) {
    data[is.na(data$x),]$x <- 0
  }
  return (data)
}

## Provides a single function for generating bar charts
standardBarChart_Plot <- function(data, file_name, title, x_label, y_label, y_range=NULL, axes=TRUE) {
  
  # Plot a basic graph of costs
  # pdf(file=paste("./figs/", file_name, ".pdf", sep=""))
  png(filename=paste("./figs/", file_name, ".png", sep=""))
  
  # Set an upper y value based on the data passed in
  # Note: this will often be too little
  if (is.null(y_range)) {
    y_range <- range(0, data)
  }
  
  # Calculate range from 0 to max value of costs
  plot(data, type="h", col="blue", ylim=y_range, axes=axes, ann=FALSE,
       cex=character.size,
       cex.lab=character.size,
       cex.axis=character.size,
       cex.main=character.size,
       cex.sub=character.size)
  
  # Add title
  title(title, col.main = "blue",
        cex=title.size,
        cex.lab=title.size,
        cex.axis=title.size,
        cex.main=title.size,
        cex.sub=title.size)
  
  # Label the x and y axes with dark green text
  title(xlab=x_label, col.lab=rgb(0,0.5,0))
  title(ylab=y_label, col.lab=rgb(0,0.5,0))
}

## Generates an axis with character widths
doAxis <- function(number, at=NULL, labels=NULL) {
  # axis(number, at=at, labels=labels,
  #      cex=character.size,
  #      cex.lab=character.size,
  #      cex.axis=character.size,
  #      cex.main=character.size,
  #      cex.sub=character.size)
}


## Generates a sheet with all the data and computed values
generateCompleteData <- function() {
  write.table(ecnd.database, file = "./output/database_computed.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")

}


## Generates Figure 3.0
totalCostsOfDisastersInAustralia <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, TRUE, FALSE)
  # Just for normalised data
  totalCostsAll <- totalCosts[c("Year.financial", "title", "Reported.Cost.normalised.millions")]
  # Mirror the usual aggregation
  names(totalCostsAll)[1] = "Year"
  names(totalCostsAll)[2] = "Group.1"
  names(totalCostsAll)[3] = "x"
  totalCostsAll <- totalCostsAll[order(totalCostsAll$Year),]
  totalCostsAll <- totalCostsAll[order(-totalCostsAll$x),]
  totalCostsAll$percentages <- totalCostsAll$x / sum(totalCostsAll$x)
  
  write.table(totalCostsAll, file = "./output/ordered_individual_events.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")

  # Cache variables
  data <- totalCostsAll
  title <- "FIGURE 3.0: TOTAL COST OF DISASTERS, 1967-2013"
  x_label <- "Years (financial)"
  y_label <- "(2013 Dollars in $millions)"
  # Graph the results
  standardBarChart(totalCostsAll,
                   "fig3_0_total_costs_of_disasters_in_australia",
                   title,
                   x_label,
                   y_label,
                   FALSE
  )
}

costSummary <- function() {
  totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)
  totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  totalCostsByYear.incl.deaths.and.injuries <- with(totalCosts, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(Year.financial), FUN=safeSum))
  
  print("Average annual cost of all disasters")
  print(mean(totalCostsByYear$x))
  print("Average annual cost of all disasters (incl. deaths and injuries)")
  print(mean(totalCostsByYear.incl.deaths.and.injuries$x))
  
  totalAllYears <- sum(totalCostsByYear$x)
  print("Total cost of all disasters")
  print(totalAllYears)
  totalAllYears.incl.deaths.and.injuries <- sum(totalCostsByYear.incl.deaths.and.injuries$x)
  print("Average annual cost of all disasters (incl. deaths and injuries)")
  print(totalAllYears.incl.deaths.and.injuries)
  
}

## Generates Figure 3.1
annualTotalCostsOfDisastersInAustralia <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, TRUE, FALSE)
  # Just for normalised data
  totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  totalCostsByYear <- includeAllYears(totalCostsByYear)
  
  print("Average annual cost of all disasters")
  print(mean(totalCostsByYear$x))
  
  print("Standard deviation of annual cost of all disasters")
  print(sd(totalCostsByYear$x))

  print("Range of annual costs of all disasters")
  print(range(totalCostsByYear$x))

  print("Ordered sequence of annual costs")
  print(totalCostsByYear[order(-totalCostsByYear$x),])
  
  print("Ratio of most expensive year to least expensive year")
  print(totalCostsByYear[order(-totalCostsByYear$x),][1,2] / totalCostsByYear[order(totalCostsByYear$x),][1,2])
  
  totalAllYears <- sum(totalCostsByYear$x)
  print("Total cost of all disasters")
  print(totalAllYears)
  
  # Top 3
  top3 <- head(totalCosts[order(-totalCosts$Reported.Cost.normalised.millions),c("title", "Year.financial", "Reported.Cost.normalised.millions")], n = 3)
  top3$percentage <- top3$Reported.Cost.normalised.millions / totalAllYears * 100
  print("Top 3 disasters")
  print(top3)
  print(sum(top3$percentage))
  
  # Top 10
  top10 <- head(totalCosts[order(-totalCosts$Reported.Cost.normalised.millions),c("title", "Year.financial", "Reported.Cost.normalised.millions")], n = 10)
  top10$percentage <- top10$Reported.Cost.normalised.millions / totalAllYears * 100
  print("Top 10 disasters")
  print(top10)
  print(sum(top10$percentage))
  
  
  # Exclude 3 biggest years
  # theRest <- totalCosts[!totalCosts$title %in% top3$title,c("title", "Year.financial", "Reported.Cost.normalised.millions")]
  theRest <- tail(totalCosts[order(-totalCosts$Reported.Cost.WithDeathsAndInjuries.normalised.millions),], n = -3)
  print("Average annual cost of all disasters, excluding top 3")
  theRestCostsByYear <- with(theRest, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  theRestCostsByYear <- includeAllYears(theRestCostsByYear)
  print(mean(theRestCostsByYear$x))
  
  print("Standard deviation of annual cost of all disasters, excluding top 3")
  print(sd(theRestCostsByYear$x))
  
  # Run the significance test
  res <- significanceTest_MannKendall(totalCostsByYear)
  print("Significance test for total costs by year")
  print(res)
  summary(res)
  
  # Run the significance test against GDP by comparing indexed values only
  indexedCostsByYear <- with(totalCosts, aggregate(Reported.Cost.indexed.millions, by=list(Year.financial), FUN=safeSum))
  indexedCostsByYear <- includeAllYears(indexedCostsByYear)
  indexedCostsByYear$x <- indexedCostsByYear$x / sapply(indexedCostsByYear$Group.1, gdpValues)
  resMK <- significanceTest_MannKendall(indexedCostsByYear)
  print("Significance test for ratio of costs to GDP")
  print(resMK)
  summary(resMK)

  resLR <- significanceTest_LinearRegression(indexedCostsByYear)
  print("Significance test (regression) for ratio of costs to GDP")
  print(resLR)
  summary(resLR)
  
  # PRINT GRAPHS
  
  # Cache variables
  data <- totalCostsByYear
  title <- "FIGURE 3.1: ANNUAL TOTAL COST OF DISASTERS, 1967-2013"
  x_label <- "Years (financial)"
  y_label <- "(2013 Dollars in $millions)"
  
  # Graph the results
  standardBarChart(totalCostsByYear,
                   "fig3_1_annual_total_costs_of_disasters_in_australia",
                   title,
                   x_label,
                   y_label
  )

  totalCostsByYear.with.deaths.injuries <- with(totalCosts, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(Year.financial), FUN=safeSum))
  mergedCostsByYear <- merge(totalCostsByYear, totalCostsByYear.with.deaths.injuries, by = "Group.1")
  names(mergedCostsByYear)[2] <- paste("Excl. deaths and injuries")
  names(mergedCostsByYear)[3] <- paste("Incl. deaths and injuries")
  meltedMergedCosts <- melt(mergedCostsByYear, id.var = "Group.1")
  standardBarChartClustered(meltedMergedCosts,
    "fig3_1_annual_total_costs_of_disasters_in_australia_incl_deaths_injuries",
    "FIGURE 3.1: ANNUAL TOTAL COST OF DISASTERS, 1967-2013",
    "Years (financial)",
    "(2013 Dollars in $millions)",
    TRUE
    )
}


## Generates Figure 3.2
australianNaturalDisasterCostsByDecade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered(NULL, TRUE, FALSE)

	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10

  # Aggregate normalised costs by decade
	totalCostsByDecade <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

  # Multiply decades back up to years
	totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10

	standardBarChart(totalCostsByDecade,
		"fig3_2_australian_natural_disaster_costs_by_decade",
		"FIGURE 3.2: AUSTRALIAN NATURAL DISASTER COSTS BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
    FALSE
		)
}


## Generate Figure 3.3
averageCostPerEvent <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered(NULL, TRUE, FALSE)
	averageCostPerYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeMean))

	standardBarChart(averageCostPerYear,
		"fig3_3_average_cost_per_event",
		"FIGURE 3.3: AVERAGE COST PER EVENT, 1967-2013",
		"Years (financial)",
		"(2013 Dollars in $millions)"
		)
	
	# Run the significance test
	res <- significanceTest_MannKendall(averageCostPerYear)
	print("Significance test for average costs by year")
	print(res)
	summary(res)
}

## Generate Figure 3.4
distributionOfDisasters <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered(NULL, TRUE, FALSE)

	# Filter by cost bracket
	cost_brackets <- list(10000000, 50000000, 100000000, 150000000, 500000000)

	totalCosts$Reported.Cost.normalised.code <- apply(data.matrix(totalCosts$Reported.Cost.normalised.millions), 1, codeCosts)
	totalCostDistribution <- with(totalCosts, aggregate(Reported.Cost.normalised, by=list(Reported.Cost.normalised.code), FUN=length))
  # Replace codes with labels
	totalCostDistribution$Group.1 <- codeCostLabels()[totalCostDistribution$Group.1]
  
	standardBarChart(totalCostDistribution,
		"fig3_4_distribution_of_disasters",
		"FIGURE 3.4: DISTRIBUTION OF DISASTERS (FREQUENCY) BY COSTS, 1967-2013",
		"Cost distributions",
		"Frequency",
    FALSE
		)
}

## Generate Figure 3.5
annualInsuranceCostOfDisasters <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered(NULL, TRUE, FALSE)
	insuranceCostsByYear <- with(totalCosts, aggregate(Insured.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))

	standardBarChart(insuranceCostsByYear,
		"fig3_5_annual_insurance_costs_of_disasters_in_australia",
		"FIGURE 3.5: ANNUAL INSURANCE COSTS OF DISASTERS, 1967-2013",
		"Years (financial)",
		"(2013 Dollars in $millions)"
		)
	
	print("Total insurance cost of all disasters")
	print(sum(insuranceCostsByYear$x))
	
	print("Average insurance cost of all disasters")
	print(mean(insuranceCostsByYear$x))
	
	print("Average insurance cost of all disasters since 2000")
	print(mean(insuranceCostsByYear[insuranceCostsByYear$Group.1 >= 2000,]$x))
	
	
	# Run the significance test
	res <- significanceTest_MannKendall(insuranceCostsByYear)
	print("Significance test for insurance costs of disasters")
	print(res)
	summary(res)
}


## Generate Figure 3.6
numberOfNaturalDisastersInAustralia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered(NULL, TRUE, FALSE)
	numberByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=length))
	# numberByYear <- with(totalCosts, aggregate(Insured.Cost.normalised.millions, by=list(Year.financial), FUN=length))

	standardBarChart(numberByYear,
		"fig3_6_number_of_natural_disasters_in_australia",
		"FIGURE 3.6: NUMBER OF NATURAL DISASTERS, 1967-2013",
		"Years (financial)",
		"Number of events"
		)
  
	print("Average annual number of all disasters")
	print(mean(numberByYear$x))
	
	print("Total number of all disasters")
	print(sum(numberByYear$x))	

  # Run the significance test
	res <- significanceTest_MannKendall(numberByYear)
	print("Significance test for number of events by year")
	print(res)
	summary(res)
  
  # Also check average counts
  yearPoints = c(1967, 1970, 1980, 1990, 2000, 2010)
  print(paste("Average number of disasters with insured costs since ", yearPoints[1]))
	print(sum(numberByYear[numberByYear$Group.1 > yearPoints[1],]$x) / (2013 - yearPoints[1]))
	print(paste("Average number of disasters with insured costs since ", yearPoints[2]))
	print(sum(numberByYear[numberByYear$Group.1 > yearPoints[2],]$x) / (2013 - yearPoints[2]))
	print(paste("Average number of disasters with insured costs since ", yearPoints[3]))
	print(sum(numberByYear[numberByYear$Group.1 > yearPoints[3],]$x) / (2013 - yearPoints[3]))
	print(paste("Average number of disasters with insured costs since ", yearPoints[4]))
	print(sum(numberByYear[numberByYear$Group.1 > yearPoints[4],]$x) / (2013 - yearPoints[4]))
	print(paste("Average number of disasters with insured costs since ", yearPoints[5]))
	print(sum(numberByYear[numberByYear$Group.1 > yearPoints[5],]$x) / (2013 - yearPoints[5]))
	print(paste("Average number of disasters with insured costs since ", yearPoints[6]))
	print(sum(numberByYear[numberByYear$Group.1 > yearPoints[6],]$x) / (2013 - yearPoints[6]))
}


## Generate Figure 3.7
naturalDisastersBetween10And75Million <- function() {
	# Correlation test
}


## Generate Figure 3.8
naturalDisastersBetween75And150Million <- function() {
	# Correlation test
}


## Generate Figure 3.9
numberOfDisastersPerMillionPeople <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, TRUE, FALSE)
  numberByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=length))
  popCounts <- data.frame(seq(1967, 2013))
  names(popCounts)[1] = "Group.1"
  popCounts$x <- apply(popCounts, 1, popForYear)
  mergedCounts <- merge(popCounts, numberByYear, by = "Group.1", all.x = TRUE)
  names(mergedCounts)[2] = "pop"
  names(mergedCounts)[3] = "disasterCount"
  mergedCounts$popRatios <- mergedCounts$disasterCount / (mergedCounts$pop / 1000000)
  
  # Set colours
  background <- '#F0D2AF'
  foreground <- '#D08728'
  textColor <- '#888888'
  
  title <- "FIGURE 3.9  NUMBER OF DISASTERS PER MILLION PEOPLE, 1967-2013"
  x_label <- "Years (financial)"
  y_label <- "Number of disasters per million people"
  
  # Calculate range from 0 to max value of costs
  p <- ggplot(mergedCounts, aes(x=Group.1, y = popRatios)) + geom_point(colour = foreground, size = 4) +
    geom_smooth(method="lm", fill=NA, colour = "#000000")
  p + ggtitle(title) + scale_x_continuous(name=x_label, breaks=yearBreaks(mergedCounts$Group.1)) + scale_y_continuous(name=y_label, labels=comma) + 
    theme(plot.title = element_text(colour = foreground, lineheight=.8, face="bold"),
          panel.grid.minor.y=element_blank(), 
          panel.grid.major.y=element_line(colour = foreground),
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.x=element_blank(),
          panel.background = element_rect(fill = background, colour = foreground),
          axis.title=element_text(color=textColor),
          axis.text.x=element_text(angle=45, vjust=1.0, hjust=1.0))
  
  ggsave(file=paste("./figs/fig3_9_number_of_disasters_per_million_people.png", sep=""))
  
  # Show regression
  print("Regression fit for population")
  summary(fit <- lm(formula = popRatios ~ Group.1, data = mergedCounts))
  
  # Store the total costs by year
  totalCosts.without.heatwaves <- totalCostForEventFiltered(NULL, TRUE, TRUE)
  numberByYear.without.heatwaves <- with(totalCosts.without.heatwaves, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=length))
  mergedCounts.without.heatwaves <- merge(popCounts, numberByYear, by = "Group.1", all.x = TRUE)
  names(mergedCounts.without.heatwaves)[2] = "pop"
  names(mergedCounts.without.heatwaves)[3] = "disasterCount"
  mergedCounts.without.heatwaves$popRatios <- mergedCounts.without.heatwaves$disasterCount / (mergedCounts.without.heatwaves$pop / 1000000)
  
  print("Number of events including heatwaves")
  print(length(totalCosts$Year.financial))
  
  print("Number of events without heatwaves")
  print(length(totalCosts.without.heatwaves$Year.financial))
  
  print("Regression fit for population without heatwaves")
  summary(fit <- lm(formula = popRatios ~ Group.1, data = mergedCounts.without.heatwaves))
}


## Generate Figure 3.10
disasterCostsByStateAndTerritory <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered(NULL, TRUE, FALSE)
	totalCosts$Reported.Cost.normalised.millions.state.1 <- totalCosts$Reported.Cost.normalised.millions * totalCosts$State.1.percent
	totalCosts$Reported.Cost.normalised.millions.state.2 <- totalCosts$Reported.Cost.normalised.millions * totalCosts$State.2.percent
  totalCostsByState1 <- with(totalCosts, aggregate(Reported.Cost.normalised.millions.state.1, by=list(State.abbreviated.1), FUN=safeSum))
  totalCostsByState2 <- with(totalCosts, aggregate(Reported.Cost.normalised.millions.state.2, by=list(State.abbreviated.2), FUN=safeSum))
  totalCostsByState <- merge(totalCostsByState1, totalCostsByState2, by="Group.1", all.x = TRUE )
	totalCostsByState$x <- rowSums(cbind(totalCostsByState$x.x, totalCostsByState$x.y), na.rm = TRUE)
	totalCostsByState <- totalCostsByState[with(totalCostsByState, order(-x)), ]
	
	# Cache variables
	data <- totalCostsByState
	x_label <- "States"
	y_label <- "(2013 Dollars in $millions)"
	title <- "FIGURE 3.10: DISASTER COSTS BY STATE AND TERRITORY"

  standardBarChart(totalCostsByState,
		"fig3_10_disaster_costs_by_state_and_territory",
		"FIGURE 3.10: DISASTER COSTS BY STATE AND TERRITORY",
		"States",
		"(2013 Dollars in $millions)",
    FALSE
		)
  
  # Generate percentages
	totalCosts$Insured.Cost.normalised.millions.state.1 <- totalCosts$Insured.Cost.normalised.millions * totalCosts$State.1.percent
	totalCosts$Insured.Cost.normalised.millions.state.2 <- totalCosts$Insured.Cost.normalised.millions * totalCosts$State.2.percent
	insuredCostsByState1 <- with(totalCosts, aggregate(Insured.Cost.normalised.millions.state.1, by=list(State.abbreviated.1), FUN=safeSum))
	insuredCostsByState2 <- with(totalCosts, aggregate(Insured.Cost.normalised.millions.state.1, by=list(State.abbreviated.2), FUN=safeSum))
	insuredCostsByState <- merge(insuredCostsByState1, insuredCostsByState2, by="Group.1", all.x = TRUE )
	insuredCostsByState$x <- rowSums(cbind(insuredCostsByState$x.x, insuredCostsByState$x.y), na.rm = TRUE)
	insuredCostsByState <- insuredCostsByState[with(insuredCostsByState, order(-x)), ]
  mergedCosts <- merge(totalCostsByState, insuredCostsByState, by="Group.1", all.x = TRUE)
	mergedCosts$totalCostsPercentages <- data.frame(mergedCosts$x.x / sum(mergedCosts$x.x))
	mergedCosts$insuredCostsPercentages <- data.frame(mergedCosts$x.y / sum(mergedCosts$x.y))
  print("Comparing total and insurable costs")
	print(mergedCosts)
	# Test correlation between total and insured costs
	print(cor.test( mergedCosts$x.x, mergedCosts$x.y, method = "pearson"))
	
	print("Percentage of event costs by state")
  orderedCosts <- mergedCosts[,c("Group.1", "totalCostsPercentages")]
  names(orderedCosts)
	orderedCosts <- orderedCosts[order(-orderedCosts$totalCostsPercentages),]
	print(mergedCosts[,c("Group.1", "totalCostsPercentages")])
  
  print("Top 2 percentages")
	print(sum(head(mergedCosts[order(-mergedCosts$totalCostsPercentages),]$totalCostsPercentages, n = 2)))
	
	print("Top 3 percentages")
	print(sum(head(mergedCosts[order(-mergedCosts$totalCostsPercentages),]$totalCostsPercentages, n = 3)))
	
	print("Combined percentage of top 3 events")
	print(sum(head(mergedCosts[order(-mergedCosts$totalCostsPercentages),]$totalCostsPercentages, 3)))
	  
  print("Percentage of Population in Queensland, New South Wales and Victoria")
  # Taken from ABS June 2014  - http://www.abs.gov.au/ausstats/abs@.nsf/mf/3101.0
  print((7518.5 + 5841.7 + 4722.4) / 23490.7)
  
  # Count number of "raw" (non-interpolated) reported costs
	print("Percentage of 'raw' to 'interpolated' reported costs")
  print(length(totalCosts$Reported.Cost[totalCosts$Reported.Cost > 0]) / 
          length(totalCosts$Reported.Cost.interpolated[totalCosts$Reported.Cost.interpolated > 0]))
  
  # Compare just those events that have both insured and reported costs - JUST FOR State.1
	bothCostsByState <- with(totalCosts[totalCosts$Reported.Cost > 0 & totalCosts$Insured.Cost > 0,], aggregate(data.frame(Reported.Cost,  Insured.Cost), by=list(State.abbreviated.1), FUN=safeSum))
	print(cor.test( bothCostsByState$Reported.Cost, bothCostsByState$Insured.Cost, method = "pearson"))
	
	# Show major events by state
	sortedByStateAndCost <- totalCosts[with(totalCosts,order(State.abbreviated.1, -Reported.Cost.normalised.millions)),]
	sortedByStateAndCost <- with(sortedByStateAndCost, data.frame(State.abbreviated.1, title, Year, Year.financial, Reported.Cost.normalised.millions))
	write.table(sortedByStateAndCost, file = "./output/ordered_events_by_state.csv", append = FALSE, quote = TRUE, sep = ",",
	            eol = "\n", na = "", dec = ".", row.names = FALSE,
	            col.names = TRUE, qmethod = c("escape", "double"),
	            fileEncoding = "")
	
	# Show major events by state and type
	totalCostsByStateAndType <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(State.abbreviated.1, resourceType), FUN=safeSum))
	totalCostsByStateAndType <- totalCostsByStateAndType[with(totalCostsByStateAndType, order(Group.1, Group.2)), ]
  print("Events aggregated by state and event type")
  print(totalCostsByStateAndType)
	print("Distinct event types")
	print(data.frame(unique(totalCosts$resourceType)))
  
}


## Generate Figure 3.11
numberOfDisasterEventsByStateAndTerritory <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, TRUE, FALSE)

  totalCosts$Reported.Cost.normalised.millions.state.1 <- totalCosts$Reported.Cost.normalised.millions * totalCosts$State.1.percent
  totalCosts$Reported.Cost.normalised.millions.state.2 <- totalCosts$Reported.Cost.normalised.millions * totalCosts$State.2.percent
  totalCountsByState1 <- with(totalCosts, aggregate(Reported.Cost.normalised.millions.state.1, by=list(State.abbreviated.1), FUN=length))
  totalCountsByState2 <- with(totalCosts, aggregate(Reported.Cost.normalised.millions.state.2, by=list(State.abbreviated.2), FUN=length))
  totalCountsByState <- merge(totalCountsByState1, totalCountsByState2, by="Group.1", all.x = TRUE )
  totalCountsByState$x <- rowSums(cbind(totalCountsByState$x.x, totalCountsByState$x.y), na.rm = TRUE)

  # Remove 'Other' column
  totalCountsByState <- totalCountsByState[!(totalCountsByState$Group.1 %in% c('Other')),]
  
  # Order by state
  totalCountsByState <- totalCountsByState[with(totalCountsByState, order(-x)), ]

	standardBarChart(totalCountsByState,
		"fig3_11_number_of_disaster_events_by_state_and_territory",
		"FIGURE 3.11: # OF DISASTER EVENTS BY STATE AND TERRITORY, 1967-2013",
		"States",
		"Number of events",
    FALSE
		)
  
  # Show relative frequencies
  totalCountsByState$totalCountsPercentages <- data.frame(totalCountsByState$x / sum(totalCountsByState$x))
  print("Percentage of event frequencies by state")
  print(totalCountsByState)
  print("Combined percentage of top 3 events")
  print(sum(head(totalCountsByState$totalCountsPercentages, 3)))
}


## Generate Figure 3.12
costsByTypeOfDisasterAndStateAndTerritory <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, TRUE, FALSE)
  totalCosts$Reported.Cost.normalised.millions.state.1 <- totalCosts$Reported.Cost.normalised.millions * totalCosts$State.1.percent
  totalCosts$Reported.Cost.normalised.millions.state.2 <- totalCosts$Reported.Cost.normalised.millions * totalCosts$State.2.percent
  totalCostsByState1 <- with(totalCosts, aggregate(Reported.Cost.normalised.millions.state.1, by=list(State.abbreviated.1, resourceType), FUN=safeSum))
  totalCostsByState2 <- with(totalCosts, aggregate(Reported.Cost.normalised.millions.state.2, by=list(State.abbreviated.2, resourceType), FUN=safeSum))
  totalCostsByStateAndDisasterType <- merge(totalCostsByState1, totalCostsByState2, by=c("Group.1", "Group.2"), all.x = TRUE )
  totalCostsByStateAndDisasterType$x <- rowSums(cbind(totalCostsByStateAndDisasterType$x.x, totalCostsByStateAndDisasterType$x.y), na.rm = TRUE)
  # totalCostsByStateAndDisasterType <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(State.abbreviated.1, resourceType), FUN=safeSum))
  totalCostsByStateAndDisasterType$x <- round(totalCostsByStateAndDisasterType$x)
  state.totals <- aggregate(x ~ Group.1, data=totalCostsByStateAndDisasterType, sum, na.rm=TRUE)
  names(state.totals)[2] = "Total"
  totals.with.state.aggregates <- merge(totalCostsByStateAndDisasterType, state.totals, by = "Group.1", all.x = TRUE)
  totals.with.state.aggregates$percentage <- totals.with.state.aggregates$x / totals.with.state.aggregates$Total
  totals.with.state.aggregates <- totals.with.state.aggregates[order(-totals.with.state.aggregates$Total),]
  
  # For individual charts, if necessary
  act <- totals.with.state.aggregates[totals.with.state.aggregates$Group.1 == 'ACT',]
  nsw <- totals.with.state.aggregates[totals.with.state.aggregates$Group.1 == 'NSW',]
  nt <- totals.with.state.aggregates[totals.with.state.aggregates$Group.1 == 'NT',]
  qld <- totals.with.state.aggregates[totals.with.state.aggregates$Group.1 == 'QLD',]
  sa <- totals.with.state.aggregates[totals.with.state.aggregates$Group.1 == 'SA',]
  tas <- totals.with.state.aggregates[totals.with.state.aggregates$Group.1 == 'TAS',]
  vic <- totals.with.state.aggregates[totals.with.state.aggregates$Group.1 == 'VIC',]
  wa <- totals.with.state.aggregates[totals.with.state.aggregates$Group.1 == 'WA',]

  base_file_name <- "fig3_12_costs_by_type_of_disaster_and_state_and_territory"
  standardPieChart(act, paste(base_file_name, "_", "act", sep = ''), "Losses (ACT)")
  standardPieChart(nsw, paste(base_file_name, "_", "nsw", sep = ''), "Losses (NSW)")
  standardPieChart(nt, paste(base_file_name, "_", "nt", sep = ''), "Losses (NT)")
  standardPieChart(qld, paste(base_file_name, "_", "qld", sep = ''), "Losses (QLD)")
  standardPieChart(sa, paste(base_file_name, "_", "sa", sep = ''), "Losses (SA)")
  standardPieChart(tas, paste(base_file_name, "_", "tas", sep = ''), "Losses (TAS)")
  standardPieChart(vic, paste(base_file_name, "_", "vic", sep = ''), "Losses (VIC)")
  standardPieChart(wa, paste(base_file_name, "_", "wa", sep = ''), "Losses (WA)")

  # Integrated pie chart
  totals.with.state.aggregates$total.neg = factor(-totals.with.state.aggregates$Total, labels = unique(totals.with.state.aggregates$Group.1))
  
  p = ggplot(data = totals.with.state.aggregates, aes(x = factor(1), y = percentage, fill = factor(Group.2)))
  p = p + geom_bar(width = 1, stat = "identity") 
  p = p + facet_wrap(~ total.neg, ncol=2)
  p = p + coord_polar(theta="y") 
  p = p + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
  p = p + xlab('') + ylab('') + labs(fill = 'Disaster Type') 
  #p = p + annotate(geom = "text", label = label)
  p
    
  ggsave(file=paste("./figs/", "fig3_12_costs_by_type_of_disaster_and_state_and_territory", ".png", sep=""))  
  
  # Stacked bar chart version
  p = ggplot(data = totals.with.state.aggregates, aes(x = Group.1, y = x, fill = factor(Group.2)))
  p = p + geom_bar(width = 0.5, stat = "identity") 
  p = p + facet_wrap(~ total.neg, ncol=2)
  p = p + coord_polar(theta="y") 
  p = p + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
  p = p + xlab('States') + ylab('(2013 Dollars in $millions)') + labs(fill = 'Disaster Type') 
  p
  ggsave(file=paste("./figs/", "fig3_12_costs_by_type_of_disaster_and_state_and_territory_stacked", ".png", sep=""))  
  
  p = ggplot(data = totals.with.state.aggregates, aes(x = Group.1, y = percentage, fill = factor(Group.2)))
  p = p + geom_bar(width = 0.5, stat = "identity") 
  p = p + xlab('States') + ylab('(2013 Dollars in $millions)') + labs(fill = 'Disaster Type') 
  p
  ggsave(file=paste("./figs/", "fig3_12_costs_by_type_of_disaster_and_state_and_territory_stacked_percent", ".png", sep=""))  
}


## Generate Figure 3.13
totalAndInsuranceCostsByDisasterType <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, TRUE, FALSE)
  totalCostsByDisasterType <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(resourceType), FUN=safeSum))
  totalCostsByDisasterType <- totalCostsByDisasterType[with(totalCostsByDisasterType, order(-x)), ]
  insuredCostsByDisasterType <- with(totalCosts, aggregate(Insured.Cost.normalised.millions, by=list(resourceType), FUN=safeSum))
  insuredCostsByDisasterType <- insuredCostsByDisasterType[with(insuredCostsByDisasterType, order(-x)), ]
  mergedCosts <- merge(totalCostsByDisasterType, insuredCostsByDisasterType, by="Group.1", all.x = TRUE)
  names(mergedCosts)[2] <- paste("Reported Cost")
  names(mergedCosts)[3] <- paste("Insured Cost")
  meltedMergedCosts <- melt(mergedCosts, id.var = "Group.1")
  meltedMergedCosts <- meltedMergedCosts[with(meltedMergedCosts, order(-value)), ]
  
  standardBarChartClustered(meltedMergedCosts,
		"fig3_13_total_and_insurance_costs_by_disaster_type",
		"FIGURE 3.13: TOTAL AND INSURANCE COSTS BY DISASTER TYPE, 1967-2013",
		"Disaster Type",
		"(2013 Dollars in $millions)",
    FALSE
		)
  
  # Get percentages
  totalCostsByDisasterType$percentages <- data.frame(totalCostsByDisasterType$x / (sum(totalCostsByDisasterType$x))) 
  print("Percentages of disaster type")
  print(totalCostsByDisasterType)
  
  print("Combined percentage of top 3 events")
  print(sum(head(totalCostsByDisasterType[order(-totalCostsByDisasterType$percentages),]$percentages, 3)))
  
  # Test for the inclusion of deaths and injuries
  totalCostsByDisasterType.WithDeathsAndInjuries <- with(totalCosts, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(resourceType), FUN=safeSum))
  totalCostsByDisasterType.WithDeathsAndInjuries <- totalCostsByDisasterType.WithDeathsAndInjuries[with(totalCostsByDisasterType.WithDeathsAndInjuries, order(-x)), ]
  totalCostsByDisasterType.WithDeathsAndInjuries$percentages <- data.frame(totalCostsByDisasterType.WithDeathsAndInjuries$x / (sum(totalCostsByDisasterType.WithDeathsAndInjuries$x))) 
  print("Combined percentage of top 3 events with deaths and injuries included")
  print(sum(head(totalCostsByDisasterType.WithDeathsAndInjuries[order(-totalCostsByDisasterType.WithDeathsAndInjuries$percentages),]$percentages, 3)))


  # Show major events by disaster type
  sortedByTypeAndCost <- totalCosts[with(totalCosts,order(resourceType, -Reported.Cost.normalised.millions)),]
  sortedByTypeAndCost <- with(sortedByTypeAndCost, data.frame(resourceType, title, Year, Year.financial, Reported.Cost.normalised.millions))
  write.table(sortedByTypeAndCost, file = "./output/ordered_events_by_type.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
}


## Generate Figure 3.14
numberOfEventsByDisasterType <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, TRUE, FALSE)
  # Exclude events without a reported cost
  totalCosts <- totalCosts[totalCosts$Reported.Cost.normalised.millions > 0,]
  totalCountsByDisasterType <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(resourceType), FUN=length))
  totalCountsByDisasterType <- totalCountsByDisasterType[with(totalCountsByDisasterType, order(-x)), ]

	standardBarChart(totalCountsByDisasterType,
		"fig3_14_number_of_events_by_disaster_type",
		"FIGURE 3.14: DISASTER COSTS BY TYPE, 1967-2013",
		"Event types",
		"Number of events",
		FALSE)
  
  # Get percentages
  totalCountsByDisasterType$percentages <- data.frame(totalCountsByDisasterType$x / (sum(totalCountsByDisasterType$x))) 
  print("Percentages of disaster type frequency")
  print(totalCountsByDisasterType)
  
  print("Combined percentage of top 3 events")
  print(sum(head(totalCountsByDisasterType[order(-totalCountsByDisasterType$percentages),]$percentages, 3)))

  # Test for the inclusion of deaths and injuries
  totalCountsByDisasterType.WithDeathsAndInjuries <- with(totalCosts, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(resourceType), FUN=length))
  totalCountsByDisasterType.WithDeathsAndInjuries <- totalCountsByDisasterType.WithDeathsAndInjuries[with(totalCountsByDisasterType.WithDeathsAndInjuries, order(-x)), ]
  totalCountsByDisasterType.WithDeathsAndInjuries$percentages <- data.frame(totalCountsByDisasterType.WithDeathsAndInjuries$x / (sum(totalCountsByDisasterType.WithDeathsAndInjuries$x))) 
  print("Percentages of disaster type frequency, with deaths and injuries included")
  print(totalCountsByDisasterType.WithDeathsAndInjuries)
  print("Combined percentage of top 3 events, with deaths and injuries included")
  print(sum(head(totalCountsByDisasterType.WithDeathsAndInjuries[order(-totalCountsByDisasterType.WithDeathsAndInjuries$percentages),]$percentages, 3)))
}


## Generate Figure 3.15
annualCostOfFloodsInAustralia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered("Flood", TRUE, FALSE)
	totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  totalCostsByYear <- includeAllYears(totalCostsByYear)

	# Graph the results
	standardBarChart(totalCostsByYear,
		"fig3_15_annual_cost_of_floods_in_australia",
		"FIGURE 3.15: ANNUAL TOTAL COSTS OF FLOODS, 1967-2013",
		"Years (financial)",
		"(2013 Dollars in $millions)"
		)
	
  # Show key statistics
	top10Floods <- head(totalCostsByYear[order(-totalCostsByYear$x),], 10)
  print("Top 10 Floods Years by Cost")
	print(top10Floods)
	
	print("Total annual cost")
	print(sum(totalCostsByYear$x))
	
	print("Average annual cost")
	print(mean(totalCostsByYear$x))
	
	# Run the significance test
	res <- significanceTest_MannKendall(totalCostsByYear)
	print("Significance test for cost of floods by year")
	print(res)
	summary(res)
	
}


## Generate Figure 3.16
totalCostOfFloodsByDecade <- function() {
		# Store the total costs by year
	totalCosts <- totalCostForEventFiltered("Flood", TRUE, FALSE)

	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
	totalCostsByDecade <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10

	standardBarChart(totalCostsByDecade,
		"fig3_16_australian_flood_costs_by_decade",
		"FIGURE 3.16: AUSTRALIAN FLOOD COSTS BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
    FALSE
		)
	
	print("Average decade cost since 1970")
	mean(totalCostsByDecade[seq(2,6),]$x)
	
	print("Min and max decade cost since 1970")
	range(totalCostsByDecade[seq(2,6),]$x)
}


## Generate Figure 3.17
annualNumberOfFloodsInAustralia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered("Flood", TRUE, FALSE)
	numberByYear <- with(totalCosts, aggregate(resourceType, by=list(Year.financial), FUN=length))
  numberByYear <- includeAllYears(numberByYear)

	standardBarChart(numberByYear,
		"fig3_17_number_of_floods_in_australia",
		"FIGURE 3.17: NUMBER OF FLOODS, 1967-2013",
		"Years (financial)",
		"Number of events"
		)
	
	# Run the significance test
	res <- significanceTest_MannKendall(numberByYear)
	print("Significance test for number of floods by year")
	print(res)
	summary(res)
	
	# Average costs
	totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  mergedCostsCounts <- merge(totalCostsByYear, numberByYear, by="Group.1", all.x = TRUE)
  # Set average to 'x' for the results summary
	mergedCostsCounts$x <- mergedCostsCounts$x.x / mergedCostsCounts$x.y
  res <- significanceTest_MannKendall(mergedCostsCounts)
	print("Significance test for average cost of floods by year")
	print(res)
	summary(res)
  # plot(mergedCostsCounts[,c(1,4)])
	
	# Total number of floods
	print("Total number of floods")
	print(sum(numberByYear$x))
  
	top10FloodsByCount <- head(numberByYear[order(-numberByYear$x),], 10)
	print("Top 10 Floods Years by Count")
	print(top10FloodsByCount)
}


## Generate Figure 3.18
annualCostOfSevereStormsByDecade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered("Severe Storm", TRUE, FALSE)
	totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  totalCostsByYear <- includeAllYears(totalCostsByYear)

	# Graph the results
	standardBarChart(totalCostsByYear,
		"fig3_18_annual_cost_of_severe_storms_in_australia",
		"FIGURE 3.18: ANNUAL TOTAL COSTS OF SEVERE STORMS, 1967-2013",
		"Years (financial)",
		"(2013 Dollars in $millions)"
		)
	
	# Show key statistics
	top10Storms <- head(totalCostsByYear[order(-totalCostsByYear$x),], 10)
	print("Top 10 Storm Years by Cost")
	print(top10Storms)
	
	print("Average annual cost")
	print(mean(totalCostsByYear$x))
	
	print("Total cost")
	print(sum(totalCostsByYear$x))
	
	# Run the significance test
	res <- significanceTest_MannKendall(totalCostsByYear)
	print("Significance test for cost of floods by year")
	print(res)
	summary(res)
}


## Generate Figure 3.19
totalCostOfSevereStormsByDecade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered("Severe Storm", TRUE, FALSE)
	
	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
	totalCostsByDecade <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10

	standardBarChart(totalCostsByDecade,
		"fig3_19_australian_severe_storm_costs_by_decade",
		"FIGURE 3.19: AUSTRALIAN SEVERE STORM COSTS BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
    FALSE
		)
}


## Generate Figure 3.20
annualNumberOfSevereStormsInAustralia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered("Severe Storm", TRUE, FALSE)
	numberByYear <- with(totalCosts, aggregate(resourceType, by=list(Year.financial), FUN=length))
  numberByYear <- includeAllYears(numberByYear)

	standardBarChart(numberByYear,
		"fig3_20_number_of_severe_storms_in_australia",
		"FIGURE 3.20: NUMBER OF SEVERE STORMS, 1967-2013",
		"Years (financial)",
		"Number of events"
		)

	
	# Total number of storms
	print("Total number of storms")
	print(sum(numberByYear$x))

  print("Average number of storms")
  # Note 'mean' does not work here - years with zero do not appear
	print(sum(numberByYear$x) / (2013 - 1966))
	
	pre1980 <- numberByYear[numberByYear$Group.1 < 1980,]
	print("Average number of storms pre 1980")
	# Note 'mean' does not work here - years with zero do not appear
	print(sum(pre1980$x) / (1979 - 1966))
	
	post1980 <- numberByYear[numberByYear$Group.1 >= 1980,]
	print("Average number of storms post 1980")
	# Note 'mean' does not work here - years with zero do not appear
	print(sum(post1980$x) / (2013 - 1979))
}


## Generate Figure 3.21
annualCostOfCyclonesInAustralia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered("Cyclone", TRUE, FALSE)
	totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
	totalCostsByYear <- includeAllYears(totalCostsByYear)
		
  # Graph the results
	standardBarChart(totalCostsByYear,
		"fig3_21_annual_cost_of_cyclone_in_australia",
		"FIGURE 3.21: ANNUAL TOTAL COSTS OF CYCLONES, 1967-2013",
		"Years (financial)",
		"(2013 Dollars in $millions)"
		)
  
	
	# Show key statistics
	top10Cyclones <- head(totalCostsByYear[order(-totalCostsByYear$x),], 10)
	print("Top 10 Cyclones Years by Cost")
	print(top10Cyclones)
	
	print("Average annual cost of cyclones")
	print(sum(totalCostsByYear$x) / (2013 - 1966))
	print("Average annual cost of cyclones since 1980")
	print(sum(totalCostsByYear[totalCostsByYear$Group.1 > 1980,]$x) / (2013 - 1980))
	print("Average annual cost of cyclones since 1999")
	print(sum(totalCostsByYear[totalCostsByYear$Group.1 > 1999,]$x) / (2013 - 1999))
	print("Average annual cost of cyclones since 2005")
	print(sum(totalCostsByYear[totalCostsByYear$Group.1 > 2005,]$x) / (2013 - 2005))
	
	print("Total cost of cyclones")
	print(sum(totalCostsByYear$x))
	
	# Run the significance test
	res <- significanceTest_MannKendall(totalCostsByYear)
	print("Significance test for cost of cyclones by year")
	print(res)
	summary(res)  
}


## Generate Figure 3.22
totalCostOfCyclonesByDecade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered("Cyclone", TRUE, FALSE)

	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
	totalCostsByDecade <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10

	standardBarChart(totalCostsByDecade,
		"fig3_22_australian_cyclone_costs_by_decade",
		"FIGURE 3.22: AUSTRALIAN CYCLONES COSTS BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
    FALSE
		)
}


## Generate Figure 3.23
annualNumberOfCyclonesCausingMoreThan10MillionDamageInAustralia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered("Cyclone", TRUE, FALSE)
	numberByYear <- with(totalCosts, aggregate(resourceType, by=list(Year.financial), FUN=length))
  numberByYear <- includeAllYears(numberByYear)
  
	standardBarChart(numberByYear,
		"fig3_23_number_of_cyclones_in_australia",
		"FIGURE 3.23: NUMBER OF CYCLONES, 1967-2013",
		"Years (financial)",
		"Number of events"
		)
  
	
	# Total number of cyclones
	print("Total number of cyclones")
	print(sum(numberByYear$x))
	
	print("Average number of cyclones")
	print(mean(numberByYear$x))
	print("Average annual cost of cyclones since 1999")
	print(mean(numberByYear[numberByYear$Group.1 > 1999,]$x))
}


## Generate Figure 3.24
totalCostOfEarthquakesByDecade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered("Earthquake", TRUE, FALSE)

	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
	totalCostsByDecade <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))
	# Merge with a sequence, to ensure years with zero events are represented
	allDecades <- data.frame(seq(196, 201))
	names(allDecades)[1] <- "Group.1"
	totalCostsByDecade <- merge(allDecades, totalCostsByDecade, by = "Group.1", all.x = TRUE)
	if (length(totalCostsByDecade[is.na(totalCostsByDecade$x),]$x) > 0) {
	  totalCostsByDecade[is.na(totalCostsByDecade$x),]$x <- 0
	}
  
	# Multiply decades back up to 000's
	totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10

	standardBarChart(totalCostsByDecade,
		"fig3_24_australian_earthquake_costs_by_decade",
		"FIGURE 3.24: AUSTRALIAN EARTHQUAKE COSTS BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
    FALSE
		)
	
	print("Average decadal cost of earthquakes")
	print(sum(totalCostsByDecade$x) / 6)

	print("Total cost of earthquakes")
	print(sum(totalCostsByDecade$x))
}


## Generate Figure 3.25
annualCostOfBushfiresInAustralia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered("Bushfire", TRUE, FALSE)
	totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
	totalCostsByYear <- includeAllYears(totalCostsByYear)
	
	# Graph the results
	standardBarChart(totalCostsByYear,
		"fig3_25_annual_cost_of_bushfire_in_australia",
		"FIGURE 3.25: ANNUAL TOTAL COSTS OF BUSHFIRES, 1967-2013",
		"Years (financial)",
		"(2013 Dollars in $millions)"
		)
	
	
	# Show key statistics
	top10Fires <- head(totalCostsByYear[order(-totalCostsByYear$x),], 10)
	print("Top 10 Fires Years by Cost")
	print(top10Fires)
	
	print("Average annual cost of fires")
	print(mean(totalCostsByYear$x))
	
	print("Total cost of fires")
	print(sum(totalCostsByYear$x))
}


## Generate Figure 3.26
totalCostOfBushfiresByDecade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered("Bushfire", TRUE, FALSE)
	
	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
	totalCostsByDecade <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10

	standardBarChart(totalCostsByDecade,
		"fig3_26_australian_bushfire_costs_by_decade",
		"FIGURE 3.26: AUSTRALIAN BUSHFIRE COSTS BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
    FALSE
		)
}


## Generate Figure 3.27
annualNumberOfBushfiresInAustralia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered("Bushfire", TRUE, FALSE)
	numberByYear <- with(totalCosts, aggregate(resourceType, by=list(Year.financial), FUN=length))
  numberByYear <- includeAllYears(numberByYear)

	standardBarChart(numberByYear,
		"fig3_27_number_of_bushfire_in_australia",
		"FIGURE 3.27: NUMBER OF BUSHFIRES, 1967-2013",
		"Years (financial)",
		"Number of events"
		)
	
	# Total number of bushfires
	print("Total number of bushfires")
	print(sum(numberByYear$x))
	
	print("Average number of bushfires")
	print(mean(numberByYear$x))
	
}


## Generate Figure 3.28
numberOfNaturalDisastersDeaths <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)
	numberByYear <- with(totalCosts, aggregate(Deaths.normalised, by=list(Year.financial), FUN=sum))
  numberByYear <- includeAllYears(numberByYear)
	numberByYearDenormalised <- with(totalCosts, aggregate(Deaths, by=list(Year.financial), FUN=sum))
	numberByYearDenormalised <- includeAllYears(numberByYearDenormalised)
  
  mergedDeaths <- merge(numberByYearDenormalised, numberByYear, by="Group.1", all.x = TRUE)
  names(mergedDeaths)[2] <- paste("Deaths")
  names(mergedDeaths)[3] <- paste("Deaths (normalised)")
  meltedMergedDeaths <- melt(mergedDeaths, id.var = "Group.1")
  meltedMergedDeaths <- meltedMergedDeaths[with(meltedMergedDeaths, order(-value)), ]
  standardBarChartClustered(meltedMergedDeaths,
    "fig3_28_number_of_natural_disasters_deaths",
    "FIGURE 3.28: NUMBER OF DEATHS BY NATURAL DISASTERS, 1967-2013",
    "Years (financial)",
    "Number of deaths"
    )
	
	# Show key statistics
  top10Deaths <- head(totalCosts[order(-totalCosts$Deaths.normalised),c("Year.financial", "Year", "title", "Deaths.normalised")], 10)
  top10DeathsDenormalised <- head(totalCosts[order(-totalCosts$Deaths),c("Year.financial", "Year", "title", "Deaths")], 10)
  print("Top 10 Events by Deaths (normalised)")
  print(top10Deaths)
  print("Top 10 Events by Deaths")
	print(top10DeathsDenormalised)
	
	print("Average annual number of deaths (normalised)")
  print(mean(numberByYear$x))
  print("Average annual number of deaths")
	print(mean(numberByYearDenormalised$x))
	
	print("Total number of deaths (normalised)")
  print(sum(numberByYear$x))  
  print("Total number of deaths")
	print(sum(numberByYearDenormalised$x))	


	numberByDenormalised <- with(totalCosts, aggregate(Deaths, by=list(State.1), FUN=sum))
	numberByDenormalised <- numberByDenormalised[order(-numberByDenormalised$x),]
	print("Total number of deaths by state")
	print(numberByDenormalised)
}


## Generate Figure 3.29
numberOfNaturalDisastersInjuries <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)
	numberByYear <- with(totalCosts, aggregate(Injuries.normalised, by=list(Year.financial), FUN=sum))
	numberByYear <- includeAllYears(numberByYear)
  numberByYearDenormalised <- with(totalCosts, aggregate(Injuries, by=list(Year.financial), FUN=sum))
  numberByYearDenormalised <- includeAllYears(numberByYearDenormalised)
  
  mergedInjuries <- merge(numberByYearDenormalised, numberByYear, by="Group.1", all.x = TRUE)
  names(mergedInjuries)[2] <- paste("Injuries")
  names(mergedInjuries)[3] <- paste("Injuries (normalised)")
  meltedMergedInjuries <- melt(mergedInjuries, id.var = "Group.1")
  meltedMergedInjuries <- meltedMergedInjuries[with(meltedMergedInjuries, order(-value)), ]
  standardBarChartClustered(meltedMergedInjuries,
    "fig3_29_number_of_natural_disasters_injuries",
    "FIGURE 3.29: NUMBER OF INJURIES BY NATURAL DISASTERS, 1967-2013",
    "Years (financial)",
    "Number of injuries"
    )
}


## Generate Figure 3.30
numberOfDeathsByDecade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)
	
	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
  numberOfDeathsByDecade <- with(totalCosts, aggregate(Deaths.normalised, by=list(floor(Year.financial / 10)), FUN=safeSum))
	numberOfDeathsByDecadeDenormalised <- with(totalCosts, aggregate(Deaths, by=list(floor(Year.financial / 10)), FUN=safeSum))

  # Multiply decades back up to 000's
  numberOfDeathsByDecade[,1] <- numberOfDeathsByDecade[,1] * 10
	numberOfDeathsByDecadeDenormalised[,1] <- numberOfDeathsByDecadeDenormalised[,1] * 10

  mergedDecadeDeaths <- merge(numberOfDeathsByDecadeDenormalised, numberOfDeathsByDecade, by="Group.1", all.x = TRUE)
  names(mergedDecadeDeaths)[2] <- paste("Deaths")
  names(mergedDecadeDeaths)[3] <- paste("Deaths (normalised)")
  meltedMergedDecadeDeaths <- melt(mergedDecadeDeaths, id.var = "Group.1")
  
  standardBarChartClustered(meltedMergedDecadeDeaths,
    "fig3_30_number_of_deaths_by_decade",
    "FIGURE 3.30: COST OF DEATHS BY NATURAL DISASTERS BY DECADE, 1967-2013",
    "Decades",
    "(2013 Dollars in $millions)",
    FALSE
    )
	
  print("Number of deaths by decade (normalised)")
  print(numberOfDeathsByDecade)
	print("Number of deaths by decade")
	print(numberOfDeathsByDecadeDenormalised)
	
	totalCosts <- totalCostForEventFiltered("Heatwave", FALSE, FALSE)
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
  numberOfDeathsByDecade <- with(totalCosts, aggregate(Deaths.normalised, by=list(floor(Year.financial / 10)), FUN=safeSum))
	numberOfDeathsByDecadeDenormalised <- with(totalCosts, aggregate(Deaths.normalised, by=list(floor(Year.financial / 10)), FUN=safeSum))
	# Multiply decades back up to 000's
	numberOfDeathsByDecade[,1] <- numberOfDeathsByDecade[,1] * 10
  print("Number of deaths by decade for heatwaves (normalised)")
  print(numberOfDeathsByDecade)
  print("Number of deaths by decade for heatwaves")
	print(numberOfDeathsByDecadeDenormalised)

}


## Generate Figure 3.31
costOfDeathsAndInjuries <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)
	totalCostsByYear <- with(totalCosts, aggregate(deathAndInjuryCosts.normalised.millions, by=list(Year.financial), FUN=safeSum))
	totalCostsByYear <- includeAllYears(totalCostsByYear)
  
	# Graph the results
	standardBarChart(totalCostsByYear,
		"fig3_31_cost_of_deaths_and_injuries",
		"FIGURE 3.31: COST OF DEATH AND INJURIES BY NATURAL DISASTERS, 1967-2013",
		"Years (financial)",
		"(2013 Dollars in $millions)"
		)
  	
	print("Average annual cost of deaths and injuries")
	print(mean(totalCostsByYear$x))
	
	print("Total cost of deaths and injuries")
	print(sum(totalCostsByYear$x))
  
	totalCostsOfDeathsAndInjuriesByDisasterType <- with(totalCosts, aggregate(deathAndInjuryCosts.normalised.millions, by=list(resourceType), FUN=safeSum))
	totalCostsOfDeathsAndInjuriesByDisasterType <- totalCostsOfDeathsAndInjuriesByDisasterType[order(-totalCostsOfDeathsAndInjuriesByDisasterType$x),]
	totalCostsOfDeathsAndInjuriesByDisasterType$percentages <- totalCostsOfDeathsAndInjuriesByDisasterType$x / sum(totalCostsOfDeathsAndInjuriesByDisasterType$x)
	print("Total cost of deaths and injuries by disaster type")
	print(totalCostsOfDeathsAndInjuriesByDisasterType)
}


## Generate Figure 3.32
costOfDeathsAndInjuriesByDecade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)

	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
	totalCostsByDecade <- with(totalCosts, aggregate(deathAndInjuryCosts.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10

	standardBarChart(totalCostsByDecade,
		"fig3_32_cost_of_deaths_and_injuries_by_decade",
		"FIGURE 3.32: COST OF DEATHS AND INJURIES BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
    FALSE
		)
}


## Generate Figure 3.33
totalCostOfNaturalDisasters <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)
  totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(Year.financial), FUN=safeSum))
  
  # Graph the results
  standardBarChart(totalCostsByYear,
                   "fig3_33_total_cost_of_natural_disasters",
                   "FIGURE 3.33: TOTAL COSTS OF DISASTERS, 1967-2013",
                   "Years (financial)",
                   "(2013 Dollars in $millions)"
  )
  
  # Show key statistics
  top10Events <- head(totalCosts[order(-totalCosts$Reported.Cost.WithDeathsAndInjuries.normalised.millions),c("Year.financial", "Year", "title", "Reported.Cost.WithDeathsAndInjuries.normalised.millions")], 10)
  print("Top 10 Events by Cost (incl. deaths and injuries)")
  print(top10Events)
  
  print("Average annual cost of all disasters (incl. deaths and injuries)")
  print(mean(totalCostsByYear$x))
  
  print("Standard deviation of annual cost of all disasters (incl. deaths and injuries)")
  print(sd(totalCostsByYear$x))
  
  print("Total cost of all disasters (incl. deaths and injuries)")
  print(sum(totalCostsByYear$x))  
  
  # Australia's population as reported by ABS June 2013
  print("Average cost of all disasters per person")
  print(1000000 * sum(totalCostsByYear$x) / 23135281)
  
  print("Total cost of all disasters (incl. deaths and injuries)")
  print(sum(totalCostsByYear$x))  
  
  # Top and bottom 3 years
  top3Years <- head(totalCostsByYear[order(-totalCostsByYear$x),], 3)
  print("Top 3 years by Cost (incl. deaths and injuries)")
  print(top3Years)
  
  bottom3Years <- head(totalCostsByYear[order(totalCostsByYear$x),], 3)
  print("Bottom 3 years by Cost (incl. deaths and injuries)")
  print(bottom3Years)
  
  print("Highest divided by the smallest")
  print(top3Years[1,2] / bottom3Years[1,2])
  
  # Exclude 3 biggest years
  totalCostsTop3 <- head(totalCosts[order(-totalCosts$Reported.Cost.WithDeathsAndInjuries.normalised.millions),], n = 3)
  totalCostsMinusTop3 <- tail(totalCosts[order(-totalCosts$Reported.Cost.WithDeathsAndInjuries.normalised.millions),], n = -3)
  totalCostsByYearMinusTop3 <- with(totalCostsMinusTop3, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(Year.financial), FUN=safeSum))
  print("Average annual cost of all disasters (incl. deaths and injuries) MINUS TOP 3")
  print(mean(totalCostsByYearMinusTop3$x))
  
  # Test
  resMK <- significanceTest_MannKendall(totalCostsByYear)
  print("Significance test for total costs (incl. deaths and injuries)")
  print(resMK)
  summary(resMK)
  
  resLR <- significanceTest_LinearRegression(totalCostsByYear)
  print("Significance test (regression) for total costs (incl. deaths and injuries)")
  print(resLR)
  summary(resLR)
  
}


## Generate Figure 3.34
totalCostOfNaturalDisastersByDecade <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)
  
  # Filter by decade
  decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
  totalCostsByDecade <- with(totalCosts, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))
  
  # Multiply decades back up to 000's
  totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10
  
  standardBarChart(totalCostsByDecade,
                   "fig3_34_total_cost_of_natural_disasters_by_decade",
                   "FIGURE 3.34: TOTAL COST OF NATURAL DISASTERS BY DECADE, 1967-2013",
                   "Decades",
                   "(2013 Dollars in $millions)",
                   FALSE
  )
}

## Generate Figure 3.35 - SYNTHETIC
totalCostOfNaturalDisastersSynthetic <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)
  totalCostsByYear <- with(totalCosts, aggregate(Synthetic.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  
  # Graph the results
  standardBarChart(totalCostsByYear,
                   "fig3_35_total_cost_of_natural_disasters_synthetic",
                   "FIGURE 3.35: TOTAL COSTS (SYNTHETIC) OF DISASTERS, 1967-2013",
                   "Years (financial)",
                   "(2013 Dollars in $millions)"
  )
  
  # Show key statistics
  top10Events <- head(totalCosts[order(-totalCosts$Synthetic.Cost.normalised.millions),c("Year.financial", "Year", "title", "Reported.Cost.WithDeathsAndInjuries.normalised.millions")], 10)
  print("Top 10 Events by Cost (Synthetic)")
  print(top10Events)
  
  print("Average annual cost of all disasters (Synthetic)")
  print(mean(totalCostsByYear$x))
  
  print("Standard deviation of annual cost of all disasters (Synthetic)")
  print(sd(totalCostsByYear$x))
  
  print("Total cost of all disasters (Synthetic)")
  print(sum(totalCostsByYear$x))  
  
  # Australia's population as reported by ABS June 2013
  print("Average cost of all disasters per person")
  print(1000000 * sum(totalCostsByYear$x) / 23135281)
  
  print("Total cost of all disasters (incl. deaths and injuries)")
  print(sum(totalCostsByYear$x))  
  
  # Top and bottom 3 years
  top3Years <- head(totalCostsByYear[order(-totalCostsByYear$x),], 3)
  print("Top 3 years by Cost (Synthetic)")
  print(top3Years)
  
  bottom3Years <- head(totalCostsByYear[order(totalCostsByYear$x),], 3)
  print("Bottom 3 years by Cost (Synthetic)")
  print(bottom3Years)
  
  print("Highest divided by the smallest")
  print(top3Years[1,2] / bottom3Years[1,2])
  
  # Exclude 3 biggest years
  totalCostsTop3 <- head(totalCosts[order(-totalCosts$Synthetic.Cost.normalised.millions),], n = 3)
  totalCostsMinusTop3 <- tail(totalCosts[order(-totalCosts$Synthetic.Cost.normalised.millions),], n = -3)
  totalCostsByYearMinusTop3 <- with(totalCostsMinusTop3, aggregate(Synthetic.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  print("Average annual cost of all disasters (Synthetic) MINUS TOP 3")
  print(mean(totalCostsByYearMinusTop3$x))
  
  # Test
  resMK <- significanceTest_MannKendall(totalCostsByYear)
  print("Significance test for total costs (Synthetic)")
  print(resMK)
  summary(resMK)
  
  resLR <- significanceTest_LinearRegression(totalCostsByYear)
  print("Significance test (regression) for total costs (Synthetic)")
  print(resLR)
  summary(resLR)
}


## Generate Figure 3.36 - SYNTHETIC
totalCostOfNaturalDisastersByDecadeSynthetic <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)
  
  # Filter by decade
  decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
  totalCostsByDecade <- with(totalCosts, aggregate(Synthetic.Cost.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))
  
  # Multiply decades back up to 000's
  totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10
  
  standardBarChart(totalCostsByDecade,
                   "fig3_36_total_cost_of_natural_disasters_by_decade_synthetic",
                   "FIGURE 3.36: TOTAL COST (SYNTHETIC) OF NATURAL DISASTERS BY DECADE, 1967-2013",
                   "Decades",
                   "(2013 Dollars in $millions)",
                   FALSE
  )
}


## Generate Figure 3.37
totalDeathsAsPercentageOfPop <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)

  numberByYearDenormalised <- with(totalCosts, aggregate(Deaths, by=list(Year.financial), FUN=sum))
  numberByYearDenormalised <- includeAllYears(numberByYearDenormalised)
  
  numberByYearDenormalised$Deaths <- numberByYearDenormalised$x
  numberByYearDenormalised$TotalPop <- apply(cbind(numberByYearDenormalised$Group.1), 1, popForYear)
  numberByYearDenormalised$percentOfPop <- 100 * numberByYearDenormalised$Deaths / numberByYearDenormalised$TotalPop
  # For graphing purposes
  numberByYearDenormalised$x <- numberByYearDenormalised$percentOfPop
  
  standardBarChart(numberByYearDenormalised,
                            "fig3_37_number_of_natural_disasters_deaths",
                            "FIGURE 3.37: NUMBER OF DEATHS AS PERCENTAGES OF POPULATION, 1967-2013",
                            "Years (financial)",
                            "(Per Cent)",
                            TRUE
  )
}


## Generate Figure 3.38
totalCostAsPercentageOfGdp <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)
  totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.interpolated.millions, by=list(Year.financial), FUN=safeSum))
  
  totalCostsByYear$Reported.Cost.interpolated.millions <- totalCostsByYear$x
  totalCostsByYear$gdp <- apply(cbind(totalCostsByYear$Group.1), 1, gdpValues)
  totalCostsByYear$percentOfGDP <- 100 * totalCostsByYear$Reported.Cost.interpolated.millions / totalCostsByYear$gdp
  # For graphing purposes
  totalCostsByYear$x <- totalCostsByYear$percentOfGDP
  
  # Graph the results
  standardBarChart(totalCostsByYear,
                   "fig3_38_total_cost_as_percentage_of_gdp",
                   "FIGURE 3.38: TOTAL COSTS AS PERCENTAGES OF GDP, 1967-2013",
                   "Years (financial)",
                   "(Per Cent)", 
                   TRUE
  )
}

## Generate Figure 3.39
insuredCostAsPercentageOfTotalCost <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)
  totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(Year.financial), FUN=safeSum))
  insuredCostsByYear <- with(totalCosts, aggregate(Insured.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  
  mergedCosts <- merge(totalCostsByYear, insuredCostsByYear, by="Group.1", all.x = TRUE)
  names(mergedCosts)[2] <- paste("Reported.Cost")
  names(mergedCosts)[3] <- paste("Insured.Cost")
  mergedCosts$x <- 100 * mergedCosts$Insured.Cost / mergedCosts$Reported.Cost
   
  # Graph the results
  standardBarChart(mergedCosts,
                   "fig3_39_insured_cost_as_percentage_of_total_cost",
                   "FIGURE 3.39: INSURED COSTS AS PERCENTAGES OF TOTAL COSTS, 1967-2013",
                   "Years (financial)",
                   "(Per Cent)", 
                   TRUE
  )
  
  print("Average percentage of insured costs")
  print(mean(mergedCosts$x))
}


## Generate Figure 3.40
totalCostsRawIndexedNormalised <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)
  totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(Year.financial), FUN=safeSum))
  totalCostsByYearIndexed <- with(totalCosts, aggregate(Reported.Cost.WithDeathsAndInjuries.indexed.millions, by=list(Year.financial), FUN=safeSum))
  totalCostsByYearRaw <- with(totalCosts, aggregate(Reported.Cost.WithDeathsAndInjuries.interpolated.millions, by=list(Year.financial), FUN=safeSum))
  
  mergedCosts <- merge(totalCostsByYear, totalCostsByYearIndexed, by="Group.1", all.x = TRUE)
  mergedCosts <- merge(mergedCosts, totalCostsByYearRaw, by="Group.1", all.x = TRUE)
  names(mergedCosts)[2] <- paste("Normalised Cost")
  names(mergedCosts)[3] <- paste("Indexed Cost")
  names(mergedCosts)[4] <- paste("Raw Cost")
  
  data <- melt(mergedCosts, id.vars="Group.1", value.name="x", variable.name="Cost.Type")
  file_name <- "fig3_40_total_costs_raw_indexed_normalised"
  title <- "FIGURE 3.40: TOTAL COSTS - RAW vs INDEXED vs NORMALISED, 1967-2013"
  x_label <- "Years (financial)"
  y_label <- "(2013 Dollars in $millions)"
  useYears <- TRUE
  
  # Set colours
  background <- '#F0F0F0'
  foreground <- '#D08728'
  textColor <- '#888888'
  
  # Calculate range from 0 to max value of costs
  x_scale <- scale_x_continuous(name = x_label, breaks = yearBreaks(data$Group.1), labels = yearLabels(data$Group.1))
  p <- ggplot(data=data, aes(x=Group.1, y = x, group = Cost.Type, colour=Cost.Type)) + 
    geom_line() + 
    ggtitle(title) + x_scale + scale_y_continuous(name=y_label, labels=comma) + 
    theme(plot.title = element_text(colour = foreground, lineheight=.8, face="bold"),
          panel.grid.minor.y=element_blank(), 
          panel.grid.major.y=element_line(colour = foreground),
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.x=element_blank(),
          panel.background = element_rect(fill = background, colour = foreground),
          axis.title=element_text(color=textColor),
          axis.text.x=element_text(angle=45, vjust=1.0, hjust=1.0, size=6))
  p
  
  ggsave(file=paste("./figs/", file_name, ".png", sep=""))  
}




## Generate Figure 3.41
totalAverageCostsNationallyAndByState <- function() {

  totalCosts <- totalCostForEventFiltered(NULL, TRUE, FALSE)
  # Just for normalised data
  averageCostsByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=mean))
  averageCostsByYear <- includeAllYears(averageCostsByYear)

  data <- averageCostsByYear
  title <- "FIGURE 3.41: ANNUAL AVERAGE COST OF DISASTERS, 1967-2013"
  x_label <- "Years (financial)"
  y_label <- "(2013 Dollars in $millions)"
  
  # Graph the results
  standardBarChart(averageCostsByYear,
                   "fig3_41_total_average_costs_nationally_and_by_state",
                   title,
                   x_label,
                   y_label
  )

}


## Generate Figure 3.42
totalCostsQldNswVic <- function() {
  # Store the total costs by state
  totalCosts <- totalCostForEventFiltered(NULL, TRUE, FALSE)
  totalCosts$Reported.Cost.normalised.millions.state.1 <- totalCosts$Reported.Cost.normalised.millions * totalCosts$State.1.percent
  totalCosts$Reported.Cost.normalised.millions.state.2 <- totalCosts$Reported.Cost.normalised.millions * totalCosts$State.2.percent
  totalCostsByState1 <- with(totalCosts, aggregate(Reported.Cost.normalised.millions.state.1, by=list(State.abbreviated.1, Year.financial), FUN=safeSum))
  totalCostsByState2 <- with(totalCosts, aggregate(Reported.Cost.normalised.millions.state.2, by=list(State.abbreviated.2, Year.financial), FUN=safeSum))
  totalCostsByState <- merge(totalCostsByState1, totalCostsByState2, by=c("Group.1", "Group.2"), all.x = TRUE )
  totalCostsByState$x <- rowSums(cbind(totalCostsByState$x.x, totalCostsByState$x.y), na.rm = TRUE)
  totalCostsByState <- totalCostsByState[with(totalCostsByState, order(-x)), ]
  # Only QLD, NSW, VIC
  totalCostsByState <- totalCostsByState[totalCostsByState$Group.1 %in% c('QLD', 'NSW', 'VIC'),]
  # Order by year
  totalCostsByState <- totalCostsByState[order(-totalCostsByState$Group.2),]

  data <- totalCostsByState
  file_name <- "fig3_42_total_costs_qld_nsw_vic"
  title <- "FIGURE 3.42: TOTAL COSTS - QLD vs NSW vs VIC, 1967-2013"
  x_label <- "Years (financial)"
  y_label <- "(2013 Dollars in $millions)"
  useYears <- TRUE
  
  # Set colours
  background <- '#F0F0F0'
  foreground <- '#D08728'
  textColor <- '#888888'
  
  # Calculate range from 0 to max value of costs
  x_scale <- scale_x_continuous(name = x_label, breaks = yearBreaks(data$Group.2), labels = yearLabels(data$Group.2))
  p <- ggplot(data=totalCostsByState, aes(x=Group.2, y = x, group = Group.1, colour=Group.1)) + 
    geom_line() + 
    ggtitle(title) + x_scale + scale_y_continuous(name=y_label, labels=comma) + 
    theme(plot.title = element_text(colour = foreground, lineheight=.8, face="bold"),
          panel.grid.minor.y=element_blank(), 
          panel.grid.major.y=element_line(colour = foreground),
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.x=element_blank(),
          panel.background = element_rect(fill = background, colour = foreground),
          axis.title=element_text(color=textColor),
          axis.text.x=element_text(angle=45, vjust=1.0, hjust=1.0, size=6))
  p
  
  ggsave(file=paste("./figs/", file_name, ".png", sep=""))  
}


# Tables

## Generate Table 3.1
averageAnnualCostOfNaturalDisastersByStateAndTerritory <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)
  totalCosts$Reported.Cost.normalised.millions.state.1 <- totalCosts$Reported.Cost.normalised.millions * totalCosts$State.1.percent
  totalCosts$Reported.Cost.normalised.millions.state.2 <- totalCosts$Reported.Cost.normalised.millions * totalCosts$State.2.percent
  totalCostsByState1 <- with(totalCosts, aggregate(Reported.Cost.normalised.millions.state.1, by=list(State.abbreviated.1, resourceType), FUN=safeSum))
  totalCostsByState2 <- with(totalCosts, aggregate(Reported.Cost.normalised.millions.state.2, by=list(State.abbreviated.2, resourceType), FUN=safeSum))
  totalCostsByStateAndDisasterType <- merge(totalCostsByState1, totalCostsByState2, by=c("Group.1", "Group.2"), all.x = TRUE )
  totalCostsByStateAndDisasterType$x <- rowSums(cbind(totalCostsByStateAndDisasterType$x.x, totalCostsByStateAndDisasterType$x.y), na.rm = TRUE)
  # totalCostsByStateAndDisasterType <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(State.abbreviated.1, resourceType), FUN=safeSum))
  totalCostsByStateAndDisasterType$x <- round(totalCostsByStateAndDisasterType$x)
  
  # Very brittle conversion to a table
  pivotted.data <- dcast(totalCostsByStateAndDisasterType, Group.1 ~ Group.2, value.var = "x", sum, margins = TRUE)
  cols <- length(pivotted.data)
  rows <- length(pivotted.data$Group.1)
  pivotted.data <- pivotted.data[order(-pivotted.data[cols]),]
  pivotted.data[rows + 1,] <- pivotted.data[1,]
  pivotted.data <- pivotted.data[seq(2, rows + 1),]
  pivotted.data <- pivotted.data[,order(-pivotted.data[rows,])]
  pivotted.data[,cols+1] <- pivotted.data[,1]
  pivotted.data[,1] <- pivotted.data[,cols]
  pivotted.data <- pivotted.data[,c(seq(1:(cols - 1)), cols + 1)]
  pivotted.data[1:rows, 2:cols] <- format(pivotted.data[1:rows, 2:cols], nsmall = 0, big.mark=",")

  write.table(pivotted.data, file = "./figs/table3_1_totals_by_state_and_disaster_type.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")

  # Repeat for deaths and injuries
  totalCosts$Reported.Cost.WithDeathsAndInjuries.normalised.millions.state.1 <- totalCosts$Reported.Cost.WithDeathsAndInjuries.normalised.millions * totalCosts$State.1.percent
  totalCosts$Reported.Cost.WithDeathsAndInjuries.normalised.millions.state.2 <- totalCosts$Reported.Cost.WithDeathsAndInjuries.normalised.millions * totalCosts$State.2.percent
  totalCostsByState1 <- with(totalCosts, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions.state.1, by=list(State.abbreviated.1, resourceType), FUN=safeSum))
  totalCostsByState2 <- with(totalCosts, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions.state.2, by=list(State.abbreviated.2, resourceType), FUN=safeSum))
  totalCostsByStateAndDisasterType <- merge(totalCostsByState1, totalCostsByState2, by=c("Group.1", "Group.2"), all.x = TRUE )
  totalCostsByStateAndDisasterType$x <- rowSums(cbind(totalCostsByStateAndDisasterType$x.x, totalCostsByStateAndDisasterType$x.y), na.rm = TRUE)
  # totalCostsByStateAndDisasterType <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(State.abbreviated.1, resourceType), FUN=safeSum))
  totalCostsByStateAndDisasterType$x <- round(totalCostsByStateAndDisasterType$x)
  
  # Very brittle conversion to a table
  pivotted.data <- dcast(totalCostsByStateAndDisasterType, Group.1 ~ Group.2, value.var = "x", sum, margins = TRUE)
  cols <- length(pivotted.data)
  rows <- length(pivotted.data$Group.1)
  pivotted.data <- pivotted.data[order(-pivotted.data[cols]),]
  pivotted.data[rows + 1,] <- pivotted.data[1,]
  pivotted.data <- pivotted.data[seq(2, rows + 1),]
  pivotted.data <- pivotted.data[,order(-pivotted.data[rows,])]
  pivotted.data[,cols+1] <- pivotted.data[,1]
  pivotted.data[,1] <- pivotted.data[,cols]
  pivotted.data <- pivotted.data[,c(seq(1:(cols - 1)), cols + 1)]
  pivotted.data[1:rows, 2:cols] <- format(pivotted.data[1:rows, 2:cols], nsmall = 0, big.mark=",")

  write.table(pivotted.data, file = "./figs/table3_1_totals_by_state_and_disaster_type_with_deaths_and_injuries.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
}

## Generate Table 3.2
deathsAndInjuriesByHazardType <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)
  
  totalNumberOfDeathsByResourceType <- with(totalCosts, aggregate(Deaths, by=list(resourceType), FUN=safeSum))
  totalNumberOfDeathsByResourceType.n <- with(totalCosts, aggregate(Deaths.normalised, by=list(resourceType), FUN=safeSum))
  totalNumberOfInjuriesByResourceType <- with(totalCosts, aggregate(Injuries, by=list(resourceType), FUN=safeSum))
  totalNumberOfInjuriesByResourceType.n <- with(totalCosts, aggregate(Injuries.normalised, by=list(resourceType), FUN=safeSum))
  totalCostsOfDeathsInjuriesByResourceType <- with(totalCosts, aggregate(deathAndInjuryCosts.normalised.millions, by=list(resourceType), FUN=safeSum))
  totalCostsByDisasterType <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(resourceType), FUN=safeSum))
  totalCostsByDisasterType$incl.deaths <- totalCostsByDisasterType$x + totalCostsOfDeathsInjuriesByResourceType$x
  totalCostsByDisasterType$percent.deaths <- totalCostsOfDeathsInjuriesByResourceType$x / totalCostsByDisasterType$incl.deaths
  merged.data <- merge(totalNumberOfDeathsByResourceType, totalNumberOfDeathsByResourceType.n, by = "Group.1", all = TRUE, suffixes = c(".a", ".b"))
  merged.data <- merge(merged.data, totalNumberOfInjuriesByResourceType, by = "Group.1", all = TRUE, suffixes = c(".b", ".c"))
  merged.data <- merge(merged.data, totalNumberOfInjuriesByResourceType.n, by = "Group.1", all = TRUE, suffixes = c(".c", ".d"))
  merged.data <- merge(merged.data, totalCostsOfDeathsInjuriesByResourceType, by = "Group.1", all = TRUE, suffixes = c(".d", ".e"))
  merged.data <- merge(merged.data, totalCostsByDisasterType, by = "Group.1", all = TRUE, suffixes = c(".e", ".f", ".g", ".h"))
  merged.data <- merged.data[order(-merged.data$x.e),]
  merged.data[, seq(2, 9)] <- round(merged.data[, seq(2, 9)])
  merged.data[, seq(2, 9)] <- format(merged.data[, seq(2, 9)], big.mark=",")
  
  print("Percentages of deaths by disaster type")
  print(totalCostsByDisasterType)
  
  write.table(merged.data, file = "./figs/table3_2_deaths_injuries_by_disaster_type.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
}

## Generate Table 3.3
multipliersJoyVsDerived <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)
  
  event.types <- data.frame(eventTypes = unique(totalCosts$resourceType))
  event.types$multipliers.Joy <- apply(data.frame(event.types$eventTypes), 1, eventTypeMultiplierJoy)
  event.types$multipliers.Derived <- apply(data.frame(event.types$eventTypes), 1, eventTypeMultiplierDerived)
  
  names(event.types)[1] <- "Hazard Type"
  names(event.types)[2] <- "Joy's (1991) multiplier"
  names(event.types)[3] <- "Derived (2015) multiplier"
  
  print("Event Type Multipliers")
  print(event.types)
  
  write.table(event.types, file = "./figs/table3_3_multipliers_joy_vs_derived.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
}

## Generate Table 3.4
costsByYearAndState <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEventFiltered(NULL, FALSE, FALSE)
  totalCosts$Reported.Cost.normalised.millions.state.1 <- totalCosts$Reported.Cost.normalised.millions * totalCosts$State.1.percent
  totalCosts$Reported.Cost.normalised.millions.state.2 <- totalCosts$Reported.Cost.normalised.millions * totalCosts$State.2.percent
  totalCostsByState1 <- with(totalCosts, aggregate(Reported.Cost.normalised.millions.state.1, by=list(State.abbreviated.1, Year.financial), FUN=safeSum))
  totalCostsByState2 <- with(totalCosts, aggregate(Reported.Cost.normalised.millions.state.2, by=list(State.abbreviated.2, Year.financial), FUN=safeSum))
  totalCostsByStateAndYear <- merge(totalCostsByState1, totalCostsByState2, by=c("Group.1", "Group.2"), all.x = TRUE )
  totalCostsByStateAndYear$x <- rowSums(cbind(totalCostsByStateAndYear$x.x, totalCostsByStateAndYear$x.y), na.rm = TRUE)
  state.year.totals <- totalCostsByStateAndYear[,c("Group.2", "Group.1", "x")]
  names(state.year.totals)[1] = "Year"
  names(state.year.totals)[2] = "State"
  names(state.year.totals)[3] = "Total"
  
  pivotted.data <- acast(state.year.totals, Year ~ State, fill = 0, value.var = "Total")

  # state.year.totals <- state.year.totals[order(state.year.totals$Year, state.year.totals$State),]
  
  write.table(pivotted.data, file = "./figs/table3_4_costs_by_year_and_state.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
}



