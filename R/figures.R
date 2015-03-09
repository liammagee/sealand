
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
title_size <- 0.8
character_size <- 0.6

# Functions for generating figures

yearBreaks <- function(years) {
  return (years[(years-1967)%%3==0]);
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
    x_scale <- scale_x_continuous(name=x_label, breaks=yearBreaks(data$Group.1))
  } else {
    x_scale <- xlab(x_label)
  }
  p + ggtitle(title) + x_scale + scale_y_continuous(name=y_label, labels=comma) + 
    theme(plot.title = element_text(colour = foreground, lineheight=.8, face="bold"),
          panel.grid.minor.y=element_blank(), 
          panel.grid.major.y=element_line(colour = foreground),
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.x=element_blank(),
          panel.background = element_rect(fill = background, colour = foreground),
          axis.title=element_text(color=textColor),
          axis.text.x=element_text(angle=45, vjust=1.0, hjust=1.0))
  
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
  p <- ggplot(data, aes(x=Group.1, y = value)) + geom_bar(aes(fill=variable), width=0.75,position = "dodge", stat="identity") +  
                                                            scale_fill_manual(values=c(foreground, background2))
  p
  if (useYears==TRUE) {
    x_scale <- scale_x_continuous(name=x_label, breaks=yearBreaks(data$Group.1))
  } else {
    x_scale <- xlab(x_label)
  }
  p + ggtitle(title) + x_scale + scale_y_continuous(name=y_label, labels=comma) + 
    theme(plot.title = element_text(colour = foreground, lineheight=.8, face="bold"),
          panel.grid.minor.y=element_blank(), 
          panel.grid.major.y=element_line(colour = foreground),
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.x=element_blank(),
          panel.background = element_rect(fill = background, colour = foreground),
          axis.title=element_text(color=textColor),
          axis.text.x=element_text(angle=45, vjust=1.0, hjust=1.0))
  
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
       cex=character_size,
       cex.lab=character_size,
       cex.axis=character_size,
       cex.main=character_size,
       cex.sub=character_size)
  
  # Add title
  title(title, col.main = "blue",
        cex=title_size,
        cex.lab=title_size,
        cex.axis=title_size,
        cex.main=title_size,
        cex.sub=title_size)
  
  # Label the x and y axes with dark green text
  title(xlab=x_label, col.lab=rgb(0,0.5,0))
  title(ylab=y_label, col.lab=rgb(0,0.5,0))
}

## Generates an axis with character widths
doAxis <- function(number, at=NULL, labels=NULL) {
  # axis(number, at=at, labels=labels,
  #      cex=character_size,
  #      cex.lab=character_size,
  #      cex.axis=character_size,
  #      cex.main=character_size,
  #      cex.sub=character_size)
}


## Generates a sheet with all the data and computed values
generate_complete_data <- function() {
  write.table(mydata, file = "./output/database_computed.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")

}


## Generates Figure 3.0
total_costs_of_disasters_in_australia <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEvent()
  # Just for normalised data
  totalCostsAll <- totalCosts[c("Year.financial", "title", "Reported.Cost.normalised.millions")]
  # Mirror the usual aggregation
  names(totalCostsAll)[1] = "Year"
  names(totalCostsAll)[2] = "Group.1"
  names(totalCostsAll)[3] = "x"
  totalCostsAll <- totalCostsAll[order(totalCostsAll$Year),]
  
  # Cache variables
  data <- totalCostsAll
  title <- "FIGURE 3.0: TOTAL COST OF DISASTERS IN AUSTRALIA, 1967-2013"
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


## Generates Figure 3.1
annual_total_costs_of_disasters_in_australia <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEvent()
  # Just for normalised data
  totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  totalCostsByYear <- includeAllYears(totalCostsByYear)
  
  # Cache variables
  data <- totalCostsByYear
  title <- "FIGURE 3.1: ANNUAL TOTAL COST OF DISASTERS IN AUSTRALIA, 1967-2013"
  x_label <- "Years (financial)"
  y_label <- "(2013 Dollars in $millions)"
  # Graph the results
  standardBarChart(totalCostsByYear,
                   "fig3_1_annual_total_costs_of_disasters_in_australia",
                   title,
                   x_label,
                   y_label
  )
  
  print("Average annual cost of all disasters")
  print(mean(totalCostsByYear$x))
  
  print("Range of annual costs of all disasters")
  print(range(totalCostsByYear$x))

  print("Ordered sequence of annual costs")
  print(totalCostsByYear[order(-totalCostsByYear$x),])
  
  print("Ratio of most expensive year to least expensive year")
  print(totalCostsByYear[order(-totalCostsByYear$x),][1,2] / totalCostsByYear[order(totalCostsByYear$x),][1,2])
  
  print("Total cost of all disasters")
  print(sum(totalCostsByYear$x))
  
  # Run the significance test
  res <- significanceTest_MannKendall(totalCostsByYear)
  print("Significance test for total costs by year")
  print(res)
  summary(res)
}


## Generates Figure 3.2
australian_natural_disaster_costs_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()

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
average_cost_per_event <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
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
distribution_of_disasters <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()

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
annual_insurance_cost_of_disasters <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	directCostsByYear <- with(totalCosts, aggregate(Insured.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))

	standardBarChart(directCostsByYear,
		"fig3_5_annual_insurance_costs_of_disasters_in_australia",
		"FIGURE 3.5: ANNUAL INSURANCE COSTS OF DISASTERS IN AUSTRALIA, 1967-2013",
		"Years (financial)",
		"(2013 Dollars in $millions)"
		)
	
	# Run the significance test
	res <- significanceTest_MannKendall(directCostsByYear)
	print("Significance test for insurance costs of disasters")
	print(res)
	summary(res)
}


## Generate Figure 3.6
number_of_natural_disasters_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
  # Exclude events without a reported cost
	totalCosts <- totalCosts[totalCosts$Reported.Cost.normalised.millions > 0,]
  numberByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=length))
	# numberByYear <- with(totalCosts, aggregate(Insured.Cost.normalised.millions, by=list(Year.financial), FUN=length))

	standardBarChart(numberByYear,
		"fig3_6_number_of_natural_disasters_in_australia",
		"FIGURE 3.6: NUMBER OF NATURAL DISASTERS IN AUSTRALIA, 1967-2013",
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
natural_disasters_between_10_and_75_million <- function() {
	# Correlation test
}


## Generate Figure 3.8
natural_disasters_between_75_and_150_million <- function() {
	# Correlation test
}


## Generate Figure 3.9
number_of_disasters_per_million_people <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEvent()
  # Exclude events without a reported cost
  totalCosts <- totalCosts[totalCosts$Reported.Cost.normalised.millions > 0,]
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
  summary(fit <- lm(formula = popRatios ~ Group.1, data = mergedCounts))
}


## Generate Figure 3.10
disaster_costs_by_state_and_territory <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	totalCostsByState <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(State.abbreviated), FUN=safeSum))
	totalCostsByState <- totalCostsByState[with(totalCostsByState, order(-x)), ]
	
	# Cache variables
	data <- totalCostsByState
	x_label <- "States"
	y_label <- "(2013 Dollars in $millions)"
	title <- "FIGURE 3.10: DISASTER COSTS BY STATE AND TERRITORY IN AUSTRALIA"

  standardBarChart(totalCostsByState,
		"fig3_10_disaster_costs_by_state_and_territory",
		"FIGURE 3.10: DISASTER COSTS BY STATE AND TERRITORY IN AUSTRALIA",
		"States",
		"(2013 Dollars in $millions)",
    FALSE
		)
  
  # Generate percentages
	insuredCostsByState <- with(totalCosts, aggregate(Insured.Cost.normalised.millions, by=list(State.abbreviated), FUN=safeSum))
	insuredCostsByState <- insuredCostsByState[with(insuredCostsByState, order(-x)), ]
  mergedCosts <- merge(totalCostsByState, insuredCostsByState, by="Group.1", all.x = TRUE)
	mergedCosts$totalCostsPercentages <- data.frame(mergedCosts$x.x / sum(mergedCosts$x.x))
	mergedCosts$insuredCostsPercentages <- data.frame(mergedCosts$x.y / sum(mergedCosts$x.y))
  print("Comparing total and insurable costs")
	print(mergedCosts)
	# Test correlation between total and insured costs
	print(cor.test( mergedCosts$x.x, mergedCosts$x.y, method = "pearson"))

	print("Combined percentage of top 3 events")
	print(sum(head(mergedCosts[order(-mergedCosts$totalCostsPercentages),]$totalCostsPercentages, 3)))
	  
  print("Percentage of Population in Queensland, New South Wales and Victoria")
  # Taken from ABS June 2014  - http://www.abs.gov.au/ausstats/abs@.nsf/mf/3101.0
  print((7518.5 + 5841.7 + 4722.4) / 23490.7)
  
  # Count number of "raw" (non-interpolated) reported costs
	print("Percentage of 'raw' to 'interpolated' reported costs")
  print(length(totalCosts$Reported.Cost[totalCosts$Reported.Cost > 0]) / 
          length(totalCosts$Reported.Cost.interpolated[totalCosts$Reported.Cost.interpolated > 0]))
  
  # Compare just those events that have both insured and reported costs
	bothCostsByState <- with(totalCosts[totalCosts$Reported.Cost > 0 & totalCosts$Insured.Cost > 0,], aggregate(data.frame(Reported.Cost,  Insured.Cost), by=list(State.abbreviated), FUN=safeSum))
	print(cor.test( bothCostsByState$Reported.Cost, bothCostsByState$Insured.Cost, method = "pearson"))
	
	# Show major events by state
	sortedByStateAndCost <- totalCosts[with(totalCosts,order(State.abbreviated, -Reported.Cost.normalised.millions)),]
	sortedByStateAndCost <- with(sortedByStateAndCost, data.frame(State.abbreviated, title, Year, Year.financial, Reported.Cost.normalised.millions))
	write.table(sortedByStateAndCost, file = "./output/ordered_events.csv", append = FALSE, quote = TRUE, sep = ",",
	            eol = "\n", na = "", dec = ".", row.names = FALSE,
	            col.names = TRUE, qmethod = c("escape", "double"),
	            fileEncoding = "")
	
	# Show major events by state and type
	totalCostsByStateAndType <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(State.abbreviated, resourceType), FUN=safeSum))
	totalCostsByStateAndType <- totalCostsByStateAndType[with(totalCostsByStateAndType, order(Group.1, Group.2)), ]
  print("Events aggregated by state and event type")
  print(totalCostsByStateAndType)
	print("Distinct event types")
	print(data.frame(unique(totalCosts$resourceType)))
  
}


## Generate Figure 3.11
number_of_disaster_events_by_state_and_territory <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEvent()
  # Exclude events without a reported cost
  totalCosts <- totalCosts[totalCosts$Reported.Cost.normalised.millions > 0,]
  totalCountsByState <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(State.abbreviated), FUN=length))

  # Remove 'Other' column
  totalCountsByState <- totalCountsByState[!(totalCountsByState$Group.1 %in% c('Other')),]
  
  # Order by state
  totalCountsByState <- totalCountsByState[with(totalCountsByState, order(-x)), ]

	standardBarChart(totalCountsByState,
		"fig3_11_number_of_disaster_events_by_state_and_territory",
		"FIGURE 3.11: # OF DISASTER EVENTS BY STATE AND TERRITORY IN AUSTRALIA, 1967-2013",
		"States",
		"Number of events",
    FALSE
		)
  
  # Show relative frequencies
  totalCountsByState$totalCountsPercentages <- data.frame(totalCountsByState$x / sum(totalCountsByState$x))
  print("Percentage of event requencies")
  print(totalCountsByState)
  print("Combined percentage of top 3 events")
  print(sum(head(totalCountsByState$totalCountsPercentages, 3)))
}


## Generate Figure 3.12
costs_by_type_of_disaster_and_state_and_territory <- function() {
	# Pie chart
}


## Generate Figure 3.13
total_and_insurance_costs_by_disaster_type <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEvent()
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
number_of_events_by_disaster_type <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEvent()
  # Exclude events without a reported cost
  totalCosts <- totalCosts[totalCosts$Reported.Cost.normalised.millions > 0,]
  totalCountsByDisasterType <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(resourceType), FUN=length))
  totalCountsByDisasterType <- totalCountsByDisasterType[with(totalCountsByDisasterType, order(-x)), ]

	standardBarChart(totalCountsByDisasterType,
		"fig3_14_number_of_events_by_disaster_type",
		"FIGURE 3.14: DISASTER COSTS BY STATE AND TERRITORY IN AUSTRALIA, 1967-2013",
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
annual_cost_of_floods_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Flood")
	totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))

	# Graph the results
	standardBarChart(totalCostsByYear,
		"fig3_15_annual_cost_of_floods_in_australia",
		"FIGURE 3.15: ANNUAL TOTAL COSTS OF FLOODS IN AUSTRALIA, 1967-2013",
		"Years (financial)",
		"(2013 Dollars in $millions)"
		)
	
  # Show key statistics
	top10Floods <- head(totalCostsByYear[order(-totalCostsByYear$x),], 10)
  print("Top 10 Floods Years by Cost")
	print(top10Floods)

  print("Average annual cost")
	print(mean(totalCostsByYear$x))
  
	# Run the significance test
	res <- significanceTest_MannKendall(totalCostsByYear)
	print("Significance test for cost of floods by year")
	print(res)
	summary(res)
	
}


## Generate Figure 3.16
total_cost_of_floods_by_decade <- function() {
		# Store the total costs by year
	totalCosts <- totalCostForEvent("Flood")

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
annual_number_of_floods_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Flood")
	# Exclude events without a reported cost
	totalCosts <- totalCosts[totalCosts$Reported.Cost.normalised.millions > 0,]
	numberByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=length))

	standardBarChart(numberByYear,
		"fig3_17_number_of_floods_in_australia",
		"FIGURE 3.17: NUMBER OF FLOODS IN AUSTRALIA, 1967-2013",
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
annual_cost_of_severe_storms_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Severe Storm")
	totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))

	# Graph the results
	standardBarChart(totalCostsByYear,
		"fig3_18_annual_cost_of_severe_storms_in_australia",
		"FIGURE 3.18: ANNUAL TOTAL COSTS OF SEVERE STORMS IN AUSTRALIA, 1967-2013",
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
total_cost_of_severe_storms_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Severe Storm")
	
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
annual_number_of_severe_storms_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Severe Storm")
	# Exclude events without a reported cost
	totalCosts <- totalCosts[totalCosts$Reported.Cost.normalised.millions > 0,]
	numberByYear <- with(totalCosts, aggregate(resourceType, by=list(Year.financial), FUN=length))

	standardBarChart(numberByYear,
		"fig3_20_number_of_severe_storms_in_australia",
		"FIGURE 3.20: NUMBER OF SEVERE STORMS IN AUSTRALIA, 1967-2013",
		"Years (financial)",
		"Number of events"
		)

	
	# Total number of storms
	print("Total number of storms")
	print(sum(numberByYear$x))

  print("Average number of storms")
  # Note 'mean' does not work here - years with zero do not appear
	print(sum(numberByYear$x) / (2013 - 1967))
}


## Generate Figure 3.21
annual_cost_of_cyclones_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Cyclone")
	totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))

	# Graph the results
	standardBarChart(totalCostsByYear,
		"fig3_21_annual_cost_of_cyclone_in_australia",
		"FIGURE 3.21: ANNUAL TOTAL COSTS OF CYCLONES IN AUSTRALIA, 1967-2013",
		"Years (financial)",
		"(2013 Dollars in $millions)"
		)
  
	
	# Show key statistics
	top10Cyclones <- head(totalCostsByYear[order(-totalCostsByYear$x),], 10)
	print("Top 10 Cyclones Years by Cost")
	print(top10Cyclones)
	
	print("Average annual cost of cyclones")
	print(sum(totalCostsByYear$x) / (2013 - 1967))
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
total_cost_of_cyclones_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Cyclone")

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
annual_number_of_cyclones_causing_more_than_10_million_damage_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Cyclone")
	# Exclude events without a reported cost
	totalCosts <- totalCosts[totalCosts$Reported.Cost.normalised.millions > 0,]
	numberByYear <- with(totalCosts, aggregate(resourceType, by=list(Year.financial), FUN=length))
  # Merge with a sequence, to ensure years with zero events are represented
  allYears <- data.frame(seq(1967, 2013))
  names(allYears)[1] <- "Group.1"
	numberByYear <- merge(allYears, numberByYear, by = "Group.1", all.x = TRUE)
	numberByYear[is.na(numberByYear$x),]$x <- 0
  
	standardBarChart(numberByYear,
		"fig3_23_number_of_cyclones_in_australia",
		"FIGURE 3.23: NUMBER OF CYCLONES IN AUSTRALIA, 1967-2013",
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
total_cost_of_earthquakes_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Earthquake")

	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
	totalCostsByDecade <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))
	# Merge with a sequence, to ensure years with zero events are represented
	allDecades <- data.frame(seq(196, 201))
	names(allDecades)[1] <- "Group.1"
	totalCostsByDecade <- merge(allDecades, totalCostsByDecade, by = "Group.1", all.x = TRUE)
	totalCostsByDecade[is.na(totalCostsByDecade$x),]$x <- 0
	
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
annual_cost_of_bushfires_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Bushfire")
	totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
	# Merge with a sequence, to ensure years with zero events are represented
	allYears <- data.frame(seq(1967, 2013))
	names(allYears)[1] <- "Group.1"
	totalCostsByYear <- merge(allYears, totalCostsByYear, by = "Group.1", all.x = TRUE)
	totalCostsByYear[is.na(totalCostsByYear$x),]$x <- 0
	
	# Graph the results
	standardBarChart(totalCostsByYear,
		"fig3_25_annual_cost_of_bushfire_in_australia",
		"FIGURE 3.25: ANNUAL TOTAL COSTS OF BUSHFIRES IN AUSTRALIA, 1967-2013",
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
total_cost_of_bushfires_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Bushfire")
	
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
annual_number_of_bushfires_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Bushfire")
	numberByYear <- with(totalCosts, aggregate(resourceType, by=list(Year.financial), FUN=length))
	# Exclude events without a reported cost
	totalCosts <- totalCosts[totalCosts$Reported.Cost.normalised.millions > 0,]
	
	standardBarChart(numberByYear,
		"fig3_27_number_of_bushfire_in_australia",
		"FIGURE 3.27: NUMBER OF BUSHFIRES IN AUSTRALIA, 1967-2013",
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
number_of_natural_disasters_deaths <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	# Exclude events without a reported cost
	totalCosts <- totalCosts[totalCosts$Reported.Cost.normalised.millions > 0,]
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
    "FIGURE 3.28: NUMBER OF DEATHS BY NATURAL DISASTERS IN AUSTRALIA, 1967-2013",
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
}


## Generate Figure 3.29
number_of_natural_disasters_injuries <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	# Exclude events without a reported cost
	totalCosts <- totalCosts[totalCosts$Reported.Cost.normalised.millions > 0,]
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
    "FIGURE 3.29: NUMBER OF INJURIES BY NATURAL DISASTERS IN AUSTRALIA, 1967-2013",
    "Years (financial)",
    "Number of injuries"
    )
}


## Generate Figure 3.30
number_of_deaths_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	# Exclude events without a reported cost
	totalCosts <- totalCosts[totalCosts$Reported.Cost.normalised.millions > 0,]
	
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
    "fig3_30_australian_natural_disaster_costs_by_decade",
    "FIGURE 3.30: COST OF DEATHS BY NATURAL DISASTERS BY DECADE, 1967-2013",
    "Decades",
    "(2013 Dollars in $millions)",
    FALSE
    )
	
  print("Number of deaths by decade (normalised)")
  print(numberOfDeathsByDecade)
	print("Number of deaths by decade")
	print(numberOfDeathsByDecadeDenormalised)
	
	totalCosts <- totalCostForEvent("Heatwave")
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
cost_of_deaths_and_injuries <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	totalCostsByYear <- with(totalCosts, aggregate(deathAndInjuryCosts.normalised.millions, by=list(Year.financial), FUN=safeSum))
	totalCostsByYear <- includeAllYears(totalCostsByYear)
  
	# Graph the results
	standardBarChart(totalCostsByYear,
		"fig3_31_cost_of_deaths_and_injuries",
		"FIGURE 3.31: COST OF DEATH AND INJURIES BY NATURAL DISASTERS IN AUSTRALIA, 1967-2013",
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
cost_of_deaths_and_injuries_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()

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
total_cost_of_natural_disasters <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	totalCostsByYear <- with(totalCosts, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(Year.financial), FUN=safeSum))

	# Graph the results
	standardBarChart(totalCostsByYear,
		"fig3_33_total_cost_of_natural_disasters",
		"FIGURE 3.33: TOTAL COSTS OF DISASTERS IN AUSTRALIA, 1967-2013",
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
	print(sum(totalCostsByYear$x) / 23135281)
	
	print("Total cost of all disasters (incl. deaths and injuries)")
	print(sum(totalCostsByYear$x))  
}


## Generate Figure 3.34
total_cost_of_natural_disasters_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()

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

# Tables

## Generate Table 3.1
average_annual_cost_of_natural_disasters_by_state_and_territory <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEvent()
  totalCostsByStateAndDisasterType <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(State.abbreviated, resourceType), FUN=safeSum))
  totalCostsByStateAndDisasterType$x <- round(totalCostsByStateAndDisasterType$x)
  
  # Very brittle conversion to a table
  pivotted.data <- dcast(totalCostsByStateAndDisasterType, Group.1 ~ Group.2, value.var = "x", sum, margins = TRUE)
  pivotted.data <- pivotted.data[order(-pivotted.data[9]),]
  pivotted.data[10,] <- pivotted.data[1,]
  pivotted.data[seq(2, 10),]
  pivotted.data <- pivotted.data[seq(2, 10),]
  pivotted.data <- pivotted.data[,order(-pivotted.data[9,])]
  pivotted.data[,10] <- pivotted.data[,1]
  pivotted.data[,1] <- pivotted.data[,9]
  pivotted.data <- pivotted.data[,c(seq(1:8), 10)]
  pivotted.data[1:9, 2:9] <- format(pivotted.data[1:9, 2:9], nsmall = 0, big.mark=",")

  write.table(pivotted.data, file = "./figs/table3_1_totals_by_state_and_disaster_type.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
}

## Generate Table 3.2
deaths_and_injuries_by_hazard_type <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEvent()
  # Exclude events without a reported cost
  totalCosts <- totalCosts[totalCosts$Reported.Cost.normalised.millions > 0,]
  totalNumberOfDeathsByResourceType <- with(totalCosts, aggregate(Deaths.normalised, by=list(resourceType), FUN=safeSum))
  totalNumberOfInjuriesByResourceType <- with(totalCosts, aggregate(Injuries.normalised, by=list(resourceType), FUN=safeSum))
  totalCostsOfDeathsInjuriesByResourceType <- with(totalCosts, aggregate(deathAndInjuryCosts.normalised.millions, by=list(resourceType), FUN=safeSum))
  mergedDeathAndInjuries <- merge(totalCostsOfDeathsByResourceType, totalCostsOfInjuriesByResourceType, by = "Group.1", all = TRUE)
  mergedDeathAndInjuryCosts <- merge(mergedDeathAndInjuries, totalCostsOfDeathsInjuriesByResourceType, by = "Group.1", all = TRUE)
  mergedDeathAndInjuryCosts[is.na(mergedDeathAndInjuryCosts$x),]$x <- 0
  mergedDeathAndInjuryCosts <- mergedDeathAndInjuryCosts[order(-mergedDeathAndInjuryCosts$x),]
  mergedDeathAndInjuryCosts[, c(2, 3, 4)] <- round(mergedDeathAndInjuryCosts[, c(2, 3, 4)])
  mergedDeathAndInjuryCosts[, c(2, 3, 4)] <- format(mergedDeathAndInjuryCosts[, c(2, 3, 4)], big.mark=",")
  
  write.table(mergedDeathAndInjuryCosts, file = "./figs/table3_2_deaths_injuries_by_disaster_type.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
}



## Helper functions for generating graphs



## Extra graphs


## Generates Figure 3.1 - FOR COMPARISON WITH BTE
annual_total_costs_of_disasters_in_australia_bte <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEvent()

  # For both normalised and denormalised data
  totalCostsByYear <- with(totalCosts, aggregate(cbind(Insured.Cost.multiplied.normalised/ 1000000, Reported.Cost.normalised / 1000000), by=list(Year), FUN=safeSum))
  data <- totalCostsByYear

  # Calculate range from 0 to max value of costs
  x_range <- range(totalCosts$Year)
  y_range <- range(0, totalCostsByYear + 2000)

  ## Graph the results

  # Plot a basic graph of costs
  pdf(file=paste("./figs/", "fig3_1_annual_total_costs_of_disasters_in_australia_bte", ".pdf", sep=""))

  # Set an upper y value based on the data passed in
  # Note: this will often be too little
  if (is.null(y_range)) {
    y_range <- range(0, data)
  }
  data <- t(cbind(totalCostsByYear[, 2], totalCostsByYear[, 3]))
  colnames(data) <- totalCostsByYear[, 1]
  rownames(data) <- c("Normalised", "Raw")

    # Plot normalised data
  barplot(data, beside=T,
          axisnames=T,
          cex=title_size,
          cex.lab=title_size,
          cex.axis=title_size,
          cex.main=title_size,
          cex.sub=title_size,
          cex.names=0.8, las=2, ylim=y_range, col=c("blue","red"))

  # Add title
  title("FIGURE 3.1b: ANNUAL TOTAL COSTS OF DISASTERS IN AUSTRALIA (BTE vs RMIT), 1967-2013", col.main = "blue",
        cex=title_size,
        cex.lab=title_size,
        cex.axis=title_size,
        cex.main=title_size,
        cex.sub=title_size)

  # Label the x and y axes with dark green text
  title(xlab="Years (financial)", col.lab=rgb(0,0.5,0))
  title(ylab="(2013 Dollars in $millions)", col.lab=rgb(0,0.5,0))

  # Default x axis
  doAxis(1, at=seq(x_range[1], x_range[2], by=1))

  # Make y axis with horizontal labels that display ticks at
  # billions <- 1000000000 * 0:(y_range[2] / 1000000000)
  # doAxis(2, at=billions, labels=format(billions / 1000000, big.mark = ","))

  dev.off()
}


## Generates Figure 3.1 - FOR COMPARISON WITH INTERPOLATED FIGURES
annual_total_costs_of_disasters_in_australia_interpolated <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEvent_Interpolated()

  # For both normalised and denormalised data
  totalCostsByYear <- with(totalCosts, aggregate(cbind(interpolatedTotals / 1000000, total / 1000000), by=list(Year), FUN=safeSum))


  # Calculate range from 0 to max value of costs
  x_range <- range(totalCosts$Year)
  y_range <- range(0, totalCostsByYear + 2000)

  ## Graph the results

  # Plot a basic graph of costs
  pdf(file=paste("./figs/", "fig3_1_annual_total_costs_of_disasters_in_australia_interpolated", ".pdf", sep=""))

  # Set an upper y value based on the data passed in
  # Note: this will often be too little
  if (is.null(y_range)) {
    y_range <- range(0, data)
  }
  data <- t(cbind(totalCostsByYear[, 2], totalCostsByYear[, 3]))
  colnames(data) <- totalCostsByYear[, 1]
  rownames(data) <- c("Interpolated", "Normal")

  # Plot normalised data
  barplot(data, beside=T,
          axisnames=T,
          cex=title_size,
          cex.lab=title_size,
          cex.axis=title_size,
          cex.main=title_size,
          cex.sub=title_size,
          cex.names=0.8, las=2, ylim=y_range, col=c("blue","red"))

  # Add title
  title("FIGURE 3.1c: ANNUAL TOTAL COSTS OF DISASTERS IN AUSTRALIA (INT vs NOR), 1967-2013", col.main = "blue",
        cex=title_size,
        cex.lab=title_size,
        cex.axis=title_size,
        cex.main=title_size,
        cex.sub=title_size)

  # Label the x and y axes with dark green text
  title(xlab="Years (financial)", col.lab=rgb(0,0.5,0))
  title(ylab="(2013 Dollars in $millions)", col.lab=rgb(0,0.5,0))

  # Default x axis
  doAxis(1, at=seq(x_range[1], x_range[2], by=1))

  # Make y axis with horizontal labels that display ticks at
  # billions <- 1000000000 * 0:(y_range[2] / 1000000000)
  # doAxis(2, at=billions, labels=format(billions / 1000000, big.mark = ","))

  dev.off()
}


## Generates Figure 3.1 - with both normalised and denormalised data
annual_total_costs_of_disasters_in_australia_denormalised <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEvent()
  # For both normalised and denormalised data
  totalCostsByYear <- with(totalCosts, aggregate(cbind(Reported.Cost.normalised / 1000000, total / 1000000), by=list(Year.financial), FUN=safeSum))


  # Checks total costs
  #write.table(totalCosts, file = "./output/totalCosts.csv", append = FALSE, quote = TRUE, sep = ",",
  #            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
  #            col.names = TRUE, qmethod = c("escape", "double"),
  #            fileEncoding = "")


  # Calculate range from 0 to max value of costs
  x_range <- range(totalCosts$Year.financial)
  y_range <- range(0, totalCostsByYear + 2000)

  ## Graph the results

  # Plot a basic graph of costs
  pdf(file=paste("./figs/", "fig3_1_annual_total_costs_of_disasters_in_australia_denormalised", ".pdf", sep=""))

  # Set an upper y value based on the data passed in
  # Note: this will often be too little
  if (is.null(y_range)) {
    y_range <- range(0, data)
  }
  data <- t(cbind(totalCostsByYear[, 2], totalCostsByYear[, 3]))
  colnames(data) <- totalCostsByYear[, 1]
  rownames(data) <- c("Normalised", "Raw")

    # Plot normalised data
  barplot(data, beside=T,
          axisnames=T,
          cex=title_size,
          cex.lab=title_size,
          cex.axis=title_size,
          cex.main=title_size,
          cex.sub=title_size,
          cex.names=0.8, las=2, ylim=y_range, col=c("blue","red"))

  # Add title
  title("FIGURE 3.1a: ANNUAL TOTAL COSTS OF DISASTERS IN AUSTRALIA, 1967-2013", col.main = "blue",
        cex=title_size,
        cex.lab=title_size,
        cex.axis=title_size,
        cex.main=title_size,
        cex.sub=title_size)

  # Label the x and y axes with dark green text
  title(xlab="Years (financial)", col.lab=rgb(0,0.5,0))
  title(ylab="(2013 Dollars in $millions)", col.lab=rgb(0,0.5,0))

  # Default x axis
  doAxis(1, at=seq(x_range[1], x_range[2], by=1))

  # Make y axis with horizontal labels that display ticks at
  # billions <- 1000000000 * 0:(y_range[2] / 1000000000)
  # doAxis(2, at=billions, labels=format(billions / 1000000, big.mark = ","))

  dev.off()
}



## Generate Figure 3.10a
## STILL EXPERIMENTAL
disaster_costs_by_state_and_territory_compared_with_ndrra <- function() {

  # Taken from http://www.budget.gov.au/2013-14/content/bp3/html/bp3_03_part_2i.htm
  ndrra <- read.table(header = T, text = "NSW  VIC  QLD  WA SA  TAS ACT NT  Total
    2012  84.3  50  1,738.60  3.2 0.2 16.7  0 0.4 1,893.30
    2013  5.9 2.6 136.6 1.2 0.1 0.4 0 0 146.8")

  # Store the total costs by year
  totalCosts <- totalCostForEvent()
  totalCosts$Reported.Cost.normalised.millions <- totalCosts$Reported.Cost.normalised / 1000000
  totalCostsByState <- with(totalCosts, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial, State.abbreviated), FUN=safeSum))

  # Get the last 10 years of data
  data <- totalCostsByState[totalCostsByState$Group.1 > 2002,]

  # Convert to a table
  pivotted_data <- cast(data, Group.1 ~ Group.2)
  write.table(pivotted_data, file = "./output/pivottedStateDataForLast10Years.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")


  o <- order(totalCostsByState[,2], decreasing=TRUE)
  totalCostsByState <- data.frame(cbind(totalCostsByState[,1][o], totalCostsByState[,2][o]))
  states <- totalCostsByState[,1]

  # Remove 'Other' column
  totalCountsByState <- totalCostsByState[!(totalCostsByState$Group.1 %in% c('Other')),]
  totalCostsByState
  ndrra

  # Replace state names with IDs
  totalCostsByState[,1] <- 1:length(totalCostsByState[,1])

  # Calculate range from 0 to max value of costs
  x_range <- range(totalCosts$State.abbreviated)
  y_range <- range(0, as.numeric(totalCostsByState[,2]) + 10000)


  ## Graph the results

  # Plot a basic graph of costs
  pdf(file=paste("./figs/", "fig3_10_disaster_costs_by_state_and_territory_compared_with_ndrra", ".pdf", sep=""))

  # Set an upper y value based on the data passed in
  # Note: this will often be too little
  if (is.null(y_range)) {
    y_range <- range(0, data)
  }
  totalCostsByState
  data <- t(cbind(totalCostsByState[, 2], totalCostsByState[, 3]))
  colnames(data) <- totalCostsByState[, 1]
  rownames(data) <- c("Calculated", "NDRRA")

  # Plot normalised data
  barplot(data, beside=T,
          axisnames=T,
          cex=title_size,
          cex.lab=title_size,
          cex.axis=title_size,
          cex.main=title_size,
          cex.sub=title_size,
          cex.names=0.8, las=2, ylim=y_range, col=c("blue","red"))

  # Add title
  title("FIGURE 3.1a: ANNUAL TOTAL COSTS OF DISASTERS IN AUSTRALIA, 1967-2013", col.main = "blue",
        cex=title_size,
        cex.lab=title_size,
        cex.axis=title_size,
        cex.main=title_size,
        cex.sub=title_size)

  # Label the x and y axes with dark green text
  title(xlab="Years (financial)", col.lab=rgb(0,0.5,0))
  title(ylab="(2013 Dollars in $millions)", col.lab=rgb(0,0.5,0))

  # Default x axis
  doAxis(1, at=seq(x_range[1], x_range[2], by=1))

  # Make y axis with horizontal labels that display ticks at
  # billions <- 1000000000 * 0:(y_range[2] / 1000000000)
  # doAxis(2, at=billions, labels=format(billions / 1000000, big.mark = ","))


  standardBarChart(totalCostsByState,
                   "fig3_10_disaster_costs_by_state_and_territory_compared_with_ndrra",
                   "FIGURE 3.10: DISASTER COSTS BY STATE AND TERRITORY IN AUSTRALIA",
                   "States",
                   "(2013 Dollars in $millions)",
                   y_range,
                   FALSE
  )
  doAxis(1, at=totalCostsByState[,1], labels=states)
  doAxis(2)
  dev.off()
}

