
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
graph.title.size <- 18
axis.title.size <- 16
axis.text.size <- 12
# Set colours
# Quasi-BTE
# background <- '#F0D2AF'
# foreground <- '#D08728'
# text.color <- '#888888'

# Experiment 1
background.color <- '#F6FFC7'
foreground.color <- '#668E39'
title.color <- '#667566'
text.color <- '#202020'

# Experiment 2
# background.color <- '#FFFBE3'
# foreground.color <- '#FF7260'
# title.color <- '#129793'
# text.color <- '#202020'



# Colour-friendly palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# The palette with black:
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
blackPalette <- c("#000000")

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


palette <- function() {
  # Returns the default palette
  return (blackPalette)
}


## Provides a single function for generating bar charts
standardBarChart <- function(data, file.name, title, x.label, y.label, use.years=TRUE) {
  

  # Ensure the order remains the same
  if (use.years==FALSE) {
    data$Group.1 <- factor(data$Group.1, as.character(data$Group.1))  
  }
  # Calculate range from 0 to max value of costs
  p <- ggplot(data, aes(x=Group.1, y = x)) + geom_bar(width=0.5, stat="identity", fill=foreground.color, colour=foreground.color)
  p
  if (use.years==TRUE) {
    x.scale <- scale_x_continuous(name = x.label, breaks = yearBreaks(data$Group.1), labels = yearLabels(data$Group.1))
  } else {
    x.scale <- xlab(x.label)
  }
  p + ggtitle(title) + x.scale + scale_y_continuous(name=y.label, labels=comma) + 
    # scale_fill_gradient(low = "pink", high = "green") + 
    theme(plot.title = element_text(colour = title.color, lineheight=1.0, face="bold", size=graph.title.size),
          panel.grid.minor.y=element_blank(), 
          panel.grid.major.y=element_line(colour = foreground.color),
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.x=element_blank(),
          panel.background = element_rect(fill = background.color, colour = foreground.color),
          axis.title=element_text(color=title.color, lineheight=1.0, size = axis.title.size),
          axis.text.x=element_text(color=text.color, angle=45, vjust=1.0, hjust=1.0, size = axis.text.size),
          axis.text.y=element_text(color=text.color, size = axis.text.size)
      )

  ggsave(file=paste("./figs/", file.name, ".png", sep=""), units = "cm", width = 32, height = 24)
  return (p)
}

## Provides a single function for generating bar charts
standardBarChartClustered <- function(data, file.name, title, x.label, y.label, use.years=TRUE) {
  
  # Set colours
  background <- '#F0D2AF'
  background2 <- '#888888'
  foreground <- '#D08728'
  text.color <- '#888888'
  
  # Ensure the order remains the same
  if (use.years==FALSE) {
    data$Group.1 <- factor(data$Group.1, as.character(data$Group.1))  
  }
  
  # Calculate range from 0 to max value of costs
  clustered.chart <- ggplot(data, aes(x=Group.1, y = value)) + 
        geom_bar(aes(fill=variable), width=0.75,position = "dodge", stat="identity") +  
        scale_fill_manual(name="", values=c(foreground, background2))
  clustered.chart
  if (use.years==TRUE) {
    x.scale <- scale_x_continuous(name=x.label, breaks=yearBreaks(data$Group.1), labels = yearLabels(data$Group.1))
  } else {
    x.scale <- xlab(x.label)
  }
  # Note: title height is 0.9, due to presence of legend on clustered charts
  clustered.chart + ggtitle(title) + x.scale + scale_y_continuous(name=y.label, labels=comma) + 
    theme(plot.title = element_text(colour = foreground.color, lineheight=1.0, face="bold", size=graph.title.size),
          panel.grid.minor.y=element_blank(), 
          panel.grid.major.y=element_line(colour = foreground.color),
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.x=element_blank(),
          panel.background = element_rect(fill = background.color, colour = foreground.color),
          axis.title=element_text(color=text.color, lineheight=1.0, size = axis.title.size),
          axis.text.x=element_text(color=text.color, angle=45, vjust=1.0, hjust=1.0, size = axis.text.size),
          axis.text.y=element_text(color=text.color, size = axis.text.size),
          legend.position="bottom")
  
  ggsave(file=paste("./figs/", file.name, ".png", sep=""), units = "cm", width = 32, height = 24)
}


## Provides a single function for generating pie charts (for 3.12)
standardPieChart <- function(data, file.name, title) {

  # Set colours
  background <- '#F0D2AF'
  background2 <- '#888888'
  foreground <- '#D08728'
  text.color <- '#888888'
  
  # Calculate range from 0 to max value of costs
  p = ggplot(data = data, aes(x = factor(1), y = percentage, fill = factor(Group.2)))
  p = p + geom_bar(width = 1, stat = "identity") 
  p = p + coord_polar(theta="y") 
  p = p + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
  p = p + xlab('') + ylab('') + labs(fill = 'Disaster Type') 
  p = p + ggtitle(title)
  p
  
  ggsave(file=paste("./figs/", file.name, ".png", sep=""), units = "cm", width = 32, height = 24)
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
standardBarChart_Plot <- function(data, file.name, title, x.label, y.label, y.range=NULL, axes=TRUE) {
  
  # Plot a basic graph of costs
  # pdf(file=paste("./figs/", file.name, ".pdf", sep=""))
  png(filename=paste("./figs/", file.name, ".png", sep=""))
  
  # Set an upper y value based on the data passed in
  # Note: this will often be too little
  if (is.null(y.range)) {
    y.range <- range(0, data)
  }
  
  # Calculate range from 0 to max value of costs
  plot(data, type="h", col="blue", ylim=y.range, axes=axes, ann=FALSE,
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
  title(xlab=x.label, col.lab=rgb(0,0.5,0))
  title(ylab=y.label, col.lab=rgb(0,0.5,0))
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
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = FALSE)
  # Just for normalised data
  total.costs.normalised <- total.costs[c("Year.financial", "title", "Reported.Cost.normalised.millions")]
  # Mirror the usual aggregation
  names(total.costs.normalised)[1] = "Year"
  names(total.costs.normalised)[2] = "Group.1"
  names(total.costs.normalised)[3] = "x"
  total.costs.normalised <- total.costs.normalised[order(total.costs.normalised$Year),]
  total.costs.normalised <- total.costs.normalised[order(-total.costs.normalised$x),]
  total.costs.normalised$percentages <- total.costs.normalised$x / sum(total.costs.normalised$x)
  
  write.table(total.costs.normalised, file = "./output/ordered_individual_events.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")

  # Cache variables
  data <- total.costs.normalised
  title <- "FIGURE 3.0: TOTAL COST OF DISASTERS, 1967-2013"
  x.label <- "Years (financial)"
  y.label <- "(2013 Dollars in $millions)"
  # Graph the results
  standardBarChart(total.costs.normalised,
                   "fig3_0_total_costs_of_disasters_in_australia",
                   title,
                   x.label,
                   y.label,
                   FALSE
  )
}

costSummary <- function() {
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)
  total.costs.by.year <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  total.costs.by.year.incl.deaths.and.injuries <- with(total.costs, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(Year.financial), FUN=safeSum))
  
  print("Average annual cost of all disasters [$millions AUD]")
  print(mean(total.costs.by.year$x))
  print("Average annual cost of all disasters (incl. deaths and injuries) [$millions AUD]")
  print(mean(total.costs.by.year.incl.deaths.and.injuries$x))
  
  total.costs.all.years <- sum(total.costs.by.year$x)
  print("Total cost of all disasters [$millions AUD]")
  print(total.costs.all.years)
  total.costs.all.years.incl.deaths.and.injuries <- sum(total.costs.by.year.incl.deaths.and.injuries$x)
  print("Total cost of all disasters (incl. deaths and injuries) [$millions AUD]")
  print(total.costs.all.years.incl.deaths.and.injuries)
  
}

## Generates Figure 3.1
annualTotalCostsOfDisastersInAustralia <- function() {
  # Store the total costs by year
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = FALSE)
  # Just for normalised data
  total.costs.by.year <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  total.costs.by.year <- includeAllYears(total.costs.by.year)
  
  print("Average annual cost of all disasters")
  print(mean(total.costs.by.year$x))
  
  print("Standard deviation of annual cost of all disasters")
  print(sd(total.costs.by.year$x))

  print("Range of annual costs of all disasters")
  print(range(total.costs.by.year$x))

  print("Ordered sequence of annual costs")
  print(total.costs.by.year[order(-total.costs.by.year$x),])
  
  print("Ratio of most expensive year to least expensive year")
  print(total.costs.by.year[order(-total.costs.by.year$x),][1,2] / total.costs.by.year[order(total.costs.by.year$x),][1,2])
  
  total.costs.all.years <- sum(total.costs.by.year$x)
  print("Total cost of all disasters")
  print(total.costs.all.years)
  
  # Top 3
  top3 <- head(total.costs[order(-total.costs$Reported.Cost.normalised.millions),c("title", "Year.financial", "Reported.Cost.normalised.millions")], n = 3)
  top3$percentage <- top3$Reported.Cost.normalised.millions / total.costs.all.years * 100
  print("Top 3 disasters")
  print(top3)
  print(sum(top3$percentage))
  
  # Top 10
  top10 <- head(total.costs[order(-total.costs$Reported.Cost.normalised.millions),c("title", "Year.financial", "Reported.Cost.normalised.millions")], n = 10)
  top10$percentage <- top10$Reported.Cost.normalised.millions / total.costs.all.years * 100
  print("Top 10 disasters")
  print(top10)
  print(sum(top10$percentage))
  
  
  # Exclude 3 biggest years
  # the.rest <- total.costs[!total.costs$title %in% top3$title,c("title", "Year.financial", "Reported.Cost.normalised.millions")]
  the.rest <- tail(total.costs[order(-total.costs$Reported.Cost.WithDeathsAndInjuries.normalised.millions),], n = -3)
  print("Average annual cost of all disasters, excluding top 3")
  the.rest.costs.by.year <- with(the.rest, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  the.rest.costs.by.year <- includeAllYears(the.rest.costs.by.year)
  print(mean(the.rest.costs.by.year$x))
  
  print("Standard deviation of annual cost of all disasters, excluding top 3")
  print(sd(the.rest.costs.by.year$x))
  
  # Run the significance test
  res <- significanceTest_MannKendall(total.costs.by.year)
  print("Significance test for total costs by year")
  print(res)
  summary(res)
  
  # Run the significance test against GDP by comparing indexed values only
  indexed.costs.by.year <- with(total.costs, aggregate(Reported.Cost.indexed.millions, by=list(Year.financial), FUN=safeSum))
  indexed.costs.by.year <- includeAllYears(indexed.costs.by.year)
  indexed.costs.by.year$x <- indexed.costs.by.year$x / sapply(indexed.costs.by.year$Group.1, gdpValues)
  resMK <- significanceTest_MannKendall(indexed.costs.by.year)
  print("Significance test for ratio of costs to GDP")
  print(resMK)
  summary(resMK)

  resLR <- significanceTest_LinearRegression(indexed.costs.by.year)
  print("Significance test (regression) for ratio of costs to GDP")
  print(resLR)
  summary(resLR)
  
  # PRINT GRAPHS
  
  # Cache variables
  data <- total.costs.by.year
  title <- "FIGURE 3.1: ANNUAL TOTAL COST OF DISASTERS, 1967-2013"
  x.label <- "Years (financial)"
  y.label <- "(2013 Dollars in $millions)"
  
  # Graph the results
  standardBarChart(total.costs.by.year,
                   "fig3_1_annual_total_costs_of_disasters_in_australia",
                   title,
                   x.label,
                   y.label
  )

  total.costs.by.year.with.deaths.injuries <- with(total.costs, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(Year.financial), FUN=safeSum))
  merged.costs.by.year <- merge(total.costs.by.year, total.costs.by.year.with.deaths.injuries, by = "Group.1")
  names(merged.costs.by.year)[2] <- paste("Excl. deaths and injuries")
  names(merged.costs.by.year)[3] <- paste("Incl. deaths and injuries")
  melted.merged.costs <- melt(merged.costs.by.year, id.var = "Group.1")
  standardBarChartClustered(melted.merged.costs,
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
	total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = FALSE)

	# Filter by decade
	decades <- unique(floor(total.costs$Year.financial / 10)) * 10

  # Aggregate normalised costs by decade
	total.costs.by.decade <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

  # Multiply decades back up to years
	total.costs.by.decade[,1] <- total.costs.by.decade[,1] * 10

	standardBarChart(total.costs.by.decade,
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
	total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = FALSE)
	averageCostPerYear <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeMean))

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
	total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = FALSE)

	# Filter by cost bracket
	cost_brackets <- list(10000000, 50000000, 100000000, 150000000, 500000000)

	total.costs$Reported.Cost.normalised.code <- apply(data.matrix(total.costs$Reported.Cost.normalised.millions), 1, codeCosts)
	totalCostDistribution <- with(total.costs, aggregate(Reported.Cost.normalised, by=list(Reported.Cost.normalised.code), FUN=length))
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
	total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = FALSE)
	insuranceCostsByYear <- with(total.costs, aggregate(Insured.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))

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
	total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = FALSE)
	numberByYear <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=length))
	# numberByYear <- with(total.costs, aggregate(Insured.Cost.normalised.millions, by=list(Year.financial), FUN=length))

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
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = FALSE)
  numberByYear <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=length))
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
  text.color <- '#888888'
  
  title <- "FIGURE 3.9  NUMBER OF DISASTERS PER MILLION PEOPLE, 1967-2013"
  x.label <- "Years (financial)"
  y.label <- "Number of disasters per million people"
  
  # Calculate range from 0 to max value of costs
  p <- ggplot(mergedCounts, aes(x=Group.1, y = popRatios)) + geom_point(colour = foreground.color, size = 4) +
    geom_smooth(method="lm", fill=NA, colour = "#000000")
  p + ggtitle(title) + scale_x_continuous(name=x.label, breaks=yearBreaks(mergedCounts$Group.1)) + scale_y_continuous(name=y.label, labels=comma) + 
    theme(plot.title = element_text(colour = foreground.color, lineheight=.8, face="bold"),
          panel.grid.minor.y=element_blank(), 
          panel.grid.major.y=element_line(colour = foreground.color),
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.x=element_blank(),
          panel.background = element_rect(fill = background.color, colour = foreground.color),
          axis.title=element_text(color=title.color),
          axis.text.x=element_text(color=text.color, size = axis.text.size, angle=45, vjust=1.0, hjust=1.0),
          axis.text.y=element_text(color=text.color, size = axis.text.size))
  
  ggsave(file=paste("./figs/fig3_9_number_of_disasters_per_million_people.png", sep=""), units = "cm", width = 32, height = 24)
  
  # Show regression
  print("Regression fit for population")
  summary(fit <- lm(formula = popRatios ~ Group.1, data = mergedCounts))
  
  # Store the total costs by year
  total.costs.without.heatwaves <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = TRUE)
  numberByYear.without.heatwaves <- with(total.costs.without.heatwaves, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=length))
  mergedCounts.without.heatwaves <- merge(popCounts, numberByYear, by = "Group.1", all.x = TRUE)
  names(mergedCounts.without.heatwaves)[2] = "pop"
  names(mergedCounts.without.heatwaves)[3] = "disasterCount"
  mergedCounts.without.heatwaves$popRatios <- mergedCounts.without.heatwaves$disasterCount / (mergedCounts.without.heatwaves$pop / 1000000)
  
  print("Number of events including heatwaves")
  print(length(total.costs$Year.financial))
  
  print("Number of events without heatwaves")
  print(length(total.costs.without.heatwaves$Year.financial))
  
  print("Regression fit for population without heatwaves")
  summary(fit <- lm(formula = popRatios ~ Group.1, data = mergedCounts.without.heatwaves))
}


## Generate Figure 3.10
disasterCostsByStateAndTerritory <- function() {
	# Store the total costs by year
	total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = FALSE)
	total.costs$Reported.Cost.normalised.millions.state.1 <- total.costs$Reported.Cost.normalised.millions * total.costs$State.1.percent
	total.costs$Reported.Cost.normalised.millions.state.2 <- total.costs$Reported.Cost.normalised.millions * total.costs$State.2.percent
  total.costs.by.state1 <- with(total.costs, aggregate(Reported.Cost.normalised.millions.state.1, by=list(State.abbreviated.1), FUN=safeSum))
  total.costs.by.state2 <- with(total.costs, aggregate(Reported.Cost.normalised.millions.state.2, by=list(State.abbreviated.2), FUN=safeSum))
  total.costs.by.state <- merge(total.costs.by.state1, total.costs.by.state2, by="Group.1", all.x = TRUE )
	total.costs.by.state$x <- rowSums(cbind(total.costs.by.state$x.x, total.costs.by.state$x.y), na.rm = TRUE)
	total.costs.by.state <- total.costs.by.state[with(total.costs.by.state, order(-x)), ]
	
	# Cache variables
	data <- total.costs.by.state
	x.label <- "States"
	y.label <- "(2013 Dollars in $millions)"
	title <- "FIGURE 3.10: DISASTER COSTS BY STATE AND TERRITORY"

  standardBarChart(total.costs.by.state,
		"fig3_10_disaster_costs_by_state_and_territory",
		"FIGURE 3.10: DISASTER COSTS BY STATE AND TERRITORY",
		"States",
		"(2013 Dollars in $millions)",
    FALSE
		)
  
  # Generate percentages
	total.costs$Insured.Cost.normalised.millions.state.1 <- total.costs$Insured.Cost.normalised.millions * total.costs$State.1.percent
	total.costs$Insured.Cost.normalised.millions.state.2 <- total.costs$Insured.Cost.normalised.millions * total.costs$State.2.percent
	insured.costs.by.state1 <- with(total.costs, aggregate(Insured.Cost.normalised.millions.state.1, by=list(State.abbreviated.1), FUN=safeSum))
	insured.costs.by.state2 <- with(total.costs, aggregate(Insured.Cost.normalised.millions.state.1, by=list(State.abbreviated.2), FUN=safeSum))
	insured.costs.by.state <- merge(insured.costs.by.state1, insured.costs.by.state2, by="Group.1", all.x = TRUE )
	insured.costs.by.state$x <- rowSums(cbind(insured.costs.by.state$x.x, insured.costs.by.state$x.y), na.rm = TRUE)
	insured.costs.by.state <- insured.costs.by.state[with(insured.costs.by.state, order(-x)), ]
  merged.costs <- merge(total.costs.by.state, insured.costs.by.state, by="Group.1", all.x = TRUE)
	merged.costs$total.costsPercentages <- data.frame(merged.costs$x.x / sum(merged.costs$x.x))
	merged.costs$insuredCostsPercentages <- data.frame(merged.costs$x.y / sum(merged.costs$x.y))
  print("Comparing total and insurable costs")
	print(merged.costs)
	# Test correlation between total and insured costs
	print(cor.test( merged.costs$x.x, merged.costs$x.y, method = "pearson"))
	
	print("Percentage of event costs by state")
  orderedCosts <- merged.costs[,c("Group.1", "total.costsPercentages")]
  names(orderedCosts)
	orderedCosts <- orderedCosts[order(-orderedCosts$total.costsPercentages),]
	print(merged.costs[,c("Group.1", "total.costsPercentages")])
  
  print("Top 2 percentages")
	print(sum(head(merged.costs[order(-merged.costs$total.costsPercentages),]$total.costsPercentages, n = 2)))
	
	print("Top 3 percentages")
	print(sum(head(merged.costs[order(-merged.costs$total.costsPercentages),]$total.costsPercentages, n = 3)))
	
	print("Combined percentage of top 3 events")
	print(sum(head(merged.costs[order(-merged.costs$total.costsPercentages),]$total.costsPercentages, 3)))
	  
  print("Percentage of Population in Queensland, New South Wales and Victoria")
  # Taken from ABS June 2014  - http://www.abs.gov.au/ausstats/abs@.nsf/mf/3101.0
  print((7518.5 + 5841.7 + 4722.4) / 23490.7)
  
  # Count number of "raw" (non-interpolated) reported costs
	print("Percentage of 'raw' to 'interpolated' reported costs")
  print(length(total.costs$Reported.Cost[total.costs$Reported.Cost > 0]) / 
          length(total.costs$Reported.Cost.interpolated[total.costs$Reported.Cost.interpolated > 0]))
  
  # Compare just those events that have both insured and reported costs - JUST FOR State.1
	bothCostsByState <- with(total.costs[total.costs$Reported.Cost > 0 & total.costs$Insured.Cost > 0,], aggregate(data.frame(Reported.Cost,  Insured.Cost), by=list(State.abbreviated.1), FUN=safeSum))
	print(cor.test( bothCostsByState$Reported.Cost, bothCostsByState$Insured.Cost, method = "pearson"))
	
	# Show major events by state
	sortedByStateAndCost <- total.costs[with(total.costs,order(State.abbreviated.1, -Reported.Cost.normalised.millions)),]
	sortedByStateAndCost <- with(sortedByStateAndCost, data.frame(State.abbreviated.1, title, Year, Year.financial, Reported.Cost.normalised.millions))
	write.table(sortedByStateAndCost, file = "./output/ordered_events_by_state.csv", append = FALSE, quote = TRUE, sep = ",",
	            eol = "\n", na = "", dec = ".", row.names = FALSE,
	            col.names = TRUE, qmethod = c("escape", "double"),
	            fileEncoding = "")
	
	# Show major events by state and type
	total.costs.by.stateAndType <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(State.abbreviated.1, resourceType), FUN=safeSum))
	total.costs.by.stateAndType <- total.costs.by.stateAndType[with(total.costs.by.stateAndType, order(Group.1, Group.2)), ]
  print("Events aggregated by state and event type")
  print(total.costs.by.stateAndType)
	print("Distinct event types")
	print(data.frame(unique(total.costs$resourceType)))
  
}


## Generate Figure 3.11
numberOfDisasterEventsByStateAndTerritory <- function() {
  # Store the total costs by year
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = FALSE)

  total.costs$Reported.Cost.normalised.millions.state.1 <- total.costs$Reported.Cost.normalised.millions * total.costs$State.1.percent
  total.costs$Reported.Cost.normalised.millions.state.2 <- total.costs$Reported.Cost.normalised.millions * total.costs$State.2.percent
  totalCountsByState1 <- with(total.costs, aggregate(Reported.Cost.normalised.millions.state.1, by=list(State.abbreviated.1), FUN=length))
  totalCountsByState2 <- with(total.costs, aggregate(Reported.Cost.normalised.millions.state.2, by=list(State.abbreviated.2), FUN=length))
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
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = FALSE)
  total.costs$Reported.Cost.normalised.millions.state.1 <- total.costs$Reported.Cost.normalised.millions * total.costs$State.1.percent
  total.costs$Reported.Cost.normalised.millions.state.2 <- total.costs$Reported.Cost.normalised.millions * total.costs$State.2.percent
  total.costs.by.state1 <- with(total.costs, aggregate(Reported.Cost.normalised.millions.state.1, by=list(State.abbreviated.1, resourceType), FUN=safeSum))
  total.costs.by.state2 <- with(total.costs, aggregate(Reported.Cost.normalised.millions.state.2, by=list(State.abbreviated.2, resourceType), FUN=safeSum))
  total.costs.by.state.and.disaster.type <- merge(total.costs.by.state1, total.costs.by.state2, by=c("Group.1", "Group.2"), all.x = TRUE )
  total.costs.by.state.and.disaster.type$x <- rowSums(cbind(total.costs.by.state.and.disaster.type$x.x, total.costs.by.state.and.disaster.type$x.y), na.rm = TRUE)
  # total.costs.by.state.and.disaster.type <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(State.abbreviated.1, resourceType), FUN=safeSum))
  total.costs.by.state.and.disaster.type$x <- round(total.costs.by.state.and.disaster.type$x)
  state.totals <- aggregate(x ~ Group.1, data=total.costs.by.state.and.disaster.type, sum, na.rm=TRUE)
  names(state.totals)[2] = "Total"
  totals.with.state.aggregates <- merge(total.costs.by.state.and.disaster.type, state.totals, by = "Group.1", all.x = TRUE)
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

  base_file.name <- "fig3_12_costs_by_type_of_disaster_and_state_and_territory"
  standardPieChart(act, paste(base_file.name, "_", "act", sep = ''), "Losses (ACT)")
  standardPieChart(nsw, paste(base_file.name, "_", "nsw", sep = ''), "Losses (NSW)")
  standardPieChart(nt, paste(base_file.name, "_", "nt", sep = ''), "Losses (NT)")
  standardPieChart(qld, paste(base_file.name, "_", "qld", sep = ''), "Losses (QLD)")
  standardPieChart(sa, paste(base_file.name, "_", "sa", sep = ''), "Losses (SA)")
  standardPieChart(tas, paste(base_file.name, "_", "tas", sep = ''), "Losses (TAS)")
  standardPieChart(vic, paste(base_file.name, "_", "vic", sep = ''), "Losses (VIC)")
  standardPieChart(wa, paste(base_file.name, "_", "wa", sep = ''), "Losses (WA)")

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
    
  ggsave(file=paste("./figs/", "fig3_12_costs_by_type_of_disaster_and_state_and_territory", ".png", sep=""), units = "cm", width = 32, height = 24)  
  
  # Stacked bar chart version
  p = ggplot(data = totals.with.state.aggregates, aes(x = Group.1, y = x, fill = factor(Group.2)))
  p = p + geom_bar(width = 0.5, stat = "identity") 
  p = p + facet_wrap(~ total.neg, ncol=2)
  p = p + coord_polar(theta="y") 
  p = p + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
  p = p + xlab('States') + ylab('(2013 Dollars in $millions)') + labs(fill = 'Disaster Type') 
  p
  ggsave(file=paste("./figs/", "fig3_12_costs_by_type_of_disaster_and_state_and_territory_stacked", ".png", sep=""), units = "cm", width = 32, height = 24)  
  
  p = ggplot(data = totals.with.state.aggregates, aes(x = Group.1, y = percentage, fill = factor(Group.2)))
  p = p + geom_bar(width = 0.5, stat = "identity") 
  p = p + xlab('States') + ylab('(2013 Dollars in $millions)') + labs(fill = 'Disaster Type') 
  p
  ggsave(file=paste("./figs/", "fig3_12_costs_by_type_of_disaster_and_state_and_territory_stacked_percent", ".png", sep=""), units = "cm", width = 32, height = 24)  
}


## Generate Figure 3.13
totalAndInsuranceCostsByDisasterType <- function() {
  # Store the total costs by year
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = FALSE)
  total.costs.by.disaster.type <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(resourceType), FUN=safeSum))
  total.costs.by.disaster.type <- total.costs.by.disaster.type[with(total.costs.by.disaster.type, order(-x)), ]
  insuredCostsByDisasterType <- with(total.costs, aggregate(Insured.Cost.normalised.millions, by=list(resourceType), FUN=safeSum))
  insuredCostsByDisasterType <- insuredCostsByDisasterType[with(insuredCostsByDisasterType, order(-x)), ]
  merged.costs <- merge(total.costs.by.disaster.type, insuredCostsByDisasterType, by="Group.1", all.x = TRUE)
  names(merged.costs)[2] <- paste("Reported Cost")
  names(merged.costs)[3] <- paste("Insured Cost")
  melted.merged.costs <- melt(merged.costs, id.var = "Group.1")
  melted.merged.costs <- melted.merged.costs[with(melted.merged.costs, order(-value)), ]
  
  standardBarChartClustered(melted.merged.costs,
		"fig3_13_total_and_insurance_costs_by_disaster_type",
		"FIGURE 3.13: TOTAL AND INSURANCE COSTS BY DISASTER TYPE, 1967-2013",
		"Disaster Type",
		"(2013 Dollars in $millions)",
    FALSE
		)
  
  # Get percentages
  total.costs.by.disaster.type$percentages <- data.frame(total.costs.by.disaster.type$x / (sum(total.costs.by.disaster.type$x))) 
  print("Percentages of disaster type")
  print(total.costs.by.disaster.type)
  
  print("Combined percentage of top 3 events")
  print(sum(head(total.costs.by.disaster.type[order(-total.costs.by.disaster.type$percentages),]$percentages, 3)))
  
  # Test for the inclusion of deaths and injuries
  total.costs.by.disaster.type.WithDeathsAndInjuries <- with(total.costs, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(resourceType), FUN=safeSum))
  total.costs.by.disaster.type.WithDeathsAndInjuries <- total.costs.by.disaster.type.WithDeathsAndInjuries[with(total.costs.by.disaster.type.WithDeathsAndInjuries, order(-x)), ]
  total.costs.by.disaster.type.WithDeathsAndInjuries$percentages <- data.frame(total.costs.by.disaster.type.WithDeathsAndInjuries$x / (sum(total.costs.by.disaster.type.WithDeathsAndInjuries$x))) 
  print("Combined percentage of top 3 events with deaths and injuries included")
  print(sum(head(total.costs.by.disaster.type.WithDeathsAndInjuries[order(-total.costs.by.disaster.type.WithDeathsAndInjuries$percentages),]$percentages, 3)))


  # Show major events by disaster type
  sortedByTypeAndCost <- total.costs[with(total.costs,order(resourceType, -Reported.Cost.normalised.millions)),]
  sortedByTypeAndCost <- with(sortedByTypeAndCost, data.frame(resourceType, title, Year, Year.financial, Reported.Cost.normalised.millions))
  write.table(sortedByTypeAndCost, file = "./output/ordered_events_by_type.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
}


## Generate Figure 3.14
numberOfEventsByDisasterType <- function() {
  # Store the total costs by year
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = FALSE)
  # Exclude events without a reported cost
  total.costs <- total.costs[total.costs$Reported.Cost.normalised.millions > 0,]
  totalCountsByDisasterType <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(resourceType), FUN=length))
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
  totalCountsByDisasterType.WithDeathsAndInjuries <- with(total.costs, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(resourceType), FUN=length))
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
	total.costs <- totalCostForEventFiltered(resource.type.param = "Flood", reported.costs.only = TRUE, no.heatwaves = FALSE)
	total.costs.by.year <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  total.costs.by.year <- includeAllYears(total.costs.by.year)

	# Graph the results
	standardBarChart(total.costs.by.year,
		"fig3_15_annual_cost_of_floods_in_australia",
		"FIGURE 3.15: ANNUAL TOTAL COSTS OF FLOODS, 1967-2013",
		"Years (financial)",
		"(2013 Dollars in $millions)"
		)
	
  # Show key statistics
	top10Floods <- head(total.costs.by.year[order(-total.costs.by.year$x),], 10)
  print("Top 10 Floods Years by Cost")
	print(top10Floods)
	
	print("Total annual cost")
	print(sum(total.costs.by.year$x))
	
	print("Average annual cost")
	print(mean(total.costs.by.year$x))
	
	# Run the significance test
	res <- significanceTest_MannKendall(total.costs.by.year)
	print("Significance test for cost of floods by year")
	print(res)
	summary(res)
	
}


## Generate Figure 3.16
totalCostOfFloodsByDecade <- function() {
		# Store the total costs by year
	total.costs <- totalCostForEventFiltered(resource.type.param = "Flood", reported.costs.only = TRUE, no.heatwaves = FALSE)

	# Filter by decade
	decades <- unique(floor(total.costs$Year.financial / 10)) * 10
	total.costs.by.decade <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	total.costs.by.decade[,1] <- total.costs.by.decade[,1] * 10

	standardBarChart(total.costs.by.decade,
		"fig3_16_australian_flood_costs_by_decade",
		"FIGURE 3.16: AUSTRALIAN FLOOD COSTS BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
    FALSE
		)
	
	print("Average decade cost since 1970")
	mean(total.costs.by.decade[seq(2,6),]$x)
	
	print("Min and max decade cost since 1970")
	range(total.costs.by.decade[seq(2,6),]$x)
}


## Generate Figure 3.17
annualNumberOfFloodsInAustralia <- function() {
	# Store the total costs by year
	total.costs <- totalCostForEventFiltered(resource.type.param = "Flood", reported.costs.only = TRUE, no.heatwaves = FALSE)
	numberByYear <- with(total.costs, aggregate(resourceType, by=list(Year.financial), FUN=length))
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
	total.costs.by.year <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  merged.costsCounts <- merge(total.costs.by.year, numberByYear, by="Group.1", all.x = TRUE)
  # Set average to 'x' for the results summary
	merged.costsCounts$x <- merged.costsCounts$x.x / merged.costsCounts$x.y
  res <- significanceTest_MannKendall(merged.costsCounts)
	print("Significance test for average cost of floods by year")
	print(res)
	summary(res)
  # plot(merged.costsCounts[,c(1,4)])
	
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
	total.costs <- totalCostForEventFiltered(resource.type.param = "Severe Storm", reported.costs.only =  TRUE, no.heatwaves = FALSE)
	total.costs.by.year <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  total.costs.by.year <- includeAllYears(total.costs.by.year)

	# Graph the results
	standardBarChart(total.costs.by.year,
		"fig3_18_annual_cost_of_severe_storms_in_australia",
		"FIGURE 3.18: ANNUAL TOTAL COSTS OF SEVERE STORMS, 1967-2013",
		"Years (financial)",
		"(2013 Dollars in $millions)"
		)
	
	# Show key statistics
	top10Storms <- head(total.costs.by.year[order(-total.costs.by.year$x),], 10)
	print("Top 10 Storm Years by Cost")
	print(top10Storms)
	
	print("Average annual cost")
	print(mean(total.costs.by.year$x))
	
	print("Total cost")
	print(sum(total.costs.by.year$x))
	
	# Run the significance test
	res <- significanceTest_MannKendall(total.costs.by.year)
	print("Significance test for cost of floods by year")
	print(res)
	summary(res)
}


## Generate Figure 3.19
totalCostOfSevereStormsByDecade <- function() {
	# Store the total costs by year
	total.costs <- totalCostForEventFiltered(resource.type.param = "Severe Storm", reported.costs.only =  TRUE, no.heatwaves = FALSE)
	
	# Filter by decade
	decades <- unique(floor(total.costs$Year.financial / 10)) * 10
	total.costs.by.decade <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	total.costs.by.decade[,1] <- total.costs.by.decade[,1] * 10

	standardBarChart(total.costs.by.decade,
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
	total.costs <- totalCostForEventFiltered(resource.type.param = "Severe Storm", reported.costs.only =  TRUE, no.heatwaves = FALSE)
	numberByYear <- with(total.costs, aggregate(resourceType, by=list(Year.financial), FUN=length))
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
	total.costs <- totalCostForEventFiltered(resource.type.param = "Cyclone", reported.costs.only = TRUE, no.heatwaves = FALSE)
	total.costs.by.year <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
	total.costs.by.year <- includeAllYears(total.costs.by.year)
		
  # Graph the results
	standardBarChart(total.costs.by.year,
		"fig3_21_annual_cost_of_cyclone_in_australia",
		"FIGURE 3.21: ANNUAL TOTAL COSTS OF CYCLONES, 1967-2013",
		"Years (financial)",
		"(2013 Dollars in $millions)"
		)
  
	
	# Show key statistics
	top10Cyclones <- head(total.costs.by.year[order(-total.costs.by.year$x),], 10)
	print("Top 10 Cyclones Years by Cost")
	print(top10Cyclones)
	
	print("Average annual cost of cyclones")
	print(sum(total.costs.by.year$x) / (2013 - 1966))
	print("Average annual cost of cyclones since 1980")
	print(sum(total.costs.by.year[total.costs.by.year$Group.1 > 1980,]$x) / (2013 - 1980))
	print("Average annual cost of cyclones since 1999")
	print(sum(total.costs.by.year[total.costs.by.year$Group.1 > 1999,]$x) / (2013 - 1999))
	print("Average annual cost of cyclones since 2005")
	print(sum(total.costs.by.year[total.costs.by.year$Group.1 > 2005,]$x) / (2013 - 2005))
	
	print("Total cost of cyclones")
	print(sum(total.costs.by.year$x))
	
	# Run the significance test
	res <- significanceTest_MannKendall(total.costs.by.year)
	print("Significance test for cost of cyclones by year")
	print(res)
	summary(res)  
}


## Generate Figure 3.22
totalCostOfCyclonesByDecade <- function() {
	# Store the total costs by year
	total.costs <- totalCostForEventFiltered(resource.type.param = "Cyclone", reported.costs.only = TRUE, no.heatwaves = FALSE)

	# Filter by decade
	decades <- unique(floor(total.costs$Year.financial / 10)) * 10
	total.costs.by.decade <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	total.costs.by.decade[,1] <- total.costs.by.decade[,1] * 10

	standardBarChart(total.costs.by.decade,
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
	total.costs <- totalCostForEventFiltered(resource.type.param = "Cyclone", reported.costs.only = TRUE, no.heatwaves = FALSE)
	numberByYear <- with(total.costs, aggregate(resourceType, by=list(Year.financial), FUN=length))
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
	total.costs <- totalCostForEventFiltered(resource.type.param = "Earthquake", reported.costs.only = TRUE, no.heatwaves = FALSE)

	# Filter by decade
	decades <- unique(floor(total.costs$Year.financial / 10)) * 10
	total.costs.by.decade <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))
	# Merge with a sequence, to ensure years with zero events are represented
	allDecades <- data.frame(seq(196, 201))
	names(allDecades)[1] <- "Group.1"
	total.costs.by.decade <- merge(allDecades, total.costs.by.decade, by = "Group.1", all.x = TRUE)
	if (length(total.costs.by.decade[is.na(total.costs.by.decade$x),]$x) > 0) {
	  total.costs.by.decade[is.na(total.costs.by.decade$x),]$x <- 0
	}
  
	# Multiply decades back up to 000's
	total.costs.by.decade[,1] <- total.costs.by.decade[,1] * 10

	standardBarChart(total.costs.by.decade,
		"fig3_24_australian_earthquake_costs_by_decade",
		"FIGURE 3.24: AUSTRALIAN EARTHQUAKE COSTS BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
    FALSE
		)
	
	print("Average decadal cost of earthquakes")
	print(sum(total.costs.by.decade$x) / 6)

	print("Total cost of earthquakes")
	print(sum(total.costs.by.decade$x))
}


## Generate Figure 3.25
annualCostOfBushfiresInAustralia <- function() {
	# Store the total costs by year
	total.costs <- totalCostForEventFiltered(resource.type.param = "Bushfire", reported.costs.only = TRUE, no.heatwaves = FALSE)
	total.costs.by.year <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
	total.costs.by.year <- includeAllYears(total.costs.by.year)
	
	# Graph the results
	standardBarChart(total.costs.by.year,
		"fig3_25_annual_cost_of_bushfire_in_australia",
		"FIGURE 3.25: ANNUAL TOTAL COSTS OF BUSHFIRES, 1967-2013",
		"Years (financial)",
		"(2013 Dollars in $millions)"
		)
	
	
	# Show key statistics
	top10Fires <- head(total.costs.by.year[order(-total.costs.by.year$x),], 10)
	print("Top 10 Fires Years by Cost")
	print(top10Fires)
	
	print("Average annual cost of fires")
	print(mean(total.costs.by.year$x))
	
	print("Total cost of fires")
	print(sum(total.costs.by.year$x))
}


## Generate Figure 3.26
totalCostOfBushfiresByDecade <- function() {
	# Store the total costs by year
	total.costs <- totalCostForEventFiltered(resource.type.param = "Bushfire", reported.costs.only = TRUE, no.heatwaves = FALSE)
	
	# Filter by decade
	decades <- unique(floor(total.costs$Year.financial / 10)) * 10
	total.costs.by.decade <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	total.costs.by.decade[,1] <- total.costs.by.decade[,1] * 10

	standardBarChart(total.costs.by.decade,
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
	total.costs <- totalCostForEventFiltered(resource.type.param = "Bushfire", reported.costs.only = TRUE, no.heatwaves = FALSE)
	numberByYear <- with(total.costs, aggregate(resourceType, by=list(Year.financial), FUN=length))
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
	total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)
	numberByYear <- with(total.costs, aggregate(Deaths.normalised, by=list(Year.financial), FUN=sum))
  numberByYear <- includeAllYears(numberByYear)
	numberByYearDenormalised <- with(total.costs, aggregate(Deaths, by=list(Year.financial), FUN=sum))
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
  top10Deaths <- head(total.costs[order(-total.costs$Deaths.normalised),c("Year.financial", "Year", "title", "Deaths.normalised")], 10)
  top10DeathsDenormalised <- head(total.costs[order(-total.costs$Deaths),c("Year.financial", "Year", "title", "Deaths")], 10)
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


	numberByDenormalised <- with(total.costs, aggregate(Deaths, by=list(State.1), FUN=sum))
	numberByDenormalised <- numberByDenormalised[order(-numberByDenormalised$x),]
	print("Total number of deaths by state")
	print(numberByDenormalised)
}


## Generate Figure 3.29
numberOfNaturalDisastersInjuries <- function() {
	# Store the total costs by year
	total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)
	numberByYear <- with(total.costs, aggregate(Injuries.normalised, by=list(Year.financial), FUN=sum))
	numberByYear <- includeAllYears(numberByYear)
  numberByYearDenormalised <- with(total.costs, aggregate(Injuries, by=list(Year.financial), FUN=sum))
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
	total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)
	
	# Filter by decade
	decades <- unique(floor(total.costs$Year.financial / 10)) * 10
  numberOfDeathsByDecade <- with(total.costs, aggregate(Deaths.normalised, by=list(floor(Year.financial / 10)), FUN=safeSum))
	numberOfDeathsByDecadeDenormalised <- with(total.costs, aggregate(Deaths, by=list(floor(Year.financial / 10)), FUN=safeSum))

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
	
  if (includeHeatwaves()) {
    total.costs <- totalCostForEventFiltered(resource.type.param = "Heatwave", reported.costs.only = FALSE, no.heatwaves = FALSE)
    decades <- unique(floor(total.costs$Year.financial / 10)) * 10
    numberOfDeathsByDecade <- with(total.costs, aggregate(Deaths.normalised, by=list(floor(Year.financial / 10)), FUN=safeSum))
    numberOfDeathsByDecadeDenormalised <- with(total.costs, aggregate(Deaths.normalised, by=list(floor(Year.financial / 10)), FUN=safeSum))
    # Multiply decades back up to 000's
    numberOfDeathsByDecade[,1] <- numberOfDeathsByDecade[,1] * 10
    print("Number of deaths by decade for heatwaves (normalised)")
    print(numberOfDeathsByDecade)
    print("Number of deaths by decade for heatwaves")
    print(numberOfDeathsByDecadeDenormalised)
  }
}


## Generate Figure 3.31
costOfDeathsAndInjuries <- function() {
	# Store the total costs by year
	total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)
	total.costs.by.year <- with(total.costs, aggregate(deathAndInjuryCosts.normalised.millions, by=list(Year.financial), FUN=safeSum))
	total.costs.by.year <- includeAllYears(total.costs.by.year)
  
	# Graph the results
	standardBarChart(total.costs.by.year,
		"fig3_31_cost_of_deaths_and_injuries",
		"FIGURE 3.31: COST OF DEATH AND INJURIES BY NATURAL DISASTERS, 1967-2013",
		"Years (financial)",
		"(2013 Dollars in $millions)"
		)
  	
	print("Average annual cost of deaths and injuries")
	print(mean(total.costs.by.year$x))
	
	print("Total cost of deaths and injuries")
	print(sum(total.costs.by.year$x))
  
	total.costsOfDeathsAndInjuriesByDisasterType <- with(total.costs, aggregate(deathAndInjuryCosts.normalised.millions, by=list(resourceType), FUN=safeSum))
	total.costsOfDeathsAndInjuriesByDisasterType <- total.costsOfDeathsAndInjuriesByDisasterType[order(-total.costsOfDeathsAndInjuriesByDisasterType$x),]
	total.costsOfDeathsAndInjuriesByDisasterType$percentages <- total.costsOfDeathsAndInjuriesByDisasterType$x / sum(total.costsOfDeathsAndInjuriesByDisasterType$x)
	print("Total cost of deaths and injuries by disaster type")
	print(total.costsOfDeathsAndInjuriesByDisasterType)
}


## Generate Figure 3.32
costOfDeathsAndInjuriesByDecade <- function() {
	# Store the total costs by year
	total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)

	# Filter by decade
	decades <- unique(floor(total.costs$Year.financial / 10)) * 10
	total.costs.by.decade <- with(total.costs, aggregate(deathAndInjuryCosts.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	total.costs.by.decade[,1] <- total.costs.by.decade[,1] * 10

	standardBarChart(total.costs.by.decade,
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
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)
  total.costs.by.year <- with(total.costs, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(Year.financial), FUN=safeSum))
  
  # Graph the results
  standardBarChart(total.costs.by.year,
                   "fig3_33_total_cost_of_natural_disasters",
                   "FIGURE 3.33: TOTAL COSTS OF DISASTERS, 1967-2013",
                   "Years (financial)",
                   "(2013 Dollars in $millions)"
  )
  
  # Show key statistics
  top10Events <- head(total.costs[order(-total.costs$Reported.Cost.WithDeathsAndInjuries.normalised.millions),c("Year.financial", "Year", "title", "Reported.Cost.WithDeathsAndInjuries.normalised.millions")], 10)
  print("Top 10 Events by Cost (incl. deaths and injuries)")
  print(top10Events)
  
  print("Average annual cost of all disasters (incl. deaths and injuries)")
  print(mean(total.costs.by.year$x))
  
  print("Standard deviation of annual cost of all disasters (incl. deaths and injuries)")
  print(sd(total.costs.by.year$x))
  
  print("Total cost of all disasters (incl. deaths and injuries)")
  print(sum(total.costs.by.year$x))  
  
  # Australia's population as reported by ABS June 2013
  print("Average cost of all disasters per person")
  print(1000000 * sum(total.costs.by.year$x) / 23135281)
  
  print("Total cost of all disasters (incl. deaths and injuries)")
  print(sum(total.costs.by.year$x))  
  
  # Top and bottom 3 years
  top3Years <- head(total.costs.by.year[order(-total.costs.by.year$x),], 3)
  print("Top 3 years by Cost (incl. deaths and injuries)")
  print(top3Years)
  
  bottom3Years <- head(total.costs.by.year[order(total.costs.by.year$x),], 3)
  print("Bottom 3 years by Cost (incl. deaths and injuries)")
  print(bottom3Years)
  
  print("Highest divided by the smallest")
  print(top3Years[1,2] / bottom3Years[1,2])
  
  # Exclude 3 biggest years
  total.costsTop3 <- head(total.costs[order(-total.costs$Reported.Cost.WithDeathsAndInjuries.normalised.millions),], n = 3)
  total.costsMinusTop3 <- tail(total.costs[order(-total.costs$Reported.Cost.WithDeathsAndInjuries.normalised.millions),], n = -3)
  total.costs.by.yearMinusTop3 <- with(total.costsMinusTop3, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(Year.financial), FUN=safeSum))
  print("Average annual cost of all disasters (incl. deaths and injuries) MINUS TOP 3")
  print(mean(total.costs.by.yearMinusTop3$x))
  
  # Test
  resMK <- significanceTest_MannKendall(total.costs.by.year)
  print("Significance test for total costs (incl. deaths and injuries)")
  print(resMK)
  summary(resMK)
  
  resLR <- significanceTest_LinearRegression(total.costs.by.year)
  print("Significance test (regression) for total costs (incl. deaths and injuries)")
  print(resLR)
  summary(resLR)
  
}


## Generate Figure 3.34
totalCostOfNaturalDisastersByDecade <- function() {
  # Store the total costs by year
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)
  
  # Filter by decade
  decades <- unique(floor(total.costs$Year.financial / 10)) * 10
  total.costs.by.decade <- with(total.costs, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))
  
  # Multiply decades back up to 000's
  total.costs.by.decade[,1] <- total.costs.by.decade[,1] * 10
  
  standardBarChart(total.costs.by.decade,
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
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)
  total.costs.by.year <- with(total.costs, aggregate(Synthetic.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  
  # Graph the results
  standardBarChart(total.costs.by.year,
                   "fig3_35_total_cost_of_natural_disasters_synthetic",
                   "FIGURE 3.35: TOTAL COSTS (SYNTHETIC) OF DISASTERS, 1967-2013",
                   "Years (financial)",
                   "(2013 Dollars in $millions)"
  )
  
  # Show key statistics
  top10Events <- head(total.costs[order(-total.costs$Synthetic.Cost.normalised.millions),c("Year.financial", "Year", "title", "Reported.Cost.WithDeathsAndInjuries.normalised.millions")], 10)
  print("Top 10 Events by Cost (Synthetic)")
  print(top10Events)
  
  print("Average annual cost of all disasters (Synthetic)")
  print(mean(total.costs.by.year$x))
  
  print("Standard deviation of annual cost of all disasters (Synthetic)")
  print(sd(total.costs.by.year$x))
  
  print("Total cost of all disasters (Synthetic)")
  print(sum(total.costs.by.year$x))  
  
  # Australia's population as reported by ABS June 2013
  print("Average cost of all disasters per person")
  print(1000000 * sum(total.costs.by.year$x) / 23135281)
  
  print("Total cost of all disasters (incl. deaths and injuries)")
  print(sum(total.costs.by.year$x))  
  
  # Top and bottom 3 years
  top3Years <- head(total.costs.by.year[order(-total.costs.by.year$x),], 3)
  print("Top 3 years by Cost (Synthetic)")
  print(top3Years)
  
  bottom3Years <- head(total.costs.by.year[order(total.costs.by.year$x),], 3)
  print("Bottom 3 years by Cost (Synthetic)")
  print(bottom3Years)
  
  print("Highest divided by the smallest")
  print(top3Years[1,2] / bottom3Years[1,2])
  
  # Exclude 3 biggest years
  total.costsTop3 <- head(total.costs[order(-total.costs$Synthetic.Cost.normalised.millions),], n = 3)
  total.costsMinusTop3 <- tail(total.costs[order(-total.costs$Synthetic.Cost.normalised.millions),], n = -3)
  total.costs.by.yearMinusTop3 <- with(total.costsMinusTop3, aggregate(Synthetic.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  print("Average annual cost of all disasters (Synthetic) MINUS TOP 3")
  print(mean(total.costs.by.yearMinusTop3$x))
  
  # Test
  resMK <- significanceTest_MannKendall(total.costs.by.year)
  print("Significance test for total costs (Synthetic)")
  print(resMK)
  summary(resMK)
  
  resLR <- significanceTest_LinearRegression(total.costs.by.year)
  print("Significance test (regression) for total costs (Synthetic)")
  print(resLR)
  summary(resLR)
}


## Generate Figure 3.36 - SYNTHETIC
totalCostOfNaturalDisastersByDecadeSynthetic <- function() {
  # Store the total costs by year
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)
  
  # Filter by decade
  decades <- unique(floor(total.costs$Year.financial / 10)) * 10
  total.costs.by.decade <- with(total.costs, aggregate(Synthetic.Cost.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))
  
  # Multiply decades back up to 000's
  total.costs.by.decade[,1] <- total.costs.by.decade[,1] * 10
  
  standardBarChart(total.costs.by.decade,
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
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)

  numberByYearDenormalised <- with(total.costs, aggregate(Deaths, by=list(Year.financial), FUN=sum))
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
totalCostAsPercentageOfGdp <- function(state = NULL, fatalities = FALSE) {
  # Store the total costs by year
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)
  if (exists("state") & !is.null(state)) {
    total.costs <- total.costs[total.costs$State.1 == state, ] 
  }
  if (fatalities == FALSE) {
    total.costs.by.year <- with(total.costs, aggregate(Reported.Cost.interpolated.millions, by=list(Year.financial), FUN=safeSum))
  }
  else {
    total.costs.by.year <- with(total.costs, aggregate(Reported.Cost.WithDeathsAndInjuries.interpolated.millions, by=list(Year.financial), FUN=safeSum))    
  }
  
  total.costs.by.year$Reported.Cost.interpolated.millions <- total.costs.by.year$x
  total.costs.by.year$gdp <- apply(cbind(total.costs.by.year$Group.1), 1, gdpValues)
  total.costs.by.year$percentOfGDP <- 100 * total.costs.by.year$Reported.Cost.interpolated.millions / total.costs.by.year$gdp
  # For graphing purposes
  total.costs.by.year$x <- total.costs.by.year$percentOfGDP
  
  # Graph the results
  standardBarChart(total.costs.by.year,
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
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL,reported.costs.only =  FALSE, no.heatwaves = FALSE)
  total.costs.by.year <- with(total.costs, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(Year.financial), FUN=safeSum))
  insuredCostsByYear <- with(total.costs, aggregate(Insured.Cost.normalised.millions, by=list(Year.financial), FUN=safeSum))
  
  merged.costs <- merge(total.costs.by.year, insuredCostsByYear, by="Group.1", all.x = TRUE)
  names(merged.costs)[2] <- paste("Reported.Cost")
  names(merged.costs)[3] <- paste("Insured.Cost")
  merged.costs$x <- 100 * merged.costs$Insured.Cost / merged.costs$Reported.Cost
   
  # Graph the results
  standardBarChart(merged.costs,
                   "fig3_39_insured_cost_as_percentage_of_total_cost",
                   "FIGURE 3.39: INSURED COSTS AS PERCENTAGES OF TOTAL COSTS, 1967-2013",
                   "Years (financial)",
                   "(Per Cent)", 
                   TRUE
  )
  
  print("Average percentage of insured costs")
  print(mean(merged.costs$x))
}


## Generate Figure 3.40
total.costsRawIndexedNormalised <- function() {
  # Store the total costs by year
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)
  total.costs.by.year <- with(total.costs, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions, by=list(Year.financial), FUN=safeSum))
  total.costs.by.yearIndexed <- with(total.costs, aggregate(Reported.Cost.WithDeathsAndInjuries.indexed.millions, by=list(Year.financial), FUN=safeSum))
  total.costs.by.yearRaw <- with(total.costs, aggregate(Reported.Cost.WithDeathsAndInjuries.interpolated.millions, by=list(Year.financial), FUN=safeSum))
  
  merged.costs <- merge(total.costs.by.year, total.costs.by.yearIndexed, by="Group.1", all.x = TRUE)
  merged.costs <- merge(merged.costs, total.costs.by.yearRaw, by="Group.1", all.x = TRUE)
  names(merged.costs)[2] <- paste("Normalised Cost")
  names(merged.costs)[3] <- paste("Indexed Cost")
  names(merged.costs)[4] <- paste("Raw Cost")
  
  data <- melt(merged.costs, id.vars="Group.1", value.name="x", variable.name="Cost.Type")
  file.name <- "fig3_40_total_costs_raw_indexed_normalised"
  title <- "FIGURE 3.40: TOTAL COSTS - RAW vs INDEXED vs NORMALISED, 1967-2013"
  x.label <- "Years (financial)"
  y.label <- "(2013 Dollars in $millions)"
  use.years <- TRUE
  
 
  # Calculate range from 0 to max value of costs
  x.scale <- scale_x_continuous(name = x.label, breaks = yearBreaks(data$Group.1), labels = yearLabels(data$Group.1))
  # p <- ggplot(data=data, aes(x=Group.1, y = x, group = Cost.Type, colour=Cost.Type)) + 
  p <- ggplot(data=data, aes(x=Group.1, y = x, group = Cost.Type)) + 
    geom_line(aes(linetype=Cost.Type), size = 1.0) + 
    ggtitle(title) + x.scale + scale_y_continuous(name=y.label, labels=comma) + 
    # scale_colour_manual(values = palette())  +
    theme(plot.title = element_text(colour = foreground.color, lineheight=.8, face="bold"),
          panel.grid.minor.y=element_blank(), 
          panel.grid.major.y=element_line(colour = foreground.color),
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.x=element_blank(),
          panel.background = element_rect(fill = background.color, colour = foreground.color),
          axis.title=element_text(color=title.color),
          axis.text.x=element_text(color=text.color, size = axis.text.size, angle=45, vjust=1.0, hjust=1.0),
          axis.text.y=element_text(color=text.color, size = axis.text.size))
  p
  
  ggsave(file=paste("./figs/", file.name, ".png", sep=""), units = "cm", width = 32, height = 24)  
}




## Generate Figure 3.41
totalAverageCostsNationallyAndByState <- function() {

  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = FALSE)
  # Just for normalised data
  averageCostsByYear <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(Year.financial), FUN=mean))
  averageCostsByYear <- includeAllYears(averageCostsByYear)

  data <- averageCostsByYear
  title <- "FIGURE 3.41: ANNUAL AVERAGE COST OF DISASTERS, 1967-2013"
  x.label <- "Years (financial)"
  y.label <- "(2013 Dollars in $millions)"
  
  # Graph the results
  standardBarChart(averageCostsByYear,
                   "fig3_41_total_average_costs_nationally_and_by_state",
                   title,
                   x.label,
                   y.label
  )

}

## Generate Figure 3.42
totalCostsQldNswVic_3_42 <- function(start.at.year = 1967) {
  # Store the total costs by state
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = FALSE)
  total.costs$Reported.Cost.normalised.millions.state.1 <- total.costs$Reported.Cost.normalised.millions * total.costs$State.1.percent
  total.costs$Reported.Cost.normalised.millions.state.2 <- total.costs$Reported.Cost.normalised.millions * total.costs$State.2.percent
  total.costs.by.state1 <- with(total.costs, aggregate(Reported.Cost.normalised.millions.state.1, by=list(State.abbreviated.1, Year.financial), FUN=safeSum))
  total.costs.by.state2 <- with(total.costs, aggregate(Reported.Cost.normalised.millions.state.2, by=list(State.abbreviated.2, Year.financial), FUN=safeSum))
  total.costs.by.state <- merge(total.costs.by.state1, total.costs.by.state2, by=c("Group.1", "Group.2"), all.x = TRUE )
  total.costs.by.state$x <- rowSums(cbind(total.costs.by.state$x.x, total.costs.by.state$x.y), na.rm = TRUE)
  # Only QLD, NSW, VIC
  total.costs.by.state <- total.costs.by.state[total.costs.by.state$Group.1 %in% c('QLD', 'NSW', 'VIC'),]
  # Order by year
  total.costs.by.state <- total.costs.by.state[order(-total.costs.by.state$Group.2),]
  # Filter by year
  if (exists('start.at.year')) {
    start.at.year <- as.numeric(start.at.year)
    total.costs.by.state <- total.costs.by.state[total.costs.by.state$Group.2 >= start.at.year,]
  }
  total.costs.by.state <- total.costs.by.state[with(total.costs.by.state, order(-x)), ]
    
  data <- total.costs.by.state
  file.name <- "fig3_42_total_costs_qld_nsw_vic"
  title <- "FIGURE 3.42: TOTAL COSTS - QLD vs NSW vs VIC, 1967-2013"
  x.label <- "Years (financial)"
  y.label <- "(2013 Dollars in $millions)"
  use.years <- TRUE
  
  # Set colours
  background <- '#F0F0F0'
  foreground <- '#D08728'
  text.color <- '#888888'
  

  # Calculate range from 0 to max value of costs
  x.scale <- scale_x_continuous(name = x.label, breaks = yearBreaks(data$Group.2), labels = yearLabels(data$Group.2))
  # p <- ggplot(data=total.costs.by.state, aes(x=Group.2, y = x, group = Group.1, color = Group.1)) + 
  p <- ggplot(data=total.costs.by.state, aes(x=Group.2, y = x, group = Group.1)) + 
    geom_line(aes(linetype=Group.1), size = 1.0) + 
    # geom_line() + 
    ggtitle(title) + x.scale + scale_y_continuous(name=y.label, labels=comma) + 
    # scale_colour_manual(values = palette())  +
    theme(plot.title = element_text(colour = foreground.color, lineheight=.8, face="bold"),
          panel.grid.minor.y=element_blank(), 
          panel.grid.major.y=element_line(colour = foreground.color),
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.x=element_blank(),
          panel.background = element_rect(fill = background.color, colour = foreground.color),
          axis.title=element_text(color=text.color),
          axis.text.x=element_text(color=text.color, size = axis.text.size, angle=45, vjust=1.0, hjust=1.0),
          axis.text.y=element_text(color=text.color, size = axis.text.size))
  p
  
  ggsave(file=paste("./figs/", file.name, ".png", sep=""))  
}


# Tables

## Generate Table 3.1
averageAnnualCostOfNaturalDisastersByStateAndTerritory <- function() {
  pivotCostsByStateAndDisasterType <- function(with.deaths.and.injuries = FALSE, year = NULL) {
    # Store the total costs by year
    total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)
    if (!is.null(year)) {
      total.costs <- total.costs[total.costs$Year.financial >= year, ]
    }
    if (with.deaths.and.injuries) {
      total.costs$Reported.Cost.WithDeathsAndInjuries.normalised.millions.state.1 <- total.costs$Reported.Cost.WithDeathsAndInjuries.normalised.millions * total.costs$State.1.percent
      total.costs$Reported.Cost.WithDeathsAndInjuries.normalised.millions.state.2 <- total.costs$Reported.Cost.WithDeathsAndInjuries.normalised.millions * total.costs$State.2.percent
      total.costs.by.state1 <- with(total.costs, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions.state.1, by=list(State.abbreviated.1, resourceType), FUN=safeSum))
      total.costs.by.state2 <- with(total.costs, aggregate(Reported.Cost.WithDeathsAndInjuries.normalised.millions.state.2, by=list(State.abbreviated.2, resourceType), FUN=safeSum))
    }
    else {
      total.costs$Reported.Cost.normalised.millions.state.1 <- total.costs$Reported.Cost.normalised.millions * total.costs$State.1.percent
      total.costs$Reported.Cost.normalised.millions.state.2 <- total.costs$Reported.Cost.normalised.millions * total.costs$State.2.percent
      total.costs.by.state1 <- with(total.costs, aggregate(Reported.Cost.normalised.millions.state.1, by=list(State.abbreviated.1, resourceType), FUN=safeSum))
      total.costs.by.state2 <- with(total.costs, aggregate(Reported.Cost.normalised.millions.state.2, by=list(State.abbreviated.2, resourceType), FUN=safeSum))
    }
    total.costs.by.state.and.disaster.type <- merge(total.costs.by.state1, total.costs.by.state2, by=c("Group.1", "Group.2"), all.x = TRUE )
    total.costs.by.state.and.disaster.type$x <- rowSums(cbind(total.costs.by.state.and.disaster.type$x.x, total.costs.by.state.and.disaster.type$x.y), na.rm = TRUE)
    # total.costs.by.state.and.disaster.type <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(State.abbreviated.1, resourceType), FUN=safeSum))
    total.costs.by.state.and.disaster.type$x <- round(total.costs.by.state.and.disaster.type$x)
    
    # Very brittle conversion to a table
    pivotted.data <- dcast(total.costs.by.state.and.disaster.type, Group.1 ~ Group.2, value.var = "x", sum, margins = TRUE)
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
    # Get rid of unhelpful row and col names
    colnames(pivotted.data)[1] <- ""
    colnames(pivotted.data)[cols] <- "Totals"
    return (pivotted.data)
  }
  
  
  old.value <- use.state.normalisations

  # NORMED BY NATIONAL AVERAGES
  useStateNormalisations(FALSE)
  if (exists("ecnd.database")) {
    rm(ecnd.database)
  }
  initialise(database.file)
  data <- pivotCostsByStateAndDisasterType(with.deaths.and.injuries = FALSE)
  write.table(data, file = "./figs/table3_1_a_totals_by_state_and_disaster_type.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
  
  data <- pivotCostsByStateAndDisasterType(with.deaths.and.injuries = TRUE)
  write.table(data, file = "./figs/table3_1_e_totals_by_state_and_disaster_type_with_deaths_and_injuries.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
  
  data <- pivotCostsByStateAndDisasterType(with.deaths.and.injuries = FALSE, year = 2000)
  write.table(data, file = "./figs/table3_1_c_totals_by_state_and_disaster_type_2000.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
  
  data <- pivotCostsByStateAndDisasterType(with.deaths.and.injuries = TRUE, year = 2000)
  write.table(data, file = "./figs/table3_1_f_totals_by_state_and_disaster_type_with_deaths_and_injuries_2000.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
  
  
  # NORMED BY STATE AVERAGES
  useStateNormalisations(TRUE)
  if (exists("ecnd.database")) {
    rm(ecnd.database)
  }
  initialise(database.file)
  data <- pivotCostsByStateAndDisasterType(with.deaths.and.injuries = FALSE)
  write.table(data, file = "./figs/table3_1_b_totals_by_state_and_disaster_type_normed_by_state.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
  
  data <- pivotCostsByStateAndDisasterType(with.deaths.and.injuries = TRUE)
  write.table(data, file = "./figs/table3_1_g_totals_by_state_and_disaster_type_with_deaths_and_injuries_normed_by_state.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
  
  data <- pivotCostsByStateAndDisasterType(with.deaths.and.injuries = FALSE, year = 2000)
  write.table(data, file = "./figs/table3_1_d_totals_by_state_and_disaster_type_2000_normed_by_state.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
  
  data <- pivotCostsByStateAndDisasterType(with.deaths.and.injuries = TRUE, year = 2000)
  write.table(data, file = "./figs/table3_1_h_totals_by_state_and_disaster_type_with_deaths_and_injuries_2000_normed_by_state.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
  
  useStateNormalisations(old.value)
  if (exists("ecnd.database")) {
    rm(ecnd.database)
  }
  initialise(database.file)
  
}

## Generate Table 3.2
deathsAndInjuriesByHazardType <- function() {
  # Store the total costs by year
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)
  
  totalNumberOfDeathsByResourceType <- with(total.costs, aggregate(Deaths, by=list(resourceType), FUN=safeSum))
  totalNumberOfDeathsByResourceType.n <- with(total.costs, aggregate(Deaths.normalised, by=list(resourceType), FUN=safeSum))
  totalNumberOfInjuriesByResourceType <- with(total.costs, aggregate(Injuries, by=list(resourceType), FUN=safeSum))
  totalNumberOfInjuriesByResourceType.n <- with(total.costs, aggregate(Injuries.normalised, by=list(resourceType), FUN=safeSum))
  total.costsOfDeathsInjuriesByResourceType <- with(total.costs, aggregate(deathAndInjuryCosts.normalised.millions, by=list(resourceType), FUN=safeSum))
  total.costs.by.disaster.type <- with(total.costs, aggregate(Reported.Cost.normalised.millions, by=list(resourceType), FUN=safeSum))
  total.costs.by.disaster.type$incl.deaths <- total.costs.by.disaster.type$x + total.costsOfDeathsInjuriesByResourceType$x
  total.costs.by.disaster.type$percent.deaths <- total.costsOfDeathsInjuriesByResourceType$x / total.costs.by.disaster.type$incl.deaths
  merged.data <- merge(totalNumberOfDeathsByResourceType, totalNumberOfDeathsByResourceType.n, by = "Group.1", all = TRUE, suffixes = c(".a", ".b"))
  merged.data <- merge(merged.data, totalNumberOfInjuriesByResourceType, by = "Group.1", all = TRUE, suffixes = c(".b", ".c"))
  merged.data <- merge(merged.data, totalNumberOfInjuriesByResourceType.n, by = "Group.1", all = TRUE, suffixes = c(".c", ".d"))
  merged.data <- merge(merged.data, total.costsOfDeathsInjuriesByResourceType, by = "Group.1", all = TRUE, suffixes = c(".d", ".e"))
  merged.data <- merge(merged.data, total.costs.by.disaster.type, by = "Group.1", all = TRUE, suffixes = c(".e", ".f", ".g", ".h"))
  merged.data <- merged.data[order(-merged.data$x.e),]
  merged.data[, seq(2, 9)] <- round(merged.data[, seq(2, 9)])
  merged.data[, seq(2, 9)] <- format(merged.data[, seq(2, 9)], big.mark=",")
  
  print("Percentages of deaths by disaster type")
  print(total.costs.by.disaster.type)
  
  write.table(merged.data, file = "./figs/table3_2_deaths_injuries_by_disaster_type.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
}

## Generate Table 3.3
multipliersJoyVsDerived <- function() {
  # Store the total costs by year
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)
  
  event.types <- data.frame(eventTypes = unique(total.costs$resourceType))
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
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = FALSE, no.heatwaves = FALSE)
  total.costs$Reported.Cost.normalised.millions.state.1 <- total.costs$Reported.Cost.normalised.millions * total.costs$State.1.percent
  total.costs$Reported.Cost.normalised.millions.state.2 <- total.costs$Reported.Cost.normalised.millions * total.costs$State.2.percent
  total.costs.by.state1 <- with(total.costs, aggregate(Reported.Cost.normalised.millions.state.1, by=list(State.abbreviated.1, Year.financial), FUN=safeSum))
  total.costs.by.state2 <- with(total.costs, aggregate(Reported.Cost.normalised.millions.state.2, by=list(State.abbreviated.2, Year.financial), FUN=safeSum))
  total.costs.by.stateAndYear <- merge(total.costs.by.state1, total.costs.by.state2, by=c("Group.1", "Group.2"), all.x = TRUE )
  total.costs.by.stateAndYear$x <- rowSums(cbind(total.costs.by.stateAndYear$x.x, total.costs.by.stateAndYear$x.y), na.rm = TRUE)
  state.year.totals <- total.costs.by.stateAndYear[,c("Group.2", "Group.1", "x")]
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



