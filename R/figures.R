
# Sources
source("R/functions.r", TRUE)

# Global variables
title_size <- 0.8
character_size <- 0.6

# Functions for generating figures

## Provides a single function for generating bar charts
standardBarChart <- function(data, file_name, title, x_label, y_label, y_range=NULL, axes=TRUE) {

	# Plot a basic graph of costs
	pdf(file=paste("./figs/", file_name, ".pdf", sep=""))

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
	axis(number, at=at, labels=labels,
      cex=character_size,
			cex.lab=character_size, 
			cex.axis=character_size, 
			cex.main=character_size, 
			cex.sub=character_size)
}

## Generates Figure 3.1
annual_total_costs_of_disasters_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	totalCostsByYear <- with(totalCosts, aggregate(total.normalised, by=list(Year.financial), FUN=safeSum))

	# Calculate range from 0 to max value of costs
	x_range <- range(totalCosts$Year.financial)
	y_range <- range(0, totalCostsByYear + 2000000000)

	# Graph the results
	standardBarChart(totalCostsByYear, 
		"fig3_1_annual_total_costs_of_disasters_in_australia",
		"FIGURE 3.1: ANNUAL TOTAL COSTS OF DISASTERS IN AUSTRALIA, 1967-2013",
		"Years",
		"(2013 Dollars in $millions)",
		y_range,
		FALSE
		)

	# Default x axis
	doAxis(1, at=seq(x_range[1], x_range[2], by=1))

	# Make y axis with horizontal labels that display ticks at 
	billions <- 1000000000 * 0:(y_range[2] / 1000000000)
	doAxis(2, at=billions, labels=format(billions / 1000000, big.mark = ","))

	dev.off()
}


## Generates Figure 3.2
australian_natural_disaster_costs_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()

	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
  
  # Aggregate normalised costs by decade
	totalCostsByDecade <- with(totalCosts, aggregate(total.normalised / 1000000, by=list(floor(Year.financial / 10)), FUN=safeSum))
	
  # Multiply decades back up to years
	totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10
	
	# Calculate range from 0 to max value of costs
	y_range <- range(0, totalCostsByDecade + 1000000000)

	standardBarChart(totalCostsByDecade, 
		"fig3_2_australian_natural_disaster_costs_by_decade",
		"FIGURE 3.2: AUSTRALIAN NATURAL DISASTER COSTS BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
		y_range
		)
	dev.off()
}


## Generate Figure 3.3
average_cost_per_event <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	averageCostPerYear <- with(totalCosts, aggregate(total.normalised, by=list(Year.financial), FUN=safeMean))

	# Calculate range from 0 to max value of costs
	y_range <- range(0, averageCostPerYear + 1000000000)
	x_range <- range(totalCosts$Year.financial)

	standardBarChart(averageCostPerYear, 
		"fig3_3_average_cost_per_event",
		"FIGURE 3.3: AVERAGE COST PER EVENT, 1967-2013",
		"Years",
		"(2013 Dollars in $millions)",
		y_range,
		FALSE
		)

	doAxis(1, at=seq(x_range[1], x_range[2], by=1))

	# Make y axis with horizontal labels that display ticks at 
	billions <- 1000000000 * 0:(y_range[2] / 1000000000)
	doAxis(2, at=billions, labels=format(billions / 1000000, big.mark = ","))

	dev.off()
}

## Generate Figure 3.4
distribution_of_disasters <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()

	# Filter by cost bracket
	cost_brackets <- list(10000000, 50000000, 100000000, 150000000, 500000000)
	totalCosts$total.normalised
	totalCosts$total.normalised.millions <- totalCosts$total.normalised / 1000000
  
	totalCosts$total.normalised.code <- apply(data.matrix(totalCosts$total.normalised.millions), 1, codeCosts)
	totalCostDistribution <- with(totalCosts, aggregate(total.normalised, by=list(total.normalised.code), FUN=length))

	# Calculate range from 0 to max value of costs
	y_range <- range(0, totalCostDistribution[,2] + 10)
	
	standardBarChart(totalCostDistribution, 
		"fig3_4_distribution_of_disasters",
		"FIGURE 3.4: DISTRIBUTION OF DISASTERS (FREQUENCY) BY COSTS, 1967-2013",
		"Cost distributions",
		"Frequency",
		y_range,
		FALSE
		)

	doAxis(1, at=totalCostDistribution[,1], labels=codeCostLabels())
	doAxis(2)

	dev.off()

}

## Generate Figure 3.5
annual_insurance_cost_of_disasters <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	directCostsByYear <- with(totalCosts, aggregate(directCost.normalised, by=list(Year.financial), FUN=safeSum))

  # Calculate range from 0 to max value of costs
	x_range <- range(totalCosts$Year.financial)
	y_range <- range(0, directCostsByYear + 2000000000)
	
	standardBarChart(directCostsByYear, 
		"fig3_5_annual_insurance_costs_of_disasters_in_australia",
		"FIGURE 3.5: ANNUAL INSURANCE COSTS OF DISASTERS IN AUSTRALIA, 1967-2013",
		"Years",
		"(2013 Dollars in $millions)",
		y_range,
		FALSE
		)

	doAxis(1, at=seq(x_range[1], x_range[2], by=1))

	# Make y axis with horizontal labels that display ticks at 
	billions <- 1000000000 * 0:(y_range[2] / 1000000000)
	doAxis(2, at=billions, labels=format(billions / 1000000, big.mark = ","))

	dev.off()
	
}


## Generate Figure 3.6
number_of_natural_disasters_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	numberByYear <- with(totalCosts, aggregate(directCost.normalised, by=list(Year.financial), FUN=length))

	# Calculate range from 0 to max value of costs
	x_range <- range(totalCosts$Year.financial)
	y_range <- range(0, numberByYear[,2] + 10)
	
	standardBarChart(numberByYear, 
		"fig3_6_number_of_natural_disasters_in_australia",
		"FIGURE 3.6: NUMBER OF NATURAL DISASTERS IN AUSTRALIA, 1967-2013",
		"Years",
		"Number of events",
		y_range
		)

	dev.off()
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
	# Correlation test
}


## Generate Figure 3.10
disaster_costs_by_state_and_territory <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	totalCosts$total.normalised.millions <- totalCosts$total.normalised / 1000000
	totalCostsByState <- with(totalCosts, aggregate(total.normalised.millions, by=list(State.abbreviated), FUN=safeSum))
	o <- order(totalCostsByState[,2], decreasing=TRUE)
	totalCostsByState <- data.frame(cbind(totalCostsByState[,1][o], totalCostsByState[,2][o]))
	states <- totalCostsByState[,1]
	totalCostsByState[,1] <- 1:length(totalCostsByState[,1])

	# Calculate range from 0 to max value of costs
  x_range <- range(totalCosts$State.abbreviated)
	y_range <- range(0, as.numeric(totalCostsByState[,2]) + 10000000000)
	
	standardBarChart(totalCostsByState, 
		"fig3_10_disaster_costs_by_state_and_territory",
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


## Generate Figure 3.11
number_of_disaster_events_by_state_and_territory <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEvent()
  totalCountsByState <- with(totalCosts, aggregate(total.normalised, by=list(State.abbreviated), FUN=length))
  o <- order(totalCountsByState[,2], decreasing=TRUE)
  totalCountsByState <- data.frame(cbind(totalCountsByState[,1][o], totalCountsByState[,2][o]))
  states <- totalCountsByState[,1]
  totalCountsByState[,1] <- 1:length(totalCountsByState[,1])
  
  # Calculate range from 0 to max value of costs
  x_range <- range(totalCosts$State.abbreviated)
  y_range <- range(0, as.numeric(totalCountsByState[,2]) + 10)
	
	standardBarChart(totalCountsByState, 
		"fig3_11_number_of_disaster_events_by_state_and_territory",
		"FIGURE 3.11: # OF DISASTER EVENTS BY STATE AND TERRITORY IN AUSTRALIA, 1967-2013",
		"Years",
		"Number of events",
		y_range,
		FALSE
		)
  doAxis(1, at=totalCountsByState[,1], labels=states)
  doAxis(2)
  dev.off()
}


## Generate Figure 3.12
costs_by_type_of_disaster_and_state_and_territory <- function() {
	# Pie chart
}


## Generate Figure 3.13
total_and_insurance_costs_by_disaster_type <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEvent()
  totalCostsByDisasterType <- with(totalCosts, aggregate(total.normalised, by=list(resourceType), FUN=safeSum))
  o <- order(totalCostsByDisasterType[,2], decreasing=TRUE)
  totalCostsByDisasterType <- data.frame(cbind(totalCostsByDisasterType[,1][o], totalCostsByDisasterType[,2][o]))
  disasterTypes <- totalCostsByDisasterType[,1]
  totalCostsByDisasterType[,1] <- 1:length(totalCostsByDisasterType[,1])
    
  # Calculate range from 0 to max value of costs
  x_range <- range(totalCosts$resourceType)
  y_range <- range(0, as.numeric(totalCostsByDisasterType[,2]) + 10000000000)
	
	standardBarChart(totalCostsByDisasterType, 
		"fig3_13_total_and_insurance_costs_by_disaster_type",
		"FIGURE 3.13: TOTAL AND INSURANCE COSTS BY DISASTER TYPE, 1967-2013",
		"Years",
		"(2013 Dollars in $millions)",
		y_range,
		FALSE
		)
  doAxis(1, at=totalCostsByDisasterType[,1], labels=disasterTypes)
  doAxis(2)
  dev.off()
}


## Generate Figure 3.14
number_of_events_by_disaster_type <- function() {
  # Store the total costs by year
  totalCosts <- totalCostForEvent()
  totalCostsByDisasterType <- with(totalCosts, aggregate(total.normalised, by=list(resourceType), FUN=length))
  o <- order(totalCostsByDisasterType[,2], decreasing=TRUE)
  totalCostsByDisasterType <- data.frame(cbind(totalCostsByDisasterType[,1][o], totalCostsByDisasterType[,2][o]))
  disasterTypes <- totalCostsByDisasterType[,1]
  totalCostsByDisasterType[,1] <- 1:length(totalCostsByDisasterType[,1])
  
  # Calculate range from 0 to max value of costs
  x_range <- range(totalCosts$resourceType)
  y_range <- range(0, as.numeric(totalCostsByDisasterType[,2]) + 10)
  
	standardBarChart(totalCostsByDisasterType, 
		"fig3_14_number_of_events_by_disaster_type",
		"FIGURE 3.14: DISASTER COSTS BY STATE AND TERRITORY IN AUSTRALIA, 1967-2013",
		"Event types",
		"Number of events",
		y_range,
		FALSE)
  doAxis(1, at=totalCostsByDisasterType[,1], labels=disasterTypes)
  doAxis(2)

  dev.off()
  
}


## Generate Figure 3.15
annual_cost_of_floods_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Flood")
	totalCostsByYear <- with(totalCosts, aggregate(total.normalised, by=list(Year.financial), FUN=safeSum))

	# Calculate range from 0 to max value of costs
	x_range <- range(totalCosts$Year.financial)
	y_range <- range(0, totalCostsByYear + 500000000)

	# Graph the results
	standardBarChart(totalCostsByYear, 
		"fig3_15_annual_cost_of_floods_in_australia",
		"FIGURE 3.15: ANNUAL TOTAL COSTS OF FLOODS IN AUSTRALIA, 1967-2013",
		"Years",
		"(2013 Dollars in $millions)",
		y_range,
		FALSE
		)

	# Default x axis
	doAxis(1, at=seq(x_range[1], x_range[2], by=1))

	# Make y axis with horizontal labels that display ticks at 
	billions <- 1000000000 * 0:(y_range[2] / 1000000000)
	doAxis(2, at=billions, labels=format(billions / 1000000, big.mark = ","))

	dev.off()
	
}


## Generate Figure 3.16
total_cost_of_floods_by_decade <- function() {
		# Store the total costs by year
	totalCosts <- totalCostForEvent("Flood")
	totalCosts$total.normalised.millions <- totalCosts$total.normalised / 1000000
  
	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
	totalCostsByDecade <- with(totalCosts, aggregate(total.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10

	# Calculate range from 0 to max value of costs
	y_range <- range(0, totalCostsByDecade + 2000)

	standardBarChart(totalCostsByDecade, 
		"fig3_16_australian_flood_costs_by_decade",
		"FIGURE 3.16: AUSTRALIAN FLOOD COSTS BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
		y_range
		)
	dev.off()
}


## Generate Figure 3.17
annual_number_of_floods_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Flood")
	numberByYear <- with(totalCosts, aggregate(directCost.normalised, by=list(Year.financial), FUN=length))

	# Calculate range from 0 to max value of costs
	x_range <- range(totalCosts$Year.financial)
	y_range <- range(0, numberByYear[,2] + 10)
	
	standardBarChart(numberByYear, 
		"fig3_17_number_of_floods_in_australia",
		"FIGURE 3.17: NUMBER OF FLOODS IN AUSTRALIA, 1967-2013",
		"Years",
		"Number of events",
		y_range
		)

	dev.off()
	
}


## Generate Figure 3.18
annual_cost_of_severe_storms_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Severe Storm")
	totalCostsByYear <- with(totalCosts, aggregate(total.normalised, by=list(Year.financial), FUN=safeSum))

	# Calculate range from 0 to max value of costs
	x_range <- range(totalCosts$Year.financial)
	y_range <- range(0, totalCostsByYear + 500000000)

	# Graph the results
	standardBarChart(totalCostsByYear, 
		"fig3_18_annual_cost_of_severe_storms_in_australia",
		"FIGURE 3.18: ANNUAL TOTAL COSTS OF SEVERE STORMS IN AUSTRALIA, 1967-2013",
		"Years",
		"(2013 Dollars in $millions)",
		y_range,
		FALSE
		)

	# Default x axis
	doAxis(1, at=seq(x_range[1], x_range[2], by=1))

	# Make y axis with horizontal labels that display ticks at 
	billions <- 1000000000 * 0:(y_range[2] / 1000000000)
	doAxis(2, at=billions, labels=format(billions / 1000000, big.mark = ","))

	dev.off()
	
}


## Generate Figure 3.19
total_cost_of_severe_storms_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Severe Storm")
	totalCosts$total.normalised.millions <- totalCosts$total.normalised / 1000000

	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
	totalCostsByDecade <- with(totalCosts, aggregate(total.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10

	# Calculate range from 0 to max value of costs
	y_range <- range(0, totalCostsByDecade + 2000)

	standardBarChart(totalCostsByDecade, 
		"fig3_19_australian_severe_storm_costs_by_decade",
		"FIGURE 3.19: AUSTRALIAN SEVERE STORM COSTS BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
		y_range
		)
	dev.off()
}


## Generate Figure 3.20
annual_number_of_severe_storms_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Severe Storm")
	numberByYear <- with(totalCosts, aggregate(directCost.normalised, by=list(Year.financial), FUN=length))

	# Calculate range from 0 to max value of costs
	x_range <- range(totalCosts$Year.financial)
	y_range <- range(0, numberByYear[,2] + 10)
	
	standardBarChart(numberByYear, 
		"fig3_20_number_of_severe_storms_in_australia",
		"FIGURE 3.20: NUMBER OF SEVERE STORMS IN AUSTRALIA, 1967-2013",
		"Years",
		"Number of events",
		y_range
		)

	dev.off()
}


## Generate Figure 3.21
annual_cost_of_cyclones_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Cyclone")
	totalCostsByYear <- with(totalCosts, aggregate(total.normalised, by=list(Year.financial), FUN=safeSum))

	# Calculate range from 0 to max value of costs
	x_range <- range(totalCosts$Year.financial)
	y_range <- range(0, totalCostsByYear + 500000000)

	# Graph the results
	standardBarChart(totalCostsByYear, 
		"fig3_21_annual_cost_of_cyclone_in_australia",
		"FIGURE 3.21: ANNUAL TOTAL COSTS OF CYCLONES IN AUSTRALIA, 1967-2013",
		"Years",
		"(2013 Dollars in $millions)",
		y_range,
		FALSE
		)

	# Default x axis
	doAxis(1, at=seq(x_range[1], x_range[2], by=1))

	# Make y axis with horizontal labels that display ticks at 
	billions <- 1000000000 * 0:(y_range[2] / 1000000000)
	doAxis(2, at=billions, labels=format(billions / 1000000, big.mark = ","))

	dev.off()
}


## Generate Figure 3.22
total_cost_of_cyclones_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Cyclone")
	totalCosts$total.normalised.millions <- totalCosts$total.normalised / 1000000  

	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
	totalCostsByDecade <- with(totalCosts, aggregate(total.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10

	# Calculate range from 0 to max value of costs
	y_range <- range(0, totalCostsByDecade + 2000000000)

	standardBarChart(totalCostsByDecade, 
		"fig3_22_australian_cyclone_costs_by_decade",
		"FIGURE 3.22: AUSTRALIAN CYCLONES COSTS BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
		y_range
		)
	dev.off()
	
}


## Generate Figure 3.23
annual_number_of_cyclones_causing_more_than_10_million_damage_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Cyclone")
	numberByYear <- with(totalCosts, aggregate(directCost.normalised, by=list(Year.financial), FUN=length))

	# Calculate range from 0 to max value of costs
	x_range <- range(totalCosts$Year.financial)
	y_range <- range(0, numberByYear[,2] + 10)
	
	standardBarChart(numberByYear, 
		"fig3_23_number_of_cyclones_in_australia",
		"FIGURE 3.23: NUMBER OF CYCLONES IN AUSTRALIA, 1967-2013",
		"Years",
		"Number of events",
		y_range
		)

	dev.off()
}


## Generate Figure 3.24
total_cost_of_earthquakes_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Earthquake")
	totalCosts$total.normalised.millions <- totalCosts$total.normalised / 1000000  

	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
	totalCostsByDecade <- with(totalCosts, aggregate(total.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10

	# Calculate range from 0 to max value of costs
	y_range <- range(0, totalCostsByDecade + 2000)

	standardBarChart(totalCostsByDecade, 
		"fig3_24_australian_earthquake_costs_by_decade",
		"FIGURE 3.24: AUSTRALIAN EARTHQUAKE COSTS BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
		y_range
		)
	dev.off()
}


## Generate Figure 3.25
annual_cost_of_bushfires_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Bushfire")
	totalCostsByYear <- with(totalCosts, aggregate(total.normalised, by=list(Year.financial), FUN=safeSum))

	# Calculate range from 0 to max value of costs
	x_range <- range(totalCosts$Year.financial)
	y_range <- range(0, totalCostsByYear + 500000000)

	# Graph the results
	standardBarChart(totalCostsByYear, 
		"fig3_25_annual_cost_of_bushfire_in_australia",
		"FIGURE 3.25: ANNUAL TOTAL COSTS OF BUSHFIRES IN AUSTRALIA, 1967-2013",
		"Years",
		"(2013 Dollars in $millions)",
		y_range,
		FALSE
		)

	# Default x axis
	doAxis(1, at=seq(x_range[1], x_range[2], by=1))

	# Make y axis with horizontal labels that display ticks at 
	billions <- 1000000000 * 0:(y_range[2] / 1000000000)
	doAxis(2, at=billions, labels=format(billions / 1000000, big.mark = ","))

	dev.off()
}


## Generate Figure 3.26
total_cost_of_bushfires_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Bushfire")
	totalCosts$total.normalised.millions <- totalCosts$total.normalised / 1000000  
  
	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
	totalCostsByDecade <- with(totalCosts, aggregate(total.normalised.millions, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10

	# Calculate range from 0 to max value of costs
	y_range <- range(0, totalCostsByDecade + 2000)

	standardBarChart(totalCostsByDecade, 
		"fig3_26_australian_bushfire_costs_by_decade",
		"FIGURE 3.26: AUSTRALIAN BUSHFIRE COSTS BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
		y_range
		)
	dev.off()
}


## Generate Figure 3.27
annual_number_of_bushfires_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent("Bushfire")
	numberByYear <- with(totalCosts, aggregate(directCost.normalised, by=list(Year.financial), FUN=length))

	# Calculate range from 0 to max value of costs
	x_range <- range(totalCosts$Year.financial)
	y_range <- range(0, numberByYear[,2] + 10)
	
	standardBarChart(numberByYear, 
		"fig3_27_number_of_bushfire_in_australia",
		"FIGURE 3.27: NUMBER OF BUSHFIRES IN AUSTRALIA, 1967-2013",
		"Years",
		"Number of events",
		y_range
		)

	dev.off()
	
}


## Generate Figure 3.28
number_of_natural_disasters_deaths <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	numberByYear <- with(totalCosts, aggregate(Deaths, by=list(Year.financial), FUN=sum))

	# Calculate range from 0 to max value of costs
	x_range <- range(totalCosts$Year.financial)
	y_range <- range(0, numberByYear[,2] + 10)
	
	standardBarChart(numberByYear, 
		"fig3_28_number_of_natural_disasters_deaths",
		"FIGURE 3.28: NUMBER OF DEATHS BY NATURAL DISASTERS IN AUSTRALIA, 1967-2013",
		"Years",
		"Number of deaths",
		y_range
		)

	dev.off()
}


## Generate Figure 3.29
number_of_natural_disasters_injuries <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	numberByYear <- with(totalCosts, aggregate(Injuries, by=list(Year.financial), FUN=sum))

	# Calculate range from 0 to max value of costs
	x_range <- range(totalCosts$Year.financial)
	y_range <- range(0, numberByYear[,2] + 10)
	
	standardBarChart(numberByYear, 
		"fig3_29_number_of_natural_disasters_injuries",
		"FIGURE 3.29: NUMBER OF INJURIES BY NATURAL DISASTERS IN AUSTRALIA, 1967-2013",
		"Years",
		"Number of injuries",
		y_range
		)

	dev.off()
}


## Generate Figure 3.30
number_of_deaths_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	
	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
	costOfDeathsByDecade <- with(totalCosts, aggregate(deathCosts.normalised / 1000000, by=list(floor(Year.financial / 10)), FUN=safeSum))
	
  # Multiply decades back up to 000's
	costOfDeathsByDecade[,1] <- costOfDeathsByDecade[,1] * 10

	# Calculate range from 0 to max value of costs
	y_range <- range(0, costOfDeathsByDecade + 1000)

	standardBarChart(costOfDeathsByDecade, 
		"fig3_30_australian_natural_disaster_costs_by_decade",
		"FIGURE 3.30: COST OF DEATHS BY NATURAL DISASTERS BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
		y_range
		)
	dev.off()
}


## Generate Figure 3.31
cost_of_deaths_and_injuries <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	totalCostsByYear <- with(totalCosts, aggregate(deathCosts.normalised + injuryCosts.normalised, by=list(Year.financial), FUN=safeSum))

	# Calculate range from 0 to max value of costs
	x_range <- range(totalCosts$Year.financial)
	y_range <- range(0, totalCostsByYear + 500000000)

	# Graph the results
	standardBarChart(totalCostsByYear, 
		"fig3_31_cost_of_deaths_and_injuries",
		"FIGURE 3.31: COST OF DEATH AND INJURIES BY NATURAL DISASTERS IN AUSTRALIA, 1967-2013",
		"Years",
		"(2013 Dollars in $millions)",
		y_range,
		FALSE
		)

	# Default x axis
	doAxis(1, at=seq(x_range[1], x_range[2], by=1))

	# Make y axis with horizontal labels that display ticks at 
	billions <- 1000000000 * 0:(y_range[2] / 1000000000)
	doAxis(2, at=billions, labels=format(billions / 1000000, big.mark = ","))

	dev.off()	
}


## Generate Figure 3.32
cost_of_deaths_and_injuries_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()

	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
	totalCostsByDecade <- with(totalCosts, aggregate((deathCosts.normalised + injuryCosts.normalised) / 1000000, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10

	# Calculate range from 0 to max value of costs
	y_range <- range(0, totalCostsByDecade + 2000)

	standardBarChart(totalCostsByDecade, 
		"fig3_32_cost_of_deaths_and_injuries_by_decade",
		"FIGURE 3.32: COST OF DEATHS AND INJURIES BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
		y_range
		)
	dev.off()
}


## Generate Figure 3.33
total_cost_of_natural_disasters <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	totalCostsByYear <- with(totalCosts, aggregate(total.normalised, by=list(Year.financial), FUN=safeSum))

	# Calculate range from 0 to max value of costs
	x_range <- range(totalCosts$Year.financial)
	y_range <- range(0, totalCostsByYear + 2000000000)

	# Graph the results
	standardBarChart(totalCostsByYear, 
		"fig3_33_total_cost_of_natural_disasters",
		"FIGURE 3.33: TOTAL COSTS OF DISASTERS IN AUSTRALIA, 1967-2013",
		"Years",
		"(2013 Dollars in $millions)",
		y_range,
		FALSE
		)

	# Default x axis
	doAxis(1, at=seq(x_range[1], x_range[2], by=1))

	# Make y axis with horizontal labels that display ticks at 
	billions <- 1000000000 * 0:(y_range[2] / 1000000000)
	doAxis(2, at=billions, labels=format(billions / 1000000, big.mark = ","))

	dev.off()	
}


## Generate Figure 3.34
total_cost_of_natural_disasters_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()

	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
	totalCostsByDecade <- with(totalCosts, aggregate(total.normalised, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10

	# Calculate range from 0 to max value of costs
	y_range <- range(0, totalCostsByDecade + 2000000000)

	standardBarChart(totalCostsByDecade, 
		"fig3_34_total_cost_of_natural_disasters_by_decade",
		"FIGURE 3.34: TOTAL COST OF NATURAL DISASTERS BY DECADE, 1967-2013",
		"Decades",
		"(2013 Dollars in $millions)",
		y_range
		)
	dev.off()
	
}

# Tables

## Generate Table 3.1
average_annual_cost_of_natural_disasters_by_state_and_territory <- function() {
	
}

## Generate Table 3.2
deaths_and_injuries_by_hazard_type <- function() {
	
}



## Helper functions for generating graphs



