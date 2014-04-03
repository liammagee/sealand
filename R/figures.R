
# Sources
source("R/functions.r", TRUE)

# Functions for generating figures

## Generates Figure 3.1
annual_total_costs_of_disasters_in_australia <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	totalCostsByYear <- with(totalCosts, aggregate(total.normalised, by=list(Year.financial), FUN=safeSum))

	# Plot a basic graph of costs
	pdf(file="./figs/fig3_1_annual_total_costs_of_disasters_in_australia.pdf")

	# Calculate range from 0 to max value of costs
	c_range <- range(0, totalCostsByYear + 2000000000)

	y_range <- range(totalCosts$Year.financial)


	plot(totalCostsByYear, type="h", col="blue", ylim=c_range, axes=FALSE, ann=FALSE) 

	axis(1, las=1, at=seq(y_range[1], y_range[2], by=1))

	# Make y axis with horizontal labels that display ticks at 
	billions <- 1000000000 * 0:(c_range[2] / 1000000000)
	axis(2, las=1, at=billions, labels=format(billions / 1000000, big.mark = ","))

	# Add title
	title("FIGURE 3.1: ANNUAL TOTAL COSTS OF DISASTERS IN AUSTRLIA, 1967-2013 ", col.main = "blue")

	# Label the x and y axes with dark green text
	title(xlab="Years", col.lab=rgb(0,0.5,0))
	title(ylab="(2013 Dollars in $millions)", col.lab=rgb(0,0.5,0))

	dev.off()
}


## Generates Figure 3.2
australian_natural_disaster_costs_by_decade <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()

	# Filter by decade
	decades <- unique(floor(totalCosts$Year.financial / 10)) * 10
	totalCostsByDecade <- with(totalCosts, aggregate(total.normalised, by=list(floor(Year.financial / 10)), FUN=safeSum))

	# Multiply decades back up to 000's
	totalCostsByDecade[,1] <- totalCostsByDecade[,1] * 10


	# Plot a basic graph of costs
	pdf(file="./figs/fig3_2_australian_natural_disaster_costs_by_decade.pdf")

	# Calculate range from 0 to max value of costs
	c_range <- range(0, totalCostsByDecade + 2000000000)

	y_range <- range(decades)


	plot(totalCostsByDecade, type="h", col="blue", ylim=c_range, axes=TRUE, ann=FALSE) 

	# Add title
	title("FIGURE 3.1: AUSTRLIAN NATURAL DISASTER COSTS BY DECADE, 1967-2013 ", col.main = "blue")

	# Label the x and y axes with dark green text
	title(xlab="Years", col.lab=rgb(0,0.5,0))
	title(ylab="(2013 Dollars in $millions)", col.lab=rgb(0,0.5,0))

	dev.off()
}


## Generate Figure 3.3
average_cost_per_event <- function() {
	# Store the total costs by year
	totalCosts <- totalCostForEvent()
	averageCostPerYear <- with(totalCosts, aggregate(total.normalised, by=list(Year.financial), FUN=safeMean))

	# Plot a basic graph of costs
	pdf(file="./figs/fig3_3_average_cost_per_event.pdf")

	# Calculate range from 0 to max value of costs
	c_range <- range(0, averageCostPerYear + 1000000000)

	y_range <- range(totalCosts$Year.financial)


	plot(averageCostPerYear, type="h", col="blue", ylim=c_range, axes=FALSE, ann=FALSE) 

	axis(1, las=1, at=seq(y_range[1], y_range[2], by=1))

	# Make y axis with horizontal labels that display ticks at 
	billions <- 1000000000 * 0:(c_range[2] / 1000000000)
	axis(2, las=1, at=billions, labels=format(billions / 1000000, big.mark = ","))

	# Add title
	title("FIGURE 3.1: ANNUAL TOTAL COSTS OF DISASTERS IN AUSTRLIA, 1967-2013 ", col.main = "blue")

	# Label the x and y axes with dark green text
	title(xlab="Years", col.lab=rgb(0,0.5,0))
	title(ylab="(2013 Dollars in $millions)", col.lab=rgb(0,0.5,0))

	dev.off()
}

## Generate Figure 3.4
distribution_of_disasters <- function() {

}

## Generate Figure 3.5
annual_insurance_cost_of_disasters <- function() {
	
}


## Generate Figure 3.6
number_of_natural_disasters_in_australia <- function() {
	
}


## Generate Figure 3.7
natural_disasters_between_10_and_75_million <- function() {
	
}


## Generate Figure 3.8
natural_disasters_between_75_and_150_million <- function() {
	
}


## Generate Figure 3.9
number_of_disasters_per_million_people <- function() {
	
}


## Generate Figure 3.10
disaster_costs_by_state_and_territory <- function() {
	
}


## Generate Figure 3.11
number_of_disaster_events_by_state_and_territory <- function() {
	
}


## Generate Figure 3.12
costs_by_type_of_disaster_and_state_and_territory <- function() {
	# Pie chart
}


## Generate Figure 3.13
total_and_insurance_costs_by_disaster_type <- function() {
	
}


## Generate Figure 3.14
number_of_events_by_disaster_type <- function() {
	
}


## Generate Figure 3.15
annual_cost_of_floods_in_australia <- function() {
	
}


## Generate Figure 3.16
total_cost_of_floods_by_decade <- function() {
	
}


## Generate Figure 3.17
annual_number_of_floods_in_australia <- function() {
	
}


## Generate Figure 3.18
annual_cost_of_severe_storms_by_decade <- function() {
	
}


## Generate Figure 3.19
total_cost_of_severe_storms_by_decade <- function() {
	
}


## Generate Figure 3.20
annual_number_of_severe_storms_in_australia <- function() {
	
}


## Generate Figure 3.21
annual_cost_of_cyclones_in_australia <- function() {
	
}


## Generate Figure 3.22
total_cost_of_cyclones_by_decade <- function() {
	
}


## Generate Figure 3.23
annual_number_of_cyclones_causing_more_than_10_million_damage_in_australia <- function() {
	
}


## Generate Figure 3.24
total_cost_of_earthquakes_by_decade <- function() {
	
}


## Generate Figure 3.25
annual_cost_of_bushfires_in_australia <- function() {
	
}


## Generate Figure 3.26
total_cost_of_bushfires_by_decade <- function() {
	
}


## Generate Figure 3.27
annual_number_of_bushfires_in_australia <- function() {
	
}


## Generate Figure 3.28
number_of_natural_disasters_deaths <- function() {
	
}


## Generate Figure 3.29
number_of_natural_disasters_injuries <- function() {
	
}


## Generate Figure 3.30
number_of_deaths_by_decade <- function() {
	
}


## Generate Figure 3.31
cost_of_deaths_and_injuries <- function() {
	
}


## Generate Figure 3.32
cost_of_deaths_and_injuries_by_decade <- function() {
	
}


## Generate Figure 3.33
total_cost_of_natural_disasters <- function() {
	
}


## Generate Figure 3.34
total_cost_of_natural_disasters_by_decade <- function() {
	
}

# Tables

## Generate Table 3.1
average_annual_cost_of_natural_disasters_by_state_and_territory <- function() {
	
}

## Generate Table 3.2
deaths_and_injuries_by_hazard_type <- function() {
	
}



## Helper functions for generating graphs



