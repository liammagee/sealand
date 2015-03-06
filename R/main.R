
# Imports
library(gdata)

# Sources
source("R/figures.R", TRUE)
source("R/functions.R", TRUE)



# Functions

## Ignores "NA" values for standard functions
initialise <- function() {
	# Load the data
	loadData()

	# Generate computed columns
	computeColumns()
}

# Generate
run <- function() {
	# Set up the data
	initialise()

  # Write out the data for checking
  writeEventDataSummary()

	# Run the reports
	annual_total_costs_of_disasters_in_australia_bte()
	annual_total_costs_of_disasters_in_australia_interpolated()
	annual_total_costs_of_disasters_in_australia()
	australian_natural_disaster_costs_by_decade()
	average_cost_per_event()
	distribution_of_disasters()
	annual_insurance_cost_of_disasters()
	number_of_natural_disasters_in_australia()
	natural_disasters_between_10_and_75_million()
	natural_disasters_between_75_and_150_million()
	number_of_disasters_per_million_people()
	disaster_costs_by_state_and_territory()
	number_of_disaster_events_by_state_and_territory()
	costs_by_type_of_disaster_and_state_and_territory()
	total_and_insurance_costs_by_disaster_type()
	number_of_events_by_disaster_type()
	annual_cost_of_floods_in_australia()
	total_cost_of_floods_by_decade()
	annual_number_of_floods_in_australia()
	annual_cost_of_severe_storms_by_decade()
	total_cost_of_severe_storms_by_decade()
	annual_number_of_severe_storms_in_australia()
	annual_cost_of_cyclones_in_australia()
	total_cost_of_cyclones_by_decade()
	annual_number_of_cyclones_causing_more_than_10_million_damage_in_australia()
	total_cost_of_earthquakes_by_decade()
	annual_cost_of_bushfires_in_australia()
	total_cost_of_bushfires_by_decade()
	annual_number_of_bushfires_in_australia()
	number_of_natural_disasters_deaths()
	number_of_natural_disasters_injuries()
	number_of_deaths_by_decade()
	cost_of_deaths_and_injuries()
	cost_of_deaths_and_injuries_by_decade()
	total_cost_of_natural_disasters()
	total_cost_of_natural_disasters_by_decade()
	average_annual_cost_of_natural_disasters_by_state_and_territory()
	deaths_and_injuries_by_hazard_type()
}
run()
