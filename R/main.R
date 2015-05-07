
# Imports
library(gdata)
library(reshape2)

# Sources
source("R/figures.R", TRUE)
source("R/functions.R", TRUE)

# Functions

## Ignores "NA" values for standard functions
initialise <- function(database_file) {
  # Hack to clear the console - http://stackoverflow.com/questions/14260340/function-to-clear-the-console-in-r
  cat("\014")  
  
	# Load the data
  loadData(database_file)

	# Generate computed columns
	computeColumns()
}

# Generate
run <- function() {
	# Set up the data
  database_file = "./data/database_25042015.xlsx"
  initialise(database_file)

  # Write out the data for checking
  writeEventDataSummary()

	# Run the reports
	#annual_total_costs_of_disasters_in_australia_bte()
	#annual_total_costs_of_disasters_in_australia_interpolated()
	total_costs_of_disasters_in_australia()
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
	total_cost_of_natural_disasters_synthetic()
	total_cost_of_natural_disasters_by_decade_synthetic()
	total_deaths_as_percentage_of_pop()
	total_cost_as_percentage_of_gdp()
	insured_cost_as_percentage_of_total_cost()
	total_costs_raw_indexed_normalised()
	average_annual_cost_of_natural_disasters_by_state_and_territory()
	deaths_and_injuries_by_hazard_type()
	multipliers_joy_vs_derived()
  costs_by_year_and_state()
  cost_summary()
}
run()
