
# Install dependencies
.pkgs = c("gdata", "reshape2", "scales", "ggplot2", "ggthemes")

# Install required packages from CRAN (if not)
.inst <- .pkgs %in% installed.packages()
if(length(.pkgs[!.inst]) > 0) install.packages(.pkgs[!.inst])


# Imports
library(gdata)
library(reshape2)

# Sources
source("R/figures.R", FALSE)
source("R/functions.R", FALSE)

# Set up the data
database.file = "./data/database_11072015.xlsx"


## Ignores "NA" values for standard functions
initialise <- function(database.file) {
  # Hack to clear the console - http://stackoverflow.com/questions/14260340/function-to-clear-the-console-in-r
  cat("\014")

  # Load the data
  loadData(database.file)

  # Generate computed columns
  computeColumns()
}

# Generate
run <- function() {
  # Set up global options
  useStateNormalisations(FALSE)
  useHeatwaves(TRUE)

  # Clear the main data object
  if (exists("ecnd.database")) {
    rm(ecnd.database)
  }
  initialise(database.file)

  # Write out the data for checking
  writeEventDataSummary()

  # Run the reports
  #annualTotalCostsOfDisastersInAustraliaBTE()
  #annualTotalCostsOfDisastersInAustraliaInterpolated()
  totalCostsOfDisastersInAustralia()
  annualTotalCostsOfDisastersInAustralia()
  australianNaturalDisasterCostsByDecade()
  averageCostPerEvent()
  distributionOfDisasters()
  annualInsuranceCostOfDisasters()
  numberOfNaturalDisastersInAustralia()
  numberOfDisastersPerMillionPeople()
  disasterCostsByStateAndTerritory()
  numberOfDisasterEventsByStateAndTerritory()
  costsByTypeOfDisasterAndStateAndTerritory()
  totalAndInsuranceCostsByDisasterType()
  numberOfEventsByDisasterType()
  annualCostOfFloodsInAustralia()
  totalCostOfFloodsByDecade()
  annualNumberOfFloodsInAustralia()
  annualCostOfSevereStormsByDecade()
  totalCostOfSevereStormsByDecade()
  annualNumberOfSevereStormsInAustralia()
  annualCostOfCyclonesInAustralia()
  totalCostOfCyclonesByDecade()
  annualNumberOfCyclonesCausingMoreThan10MillionDamageInAustralia()
  totalCostOfEarthquakesByDecade()
  annualCostOfBushfiresInAustralia()
  totalCostOfBushfiresByDecade()
  annualNumberOfBushfiresInAustralia()
  numberOfNaturalDisastersDeaths()
  numberOfNaturalDisastersInjuries()
  numberOfDeathsByDecade()
  costOfDeathsAndInjuries()
  costOfDeathsAndInjuriesByDecade()
  totalCostOfNaturalDisasters()
  totalCostOfNaturalDisastersByDecade()
  totalCostOfNaturalDisastersSynthetic()
  totalCostOfNaturalDisastersByDecadeSynthetic()
  totalDeathsAsPercentageOfPop()
  totalCostAsPercentageOfGdp(state = NULL, fatalities = FALSE)
  totalCostAsPercentageOfGdp(state = NULL, fatalities = TRUE)
  insuredCostAsPercentageOfTotalCost()
  totalCostsRawIndexedNormalised()
  totalAverageCostsNationallyAndByState()
  totalCostsQldNswVic()
  deathsAndInjuriesByHazardType()
  multipliersJoyVsDerived()
  costsByYearAndState()
  averageAnnualCostOfNaturalDisastersByStateAndTerritory()
  costSummary()
}
run()
