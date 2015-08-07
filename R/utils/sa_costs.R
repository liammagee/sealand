
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

# Generate a list of disasters for
saCosts <- function() {
  total.costs <- totalCostForEventFiltered(resource.type.param = NULL, reported.costs.only = TRUE, no.heatwaves = FALSE)

  total.costs$Reported.Cost.interpolated.millions.state.1 <- total.costs$Reported.Cost.interpolated.millions * total.costs$State.1.percent
  total.costs$Reported.Cost.interpolated.millions.state.2 <- total.costs$Reported.Cost.interpolated.millions * total.costs$State.2.percent
  total.costs$Reported.Cost.interpolated.millions.apportioned <- total.costs$Reported.Cost.interpolated.millions.state.1 + total.costs$Reported.Cost.interpolated.millions.state.2

  total.costs$Reported.Cost.indexed.millions.state.1 <- total.costs$Reported.Cost.indexed.millions * total.costs$State.1.percent
  total.costs$Reported.Cost.indexed.millions.state.2 <- total.costs$Reported.Cost.indexed.millions * total.costs$State.2.percent
  total.costs$Reported.Cost.indexed.millions.apportioned <- total.costs$Reported.Cost.indexed.millions.state.1 + total.costs$Reported.Cost.indexed.millions.state.2

  total.costs$Reported.Cost.normalised.millions.state.1 <- total.costs$Reported.Cost.normalised.millions * total.costs$State.1.percent
  total.costs$Reported.Cost.normalised.millions.state.2 <- total.costs$Reported.Cost.normalised.millions * total.costs$State.2.percent
  total.costs$Reported.Cost.normalised.millions.apportioned <- total.costs$Reported.Cost.normalised.millions.state.1 + total.costs$Reported.Cost.normalised.millions.state.2

  total.costs$Reported.Cost.WithDeathsAndInjuries.normalised.millions.state.1 <- total.costs$Reported.Cost.WithDeathsAndInjuries.normalised.millions * total.costs$State.1.percent
  total.costs$Reported.Cost.WithDeathsAndInjuries.normalised.millions.state.2 <- total.costs$Reported.Cost.WithDeathsAndInjuries.normalised.millions * total.costs$State.2.percent
  total.costs$Reported.Cost.WithDeathsAndInjuries.normalised.millions.apportioned <- total.costs$Reported.Cost.WithDeathsAndInjuries.normalised.millions.state.1 + total.costs$Reported.Cost.WithDeathsAndInjuries.normalised.millions.state.2

  sa.costs <- total.costs[total.costs$State.1 == 'South Australia' | total.costs$State.2 == 'South Australia',]
  sa.costs.distilled <- sa.costs[, c(
    "Year.financial",
    "title",
    "description",
    "Reported.Cost.interpolated.millions",
    "Reported.Cost.indexed.millions",
    "Reported.Cost.normalised.millions.apportioned",
    "Reported.Cost.WithDeathsAndInjuries.normalised.millions.apportioned"
    )]
  sa.costs.distilled <- sa.costs.distilled[order(sa.costs.distilled$Year.financial),]
  sa.costs.distilled
  write.table(sa.costs.distilled, file = "./output/sa_disaster_costs.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
  
}

# Generate
runAdHoc <- function() {
  # Set up global options
  useStateNormalisations(FALSE)
  useHeatwaves(TRUE)

  saCosts()

}
runAdHoc()
