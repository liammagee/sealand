
# Imports
library(gdata)
library(Kendall)

# Global options
options(stringsAsFactors=F)
options(scipen=999)


# Functions

## Ignores "NA" values for standard functions
safeSum <- function(value) sum(value, na.rm = TRUE)
safeMean <- function(value) mean(value, na.rm = TRUE)

## Parses a currency value in the form "$1,000,000"
parseCurrency <- function(value) as.numeric(gsub(",", "", sub("\\$","", value)))

## Return an abbreviated state
abbreviateState <- function(value) {
  if (value == "Victoria") {
    "VIC"
  } else if (value == "New South Wales") {
    "NSW"
  } else if (value == "Tasmania") {
    "TAS"
  } else if (value == "Western Australia") {
    "WA"
  } else if (value == "Queensland") {
    "QLD"
  } else if (value == "Australian Capital Territory") {
    "ACT"
  } else if (value == "Northern Territory") {
    "NT"
  } else if (value == "South Australia") {
    "SA"
  } else  {
    "Other"
  }
}
# Helper function - zero out NAs
zeroNA <- function(data) {
  data[is.na(data)] <- 0
}

# Helper method - taken from http://www.ozforex.com.au/forex-tools/historical-rate-tools/yearly-average-rates
exchangeUSD <- function(year, amount) {
  rates <- list(
    "1990" = 0.780686,
    "1991" = 0.778719,
    "1992" = 0.735213,
    "1993" = 0.679935,
    "1994" = 0.731612,
    "1995" = 0.740727,
    "1996" = 0.782833,
    "1997" = 0.743682,
    "1998" = 0.629118,
    "1999" = 0.645484,
    "2000" = 0.581223,
    "2001" = 0.517943,
    "2002" = 0.543050,
    "2003" = 0.654801,
    "2004" = 0.736399,
    "2005" = 0.762474,
    "2006" = 0.753457,
    "2007" = 0.838635,
    "2008" = 0.853159,
    "2009" = 0.792116,
    "2010" = 0.919691,
    "2011" = 1.033853,
    "2012" = 1.035937,
    "2013" = 0.967915,
    "2014" = 0.902813,
    "2015" = 0.800343
  )
  return (amount * rates[[as.character(year)]])
}

## Returns the correct financial year for a month and year
financialYear <- function(range) {
  month <- range[1]
  year <- range[2]
  firstMonths = c("January", "February", "March", "April", "May", "June")
  finYear <- if (is.element(month, firstMonths)) as.numeric(year) else as.numeric(year) + 1
  return(as.numeric(finYear))
}

## Generates a population value for a given year
popForYear <- function(year, state = NULL) {
  year <- as.numeric(year)
  if ((year - 1967) < 14) {
    popRow <- 10 + (year - 1967)
  } else {
    popRow <- 24 + (year - 1981) * 4
  }

  # Obtain column reference
  state.col.ref <- switch(state, 
    "New South Wales" = 20,
    "Victoria" = 21,
    "Queensland" = 22,
    "South Australia" = 23,
    "Western Australia" = 24,
    "Tasmania" = 25,
    "Northern Territory" = 26,
    "Australian Capital Territory" = 27
  )
  if (is.na(state)) {
    state.col.ref <- 28
  }

  popValue <- as.numeric(pop.data[popRow, state.col.ref])
  return (popValue)
}

## Generates a population ratio (based on June 2013)
popRatio <- function(baseYear, state = NULL) {
  popTest <- popForYear(baseYear, state)
  pop2013 <- popForYear(2013, state)
  return(pop2013 / popTest)
}

## Generates a cpi ratio (based on June 2013)
cpiRatio <- function(baseYear, state = NULL) {
  cpiRow <- 85 + (baseYear - 1967) * 4
  cpiTest <- as.numeric(cpi.data$Index.Numbers....All.groups.CPI....Australia[cpiRow])
  cpi2013 <- as.numeric(cpi.data$Index.Numbers....All.groups.CPI....Australia[269])
  if ( is.na( cpiTest ) )   {
    cpiTest <- cpi2013
  }
  return(cpi2013 / cpiTest)
}

## Generates a gdp ratio based on chain volume measures (based on June 2013)
gdpRatio <- function(baseYear, state = NULL) {
  gdpRow <- 41 + (baseYear - 1967) * 4
  gdpTest <- as.numeric(gdp.data$Gross.domestic.product..Chain.volume.measures..[gdpRow])
  gdp2013 <- as.numeric(gdp.data$Gross.domestic.product..Chain.volume.measures..[225])
  if ( is.na( gdpTest ) )   {
    gdpTest <- gdp2013
  }
  gdpGross <- (gdp2013 / gdpTest)
  # Correct for Pop ratio to correct for per capita
  gdpGross <- gdpGross / popRatio(baseYear)
  return(gdpGross)
}

## Generates a gdp ratio based on chain volume measures (based on June 2013)
gdpNominalRatio <- function(baseYear, state = NULL) {
  gdp.national.row <- 17 + (baseYear - 1967) * 1
  gdp.national.test <- gdpNominalValues(baseYear)
  gdp.national.2013 <- gdpNominalValues(2013)
  if ( is.na( gdp.national.test ) )   {
    gdp.national.test <- gdp2013
  }
  gdp.gross <- ( gdp.national.2013 / gdp.national.test )
  return( gdp.gross )
}

gdpValues <- function(baseYear, state = NULL) {
  gdpRow <- 41 + (baseYear - 1967) * 4
  gdpValue <- as.numeric(gdp.data$Gross.domestic.product..Chain.volume.measures..[gdpRow])
  return(gdpValue)
}

gdpNominalValues <- function(baseYear, state = NULL) {
  gdp.national.row <- 17 + (baseYear - 1967) * 1
  gdp.national.value <- as.numeric(gdp.national.data$GROSS.DOMESTIC.PRODUCT..Current.prices..[gdp.national.row])
  return(gdp.national.value)
}

## Combined CPI, population and GDP ratio
combinedRatio <- function(baseYear, state = NULL) {
  return (cpiRatio(baseYear, state) * popRatio(baseYear, state) * gdpRatio(baseYear, state))
}

## Generates an CPI indexed cost (based on June 2013)
indexCosts <- function(range) {
  baseYear <- range[1]
  insuredCost <- range[2]
  state <- range[3]
  return(insuredCost * cpiRatio(baseYear, state))
}

## Normalise cost - TODO: this needs to be much more robust (cf. discussion on normalisation)
normalisedCosts <- function(range) {
  baseYear <- range[1]
  cost <- range[2]
  state <- range[3]
  # Normalise for [1] inflation; [2] population growth; [3] wealth increase (GDP as a temporary proxy)
  # TODO: Add at least state-based equivalents
  return(cost * cpiRatio(baseYear, state) * popRatio(baseYear, state) * gdpRatio(baseYear, state))
}

## Normalise cost - TODO: this needs to be much more robust (cf. discussion on normalisation)
normalisedCostsWithoutIndexation <- function(range) {
  baseYear <- range[1]
  cost <- range[2]
  state <- range[3]
  return(cost * popRatio(baseYear, state) * gdpRatio(baseYear, state))
}

## Normalise population
normalisedPopulation <- function(range) {
  baseYear <- as.numeric(range[1])
  pop <- as.numeric(range[2])
  state <- range[3]
  # Normalise for inflation
  return(pop * popRatio(baseYear, state))
}

## Load data
loadData <- function(database.file) {
  # Clear the main data object
  if (exists('ecnd.database')) {
    rm(ecnd.database)
  }
  
  perl <- 'D:/strawberry/perl/bin/perl.exe'
  # ecnd.database <<- read.xls("./data/database.xlsx", 2, perl = perl)
  # # Hack to ignore any rows without a year value - such as rows added for computation
  # ecnd.database <<- ecnd.database[!is.na(ecnd.database$Year), ]
  # cpi <<- read.xls("./data/cpi.xlsx", 2, perl = perl)
  # pop.data <<- read.xls("./data/pop_consolidate.xlsx", 1, perl = perl)
  # gdp <<- read.xls("./data/5206001_key_aggregates.xlsx", 2, perl = perl)


  # MAC VERSION
  ecnd.database <<- read.xls(database.file, 2)
  # Hack to ignore any rows without a year value - such as rows added for computation
  ecnd.database <<- ecnd.database[!is.na(ecnd.database$Year), ]
  print(paste("Read in ", length(ecnd.database$Year), " rows."))
  cpi.data <<- read.xls("./data/cpi.xlsx", 2)
  pop.data <<- read.xls("./data/pop_consolidate.xlsx", 1)
  gdp.data <<- read.xls("./data/5206001_key_aggregates.xlsx", 2)
  gdp.national.data <<- read.xls("./data/5204001_key_national_aggregates.xlsx", 2)
}


cleanData <- function() {

  # ... for cleaned up costs
  ecnd.database$Insured.Cost.cleaned <<- apply(data.matrix(ecnd.database$Insured.Cost), 1, parseCurrency)

  # ... for cleaned up states
  ecnd.database$State.abbreviated.1 <<- apply(data.matrix(ecnd.database$State.1), 1, abbreviateState)
  ecnd.database$State.abbreviated.2 <<- apply(data.matrix(ecnd.database$State.2), 1, abbreviateState)

  # ... for financial years
  ecnd.database$Year.financial <<- apply(ecnd.database[c("Month", "Year")], 1, financialYear)

}

# Interpolate reported costs, based on the relationship between Insured and Reported costs.
interpolateReportedCosts <- function() {
  ag <- generateDerivedMultipliers()
  data.Reported.Cost.na <- ecnd.database[is.na(ecnd.database$Reported.Cost),]
  ecnd.database <<- merge(ecnd.database[, ], ag[,c("resourceType", "Event.Factor")], by="resourceType", all.x = TRUE)
  ecnd.database$Reported.Cost.interpolated <<- ecnd.database$Reported.Cost
  # Interpolate reported cost based on event multiplier * insured cost
  ecnd.database[is.na(ecnd.database$Reported.Cost.interpolated), ]$Reported.Cost.interpolated <<- ecnd.database[is.na(ecnd.database$Reported.Cost.interpolated), ]$Insured.Cost * ecnd.database[is.na(ecnd.database$Reported.Cost.interpolated), ]$Event.Factor
}

normaliseInsuredCost <- function() {
  # ... for CPI-indexed insured costs
  ecnd.database$Insured.Cost.indexed <<- apply(ecnd.database[c("Year.financial", "Insured.Cost.cleaned")], 1, indexCosts)
  # ... for normalised insured costs
  ecnd.database$Insured.Cost.normalised <<- apply(ecnd.database[c("Year.financial", "Insured.Cost.cleaned")], 1, normalisedCosts)
  ecnd.database$Insured.Cost.normalised.millions <<- ecnd.database$Insured.Cost.normalised / 1000000
}

normaliseReportedCost <- function() {
  # ... for CPI-indexed reported costs
  ecnd.database$Reported.Cost.indexed <<- apply(ecnd.database[c("Year.financial", "Reported.Cost.interpolated")], 1, indexCosts)
  # ... for normalised insured costs
  ecnd.database$Reported.Cost.normalised <<- apply(ecnd.database[c("Year.financial", "Reported.Cost.interpolated")], 1, normalisedCosts)
}

normaliseDeathsAndInjuries <- function() {
  # ... for population-inflated deaths and injuries
  ecnd.database$Deaths.normalised <<- apply(ecnd.database[c("Year.financial", "Deaths")], 1, normalisedPopulation)
  ecnd.database$Injuries.normalised <<- apply(ecnd.database[c("Year.financial", "Injuries")], 1, normalisedPopulation)
}



# Interpolate reported costs, based on:
# 1. The relationship between Insured and Reported costs.
# 2. Death and injuries
interpolateNormalisedReportedCosts <- function() {
  ecnd.database[is.na(ecnd.database$Reported.Cost.interpolated), ]$Reported.Cost.interpolated <<- ecnd.database[is.na(ecnd.database$Reported.Cost.interpolated), ]$Insured.Cost * ecnd.database[is.na(ecnd.database$Reported.Cost.interpolated), ]$Event.Factor
}

## Generate computed columns
computeColumns <- function() {

  # First convert dollars to a basic count
  adjustDollarsToCounts()

  # Clean data
  cleanData()

  # Interpolate reported costs
  interpolateReportedCosts()
  
  # Index and normalise costs
  normaliseInsuredCost()
  normaliseReportedCost()
  normaliseDeathsAndInjuries()

  # Index and normalise costs
  interpolateNormalisedReportedCosts()

  # Do interpollation
  interpollateSyntheticCosts()

  # (Optionally) copy the interpollated values
  swapInterpollatedForNormalCosts()
}



###########################################
## Specific cost estimation functions
###########################################


### Cost of public services
costOfPublicServiceCalls <- function(events) {
  # Cost per call? Say $10 - TODO: NEEDS BETTER EVIDENCE
  events$Calls.to.SES * 0
}

### Intangibles
#### Cost of life
costOfLife <- function() {
  # 2006 BTE figure, adjusted to 2013
  # return(indexCosts(c(2006, 2400000)))
  # 2008 Best Practice Regulation - Value of Statistical Life
  # return(indexCosts(c(2008, 3500000)))
  # Victorian Guidance Note on Dam Safety Decision DSE 2012
  #return(indexCosts(c(2012, 4500000)))
  # 2014 Office of Best Practice Regulation
  return(indexCosts(c(2014, 4200000)))
}

costOfHospitalisedInjury <- function() {
  # 2006 BTE figure, adjusted to 2013
  # return(indexCosts(c(2006, 214000)))
  # Deloitte 2013, from BTE 2001.
  return(indexCosts(c(1999, 853000)))
}
costOfNonHospitalisedInjury <- function() {
  # 2006 BTE figure, adjusted to 2013
  # return(indexCosts(c(2006, 2200)))
  # Deloitte 2013, from BTE 2001.
  return(indexCosts(c(1999, 29000)))
}
## Returns the proportion of hospitalised injury to overall injuries
proportionOfHospitalisedInjury <- function() {
  # SOURCED FROM DELOITTE'S FIGURES
  # 0.33
  # NRMA
  0.126
}


# Costs

## Get raw events, without data cleaning
getRawEvents <- function(resourceTypeParam = NULL) {

  events <- ecnd.database[c(
    "Year.financial",
    "Year",
    "title",
    "resourceType",
    "State.1",
    "State.abbreviated.1",
    "State.1.percent",
    "State.2",
    "State.abbreviated.2",
    "State.2.percent",
    "Evacuated",
    "Homeless",
    "Calls.to.SES",
    "Deaths",
    "Deaths.normalised",
    "Injuries",
    "Injuries.normalised",
    "Minor",
    "Severe",
    "Insured.Cost",
    "Insured.Cost.indexed",
    "Insured.Cost.normalised",
    "Insured.Cost.normalised.millions",
    "Reported.Cost",
    "Reported.Cost.interpolated",
    "Reported.Cost.indexed",
    "Reported.Cost.normalised",
    "Assistance_numbers",
    "Assistance_dollars",
    "Infrastructure_Public_Destroyed_Count",
    "Infrastructure_Public_Damaged_Count",
    "Infrastructure_Public_Destroyed_Count_Roads_Urban",
    "Infrastructure_Public_Damaged_Count_Roads_Urban",
    "Infrastructure_Public_Destroyed_Count_Roads_Rural",
    "Infrastructure_Public_Damaged_Count_Roads_Rural",
    "Infrastructure_Public_Destroyed_Count_Bridges",
    "Infrastructure_Public_Damaged_Count_Bridges",
    "Infrastructure_Public_Destroyed_Count_Rail",
    "Infrastructure_Public_Destroyed_Dollars_Rail",
    "Infrastructure_Public_Damaged_Count_Rail",
    "Infrastructure_Public_Destroyed_Count_Power_Poles",
    "Infrastructure_Public_Damaged_Count_Power_Poles",
    "Buildings_Commercial_Destroyed_Count",
    "Buildings_Commercial_Damaged_Count",
    "Buildings_Commercial_Destroyed_Count_General",
    "Buildings_Commercial_Damaged_Count_General",
    "Buildings_Commercial_Destroyed_Count_Industrial",
    "Buildings_Commercial_Damaged_Count_Industrial",
    "Buildings_Commercial_Destroyed_Count_Hotels",
    "Buildings_Commercial_Damaged_Count_Hotels",
    "Buildings_Private_Destroyed_Count",
    "Buildings_Private_Damaged_Count",
    "Buildings_Public_Destroyed_Count",
    "Buildings_Public_Destroyed_Type",
    "Buildings_Public_Damaged_Count",
    "Buildings_Public_Damaged_Type",
    "Vehicle_Public_Destroyed_Count",
    "Vehicle_Public_Destroyed_Count_Aircraft",
    "Vehicle_Public_Destroyed_Count_Train",
    "Vehicle_Public_Damaged_Count",
    "Vehicle_Public_Damaged_Count_Aircraft",
    "Vehicle_Public_Damaged_Count_Train",
    "Vehicle_Private_Destroyed_Count",
    "Vehicle_Private_Destroyed_Count_Boats",
    "Vehicle_Private_Destroyed_Count_Cars",
    "Vehicle_Private_Destroyed_Count_Caravans",
    "Vehicle_Private_Damaged_Count",
    "Vehicle_Private_Damaged_Count_Boats",
    "Vehicle_Private_Damaged_Count_Cars",
    "Vehicle_Private_Damaged_Count_Caravans",

    "Infrastructure_Private_Destroyed_Count",
    "Infrastructure_Private_Damaged_Count",
    "Land_Public_Count",
    "Land_Private_Count",
    "Crops_Destroyed_Count",
    "Crops_Destroyed_Units",
    "Crops_Destroyed_Type",
    "Livestock_Destroyed_Count",
    "Livestock_Destroyed_Count_Cattle",
    "Livestock_Destroyed_Count_Sheep_Goats",
    "Livestock_Destroyed_Count_Poultry",
    "Livestock_Destroyed_Count_Pigs",
    "Livestock_Destroyed_Count_Other",
    "Environmental_Count",
    "Fencing",

    "Evacuated.i",
    "Homeless.i",
    "Calls.to.SES.i",
    "Assistance_dollars.i",
    "Buildings_Commercial_Destroyed_Count.i",
    "Buildings_Commercial_Damaged_Count.i",
    "Buildings_Private_Destroyed_Count.i",
    "Buildings_Private_Damaged_Count.i",
    "Buildings_Public_Destroyed_Count.i",
    "Buildings_Public_Damaged_Count.i",
    "Land_Public_Count.i",
    "Land_Private_Count.i",
    "Crops_Destroyed_Count.i",
    "Livestock_Destroyed_Count.i",
    "Environmental_Count.i"
    )
  ]
  if (! is.null(resourceTypeParam)) {
    events <- subset(events, resourceType == resourceTypeParam)
  }
  events <- events[events$Year.financial <= 2013,]
  return (events)
}


## Get all events for the purpose of generating costs
getEvents <- function(resourceTypeParam = NULL) {
  events <- getRawEvents(resourceTypeParam)

  events$Deaths <- as.numeric(events$Deaths)
  events$Injuries <- as.numeric(events$Injuries)
  events$Deaths.normalised <- as.numeric(events$Deaths.normalised)
  events$Injuries.normalised <- as.numeric(events$Injuries.normalised)
  xsub <- events[,9:27]
  xsub[is.na(xsub)] <- 0
  events[,9:27]<-xsub
  return (events)
}


# Calculate cost of housing contents
costOfResidentialBuildingContents <- function() {
  # Use an average cost, as per  http://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/4102.0Main+Features10Dec+2011#Contents5
  return (indexCosts(c(2010, 61000)))
}

# Calculate cost of housing
costOfResidentialBuilding <- function(events) {
  destroyed <- numericise(events$Buildings_Private_Destroyed_Count)
  damaged <- numericise(events$Buildings_Private_Damaged_Count)

  # From Methodology chapter; 2014 $470,318 adjusted to 2013 dollars
  weightedAverageConstructionCostPerResidence <- 469536.015
  damagedRate <- 0.25
  destroyedCostStructure <- destroyed * weightedAverageConstructionCostPerResidence
  damagedCostStructure <- damaged * weightedAverageConstructionCostPerResidence * damagedRate
  structureCosts <- destroyedCostStructure + damagedCostStructure

  destroyedCostContents <- destroyed * costOfResidentialBuildingContents()
  damagedCostContents <- destroyed * costOfResidentialBuildingContents() * damagedRate
  contentsCosts <- destroyedCostContents + damagedCostContents

  return (structureCosts + contentsCosts)
}


# Calculate cost of commerial buildings (General)
costOfCommercialBuildingsGeneral <- function(events) {
  destroyedGeneral <- as.numeric(events$Buildings_Commercial_Destroyed_Count_General)
  damagedGeneral <- as.numeric(events$Buildings_Commercial_Damaged_Count_General)
  destroyedGeneral[is.na(destroyedGeneral)] <- 0
  damagedGeneral[is.na(damagedGeneral)] <- 0

  damagedRate <- 0.25
  generalPerBuildingCost <- 431232
  costsGeneralStructural <- destroyedGeneral * generalPerBuildingCost + 
                            damagedGeneral * generalPerBuildingCost * damagedRate
  costsGeneralContents <- 0

  return (costsGeneralStructural + costsGeneralContents)
}

# Calculate cost of commerial buildings (Industrial)
costOfCommercialBuildingsIndustrial <- function(events) {
  destroyedIndustrial <- as.numeric(events$Buildings_Commercial_Destroyed_Count_Industrial)
  damagedIndustrial <- as.numeric(events$Buildings_Commercial_Damaged_Count_Industrial)
  destroyedIndustrial[is.na(destroyedIndustrial)] <- 0
  damagedIndustrial[is.na(damagedIndustrial)] <- 0

  damagedRate <- 0.25
  industrialPerBuildingCost <- 498750
  costsIndustrialStructural <- destroyedIndustrial * industrialPerBuildingCost + 
                                damagedIndustrial * industrialPerBuildingCost * damagedRate
  costsIndustrialContents <- 0

  return (costsIndustrialStructural + costsIndustrialContents)
}

# Calculate cost of commerial buildings (Hotels)
costOfCommercialBuildingsHotels <- function(events) {
  destroyedHotels <- as.numeric(events$Buildings_Commercial_Destroyed_Count_Hotels)
  damagedHotels <- as.numeric(events$Buildings_Commercial_Damaged_Count_Hotels)
  destroyedHotels[is.na(destroyedHotels)] <- 0
  damagedHotels[is.na(damagedHotels)] <- 0

  damagedRate <- 0.25
  hotelsPerBuildingCost <- indexCosts(c(2005, 3885000))
  costsHotelsStructural <- destroyedHotels * hotelsPerBuildingCost + 
                            damagedHotels * hotelsPerBuildingCost * damagedRate
  costsHotelsContents <- 0

  return (costsHotelsStructural + costsHotelsContents)
}

# Calculate cost of commerial buildings
costOfCommercialBuildings <- function(events) {
  costsGeneral <- costOfCommercialBuildingsGeneral(events)
  costsIndustrial <- costOfCommercialBuildingsIndustrial(events)
  costsHotels <- costOfCommercialBuildingsHotels(events)

  return (costsGeneral + costsIndustrial + costsHotels)
}


# Determines the unit cost of a public building
publicBuildingUnitCostDestroyed <- function(buildingType) {
  unitCost <- switch(buildingType,
    "School" = indexCosts(c(2011, 14001152)),
    "Church" = indexCosts(c(2010, 4082351)),
    "Community centre" = indexCosts(c(2011, 17350000)),
    "Hospital" = indexCosts(c(2014, 35743396))
  )
  if (is.null(unitCost)) {
    unitCost <- 0
  }
  return (unitCost)
}
# Determines the unit cost of a public building
publicBuildingUnitCostDamaged <- function(buildingType) {
  unitCost <- switch(buildingType,
    "School" = indexCosts(c(2011, 14001152)) * 0.25,
    "Church" = indexCosts(c(2013, 1500000)),
    "Community centre" = indexCosts(c(2011, 17350000)) * 0.25,
    "Hospital" = indexCosts(c(2014, 35743396)) * 0.25
  )
  if (is.null(unitCost)) {
    unitCost <- 0
  }
  return (unitCost)
}

# Calculate cost of public buildings
costOfPublicBuildings <- function(events) {
  destroyed <- numericise(events$Buildings_Public_Destroyed_Count)
  destroyedType <- events$Buildings_Public_Destroyed_Type
  damaged <- numericise(events$Buildings_Public_Damaged_Count)
  damagedType <- events$Buildings_Public_Damaged_Type

  destroyedUnitCost <- apply(data.frame(destroyedType), 1, publicBuildingUnitCostDestroyed)
  damagedUnitCost <- apply(data.frame(damagedType), 1, publicBuildingUnitCostDamaged)

  costOfStructures <- destroyed * destroyedUnitCost + damaged * damagedUnitCost
  costOfContents <- 0

  return (costOfStructures + costOfContents)
}


# Calculate cost of infrastructure
costOfInfrastructure <- function(events) {
  destroyedRoadsUrban <- numericise(events$Infrastructure_Public_Destroyed_Count_Roads_Urban)
  damagedRoadsUrban <- numericise(events$Infrastructure_Public_Damaged_Count_Roads_Urban)
  destroyedRoadsRural <- numericise(events$Infrastructure_Public_Destroyed_Count_Roads_Rural)
  damagedRoadsRural <- numericise(events$Infrastructure_Public_Damaged_Count_Roads_Rural)
  destroyedBridges <- numericise(events$Infrastructure_Public_Destroyed_Count_Bridges)
  damagedBridges <- numericise(events$Infrastructure_Public_Damaged_Count_Bridges)
  destroyedRail <- numericise(events$Infrastructure_Public_Destroyed_Count_Rail)
  destroyedRailDollars <- numericise(events$Infrastructure_Public_Destroyed_Dollars_Rail)
  damagedRail <- numericise(events$Infrastructure_Public_Damaged_Count_Rail)
  destroyedPowerPoles <- numericise(events$Infrastructure_Public_Destroyed_Count_Power_Poles)
  damagedPowerPoles <- numericise(events$Infrastructure_Public_Damaged_Count_Power_Poles)

  if (is.null(destroyedRailDollars)) {
    destroyedRailDollars <- destroyedRail * indexCosts(c(2009, 3904406))
  }

  destroyedCosts <-  destroyedRoadsUrban * indexCosts(c(2003, 1633993)) + 
            destroyedRoadsRural * indexCosts(c(2003, 573669)) + 
            destroyedBridges * indexCosts(c(2006, 4214400)) + 
            destroyedRailDollars + 
            destroyedPowerPoles * indexCosts(c(2013, 9000)) 

  damagedCosts <-  damagedRoadsUrban * indexCosts(c(2011, 2168975)) + 
            damagedRoadsRural * indexCosts(c(2011, 666600)) + 
            damagedBridges * indexCosts(c(2006, 4214400)) + 
            damagedRail * indexCosts(c(2007, 1300000)) + 
            damagedPowerPoles * indexCosts(c(2013, 9000)) 

  costs <- destroyedCosts + damagedCosts

  return (costs)
}


# Calculate cost of transport
costOfVehicles <- function(events) {
  destroyedAircraft <- as.numeric(events$Vehicle_Public_Destroyed_Count_Aircraft)
  damagedAircraft <- as.numeric(events$Vehicle_Public_Damaged_Count_Aircraft)
  destroyedTrains <- as.numeric(events$Vehicle_Public_Destroyed_Count_Train)
  damagedTrains <- as.numeric(events$Vehicle_Public_Damaged_Count_Train)
  destroyedCars <- as.numeric(events$Vehicle_Private_Destroyed_Count_Cars)
  damagedCars <- as.numeric(events$Vehicle_Private_Damaged_Count_Cars)
  destroyedBoats <- as.numeric(events$Vehicle_Private_Destroyed_Count_Boats)
  damagedBoats <- as.numeric(events$Vehicle_Private_Damaged_Count_Boats)
  destroyedCaravans <- as.numeric(events$Vehicle_Private_Destroyed_Count_Caravans)
  damagedCaravans <- as.numeric(events$Vehicle_Private_Damaged_Count_Caravans)

  publicVehicles <- destroyedAircraft * indexCosts(c(2015, 2115832)) +
                    damagedAircraft * indexCosts(c(2015, 2115832)) * 0.2 +
                    destroyedTrains  * indexCosts(c(2011, 15800000)) +
                    damagedTrains  * indexCosts(c(2015, 2115832)) * 0.2
  privateVehicles <- destroyedCars * indexCosts(c(2010, 18000)) +
                    damagedCars * indexCosts(c(2008, 3853)) +
                    destroyedBoats * indexCosts(c(2014, 110000)) +
                    damagedBoats * indexCosts(c(2014, 110000)) * 0.2 +
                    destroyedCaravans * indexCosts(c(2014, 30000)) +
                    damagedCaravans * indexCosts(c(2014, 30000)) * 0.2

  total <- publicVehicles + privateVehicles

  return (total)
}


# Get the cost of specific crop types
costOfCropType <- function(cropType) {
  cropTypeCost <- 0
  if (!is.null(cropType)) {
    cropTypeCost <- switch(cropType,
                           "Cotton" = indexCosts(c(2011, 460)),
                           "Wheat and Barley" = indexCosts(c(2013, 570.67)),
                           "Cereal and fruit" = indexCosts(c(2013, 0)), # TBD
                           "Pasture and lumber" = indexCosts(c(2013, 0)), # TBD
                           "Pasture" = indexCosts(c(2013, 60)),
                           "Sugar cane" = indexCosts(c(2013, 40)),
                           "Fruit" = indexCosts(c(2013, 40000))
    )
  }
  if (is.null(cropTypeCost)) {
    cropTypeCost <- 0
  }
  return (cropTypeCost)
}

numericise <- function(data) {
  data <- as.numeric(data)
  data[is.na(data)] <- 0
  return (data)
}

# Calculate cost of agriculture
costOfAgriculture <- function(events) {
  destroyedSheds <- numericise(events$Infrastructure_Private_Destroyed_Count)
  damagedSheds <- numericise(events$Infrastructure_Private_Damaged_Count)
  fencing <- numericise(events$Fencing)
  land <- numericise(events$Land_Private_Count)
  crops <- numericise(events$Crops_Destroyed_Count)
  cropType <- events$Crops_Destroyed_Type
  cropCosts <- crops * apply(data.matrix(events$Crops_Destroyed_Type), 1, costOfCropType)

  livestock <- numericise(events$Livestock_Destroyed_Count)
  cattle <- numericise(events$Livestock_Destroyed_Count_Cattle)
  sheepGoats <- numericise(events$Livestock_Destroyed_Count_Sheep_Goats)
  poultry <- numericise(events$Livestock_Destroyed_Count_Poultry)
  pigs <- numericise(events$Livestock_Destroyed_Count_Pigs)
  other <- numericise(events$Livestock_Destroyed_Count_Other)

  livestockCosts <- cattle * indexCosts(c(2012, 788)) +
                    sheepGoats * indexCosts(c(2012, 87)) +
                    poultry * indexCosts(c(2009, 50)) +
                    pigs * indexCosts(c(2015, 200)) +
                    other * indexCosts(c(2007, 771.50))

  total <- destroyedSheds * indexCosts(c(2015, 18975)) +
          damagedSheds * indexCosts(c(2015, 18975)) * 0.4 +
          fencing * indexCosts(c(2014, 11600)) +
          land * exchangeUSD(2005, indexCosts(c(2005, 2067)))  +
          cropCosts +
          livestockCosts

  return (total)
}



# Calculate cost of transport
costOfTransport <- function(events) {

  return (0)
}


# Calculate direct costs, via component-wise computation
computedDirectCosts <- function(events) {
  computedDirectCosts <- 0

  # Add various cost components
  computedDirectCosts <- computedDirectCosts + costOfResidentialBuilding(events)
  computedDirectCosts <- computedDirectCosts + costOfCommercialBuildings(events)
  computedDirectCosts <- computedDirectCosts + costOfPublicBuildings(events)
  computedDirectCosts <- computedDirectCosts + costOfInfrastructure(events)
  computedDirectCosts <- computedDirectCosts + costOfVehicles(events)
  computedDirectCosts <- computedDirectCosts + costOfAgriculture(events)
  # computedDirectCosts <- computedDirectCosts + costOfTransport(events)

  events$directCost <- computedDirectCosts

  # Normalised values
  # THESE FIGURES ARE ALREADY NORMALISED
  # events$directCost.normalised <- apply(events[c("Year.financial", "directCost")], 1, normalisedCosts)
  events$directCost.normalised <- apply(events[c("Year.financial", "directCost")], 1, normalisedCostsWithoutIndexation)

  return (events)
}



# Generate a proportion of industrial to total commercial property
industrialProportionOfCommercial <- function() {
  # What is a reasonable ratio of industrial to commercial? Assuming 50% for now.
 return (0.5)
}


# Emergency response
emergencyResponseCosts <- function(events) {
  assistance <- as.numeric(events$Assistance_numbers)

  calls <- costOfPublicServiceCalls(events)
  assistanceCosts <- assistance * 24

  total <- calls + assistanceCosts

  return (total)
}


# Residential disruption
householdDisruptionCosts <- function(events) {
  # Get key fields
  assistance <- as.numeric(events$Assistance_numbers)
  evacuated <- as.numeric(events$Evacuated)
  homeless <- as.numeric(events$Homeless)
  affected <- as.numeric(events$Affected)
  destroyed <- as.numeric(events$Buildings_Private_Destroyed_Count)
  damaged <- as.numeric(events$Buildings_Private_Damaged_Count)

  # NOTE: COST BASIS FOR DAMAGED & DESTROYED IS THE SAME, AS PER SANDRA'S / DELOITTE'S COMMENTS
  cleanupCosts <- destroyed * indexCosts(c(2011, 5900)) +
                  damaged * indexCosts(c(2011, 5900))

  # http://australia.etbtravelnews.com/244884/hotels-com-releases-2014-hotel-price-index/
  hotelCostPerNight <- 175

  evacuatedHotelCosts <- evacuated * indexCosts(c(2013, hotelCostPerNight)) * 5 * 0.5 # Assume 2 -night stay, divided by 2 people
  homelessCosts <- 0
  affectedCosts <- 0

  total <- cleanupCosts + evacuatedHotelCosts + homelessCosts + affectedCosts

  # Return total
  return (total)
}

# Commerical disruption
commercialDisruptionCosts <- function(events) {
  # Get key fields
  destroyed <- as.numeric(events$Buildings_Commercial_Destroyed_Count)
  damaged <- as.numeric(events$Buildings_Commercial_Damaged_Count)

  # Substitute for zeros
  destroyed[is.na(destroyed)] <- 0
  damaged[is.na(damaged)] <- 0

  # Taken from Kershaw 2013
  lossOfProductionAndServices <- costOfCommercialBuildingsGeneral(events)  * 0.2 + 
                                  costOfCommercialBuildingsIndustrial(events)  * 0.65 + 
                                  costOfCommercialBuildingsHotels(events)  * 0.2

  # NOTE: COST BASIS FOR DAMAGED & DESTROYED IS THE SAME, AS PER SANDRA'S / DELOITTE'S COMMENTS
  cleanupDestroyedLoss <- destroyed * indexCosts(c(2011, 3800))
  cleanupDamagedLoss <- damaged * indexCosts(c(2011, 3800))
  cleanupCosts <- cleanupDestroyedLoss + cleanupDamagedLoss

  total <- lossOfProductionAndServices + cleanupCosts

  return (total)
}


# Public service disruption
publicServiceDisruptionCosts <- function(events) {
  # Get key fields
  destroyed <- as.numeric(events$Buildings_Public_Destroyed_Count)
  damaged <- as.numeric(events$Buildings_Public_Damaged_Count)
  land <- as.numeric(events$Land_Public_Count)

  # Substitute for zeros
  destroyed[is.na(destroyed)] <- 0
  damaged[is.na(damaged)] <- 0
  land[is.na(land)] <- 0

  # Taken from Kershaw 2013
  lossOfProductionAndServices <- costOfPublicBuildings(events) * 0.25


  cleanupDestroyedLoss <- destroyed * indexCosts(c(2011, 15000))
  cleanupDamagedLoss <- damaged * indexCosts(c(2011, 15000))
  cleanupCosts <- cleanupDestroyedLoss + cleanupDamagedLoss

  total <- lossOfProductionAndServices + cleanupCosts

  return (total)
}


# Agricultural costs
agriculturalDisruptionCosts <- function(events) {

  # Just the case for FIRE?
  disruptionAgriculture <- costOfAgriculture(events) * 1.178

  # Get variables
  livestock <- as.numeric(events$Livestock_Destroyed_Count)
  livestockSheep <- as.numeric(events$Livestock_Destroyed_Count_Sheep_Goats)
  livestockCattle <- as.numeric(events$Livestock_Destroyed_Count_Cattle)
  land <- as.numeric(events$Land_Private_Count)

  livestock[is.na(livestock)] <- 0
  livestockSheep[is.na(livestockSheep)] <- 0
  livestockCattle[is.na(livestockCattle)] <- 0
  land[is.na(land)] <- 0


  # Costs for clean-up of:
  # Sheep: $6-10
  # Cows: $40-80
  # Averaged at $34
  cleanupLivestock <-livestockSheep * indexCosts(c(2000, 10)) +
                     livestockCattle * indexCosts(c(2000, 80))

  cleanupCrops <- 0

  # Assumes 3 hours per
  cleanupLand <- land * 3 * 20

  return (disruptionAgriculture + cleanupLivestock + cleanupCrops + cleanupLand)
}

# Calculate road transport delay costs
roadTransportDelayCosts <- function(events) {
  # We have no data for this currently
  # However the BTE 2001 report makes the following assuptions:
  #
  #  - Non-business Cars: $12.94 per hour
  #  - Business Cars: $31.67 per hour
  #  - Rigid Trucks: $39.80 per hour
  #  - Articuated Trucks: $44.58 per hour
  #
  # Consequently to calculate this properly we need types of vehicles
  # and hours delayed

  destroyedCars <- as.numeric(events$Vehicle_Private_Destroyed_Count_Cars)
  damagedCars <- as.numeric(events$Vehicle_Private_Damaged_Count_Cars)

  return (0)
}


# Calculate network costs
networkCosts <- function(events) {
  networkCosts = 0

  # Add road delay costs
  networkCosts = networkCosts + roadTransportDelayCosts(events)

  # Need further costs for:
  #
  # - Ports
  # - Bridges
  # - Aircrafts
  # - Motor vehicles
  # - Trains
  # - Water vessels


  # Need cost bases for
  return (networkCosts)
}


# Calculate indirect costs
indirectCosts <- function(events) {

  # Disruption costs
  emergencyResponse <- emergencyResponseCosts(events)
  household <- householdDisruptionCosts(events)
  commercial <- commercialDisruptionCosts(events)
  publicService <- publicServiceDisruptionCosts(events)
  agricultural <- agriculturalDisruptionCosts(events)
  network <- networkCosts(events)

  disruptionCosts <- emergencyResponse + household + commercial + agricultural + network

  # Add other indirect costs for water, energy, communications
  otherIndirects <- 0

  total <- disruptionCosts + otherIndirects

  events$indirectCost <- total
  # Normalised values - but figures already normalised?
  events$indirectCost.normalised <- apply(events[c("Year.financial", "indirectCost")], 1, normalisedCostsWithoutIndexation)

  return (events)
}

# Calculate death costs
deathCosts <- function(events) {
  return (with(events, Deaths * costOfLife()))
}

# Calculate death costs
deathCostsNormalised <- function(events) {
  return (with(events, Deaths.normalised * costOfLife()))
}

# Calculate injury costs
injuryCosts <- function(events) {
  return (with(events,
               Injuries * proportionOfHospitalisedInjury() * costOfHospitalisedInjury() +
               Injuries * (1 - proportionOfHospitalisedInjury()) * costOfNonHospitalisedInjury()))
}

# Calculate normalised injury costs
injuryCostsNormalised <- function(events) {
  return (with(events,
              Injuries.normalised * proportionOfHospitalisedInjury() * costOfHospitalisedInjury() +
              Injuries.normalised * (1 - proportionOfHospitalisedInjury()) * costOfNonHospitalisedInjury()
      )
  )
}

# Calculate ecosystem services
ecosystemCosts <- function(events) {
  # Get the hectares affected

  return (0)
}

# Calculate health impact
healthImpactCosts <- function(events) {
  # Work out relationship to Affected / Evacuated / Homes destroyed, damaged / Homeless

  # For FIRE, $1000 per person

  return (0)
}

# Calculate memorabilia costs
memorabiliaCosts <- function(events) {
  # ???

  return (0)
}

# Calculate cultural heritage costs
culturalHeritageCosts <- function(events) {
  # ???

  return (0)
}


# Calculate intangible costs
intangibleCosts <- function(events) {
  events$deathCosts <- deathCosts(events)
  events$deathCosts.millions <- events$deathCosts / 1000000
  events$injuryCosts <- injuryCosts(events)
  events$injuryCosts.millions <- events$injuryCosts / 1000000
  deathAndInjuryCosts <- rowSums(subset(events, select = c(deathCosts, injuryCosts)), na.rm = TRUE)

  # Normalised values
  events$deathCosts.normalised <- deathCostsNormalised(events)
  events$deathCosts.normalised.millions <- events$deathCosts.normalised / 1000000
  events$injuryCosts.normalised <- injuryCostsNormalised(events)
  events$injuryCosts.normalised.millions <- events$injuryCosts.normalised / 1000000
  deathAndInjuryCostsNormalised <- rowSums(subset(events, select = c(deathCosts.normalised, injuryCosts.normalised)), na.rm = TRUE)

  ecosystemCosts <- ecosystemCosts(events)
  healthImpactCosts <- healthImpactCosts(events)
  memorabiliaCosts <- memorabiliaCosts(events)
  culturalHeritageCosts <- culturalHeritageCosts(events)
  nonDeathAndInjuryIntangibles <- ecosystemCosts + healthImpactCosts + memorabiliaCosts + culturalHeritageCosts

  events$deathAndInjuryCosts <- deathAndInjuryCosts
  events$deathAndInjuryCosts.millions <- deathAndInjuryCosts / 1000000
  events$nonDeathAndInjuryIntangibles <- nonDeathAndInjuryIntangibles
  events$intangibleCost <- deathAndInjuryCosts + nonDeathAndInjuryIntangibles

  nonDeathAndInjuryIntangiblesNormalised <- apply(events[c("Year.financial", "nonDeathAndInjuryIntangibles")], 1, normalisedCostsWithoutIndexation)
  events$deathAndInjuryCosts.normalised <- deathAndInjuryCostsNormalised
  events$deathAndInjuryCosts.normalised.millions <- events$deathAndInjuryCosts.normalised / 1000000
  events$nonDeathAndInjuryIntangibles.normalised <- nonDeathAndInjuryIntangiblesNormalised
  events$nonDeathAndInjuryIntangibles.normalised.millions <- events$nonDeathAndInjuryIntangibles.normalised / 1000000
  events$intangibleCost.normalised <- deathAndInjuryCostsNormalised + nonDeathAndInjuryIntangiblesNormalised
  events$intangibleCost.normalised.millions <- events$intangibleCost.normalised / 1000000

  return (events)
}

## Total cost for event
totalCostForEventSynthetic <- function(resourceTypeParam = NULL) {
  events <- getEvents(resourceTypeParam)
  events <- computedDirectCosts(events)
  events <- indirectCosts(events)
  events <- intangibleCosts(events)
  events$Synthetic.Cost <- rowSums(subset(events, select = c(directCost, indirectCost, intangibleCost)), na.rm = TRUE)
  # Synthetic costs are implicitly indexed
  events$Synthetic.Cost.indexed <- events$Synthetic.Cost
  events$Synthetic.Cost.normalised <- rowSums(subset(events, select = c(directCost.normalised, indirectCost.normalised, intangibleCost.normalised)), na.rm = TRUE)
  events$Synthetic.Cost.normalised.millions <- events$Synthetic.Cost.normalised / 1000000
  return(events)
}

## Total cost for event, including insured and reported costs
totalCostForEvent <- function(resourceTypeParam = NULL) {
  events <- totalCostForEventSynthetic(resourceTypeParam)

  # Add insured and reported costs
  

  # Use Derived multipliers
  multipliers <- apply(cbind(events['resourceType']), 1, eventTypeMultiplierDerived)
  # Use Joy's multipliers
  # multipliers <- apply(cbind(events['resourceType']), 1, eventTypeMultiplierJoy)
  
  events$Insured.Cost.multiplied <- events$Insured.Cost * multipliers
  events$Insured.Cost.multiplied.indexed <- apply(events[c("Year.financial", "Insured.Cost.multiplied")], 1, indexCosts)
  events$Insured.Cost.multiplied.normalised <- apply(events[c("Year.financial", "Insured.Cost.multiplied")], 1, normalisedCosts)
  events$Insured.Cost.multiplied.normalised.millions <- events$Insured.Cost.multiplied.normalised / 1000000
  
  events$Reported.Cost.interpolated.millions <- events$Reported.Cost.interpolated / 1000000
  events$Reported.Cost.WithDeathsAndInjuries.interpolated <- events$Reported.Cost.interpolated +  events$deathAndInjuryCosts
  events$Reported.Cost.WithDeathsAndInjuries.interpolated.millions <- events$Reported.Cost.WithDeathsAndInjuries.interpolated / 1000000
  events$Reported.Cost.indexed <- apply(events[c("Year.financial", "Reported.Cost.interpolated")], 1, indexCosts)
  events$Reported.Cost.indexed.millions <- events$Reported.Cost.indexed / 1000000
  events$Reported.Cost.WithDeathsAndInjuries.indexed <- events$Reported.Cost.indexed + events$deathAndInjuryCosts
  events$Reported.Cost.WithDeathsAndInjuries.indexed.millions <- events$Reported.Cost.WithDeathsAndInjuries.indexed / 1000000
  events$Reported.Cost.normalised <- apply(events[c("Year.financial", "Reported.Cost.interpolated")], 1, normalisedCosts)
  events$Reported.Cost.normalised.millions <- events$Reported.Cost.normalised / 1000000
  events$Reported.Cost.WithDeathsAndInjuries.normalised <- events$Reported.Cost.normalised +  events$deathAndInjuryCosts.normalised
  events$Reported.Cost.WithDeathsAndInjuries.normalised.millions <- events$Reported.Cost.WithDeathsAndInjuries.normalised / 1000000

  return(events)
}

## Total cost for events without heatwaves
totalCostForEventWithoutHeatwwave <- function(resourceTypeParam = NULL) {
  events <- totalCostForEvent(resourceTypeParam)

  # Remove heatwaves
  events <- events[events$resourceType != 'Heatwave',]

  return(events)
}

## Total cost for events filtered by resourceType, reported costs and no heatwaves
totalCostForEventFiltered <- function(resourceTypeParam = NULL, reportedCostsOnly = TRUE, noHeatwaves = FALSE) {
  events <- totalCostForEvent(resourceTypeParam)

  # Remove heatwaves
  if (reportedCostsOnly == TRUE) {
    events <- events[events$Reported.Cost.normalised.millions > 0,]
  }

  # Remove heatwaves
  if (noHeatwaves == TRUE) {
    events <- events[events$resourceType != 'Heatwave',]
  }

  return(events)
}


## Total cost for event - Interpolated basis
totalCostForEvent_Interpolated <- function(resourceTypeParam = NULL) {

  events <- getEvents(resourceTypeParam)
  events <- computedDirectCosts(events)
  events <- indirectCosts(events)
  events <- intangibleCosts(events)
  interpolatedTotals <- rowSums(subset(events, select = c(directCost, indirectCost, intangibleCost)), na.rm = TRUE)
  interpolatedTotals.normalised <- rowSums(subset(events, select = c(directCost.normalised, indirectCost.normalised, intangibleCost.normalised)), na.rm = TRUE)

  swapNormalForInterpollatedCosts()
  events <- getEvents(resourceTypeParam)
  events <- computedDirectCosts(events)
  events <- indirectCosts(events)
  events <- intangibleCosts(events)
  events$total <- rowSums(subset(events, select = c(directCost, indirectCost, intangibleCost)), na.rm = TRUE)
  events$total.normalised <- rowSums(subset(events, select = c(directCost.normalised, indirectCost.normalised, intangibleCost.normalised)), na.rm = TRUE)
  events$interpolatedTotals <- interpolatedTotals
  events$interpolatedTotals.normalised <- interpolatedTotals.normalised
  swapInterpollatedForNormalCosts()
  return(events)
}


## Provide a code for cost breaks
codeCosts <- function(value) {
  if (value < 10) {
    1
  } else if (value < 50) {
    2
  } else if (value < 100) {
    3
  } else if (value < 150) {
    4
  } else if (value < 500) {
    5
  } else {
    6
  }
}

## Provide a code for cost breaks
codeCostLabels <- function() {
  c("< $10m", "$10 to $50m", "$50 to $100m", "$100 to $150m", "$150 to $500m", "> $500m")
}


## Generate event-type multipliers, to estimate total direct costs from insured losses
eventTypeMultiplierJoy <- function(eventType) {
  ## NOTE: This approach is derived from Table 2.2 of 1999 BTE report,
  ## which in turn is derived from Joy 1991, which states:
  ## "These estimates were provided by the ICA and are subjective impressions based on experience rather than analytical estimates… The estimates include the effects of underinsurance."
  ## OTHER APPROACHES ARE POSSIBLE -- SEE
  ## http://www.gao.gov/new.items/d02700r.pdf
  ## http://www.investigativeproject.org/documents/testimony/105.pdf
  if (eventType == "Bushfire") {
    3.0
  } else if (eventType == "Cyclone") {
    5.0
  } else if (eventType == "Flood") {
    10.0
  } else if (eventType == "Severe Storm") {
    3.0
  } else if (eventType == "Earthquake") {
    4.0
  } else if (eventType == "Heatwave") {
    1.0
  } else if (eventType == "Landslide") {
    1.0
  } else if (eventType == "Storm") {
    1.0
  } else  {
    1.0
  }
}


# Generates the multiplier from the "empirically based ratio"
eventTypeMultiplierDerived <- function(eventType) {
  ag <- generateDerivedMultipliers()
  multiplier <- ag[ag$resourceType == eventType,]$Event.Factor
  return (multiplier)
}

# Generates a list of derived multipliers
generateDerivedMultipliers <- function() {
  resourceTypes <- data.frame(resourceType = cbind(unique(unlist(ecnd.database$resourceType))))
  ag <- aggregate(cbind(Insured.Cost.cleaned, Reported.Cost) ~ resourceType, ecnd.database, sum)
  ag <- merge(ag[,], resourceTypes, by="resourceType", all = TRUE)
  ag$Event.Factor <- ag$Reported.Cost / ag$Insured.Cost 
  # Don't convert N/As to 1.0
  ag[is.na(ag$Event.Factor),"Event.Factor"] <- 1.0
  return (ag)
}

# Count number of empty values
countEmptyValues <- function(data) {
  data <- as.numeric(data)
  emptyValues <- length(data[is.na(data)])
  return (emptyValues)
}

# Report on the number of non-empty values each event contains
countEmptyValuesInEvents <- function() {
  events <- getRawEvents()

  xsub <- events[,7:28]
  events$emptyValues <- apply(xsub, 1, countEmptyValues)

  return (events)
}

# Interpollation functions

## Interpollate all costs
interpollateSyntheticCosts <- function() {
  # Do costs
  data <- ecnd.database[c("Insured.Cost.normalised", "Reported.Cost")]
  ecnd.database$Insured.Cost.normalised.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Insured.Cost.indexed", "Reported.Cost")]
  ecnd.database$Insured.Cost.indexed.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)

  # Do other columns
  data <- ecnd.database[c("Evacuated", "Insured.Cost.indexed.i")]
  ecnd.database$Evacuated.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Homeless", "Insured.Cost.indexed.i")]
  ecnd.database$Homeless.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Calls.to.SES", "Insured.Cost.indexed.i")]
  ecnd.database$Calls.to.SES.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Assistance_numbers", "Insured.Cost.indexed.i")]
  ecnd.database$Assistance_numbers.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Assistance_dollars", "Insured.Cost.indexed.i")]
  ecnd.database$Assistance_dollars.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Infrastructure_Public_Destroyed_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Infrastructure_Public_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Infrastructure_Public_Damaged_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Infrastructure_Public_Damaged_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Infrastructure_Public_Destroyed_Count_Roads_Urban", "Insured.Cost.indexed.i")]
  ecnd.database$Infrastructure_Public_Destroyed_Count_Roads_Urban.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Infrastructure_Public_Damaged_Count_Roads_Urban", "Insured.Cost.indexed.i")]
  ecnd.database$Infrastructure_Public_Damaged_Count_Roads_Urban.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Infrastructure_Public_Destroyed_Count_Roads_Rural", "Insured.Cost.indexed.i")]
  ecnd.database$Infrastructure_Public_Destroyed_Count_Roads_Rural.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Infrastructure_Public_Damaged_Count_Roads_Rural", "Insured.Cost.indexed.i")]
  ecnd.database$Infrastructure_Public_Damaged_Count_Roads_Rural.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Infrastructure_Public_Destroyed_Count_Bridges", "Insured.Cost.indexed.i")]
  ecnd.database$Infrastructure_Public_Destroyed_Count_Bridges.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Infrastructure_Public_Damaged_Count_Bridges", "Insured.Cost.indexed.i")]
  ecnd.database$Infrastructure_Public_Damaged_Count_Bridges.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Infrastructure_Public_Destroyed_Count_Rail", "Insured.Cost.indexed.i")]
  ecnd.database$Infrastructure_Public_Destroyed_Count_Rail.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Infrastructure_Public_Damaged_Count_Rail", "Insured.Cost.indexed.i")]
  ecnd.database$Infrastructure_Public_Damaged_Count_Rail.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Infrastructure_Public_Destroyed_Count_Power_Poles", "Insured.Cost.indexed.i")]
  ecnd.database$Infrastructure_Public_Destroyed_Count_Power_Poles.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Infrastructure_Public_Damaged_Count_Power_Poles", "Insured.Cost.indexed.i")]
  ecnd.database$Infrastructure_Public_Damaged_Count_Power_Poles.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Vehicle_Public_Destroyed_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Vehicle_Public_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Vehicle_Public_Damaged_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Vehicle_Public_Damaged_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Vehicle_Public_Destroyed_Count_Aircraft", "Insured.Cost.indexed.i")]
  ecnd.database$Vehicle_Public_Destroyed_Count_Aircraft.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Vehicle_Public_Damaged_Count_Aircraft", "Insured.Cost.indexed.i")]
  ecnd.database$Vehicle_Public_Damaged_Count_Aircraft.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Vehicle_Public_Destroyed_Count_Train", "Insured.Cost.indexed.i")]
  ecnd.database$Vehicle_Public_Destroyed_Count_Train.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Vehicle_Public_Damaged_Count_Train", "Insured.Cost.indexed.i")]
  ecnd.database$Vehicle_Public_Damaged_Count_Train.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Vehicle_Private_Destroyed_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Vehicle_Private_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Vehicle_Private_Damaged_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Vehicle_Private_Damaged_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Vehicle_Private_Destroyed_Count_Boats", "Insured.Cost.indexed.i")]
  ecnd.database$Vehicle_Private_Destroyed_Count_Boats.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Vehicle_Private_Damaged_Count_Boats", "Insured.Cost.indexed.i")]
  ecnd.database$Vehicle_Private_Damaged_Count_Boats.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Vehicle_Private_Destroyed_Count_Cars", "Insured.Cost.indexed.i")]
  ecnd.database$Vehicle_Private_Destroyed_Count_Cars.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Vehicle_Private_Damaged_Count_Cars", "Insured.Cost.indexed.i")]
  ecnd.database$Vehicle_Private_Damaged_Count_Cars.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Vehicle_Private_Destroyed_Count_Caravans", "Insured.Cost.indexed.i")]
  ecnd.database$Vehicle_Private_Destroyed_Count_Caravans.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Vehicle_Private_Damaged_Count_Caravans", "Insured.Cost.indexed.i")]
  ecnd.database$Vehicle_Private_Damaged_Count_Caravans.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Buildings_Commercial_Destroyed_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Buildings_Commercial_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Buildings_Commercial_Damaged_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Buildings_Commercial_Damaged_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Buildings_Private_Destroyed_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Buildings_Private_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Buildings_Private_Damaged_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Buildings_Private_Damaged_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Buildings_Public_Destroyed_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Buildings_Public_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Buildings_Public_Damaged_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Buildings_Public_Damaged_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Land_Public_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Land_Public_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Land_Private_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Land_Private_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Crops_Destroyed_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Crops_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Livestock_Destroyed_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Livestock_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Livestock_Destroyed_Count_Cattle", "Insured.Cost.indexed.i")]
  ecnd.database$Livestock_Destroyed_Count_Cattle.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Livestock_Destroyed_Count_Sheep_Goats", "Insured.Cost.indexed.i")]
  ecnd.database$Livestock_Destroyed_Count_Sheep_Goats.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Livestock_Destroyed_Count_Poultry", "Insured.Cost.indexed.i")]
  ecnd.database$Livestock_Destroyed_Count_Poultry.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Livestock_Destroyed_Count_Pigs", "Insured.Cost.indexed.i")]
  ecnd.database$Livestock_Destroyed_Count_Pigs.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Livestock_Destroyed_Count_Other", "Insured.Cost.indexed.i")]
  ecnd.database$Livestock_Destroyed_Count_Other.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Environmental_Count", "Insured.Cost.indexed.i")]
  ecnd.database$Environmental_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- ecnd.database[c("Fencing", "Insured.Cost.indexed.i")]
  ecnd.database$Fencing.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)

  return ()
}

## Swaps interpollated costs
swapInterpollatedForNormalCosts <- function() {
  ecnd.database$Insured.Cost.normalised.ni <<- ecnd.database$Insured.Cost.normalised
  ecnd.database$Insured.Cost.normalised <<- ecnd.database$Insured.Cost.normalised.i
  ecnd.database$Insured.Cost.indexed.ni <<- ecnd.database$Insured.Cost.indexed
  ecnd.database$Insured.Cost.indexed <<- ecnd.database$Insured.Cost.indexed.i
  ecnd.database$Evacuated.ni <<- ecnd.database$Evacuated
  ecnd.database$Evacuated <<- ecnd.database$Evacuated.i
  ecnd.database$Homeless.ni <<- ecnd.database$Homeless
  ecnd.database$Homeless <<- ecnd.database$Homeless.i
  ecnd.database$Calls.to.SES.ni <<- ecnd.database$Calls.to.SES
  ecnd.database$Calls.to.SES <<- ecnd.database$Calls.to.SES.i
  ecnd.database$Assistance_numbers.ni <<- ecnd.database$Assistance_numbers
  ecnd.database$Assistance_numbers <<- ecnd.database$Assistance_numbers.i
  ecnd.database$Assistance_dollars.ni <<- ecnd.database$Assistance_dollars
  ecnd.database$Assistance_dollars <<- ecnd.database$Assistance_dollars.i
  ecnd.database$Infrastructure_Public_Damaged_Count.ni <<- ecnd.database$Infrastructure_Public_Damaged_Count
  ecnd.database$Infrastructure_Public_Damaged_Count <<- ecnd.database$Infrastructure_Public_Damaged_Count.i
  ecnd.database$Infrastructure_Public_Destroyed_Count.ni <<- ecnd.database$Infrastructure_Public_Destroyed_Count
  ecnd.database$Infrastructure_Public_Destroyed_Count <<- ecnd.database$Infrastructure_Public_Destroyed_Count.i
  ecnd.database$Infrastructure_Public_Damaged_Count_Roads_Urban.ni <<- ecnd.database$Infrastructure_Public_Damaged_Count_Roads_Urban
  ecnd.database$Infrastructure_Public_Damaged_Count_Roads_Urban <<- ecnd.database$Infrastructure_Public_Damaged_Count_Roads_Urban.i
  ecnd.database$Infrastructure_Public_Destroyed_Count_Roads_Urban.ni <<- ecnd.database$Infrastructure_Public_Destroyed_Count_Roads_Urban
  ecnd.database$Infrastructure_Public_Destroyed_Count_Roads_Urban <<- ecnd.database$Infrastructure_Public_Destroyed_Count_Roads_Urban.i
  ecnd.database$Infrastructure_Public_Damaged_Count_Roads_Rural.ni <<- ecnd.database$Infrastructure_Public_Damaged_Count_Roads_Rural
  ecnd.database$Infrastructure_Public_Damaged_Count_Roads_Rural <<- ecnd.database$Infrastructure_Public_Damaged_Count_Roads_Rural.i
  ecnd.database$Infrastructure_Public_Destroyed_Count_Roads_Rural.ni <<- ecnd.database$Infrastructure_Public_Destroyed_Count_Roads_Rural
  ecnd.database$Infrastructure_Public_Destroyed_Count_Roads_Rural <<- ecnd.database$Infrastructure_Public_Destroyed_Count_Roads_Rural.i
  ecnd.database$Infrastructure_Public_Damaged_Count_Bridges.ni <<- ecnd.database$Infrastructure_Public_Damaged_Count_Bridges
  ecnd.database$Infrastructure_Public_Damaged_Count_Bridges <<- ecnd.database$Infrastructure_Public_Damaged_Count_Bridges.i
  ecnd.database$Infrastructure_Public_Destroyed_Count_Bridges.ni <<- ecnd.database$Infrastructure_Public_Destroyed_Count_Bridges
  ecnd.database$Infrastructure_Public_Destroyed_Count_Bridges <<- ecnd.database$Infrastructure_Public_Destroyed_Count_Bridges.i
  ecnd.database$Infrastructure_Public_Damaged_Count_Rail.ni <<- ecnd.database$Infrastructure_Public_Damaged_Count_Rail
  ecnd.database$Infrastructure_Public_Damaged_Count_Rail <<- ecnd.database$Infrastructure_Public_Damaged_Count_Rail.i
  ecnd.database$Infrastructure_Public_Destroyed_Count_Rail.ni <<- ecnd.database$Infrastructure_Public_Destroyed_Count_Rail
  ecnd.database$Infrastructure_Public_Destroyed_Count_Rail <<- ecnd.database$Infrastructure_Public_Destroyed_Count_Rail.i
  ecnd.database$Infrastructure_Public_Damaged_Count_Power_Poles.ni <<- ecnd.database$Infrastructure_Public_Damaged_Count_Power_Poles
  ecnd.database$Infrastructure_Public_Damaged_Count_Power_Poles <<- ecnd.database$Infrastructure_Public_Damaged_Count_Power_Poles.i
  ecnd.database$Infrastructure_Public_Destroyed_Count_Power_Poles.ni <<- ecnd.database$Infrastructure_Public_Destroyed_Count_Power_Poles
  ecnd.database$Infrastructure_Public_Destroyed_Count_Power_Poles <<- ecnd.database$Infrastructure_Public_Destroyed_Count_Power_Poles.i
  ecnd.database$Buildings_Commercial_Destroyed_Count.ni <<- ecnd.database$Buildings_Commercial_Destroyed_Count
  ecnd.database$Buildings_Commercial_Destroyed_Count <<- ecnd.database$Buildings_Commercial_Destroyed_Count.i
  ecnd.database$Buildings_Commercial_Destroyed_General_Count.ni <<- ecnd.database$Buildings_Commercial_Destroyed_General_Count
  ecnd.database$Buildings_Commercial_Destroyed_General_Count <<- ecnd.database$Buildings_Commercial_Destroyed_General_Count.i
  ecnd.database$Buildings_Commercial_Destroyed_Industrial_Count.ni <<- ecnd.database$Buildings_Commercial_Destroyed_Industrial_Count
  ecnd.database$Buildings_Commercial_Destroyed_Industrial_Count <<- ecnd.database$Buildings_Commercial_Destroyed_Industrial_Count.i
  ecnd.database$Buildings_Commercial_Destroyed_Hotels_Count.ni <<- ecnd.database$Buildings_Commercial_Destroyed_Hotels_Count
  ecnd.database$Buildings_Commercial_Destroyed_Hotels_Count <<- ecnd.database$Buildings_Commercial_Destroyed_Hotels_Count.i
  ecnd.database$Buildings_Commercial_Damaged_Count.ni <<- ecnd.database$Buildings_Commercial_Damaged_Count
  ecnd.database$Buildings_Commercial_Damaged_Count <<- ecnd.database$Buildings_Commercial_Damaged_Count.i
  ecnd.database$Buildings_Commercial_Damaged_General_Count.ni <<- ecnd.database$Buildings_Commercial_Damaged_General_Count
  ecnd.database$Buildings_Commercial_Damaged_General_Count <<- ecnd.database$Buildings_Commercial_Damaged_General_Count.i
  ecnd.database$Buildings_Commercial_Damaged_Industrial_Count.ni <<- ecnd.database$Buildings_Commercial_Damaged_Industrial_Count
  ecnd.database$Buildings_Commercial_Damaged_Industrial_Count <<- ecnd.database$Buildings_Commercial_Damaged_Industrial_Count.i
  ecnd.database$Buildings_Commercial_Damaged_Hotels_Count.ni <<- ecnd.database$Buildings_Commercial_Damaged_Hotels_Count
  ecnd.database$Buildings_Commercial_Damaged_Hotels_Count <<- ecnd.database$Buildings_Commercial_Damaged_Hotels_Count.i
  ecnd.database$Buildings_Private_Destroyed_Count.ni <<- ecnd.database$Buildings_Private_Destroyed_Count
  ecnd.database$Buildings_Private_Destroyed_Count <<- ecnd.database$Buildings_Private_Destroyed_Count.i
  ecnd.database$Buildings_Private_Damaged_Count.ni <<- ecnd.database$Buildings_Private_Damaged_Count
  ecnd.database$Buildings_Private_Damaged_Count <<- ecnd.database$Buildings_Private_Damaged_Count.i
  ecnd.database$Buildings_Private_Destroyed_Count.ni <<- ecnd.database$Buildings_Private_Destroyed_Count
  ecnd.database$Buildings_Private_Destroyed_Count <<- ecnd.database$Buildings_Private_Destroyed_Count.i
  ecnd.database$Buildings_Private_Damaged_Count.ni <<- ecnd.database$Buildings_Private_Damaged_Count
  ecnd.database$Buildings_Private_Damaged_Count <<- ecnd.database$Buildings_Private_Damaged_Count.i

  ecnd.database$Vehicle_Public_Damaged_Count.ni <<- ecnd.database$Vehicle_Public_Damaged_Count
  ecnd.database$Vehicle_Public_Damaged_Count <<- ecnd.database$Vehicle_Public_Damaged_Count.i
  ecnd.database$Vehicle_Public_Damaged_Count_Aircraft.ni <<- ecnd.database$Vehicle_Public_Damaged_Count_Aircraft
  ecnd.database$Vehicle_Public_Damaged_Count_Aircraft <<- ecnd.database$Vehicle_Public_Damaged_Count_Aircraft.i
  ecnd.database$Vehicle_Public_Damaged_Count_Train.ni <<- ecnd.database$Vehicle_Public_Damaged_Count_Train
  ecnd.database$Vehicle_Public_Damaged_Count_Train <<- ecnd.database$Vehicle_Public_Damaged_Count_Train.i
  ecnd.database$Vehicle_Public_Destroyed_Count.ni <<- ecnd.database$Vehicle_Public_Destroyed_Count
  ecnd.database$Vehicle_Public_Destroyed_Count <<- ecnd.database$Vehicle_Public_Destroyed_Count.i
  ecnd.database$Vehicle_Public_Destroyed_Count_Aircraft.ni <<- ecnd.database$Vehicle_Public_Destroyed_Count_Aircraft
  ecnd.database$Vehicle_Public_Destroyed_Count_Aircraft <<- ecnd.database$Vehicle_Public_Destroyed_Count_Aircraft.i
  ecnd.database$Vehicle_Public_Destroyed_Count_Train.ni <<- ecnd.database$Vehicle_Public_Destroyed_Count_Train
  ecnd.database$Vehicle_Public_Destroyed_Count_Train <<- ecnd.database$Vehicle_Public_Destroyed_Count_Train.i

  ecnd.database$Vehicle_Private_Damaged_Count.ni <<- ecnd.database$Vehicle_Private_Damaged_Count
  ecnd.database$Vehicle_Private_Damaged_Count <<- ecnd.database$Vehicle_Private_Damaged_Count.i
  ecnd.database$Vehicle_Private_Damaged_Count_Boats.ni <<- ecnd.database$Vehicle_Private_Damaged_Count_Boats
  ecnd.database$Vehicle_Private_Damaged_Count_Boats <<- ecnd.database$Vehicle_Private_Damaged_Count_Boats.i
  ecnd.database$Vehicle_Private_Damaged_Count_Cars.ni <<- ecnd.database$Vehicle_Private_Damaged_Count_Cars
  ecnd.database$Vehicle_Private_Damaged_Count_Cars <<- ecnd.database$Vehicle_Private_Damaged_Count_Cars.i
  ecnd.database$Vehicle_Private_Damaged_Count_Caravans.ni <<- ecnd.database$Vehicle_Private_Damaged_Count_Caravans
  ecnd.database$Vehicle_Private_Damaged_Count_Caravans <<- ecnd.database$Vehicle_Private_Damaged_Count_Caravans.i
  ecnd.database$Vehicle_Private_Destroyed_Count.ni <<- ecnd.database$Vehicle_Private_Destroyed_Count
  ecnd.database$Vehicle_Private_Destroyed_Count <<- ecnd.database$Vehicle_Private_Destroyed_Count.i
  ecnd.database$Vehicle_Private_Destroyed_Count_Boats.ni <<- ecnd.database$Vehicle_Private_Destroyed_Count_Boats
  ecnd.database$Vehicle_Private_Destroyed_Count_Boats <<- ecnd.database$Vehicle_Private_Destroyed_Count_Boats.i
  ecnd.database$Vehicle_Private_Destroyed_Count_Cars.ni <<- ecnd.database$Vehicle_Private_Destroyed_Count_Cars
  ecnd.database$Vehicle_Private_Destroyed_Count_Cars <<- ecnd.database$Vehicle_Private_Destroyed_Count_Cars.i
  ecnd.database$Vehicle_Private_Destroyed_Count_Caravans.ni <<- ecnd.database$Vehicle_Private_Destroyed_Count_Caravans
  ecnd.database$Vehicle_Private_Destroyed_Count_Caravans <<- ecnd.database$Vehicle_Private_Destroyed_Count_Caravans.i

  ecnd.database$Public.buildings.destroyed.ni <<- ecnd.database$Public.buildings.destroyed
  ecnd.database$Public.buildings.destroyed <<- ecnd.database$Public.buildings.destroyed.i
  ecnd.database$Public.buildings.damaged.ni <<- ecnd.database$Public.buildings.damaged
  ecnd.database$Public.buildings.damaged <<- ecnd.database$Public.buildings.damaged.i
  ecnd.database$Land_Public_Count.ni <<- ecnd.database$Land_Public_Count
  ecnd.database$Land_Public_Count <<- ecnd.database$Land_Public_Count.i
  ecnd.database$Land_Private_Count.ni <<- ecnd.database$Land_Private_Count
  ecnd.database$Land_Private_Count <<- ecnd.database$Land_Private_Count.i
  ecnd.database$Crops_Destroyed_Count.ni <<- ecnd.database$Crops_Destroyed_Count
  ecnd.database$Crops_Destroyed_Count <<- ecnd.database$Crops_Destroyed_Count.i
  ecnd.database$Livestock_Destroyed_Count.ni <<- ecnd.database$Livestock_Destroyed_Count
  ecnd.database$Livestock_Destroyed_Count <<- ecnd.database$Livestock_Destroyed_Count.i
  ecnd.database$Environmental_Count.ni <<- ecnd.database$Environmental_Count
  ecnd.database$Environmental_Count <<- ecnd.database$Environmental_Count.i
}

## Swaps interpollated costs
swapNormalForInterpollatedCosts <- function() {
  ecnd.database$Insured.Cost.normalised <<- ecnd.database$Insured.Cost.normalised.ni
  ecnd.database$Insured.Cost.indexed <<- ecnd.database$Insured.Cost.indexed.ni
  ecnd.database$Evacuated <<- ecnd.database$Evacuated.ni
  ecnd.database$Homeless <<- ecnd.database$Homeless.ni
  ecnd.database$Calls.to.SES <<- ecnd.database$Calls.to.SES.ni
  ecnd.database$Assistance_numbers <<- ecnd.database$Assistance_numbers.ni
  ecnd.database$Assistance_dollars <<- ecnd.database$Assistance_dollars.ni
  ecnd.database$Buildings_Commercial_Destroyed_Count <<- ecnd.database$Buildings_Commercial_Destroyed_Count.ni
  ecnd.database$Buildings_Commercial_Damaged_Count <<- ecnd.database$Buildings_Commercial_Damaged_Count.ni
  ecnd.database$Buildings_Private_Destroyed_Count <<- ecnd.database$Buildings_Private_Destroyed_Count.ni
  ecnd.database$Buildings_Private_Damaged_Count <<- ecnd.database$Buildings_Private_Damaged_Count.ni
  ecnd.database$Public.buildings.destroyed <<- ecnd.database$Public.buildings.destroyed.ni
  ecnd.database$Public.buildings.damaged <<- ecnd.database$Public.buildings.damaged.ni
  ecnd.database$Land_Public_Count <<- ecnd.database$Land_Public_Count.ni
  ecnd.database$Land_Private_Count <<- ecnd.database$Land_Private_Count.ni
  ecnd.database$Crops_Destroyed_Count <<- ecnd.database$Crops_Destroyed_Count.ni
  ecnd.database$Livestock_Destroyed_Count <<- ecnd.database$Livestock_Destroyed_Count.ni
  ecnd.database$Environmental_Count <<- ecnd.database$Environmental_Count.ni
}

## Interpollate from insured costs
interpollate <- function(range) {
  a <- as.numeric(range[1])
  b <- as.numeric(range[2])

  # Add in a further multiplier to dampen the impact
  correctionFactor <- 1.0 # 0.5

  ratio <- as.numeric(range[3])
  if (is.na(a)) {
    a = b * ratio * correctionFactor
  }
  return (a)
}


# Interpollation functions
ratio <- function(range) {
  a <- as.numeric(range[,1])
  b <- as.numeric(range[,2])
  newdata <- subset(ecnd.database, !is.na(a) & !is.na(b))
  a <- newdata[colnames(range)[1]]
  b <- newdata[colnames(range)[2]]
  ratio <- mean(as.numeric(as.data.frame(a)[,1])) / mean(as.numeric(as.data.frame(b)[,1]))
  if (is.na(ratio)) {
    ratio = 0
  }
  return (ratio)
}



# Cost Conversion functions

convertCountsToDollars <- function() {
  ecnd.database$Infrastructure_Public_Damaged_Dollars <- apply(cbind(ecnd.database[c("Infrastructure_Public_Damaged_Count", "Infrastructure_Public_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Infrastructure_Public_Destroyed_Dollars <- apply(cbind(ecnd.database[c("Infrastructure_Public_Destroyed_Count", "Infrastructure_Public_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Vehicle_Public_Damaged_Dollars <- apply(cbind(ecnd.database[c("Vehicle_Public_Damaged_Count", "Vehicle_Public_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Vehicle_Public_Destroyed_Dollars <- apply(cbind(ecnd.database[c("Vehicle_Public_Destroyed_Count", "Vehicle_Public_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Vehicle_Private_Damaged_Dollars <- apply(cbind(ecnd.database[c("Vehicle_Private_Damaged_Count", "Vehicle_Private_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Vehicle_Private_Destroyed_Dollars <- apply(cbind(ecnd.database[c("Vehicle_Private_Destroyed_Count", "Vehicle_Private_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Buildings_Public_Damaged_Dollars <- apply(cbind(ecnd.database[c("Buildings_Public_Damaged_Count", "Buildings_Public_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Buildings_Public_Destroyed_Dollars <- apply(cbind(ecnd.database[c("Buildings_Public_Destroyed_Count", "Buildings_Public_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Buildings_Private_Damaged_Dollars <- apply(cbind(ecnd.database[c("Buildings_Private_Damaged_Count", "Buildings_Private_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Buildings_Private_Destroyed_Dollars <- apply(cbind(ecnd.database[c("Buildings_Private_Destroyed_Count", "Buildings_Private_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Buildings_Commercial_Damaged_Dollars <- apply(cbind(ecnd.database[c("Buildings_Commercial_Damaged_Count", "Buildings_Commercial_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Buildings_Commercial_Destroyed_Dollars <- apply(cbind(ecnd.database[c("Buildings_Commercial_Destroyed_Count", "Buildings_Commercial_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Infrastructure_Public_Damaged_Dollars <- apply(cbind(ecnd.database[c("Infrastructure_Public_Damaged_Count", "Infrastructure_Public_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Infrastructure_Public_Destroyed_Dollars <- apply(cbind(ecnd.database[c("Infrastructure_Public_Destroyed_Count", "Infrastructure_Public_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Infrastructure_Private_Damaged_Dollars <- apply(cbind(ecnd.database[c("Infrastructure_Private_Damaged_Count", "Infrastructure_Private_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Infrastructure_Private_Destroyed_Dollars <- apply(cbind(ecnd.database[c("Infrastructure_Private_Destroyed_Count", "Infrastructure_Private_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Vehicle_Public_Damaged_Dollars <- apply(cbind(ecnd.database[c("Vehicle_Public_Damaged_Count", "Vehicle_Public_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Vehicle_Public_Destroyed_Dollars <- apply(cbind(ecnd.database[c("Vehicle_Public_Destroyed_Count", "Vehicle_Public_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Vehicle_Private_Damaged_Dollars <- apply(cbind(ecnd.database[c("Vehicle_Private_Damaged_Count", "Vehicle_Private_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Vehicle_Private_Destroyed_Dollars <- apply(cbind(ecnd.database[c("Vehicle_Private_Destroyed_Count", "Vehicle_Private_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Land_Public_Dollars <- apply(cbind(ecnd.database[c("Land_Public_Count", "Land_Public_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Land_Private_Dollars <- apply(cbind(ecnd.database[c("Land_Private_Count", "Land_Private_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Crops_Destroyed_Dollars <- apply(cbind(ecnd.database[c("Crops_Destroyed_Count", "Crops_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Livestock_Destroyed_Dollars <- apply(cbind(ecnd.database[c("Livestock_Destroyed_Count", "Livestock_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  ecnd.database$Environmental_Dollars <- apply(cbind(ecnd.database[c("Environmental_Count", "Environmental_Dollars")], 1), 1, convertSingleCountToDollars)
}

adjustDollarsToCounts <- function() {
  ecnd.database$Infrastructure_Public_Damaged_Count <<- apply(ecnd.database[c("Infrastructure_Public_Damaged_Dollars", "Infrastructure_Public_Damaged_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Infrastructure_Public_Destroyed_Count <<- apply(ecnd.database[c("Infrastructure_Public_Destroyed_Dollars", "Infrastructure_Public_Destroyed_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Infrastructure_Private_Damaged_Count <<- apply(ecnd.database[c("Infrastructure_Private_Damaged_Dollars", "Infrastructure_Private_Damaged_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Infrastructure_Private_Destroyed_Count <<- apply(ecnd.database[c("Infrastructure_Private_Destroyed_Dollars", "Infrastructure_Private_Destroyed_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Vehicle_Public_Damaged_Count <<- apply(ecnd.database[c("Vehicle_Public_Damaged_Dollars", "Vehicle_Public_Damaged_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Vehicle_Public_Destroyed_Count <<- apply(ecnd.database[c("Vehicle_Public_Destroyed_Dollars", "Vehicle_Public_Destroyed_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Vehicle_Private_Damaged_Count <<- apply(ecnd.database[c("Vehicle_Private_Damaged_Dollars", "Vehicle_Private_Damaged_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Vehicle_Private_Destroyed_Count <<- apply(ecnd.database[c("Vehicle_Private_Destroyed_Dollars", "Vehicle_Private_Destroyed_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Buildings_Public_Damaged_Count <<- apply(ecnd.database[c("Buildings_Public_Damaged_Dollars", "Buildings_Public_Damaged_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Buildings_Public_Destroyed_Count <<- apply(ecnd.database[c("Buildings_Public_Destroyed_Dollars", "Buildings_Public_Destroyed_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Buildings_Private_Damaged_Count <<- apply(ecnd.database[c("Buildings_Private_Damaged_Dollars", "Buildings_Private_Damaged_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Buildings_Private_Destroyed_Count <<- apply(ecnd.database[c("Buildings_Private_Destroyed_Dollars", "Buildings_Private_Destroyed_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Buildings_Commercial_Damaged_Count <<- apply(ecnd.database[c("Buildings_Commercial_Damaged_Dollars", "Buildings_Commercial_Damaged_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Buildings_Commercial_Destroyed_Count <<- apply(ecnd.database[c("Buildings_Commercial_Destroyed_Dollars", "Buildings_Commercial_Destroyed_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Infrastructure_Public_Damaged_Count <<- apply(ecnd.database[c("Infrastructure_Public_Damaged_Dollars", "Infrastructure_Public_Damaged_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Infrastructure_Public_Destroyed_Count <<- apply(ecnd.database[c("Infrastructure_Public_Destroyed_Dollars", "Infrastructure_Public_Destroyed_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Infrastructure_Private_Damaged_Count <<- apply(ecnd.database[c("Infrastructure_Private_Damaged_Dollars", "Infrastructure_Private_Damaged_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Infrastructure_Private_Destroyed_Count <<- apply(ecnd.database[c("Infrastructure_Private_Destroyed_Dollars", "Infrastructure_Private_Destroyed_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Vehicle_Public_Damaged_Count <<- apply(ecnd.database[c("Vehicle_Public_Damaged_Dollars", "Vehicle_Public_Damaged_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Vehicle_Public_Destroyed_Count <<- apply(ecnd.database[c("Vehicle_Public_Destroyed_Dollars", "Vehicle_Public_Destroyed_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Vehicle_Private_Damaged_Count <<- apply(ecnd.database[c("Vehicle_Private_Damaged_Dollars", "Vehicle_Private_Damaged_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Vehicle_Private_Destroyed_Count <<- apply(ecnd.database[c("Vehicle_Private_Destroyed_Dollars", "Vehicle_Private_Destroyed_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Land_Public_Count <<- apply(ecnd.database[c("Land_Public_Dollars", "Land_Public_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Land_Private_Count <<- apply(ecnd.database[c("Land_Private_Dollars", "Land_Private_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Crops_Destroyed_Count <<- apply(ecnd.database[c("Crops_Destroyed_Dollars", "Crops_Destroyed_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Livestock_Destroyed_Count <<- apply(ecnd.database[c("Livestock_Destroyed_Dollars", "Livestock_Destroyed_Count", "Year")], 1, dollarsToCount)
  ecnd.database$Environmental_Count <<- apply(ecnd.database[c("Environmental_Dollars", "Environmental_Count", "Year")], 1, dollarsToCount)
}

# Multipliers
dollarsToCount <- function(range) {
  category <- names(range)[1]
  dollars <- as.numeric(range[1])
  count <- as.numeric(range[2])
  year <- as.numeric(range[3])
  multiplier <- switch(category,
          Infrastructure_Public_Damaged_Dollars = 1750000,
          Infrastructure_Public_Destroyed_Dollars = 1750000 * 2,
          Infrastructure_Private_Damaged_Dollars = 1750000,
          Infrastructure_Private_Destroyed_Dollars = 1750000 * 2,
          Vehicle_Public_Damaged_Dollars = 43315,
          Vehicle_Public_Destroyed_Dollars = 43315 * 2,
          Vehicle_Private_Damaged_Dollars = 4716,
          Vehicle_Private_Destroyed_Dollars = 4716 * 2,
          Buildings_Public_Damaged_Dollars = 165000 / 2,
          Buildings_Public_Destroyed_Dollars = 165000,
          Buildings_Private_Damaged_Dollars = 165000 / 2,
          Buildings_Private_Destroyed_Dollars = 165000,
          Buildings_Commercial_Damaged_Dollars = 165000 / 2,
          Buildings_Commercial_Destroyed_Dollars = 165000,
          Infrastructure_Public_Damaged_Dollars = 1,
          Infrastructure_Public_Destroyed_Dollars = 1,
          Infrastructure_Private_Damaged_Dollars = 1,
          Infrastructure_Private_Destroyed_Dollars = 1,
          Vehicle_Public_Damaged_Dollars = 1,
          Vehicle_Public_Destroyed_Dollars = 1,
          Vehicle_Private_Damaged_Dollars = 1,
          Vehicle_Private_Destroyed_Dollars = 1,
          Land_Public_Dollars = 172,
          Land_Private_Dollars = 172,
          Crops_Destroyed_Dollars = 273  + 50,
          Livestock_Destroyed_Dollars = 320,
          Environmental_Dollars = 1
  )
  inflated_dollars = (indexCosts(cbind(year, dollars)))
  if (is.na(count)) {
    count = inflated_dollars / multiplier;
  }
  return (count)
}

convertSingleCountToDollars <- function(range) {
  count <- as.numeric(range[1])
  dollarValue <- as.numeric(range[2])
  multiplier <- as.numeric(range[3])
  if (is.na(dollarValue) & !is.na(count)) {
    dollarValue <- multiplier * count
  }
  return (dollarValue)
}

# Conducts a basic non-parametric statistic test for the significance of a time series,
# and outputs the results
significanceTest_MannKendall <- function(data) {
  timeSeries <- ts(data$x)
  par(mfrow=c(2,1))
  # Autocorrelation
  acf(timeSeries)
  # Partial Autocorrelation
  pacf(timeSeries)
  res <- MannKendall(timeSeries)
  return (res)
}

significanceTest_LinearRegression <- function(data) {
  timeSeries <- ts(data$x)
  res <- lm(x ~ Group.1, data)
  return (res)
}


# Write ecnd.database back to a file
writeData <- function() {

  swapInterpollatedForNormalCosts()


  # Repeats logic from totalCostForEvent(), getEvents()
  ecnd.database$Deaths <<- as.numeric(ecnd.database$Deaths)
  ecnd.database$Injuries <<- as.numeric(ecnd.database$Injuries)
  ecnd.database$Deaths.normalised <<- as.numeric(ecnd.database$Deaths.normalised)
  ecnd.database$Injuries.normalised <<- as.numeric(ecnd.database$Injuries.normalised)
  # xsub <- ecnd.database[,6:24]
  # xsub[is.na(xsub)] <- 0
  # ecnd.database[,6:24]<-xsub

  ecnd.database <<- computedDirectCosts(ecnd.database)
  # ecnd.database <- directCosts(ecnd.database)
  ecnd.database <<- indirectCosts(ecnd.database)
  ecnd.database <<- intangibleCosts(ecnd.database)
  ecnd.database$total <<- rowSums(subset(ecnd.database, select = c(directCost, indirectCost, intangibleCost)), na.rm = TRUE)
  ecnd.database$total.normalised <<- rowSums(subset(ecnd.database, select = c(directCost.normalised, indirectCost.normalised, intangibleCost.normalised)), na.rm = TRUE)


  swapNormalForInterpollatedCosts()
  write.table(ecnd.database, file = "./output/data.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
}


# Write events back to a file
writeEventData <- function() {
  events <- totalCostForEvent()

  write.table(events, file = "./output/data_events.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
}



# Write events back to a file
writeEventDataSummary <- function() {
  events <- totalCostForEvent()
  summary <- events[c(
    "Year.financial",
    "Year",
    "title",
    "resourceType",
    "Reported.Cost",
    "Reported.Cost.interpolated",
    "Reported.Cost.indexed",
    "Reported.Cost.normalised",
    "Insured.Cost",
    "Insured.Cost.multiplied",
    "Insured.Cost.multiplied.indexed",
    "Insured.Cost.multiplied.normalised",
    "Synthetic.Cost",
    "Synthetic.Cost.normalised"
    )
  ]
  
  Reported.Cost.normalised.nonzero <- events$Reported.Cost.normalised[events$Reported.Cost.normalised != 0]
  Reported.Cost.normalised.sum <- sum(Reported.Cost.normalised.nonzero, na.rm = TRUE)
  Reported.Cost.normalised.average <- mean(Reported.Cost.normalised.nonzero, na.rm = TRUE)
  Reported.Cost.normalised.stddev <- sd(Reported.Cost.normalised.nonzero, na.rm = TRUE)
  Reported.Cost.normalised.nonzero.percentage <- ( Reported.Cost.normalised.nonzero * 100 ) / Reported.Cost.normalised.sum
  Reported.Cost.normalised.nonzero.sorted <- sort(Reported.Cost.normalised.nonzero, TRUE)
  
  print(paste("Total costs: $",format(Reported.Cost.normalised.sum, big.mark=","), sep="" ))
  print(paste("Average cost: $",format(Reported.Cost.normalised.average, big.mark=","), sep="" ))
  print(paste("Standard deviation: $",format(Reported.Cost.normalised.stddev, big.mark=","), sep="" ))
  
  write.table(summary, file = "./output/data_events_summary.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
}


