
# Imports
library(gdata)

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
  }
  else if (value == "New South Wales") {
    "NSW"
  }
  else if (value == "Tasmania") {
    "TAS"
  }
  else if (value == "Western Australia") {
    "WA"
  }
  else if (value == "Queensland") {
    "QLD"
  }
  else if (value == "Australian Capital Territory") {
    "ACT"
  }
  else if (value == "Northern Territory") {
    "NT"
  }
  else if (value == "South Australia") {
    "SA"
  }
  else  {
    "Other"
  }
}
# Helper function - zero out NAs
zeroNA <- function(data) {
  data[is.na(data)] <- 0 
}

## Returns the correct financial year for a month and year
financialYear <- function(range) {
  month <- range[1]
  year <- range[2]
  firstMonths = c("January", "February", "March", "April", "May", "June")
  finYear <- if (is.element(month, firstMonths)) as.numeric(year) else as.numeric(year) + 1
  return(as.numeric(finYear))
}
## Generates a population ratio (based on June 2013)
popRatio <- function(baseYear) {
  baseYear <- as.numeric(baseYear)
  if ((baseYear - 1967) < 14) {
    popRow <- 10 + (baseYear - 1967)
  }
  else {
    popRow <- 24 + (baseYear - 1981) * 4  
  }
  popTest <- as.numeric(pop$Estimated.Resident.Population....Persons....Australia..[popRow])
  pop2013 <- as.numeric(pop$Estimated.Resident.Population....Persons....Australia..[152])
  return(pop2013 / popTest)
}
## Generates a cpi ratio (based on June 2013)
cpiRatio <- function(baseYear) {
  cpiRow <- 85 + (baseYear - 1967) * 4
  cpiTest <- as.numeric(cpi$Index.Numbers....All.groups.CPI....Australia[cpiRow])
  cpi2013 <- as.numeric(cpi$Index.Numbers....All.groups.CPI....Australia[269])
  return(cpi2013 / cpiTest)
}
## Generates a gdp ratio based on chain volume measures (based on June 2013)
gdpRatio <- function(baseYear) {
  gdpRow <- 41 + (baseYear - 1967) * 4
  gdpTest <- as.numeric(gdp$Gross.domestic.product..Chain.volume.measures..[gdpRow])
  gdp2013 <- as.numeric(gdp$Gross.domestic.product..Chain.volume.measures..[225])
  gdpGross <- (gdp2013 / gdpTest)
  # Correct for Pop ratio to correct for per capita
  gdpGross <- gdpGross / popRatio(baseYear)
  return(gdpGross)
}
## Generates an CPI indexed cost (based on June 2013)
indexCosts <- function(range) {
  baseYear <- range[1]
  insuredCost <- range[2]
  return(insuredCost * cpiRatio(baseYear))
}
## Normalise cost - TODO: this needs to be much more robust (cf. discussion on normalisation)
normalisedCosts <- function(range) {
  baseYear <- range[1]
  cost <- range[2]
  # Normalise for [1] inflation; [2] population growth; [3] wealth increase (GDP as a temporary proxy)
  # TODO: Add at least state-based equivalents
  return(cost * cpiRatio(baseYear) * popRatio(baseYear) * gdpRatio(baseYear))
}
## Normalise population
normalisedPopulation <- function(range) {
  baseYear <- as.numeric(range[1])
  pop <- as.numeric(range[2])
  # Normalise for inflation
  return(pop * popRatio(baseYear))
}
## Load data
loadData <- function() {
  mydata <<- read.xls("./data/report_v5.xlsx", 1)
  # Hack to ignore any rows without a year value - such as rows added for computation
  mydata <<- mydata[!is.na(mydata$Year), ]
  cpi <<- read.xls("./data/cpi.xlsx", 2)
  pop <<- read.xls("./data/pop_consolidate.xlsx", 1)
  gdp <<- read.xls("./data/5206001_key_aggregates.xlsx", 2)
}

## Generate computed columns
computeColumns <- function() {
  
  # ... for cleaned up costs 
  mydata$Costs.cleaned <<- apply(data.matrix(mydata$Insured.Cost), 1, parseCurrency)
  
  # ... for cleaned up states
  mydata$State.abbreviated <<- apply(data.matrix(mydata$State.1), 1, abbreviateState)
  
  # ... for financial years
  mydata$Year.financial <<- apply(mydata[c("Month", "Year")], 1, financialYear)

  # ... for CPI-indexed insured costs
  mydata$Insured.Costs.indexed <<- apply(mydata[c("Year.financial", "Costs.cleaned")], 1, indexCosts)
  
  # ... for normalised insured costs
  mydata$Insured.Costs.normalised <<- apply(mydata[c("Year.financial", "Costs.cleaned")], 1, normalisedCosts)
  
  # ... for population-inflated deaths and injuries
  mydata$Deaths.normalised <<- apply(mydata[c("Year.financial", "Deaths")], 1, normalisedPopulation)
  mydata$Injuries.normalised <<- apply(mydata[c("Year.financial", "Injuries")], 1, normalisedPopulation)
}

## Specific cost estimation functions
### Cost of public services
costOfPublicServices <- function() {
  # Cost per call? Say $10 - TODO: NEEDS BETTER EVIDENCE
  callsToSES * 10
}
### Intangibles
#### Cost of life
costOfLife <- function() {
  # 2006 BTE figure, adjusted to 2013
  # return(indexCosts(c(2006, 2400000)))
  # 2008 Best Practice Regulation - Value of Statistical Life
  return(indexCosts(c(2008, 3500000)))
  # Victorian Guidance Note on Dam Safety Decision DSE 2012
  #return(indexCosts(c(2012, 4500000)))
  # US - value of safety in different industries
}
costOfLife()

costOfHospitalisedInjury <- function() {
  # 2006 BTE figure, adjusted to 2013
  return(indexCosts(c(2006, 214000)))
}
costOfNonHospitalisedInjury <- function() {
  # 2006 BTE figure, adjusted to 2013
  return(indexCosts(c(2006, 2200)))
}
## Returns the proportion of hospitalised injury to overall injuries
proportionOfHospitalisedInjury <- function() {
  # Completely manufactured - TODO: NEEDS BETTER EVIDENCE
  # 0.2
  # NO LONGER MADE UP - SOURCED FROM DELOITTE'S (MADE-UP?) FIGURES
  0.33
}


# Costs

## Get all events for the purpose of generating costs
getEvents <- function(resourceTypeParam = NULL) {
  events <- mydata[c("Year.financial", "resourceType", "State.abbreviated", "State.1", "State.2..", "Insured.Costs.indexed", "Insured.Costs.normalised", "Calls.to.SES", "Deaths", "Injuries", "Deaths.normalised", "Injuries.normalised")]
  if (! is.null(resourceTypeParam)) {
    events <- subset(events, resourceType == resourceTypeParam)
  }
  events$Deaths <- as.numeric(events$Deaths)
  events$Injuries <- as.numeric(events$Injuries)
  events$Deaths.normalised <- as.numeric(events$Deaths.normalised)
  events$Injuries.normalised <- as.numeric(events$Injuries.normalised)
  xsub <- events[,5:12] 
  xsub[is.na(xsub)] <- 0 
  events[,5:12]<-xsub
  return (events)
}

# Calculate direct costs
directCosts <- function(events) {
  events$directCost <- with(events, Insured.Costs.indexed)
  
  # Normalised values
  events$directCost.normalised <- with(events, Insured.Costs.normalised)

  return (events)
}

# Calculate cost of housing
costOfHousing <- function(events) {
  destroyed <- as.numeric(mydata$Private.buildings.destroyed)
  damaged <- as.numeric(mydata$Private.buildings.damaged)
  destroyed[is.na(destroyed)] <- 0
  damaged[is.na(damaged)] <- 0
  
  # WE ASSUME A COST OF $165K in 2013 dollars per house destroyed; 50% of that per house damaged
  # COST ARE AN AVERAGE OF AVERAGE FLOOD COSTS ($140.75K) & CAT'S NUMBER FOR FIRE (85% of $225K = $191K)
  destroyedCost = 165000
  damagedCost = destroyedCost * 0.5
  
  return (destroyed * destroyedCost + damaged * damagedCost)
}

# Calculate cost of agriculture
costOfAgriculture <- function() {
  land <- as.numeric(mydata$Private.land)
  crops <- as.numeric(mydata$Crop.s..destroyed)
  livestock <- as.numeric(mydata$Livestock.destroyed)

  land[is.na(land)] <- 0
  crops[is.na(crops)] <- 0
  livestock[is.na(livestock)] <- 0
  
  # Loss of land is assumed to be $172 per hectare
  landLoss = land * 172
  
  # Loss of crops is dependent upon type
  # * Silage - $51
  # * Wheat - $291
  # * Oats - $210
  # * Barley - $253
  # * Canola - $562
  # We assume an average of $273 per tonne
  cropLoss = crops * 273
  
  # Fencing - TODO: but we have no direct data for this
  # Measurement is per km
  # We estimate from hectares, assuming 100 hectares (1km square) needs 4km of fencing
  # An over-estimate most likely
  fencingLoss = 5000 * land * 0.01
  
  # Equipment - don't know what we do here
  equipmentLoss = 0
  
  # Livestock based on following figures:
  # * Wool sheep: $63
  # * Prime lambs: $111
  # * Cattle: $788
  # Average of $320 used
  livestockLoss = 320 * livestock
  
  return (landLoss + cropLoss + fencingLoss + equipmentLoss + livestockLoss)
}

# Calculate direct costs, via component-wise computation
computedDirectCosts <- function() {
  computedDirectCosts <- 0
  
  # Add various cost components
  computedDirectCosts <- computedDirectCosts + costOfHousing()
  computedDirectCosts <- computedDirectCosts + costOfAgriculture()
  
  return (computedDirectCosts)
}

# Generate a proportion of industrial to total commercial property
industrialProportionOfCommercial <- function() {
  # What is a reasonable ratio of industrial to commercial? Assuming 50% for now.
 return (0.5)
}


# Residential disruption
residentialDisruptionCosts <- function() {
  # Get key fields
  evacuated <- as.numeric(mydata$Evacuated)
  homeless <- as.numeric(mydata$Homeless)
  destroyed <- as.numeric(mydata$Private.buildings.destroyed)
  damaged <- as.numeric(mydata$Private.buildings.damaged)

  # Substitute for zeros
  evacuated[is.na(evacuated)] <- 0
  homeless[is.na(homeless)] <- 0
  destroyed[is.na(destroyed)] <- 0
  damaged[is.na(damaged)] <- 0
  
  totalAffected = destroyed + damaged
  
  cleanup <- 330 * totalAffected
  
  # Accom costs - NOTE THIS IS MUCH TOO CHEAP
  alternativeAccomPerNight <- indexCosts(c(1999, 26))
  evacuatedCosts = alternativeAccomPerNight * evacuated * 7
  homelessCosts = alternativeAccomPerNight * homeless * 70
  accom = evacuatedCosts + homelessCosts
  
  # Return total
  return (cleanup + accom)
}

# Commerical disruption
commercialDisruptionCosts <- function() {
  # Get key fields
  destroyed <- as.numeric(mydata$Commercial.buildings.destroyed)
  damaged <- as.numeric(mydata$Commercial.buildings.damaged)
  
  # Substitute for zeros
  destroyed[is.na(destroyed)] <- 0
  damaged[is.na(damaged)] <- 0

  # Calculate losses - ??? NOT SURE WHAT VALUE WE SHOULD BE USING
  # TODO: Replace 0 with proper estimates
  destroyedLoss = 0 * destroyed
  damagedLoss = 0 * damaged

  totalLoss  = destroyedLoss + damagedLoss
  
  industrialLoss = totalLoss * industrialProportionOfCommercial()
  commercialLoss =  totalLoss - industrialLoss
  
  disruption = commercialLoss * 0.1 + industrialLoss * 0.65
  
  return (disruption)
}

# Public service disruption
publicServiceDisruptionCosts <- function() {
  # Get key fields
  destroyed <- as.numeric(mydata$Public.building.destroyed)
  damaged <- as.numeric(mydata$Public.building.damaged)
  land <- as.numeric(mydata$Public.land)
  
  # Substitute for zeros
  destroyed[is.na(destroyed)] <- 0
  damaged[is.na(damaged)] <- 0
  land[is.na(land)] <- 0
  
  # Calculate losses - ??? NOT SURE WHAT VALUE WE SHOULD BE USING
  # TODO: Replace 0 with proper estimates
  destroyedLoss = 0 * destroyed
  damagedLoss = 0 * damaged
  landLoss = 0 * land
  
  disruption = destroyedLoss * 0.25 + damagedLoss * 0.25 + landLoss * 0.25
  
  return (disruption)
}

# Clean-up costs
cleanupDisruptionCosts <- function() {
  # Get key fields
  destroyed <- as.numeric(mydata$Commercial.buildings.destroyed)
  damaged <- as.numeric(mydata$Commercial.buildings.damaged)
  
  # Substitute for zeros
  destroyed[is.na(destroyed)] <- 0
  damaged[is.na(damaged)] <- 0
  
  # Calculate losses - ??? NOT SURE WHAT VALUE WE SHOULD BE USING
  # TODO: Replace 0 with proper estimates
  destroyedLoss = 0 * destroyed
  damagedLoss = 0 * damaged
  
  totalLoss  = destroyedLoss + damagedLoss
  
  # What is a reasonable ratio of industrial to commercial? Assuming 50% for now.
  industrialLoss = totalLoss * industrialProportionOfCommercial()
  commercialLoss =  totalLoss - industrialLoss
  
  cleanup = commercialLoss * 0.09 + industrialLoss * 0.1
  
  return (cleanup)
}

# Calculate indirect costs
indirectCosts <- function(events) {
  
  # Add different types of indirects
  residential <- residentialDisruptionCosts()
  commercial <- commercialDisruptionCosts()
  publicService <- publicServiceDisruptionCosts()
  cleanup <- cleanupDisruptionCosts()
  
  events$indirectCost <- residential + commercial + publicService + cleanup
  
  # Normalised values
  events$indirectCost.normalised <- events$indirectCost

  return (events)
}

# Calculate intangible costs
intangibleCosts <- function(events) {
  events$deathCosts <- with(events, Deaths * costOfLife())
  events$injuryCosts <- with(events, 
                             Injuries * proportionOfHospitalisedInjury() * costOfHospitalisedInjury() +
                               Injuries * (1 - proportionOfHospitalisedInjury()) * costOfNonHospitalisedInjury())
  events$intangibleCost <- rowSums(subset(events, select = c(deathCosts, injuryCosts)), na.rm = TRUE)

  # Normalised values
  events$deathCosts.normalised <- with(events, Deaths.normalised * costOfLife())
  events$injuryCosts.normalised <- with(events, 
                                        Injuries.normalised * proportionOfHospitalisedInjury() * costOfHospitalisedInjury() +
                                          Injuries.normalised * (1 - proportionOfHospitalisedInjury()) * costOfNonHospitalisedInjury())
  events$intangibleCost.normalised <- rowSums(subset(events, select = c(deathCosts.normalised, injuryCosts.normalised)), na.rm = TRUE)
  return (events)
}

## Total cost for evant
totalCostForEvent <- function(resourceTypeParam = NULL) {
  events <- getEvents(resourceTypeParam)
  events <- directCosts(events)
  events <- indirectCosts(events)
  events <- intangibleCosts(events)
  events$total <- rowSums(subset(events, select = c(directCost, indirectCost, intangibleCost)), na.rm = TRUE)
  events$total.normalised <- rowSums(subset(events, select = c(directCost.normalised, indirectCost.normalised, intangibleCost.normalised)), na.rm = TRUE)
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
eventTypeMultiplier <- function(eventType) {
  ## NOTE: This approach is derived from Table 2.2 of 1999 BTE report,
  ## which in turn is derived from Joy 1991, which states:
  ## "These estimates were provided by the ICA and are subjective impressions based on experience rather than analytical estimates… The estimates include the effects of underinsurance."
  ## OTHER APPROACHES ARE POSSIBLE -- SEE 
  ## http://www.gao.gov/new.items/d02700r.pdf
  ## http://www.investigativeproject.org/documents/testimony/105.pdf
  if (eventType == "Bushfire") {
    3.0
  }
  else if (value == "Cyclone") {
    5.0
  }
  else if (value == "Flood") {
    10.0
  }
  else if (value == "Severe Storm") {
    3.0
  }
  else if (value == "Earthquake") {
    4.0
  }
  else if (value == "Heatwave") {
    1.0
  }
  else if (value == "Landslide") {
    1.0
  }
  else if (value == "Storm") {
    1.0
  }
  else  {
    1.0
  }  
}
