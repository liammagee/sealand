
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
  mydata <<- read.xls("./data/database.xlsx", 1)
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
  mydata$Insured.Costs.indexed <<- apply(mydata[c("Year.financial", "Costs.cleaned")], 1, "IndexCosts)
  
  # ... for normalised insured costs
  mydata$Insured.Costs.normalised <<- apply(mydata[c("Year.financial", "Costs.cleaned")], 1, normalisedCosts)
  
  # ... for population-inflated deaths and injuries
  mydata$Deaths.normalised <<- apply(mydata[c("Year.financial", "Deaths")], 1, normalisedPopulation)
  mydata$Injuries.normalised <<- apply(mydata[c("Year.financial", "Injuries")], 1, normalisedPopulation)
  
  interpollateAllCosts()
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
  return(indexCosts(c(2008, 3500000)))
  # Victorian Guidance Note on Dam Safety Decision DSE 2012
  #return(indexCosts(c(2012, 4500000)))
  # US - value of safety in different industries
}


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

## Get raw events, without data cleaning
getRawEvents <- function(resourceTypeParam = NULL) {
  

  events <- mydata[c(
    "Year.financial", 
    "Year", 
    "resourceType", 
    "State.abbreviated", 
    "State.1", 
    "State.2..", 
    "Evacuated",
    "Homeless",
    "Calls.to.SES", 
    "Deaths", 
    "Deaths.normalised", 
    "Injuries", 
    "Injuries.normalised",
    "Minor", 
    "Severe", 
    "Insured.Costs.indexed", 
    "Insured.Costs.normalised", 
    "Estimated.clean.up.costs", 
    "Commercial.buildings.destroyed",
    "Commercial.buildings.damaged",
    "Private.buildings.destroyed",
    "Private.buildings.damaged",
    "Public.building.destroyed",
    "Public.building.damaged",
    "Public.land",
    "Private.land",
    "Crop.s..destroyed",
    "Livestock.destroyed", 

    "Evacuated.i",
    "Homeless.i",
    "Calls.to.SES.i", 
    "Estimated.clean.up.costs.i", 
    "Commercial.buildings.destroyed.i",
    "Commercial.buildings.damaged.i",
    "Private.buildings.destroyed.i",
    "Private.buildings.damaged.i",
    "Public.building.destroyed.i",
    "Public.building.damaged.i",
    "Public.land.i",
    "Private.land.i",
    "Crop.s..destroyed.i",
    "Livestock.destroyed.i"
    )
  ]
  if (! is.null(resourceTypeParam)) {
    events <- subset(events, resourceType == resourceTypeParam)
  }
  return (events)
}


## Get all events for the purpose of generating costs
getEvents <- function(resourceTypeParam = NULL) {
  events <- getRawEvents(resourceTypeParam = NULL)

  events$Deaths <- as.numeric(events$Deaths)
  events$Injuries <- as.numeric(events$Injuries)
  events$Deaths.normalised <- as.numeric(events$Deaths.normalised)
  events$Injuries.normalised <- as.numeric(events$Injuries.normalised)
  xsub <- events[,6:24] 
  xsub[is.na(xsub)] <- 0 
  events[,6:24]<-xsub
  return (events)
}


# Calculate cost of housing
costOfHousing <- function(events) {
  destroyed <- as.numeric(events$Private.buildings.destroyed.i)
  damaged <- as.numeric(events$Private.buildings.damaged.i)
  destroyed[is.na(destroyed)] <- 0
  damaged[is.na(damaged)] <- 0
  
  # WE ASSUME A COST OF $165K in 2013 dollars per house destroyed; 50% of that per house damaged
  # COST ARE AN AVERAGE OF AVERAGE FLOOD COSTS ($140.75K) & CAT'S NUMBER FOR FIRE (85% of $225K = $191K)
  destroyedCost = 165000
  damagedCost = destroyedCost * 0.5
  
  return (destroyed * destroyedCost + damaged * damagedCost)
}

# Calculate cost of agriculture
costOfAgriculture <- function(events) {
  land <- as.numeric(events$Private.land.i)
  crops <- as.numeric(events$Crop.s..destroyed.i)
  livestock <- as.numeric(events$Livestock.destroyed.i)

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
computedDirectCosts <- function(events) {
  computedDirectCosts <- 0
  
  # Add various cost components
  computedDirectCosts <- computedDirectCosts + costOfHousing(events)
  computedDirectCosts <- computedDirectCosts + costOfAgriculture(events)

  events$directCost <- computedDirectCosts
  
  # Normalised values
  events$directCost.normalised <- apply(events[c("Year.financial", "directCost")], 1, normalisedCosts)

  return (events)
}

# Calculate direct costs
directCosts <- function(events) {
  events$directCost <- with(events, Insured.Costs.indexed)
  
  # Normalised values
  events$directCost.normalised <- with(events, Insured.Costs.normalised)

  return (events)
}

# Generate a proportion of industrial to total commercial property
industrialProportionOfCommercial <- function() {
  # What is a reasonable ratio of industrial to commercial? Assuming 50% for now.
 return (0.5)
}


# Residential disruption
residentialDisruptionCosts <- function(events) {
  # Get key fields
  evacuated <- as.numeric(events$Evacuated.i)
  homeless <- as.numeric(events$Homeless.i)
  destroyed <- as.numeric(events$Private.buildings.destroyed.i)
  damaged <- as.numeric(events$Private.buildings.damaged.i)

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
commercialDisruptionCosts <- function(events) {
  # Get key fields
  destroyed <- as.numeric(events$Commercial.buildings.destroyed.i)
  damaged <- as.numeric(events$Commercial.buildings.damaged.i)
  
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
publicServiceDisruptionCosts <- function(events) {
  # Get key fields
  destroyed <- as.numeric(events$Public.building.destroyed.i)
  damaged <- as.numeric(events$Public.building.damaged.i)
  land <- as.numeric(events$Public.land.i)
  
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
cleanupDisruptionCosts <- function(events) {
  # Get key fields
  destroyed <- as.numeric(events$Commercial.buildings.destroyed.i)
  damaged <- as.numeric(events$Commercial.buildings.damaged.i)
  
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


# Agricultural costs
agriculturalDisruptionCosts <- function(events) {
  
  # Just the case for FIRE?
  disruptionAgriculture =  costOfAgriculture(events) * 1.178
  
  # Get variables
  livestock <- as.numeric(events$Livestock.destroyed.i)
  crops <- as.numeric(events$Crop.s..destroyed.i)
  
  livestock[is.na(livestock)] <- 0
  crops[is.na(crops)] <- 0
  
  
  # Costs for clean-up of:
  # Sheep: $6-10
  # Cows: $40-80
  # Averaged at $34
  cleanupLivestock = livestock * 34
  
  # These costs are for FLOODS
  # $25ha floodway areas
  # $10ha low velocity flood events
  # $350 horticultural
  # ASSUMPTION: Taking low value of $15ha
  cleanupCrops = crops * 15
  
  return (disruptionAgriculture + cleanupLivestock + cleanupCrops)
}

# Calculate road transport delay costs
roadTransportDelayCosts <- function(events) {
  # We have no data for this currently
  # However the BTE 2001 report makes the following assuptions:
  #
  #  - Non-business Cars: $12.94 per hour
  #  - Business Cars: $31.67 per hour
  #  - Rigid Trucks: $39.80 per hour
  #  - Artic Cars: $44.58 per hour
  # 
  # Consequently to calculate this properly we need types of vehicles
  # and hours delayed

  return (0)
}


# Calculate network costs
networkCosts <- function(events) {
  networkCosts = 0

  # Add road delay costs
  networkCosts = networkCosts + roadTransportDelayCosts()

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
  residential <- residentialDisruptionCosts(events)
  commercial <- commercialDisruptionCosts(events)
  publicService <- publicServiceDisruptionCosts(events)
  cleanup <- cleanupDisruptionCosts(events)
  agricultural <- agriculturalDisruptionCosts(events)
  
  disruptionCosts <- residential + commercial + publicService + cleanup + agricultural

  # Network costs
  network <- networkCosts(events)

  # Provision of services
  calls <- costOfPublicServiceCalls(events)

  # Add other indirect costs for water, energy, communications
  otherIndirects = 0

  events$indirectCost <- disruptionCosts + network + calls + otherIndirects

  # Normalised values
  events$indirectCost.normalised <- events$indirectCost

  return (events)
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
  events$deathCosts <- with(events, Deaths * costOfLife())
  events$injuryCosts <- with(events, 
                             Injuries * proportionOfHospitalisedInjury() * costOfHospitalisedInjury() +
                               Injuries * (1 - proportionOfHospitalisedInjury()) * costOfNonHospitalisedInjury())
  deathAndInjuryCosts <- rowSums(subset(events, select = c(deathCosts, InjuryCosts)), na.rm = TRUE)

  ecosystemCosts <- ecosystemCosts(events)
  healthImpactCosts <- healthImpactCosts(events)
  memorabiliaCosts <- memorabiliaCosts(events)
  culturalHeritageCosts <- culturalHeritageCosts(events)
  nonDeathAndInjuryIntangibles = ecosystemCosts + healthImpactCosts + memorabiliaCosts + culturalHeritageCosts

  events$intangibleCost = deathAndInjuryCosts + nonDeathAndInjuryIntangibles

  # Normalised values
  events$deathCosts.normalised <- with(events, Deaths.normalised * costOfLife())
  events$injuryCosts.normalised <- with(events, 
                                        Injuries.normalised * proportionOfHospitalisedInjury() * costOfHospitalisedInjury() +
                                          Injuries.normalised * (1 - proportionOfHospitalisedInjury()) * costOfNonHospitalisedInjury())
  deathAndInjuryCostsNormalised <- rowSums(subset(events, select = c(deathCosts.normalised, "InjuryCosts.normalised)), na.rm = TRUE)

  # TODO: Normalise this value
  nonDeathAndInjuryIntangiblesNormalised <- nonDeathAndInjuryIntangibles

  events$intangibleCost.normalised = deathAndInjuryCostsNormalised + nonDeathAndInjuryIntangiblesNormalised
  return (events)
}

## Total cost for event
totalCostForEvent <- function(resourceTypeParam = NULL) {
  events <- getEvents(resourceTypeParam)
  events <- computedDirectCosts(events)
  # events <- directCosts(events)
  events <- indirectCosts(events)
  events <- intangibleCosts(events)
  events$total <- rowSums(subset(events, select = c(directCost, indirectCost, intangibleCost)), na.rm = TRUE)
  events$total.normalised <- rowSums(subset(events, select = c(directCost.normalised, "IndirectCost.normalised, "IntangibleCost.normalised)), na.rm = TRUE)
  return(events) 
}

## Total cost for event - BTE basis
totalCostForEvent_BTEBasis <- function(resourceTypeParam = NULL) {
  events <- getEvents(resourceTypeParam)
  events <- directCosts(events)
  events <- indirectCosts(events)
  events <- intangibleCosts(events)
  events$total <- rowSums(subset(events, select = c(directCost, indirectCost, intangibleCost)), na.rm = TRUE)
  events$total.normalised <- rowSums(subset(events, select = c(directCost.normalised, indirectCost.normalised, intangibleCost.normalised)), na.rm = TRUE)
  multipliers <- apply(cbind(events['resourceType']), 1, eventTypeMultiplier)
  events$bteTotals <- events$directCost * multipliers
  events$bteTotals.normalised <- apply(events[c("Year", "bteTotals")], 1, normalisedCosts)
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
  else if (eventType == "Cyclone") {
    5.0
  }
  else if (eventType == "Flood") {
    10.0
  }
  else if (eventType == "Severe Storm") {
    3.0
  }
  else if (eventType == "Earthquake") {
    4.0
  }
  else if (eventType == "Heatwave") {
    1.0
  }
  else if (eventType == "Landslide") {
    1.0
  }
  else if (eventType == "Storm") {
    1.0
  }
  else  {
    1.0
  }  
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
interpollateAllCosts <- function() {
  # Do costs
  data <- mydata[c("Insured.Cost", "Reported.cost")]
  mydata$Insured.Cost.i <- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  
  # Do other columns
  data <- mydata[c("Evacuated", "Insured.Cost.i")]
  mydata$Evacuated.i <- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Homeless", "Insured.Cost.i")]
  mydata$Homeless.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Calls.to.SES", "Insured.Cost.i")]
  mydata$Calls.to.SES.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Estimated.clean.up.costs", "Insured.Cost.i")]
  mydata$Estimated.clean.up.costs.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Commercial.buildings.destroyed", "Insured.Cost.i")]
  mydata$Commercial.buildings.destroyed <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Commercial.buildings.damaged", "Insured.Cost.i")]
  mydata$Commercial.buildings.damaged.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Private.buildings.destroyed", "Insured.Cost.i")]
  mydata$Private.buildings.destroyed.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Private.buildings.damaged", "Insured.Cost.i")]
  mydata$Private.buildings.damaged.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Public.building.destroyed", "Insured.Cost.i")]
  mydata$Public.building.destroyed.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Public.building.damaged", "Insured.Cost.i")]
  mydata$Public.building.damaged.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Public.land", "Insured.Cost.i")]
  mydata$Public.land.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Private.land", "Insured.Cost.i")]
  mydata$Private.land.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Crop.s..destroyed", "Insured.Cost.i")]
  mydata$Crop.s..destroyed.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Livestock.destroyed", "Insured.Cost.i")]
  mydata$Livestock.destroyed.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)

  return ()
}


## Interpollate from insured costs
interpollate <- function(range) {
  a <- as.numeric(range[1])
  b <- as.numeric(range[2])
  ratio <- as.numeric(range[3])
  if (is.na(a)) {
    a = b * ratio
  }
  return (a)
}


# Interpollation functions
ratio <- function(range) {
  a <- as.numeric(range[,1])
  b <- as.numeric(range[,2])
  newdata <- subset(mydata, !is.na(a) & !is.na(b))
  ratio <- mean(newdata$a) / mean(newdata$b)
  if (is.na(ratio)) {
    ratio = 0
  }
  return (ratio)
}


# Write mydata back to a file
writeData <- function() {
  write.table(mydata, file = "./output/data.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
}


