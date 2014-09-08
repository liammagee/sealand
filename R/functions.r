
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
  perl <- 'D:/strawberry/perl/bin/perl.exe'
  mydata <<- read.xls("./data/database.xlsx", 2, perl = perl)
  # Hack to ignore any rows without a year value - such as rows added for computation
  mydata <<- mydata[!is.na(mydata$Year), ]
  cpi <<- read.xls("./data/cpi.xlsx", 2, perl = perl)
  pop <<- read.xls("./data/pop_consolidate.xlsx", 1, perl = perl)
  gdp <<- read.xls("./data/5206001_key_aggregates.xlsx", 2, perl = perl)
}

## Generate computed columns
computeColumns <- function() {

  # First convert dollars to a basic count
  adjustDollarsToCounts()
  
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


  # Do interpollation
  interpollateAllCosts()
  
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
    "Buildings_Commercial_Destroyed_Count",
    "Buildings_Commercial_Damaged_Count",
    "Buildings_Private_Destroyed_Count",
    "Buildings_Private_Damaged_Count",
    "Buildings_Public_Destroyed_Count",
    "Buildings_Public_Damaged_Count",
    "Land_Public_Count",
    "Land_Private_Count",
    "Crops_Destroyed_Count",
    "Livestock_Destroyed_Count", 
    "Environmental_Count", 


    "Evacuated.i",
    "Homeless.i",
    "Calls.to.SES.i", 
    "Estimated.clean.up.costs.i", 
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
  destroyed <- as.numeric(events$Buildings_Private_Destroyed_Count)
  damaged <- as.numeric(events$Buildings_Private_Damaged_Count)
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
  land <- as.numeric(events$Land_Private_Count)
  crops <- as.numeric(events$Crops_Destroyed_Count)
  livestock <- as.numeric(events$Livestock_Destroyed_Count)

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
  # ACTUALLY RIGHT NOW WE NEED TO ASSUME THIS IS A DOLLAR VALUE
  # cropLoss = crops
  
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
  # THESE FIGURES ARE ALREADY NORMALISED
  # events$directCost.normalised <- apply(events[c("Year.financial", "directCost")], 1, normalisedCosts)
  events$directCost.normalised <- events$directCost

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
  evacuated <- as.numeric(events$Evacuated)
  homeless <- as.numeric(events$Homeless)
  destroyed <- as.numeric(events$Buildings_Private_Destroyed_Count)
  damaged <- as.numeric(events$Buildings_Private_Damaged_Count)

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
  destroyed <- as.numeric(events$Buildings_Commercial_Destroyed_Count)
  damaged <- as.numeric(events$Buildings_Commercial_Damaged_Count)
  
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
  destroyed <- as.numeric(events$Buildings_Public_Destroyed_Count)
  damaged <- as.numeric(events$Buildings_Public_Damaged_Count)
  land <- as.numeric(events$Land_Public_Count)
  
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
  destroyed <- as.numeric(events$Buildings_Commercial_Destroyed_Count.i)
  damaged <- as.numeric(events$Buildings_Commercial_Damaged_Count.i)
  
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
  livestock <- as.numeric(events$Livestock_Destroyed_Count)
  land <- as.numeric(events$Land_Private_Count)
  
  livestock[is.na(livestock)] <- 0
  land[is.na(land)] <- 0
  
  
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
  cleanupLand = land * 15
  
  return (disruptionAgriculture + cleanupLivestock + cleanupLand)
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
  deathAndInjuryCosts <- rowSums(subset(events, select = c(deathCosts, injuryCosts)), na.rm = TRUE)

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
  deathAndInjuryCostsNormalised <- rowSums(subset(events, select = c(deathCosts.normalised, injuryCosts.normalised)), na.rm = TRUE)

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
  events$total.normalised <- rowSums(subset(events, select = c(directCost.normalised, indirectCost.normalised, intangibleCost.normalised)), na.rm = TRUE)
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
  data <- mydata[c("Insured.Costs.normalised", "Reported.cost")]
  mydata$Insured.Costs.normalised.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Insured.Costs.indexed", "Reported.cost")]
  mydata$Insured.Costs.indexed.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  
  # Do other columns
  data <- mydata[c("Evacuated", "Insured.Costs.normalised.i")]
  mydata$Evacuated.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Homeless", "Insured.Costs.normalised.i")]
  mydata$Homeless.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Calls.to.SES", "Insured.Costs.normalised.i")]
  mydata$Calls.to.SES.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Estimated.clean.up.costs", "Insured.Costs.normalised.i")]
  mydata$Estimated.clean.up.costs.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Buildings_Commercial_Destroyed_Count", "Insured.Costs.normalised.i")]
  mydata$Buildings_Commercial_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Buildings_Commercial_Damaged_Count", "Insured.Costs.normalised.i")]
  mydata$Buildings_Commercial_Damaged_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Buildings_Private_Destroyed_Count", "Insured.Costs.normalised.i")]
  mydata$Buildings_Private_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Buildings_Private_Damaged_Count", "Insured.Costs.normalised.i")]
  mydata$Buildings_Private_Damaged_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Buildings_Public_Destroyed_Count", "Insured.Costs.normalised.i")]
  mydata$Buildings_Public_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Buildings_Public_Damaged_Count", "Insured.Costs.normalised.i")]
  mydata$Buildings_Public_Damaged_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Land_Public_Count", "Insured.Costs.normalised.i")]
  mydata$Land_Public_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Land_Private_Count", "Insured.Costs.normalised.i")]
  mydata$Land_Private_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Crops_Destroyed_Count", "Insured.Costs.normalised.i")]
  mydata$Crops_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Livestock_Destroyed_Count", "Insured.Costs.normalised.i")]
  mydata$Livestock_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Environmental_Count", "Insured.Costs.normalised.i")]
  mydata$Environmental_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)

  return ()
}

## Swaps interpollated costs
swapInterpollatedForNormalCosts <- function() {
  mydata$Insured.Costs.normalised.ni <<- mydata$Insured.Costs.normalised
  mydata$Insured.Costs.normalised <<- mydata$Insured.Costs.normalised.i
  mydata$Insured.Costs.indexed.ni <<- mydata$Insured.Costs.indexed
  mydata$Insured.Costs.indexed <<- mydata$Insured.Costs.indexed.i
  mydata$Evacuated.ni <<- mydata$Evacuated
  mydata$Evacuated <<- mydata$Evacuated.i
  mydata$Homeless.ni <<- mydata$Homeless
  mydata$Homeless <<- mydata$Homeless.i
  mydata$Calls.to.SES.ni <<- mydata$Calls.to.SES
  mydata$Calls.to.SES <<- mydata$Calls.to.SES.i
  mydata$Estimated.clean.up.costs.ni <<- mydata$Estimated.clean.up.costs
  mydata$Estimated.clean.up.costs <<- mydata$Estimated.clean.up.costs.i
  mydata$Buildings_Commercial_Destroyed_Count.ni <<- mydata$Buildings_Commercial_Destroyed_Count
  mydata$Buildings_Commercial_Destroyed_Count <<- mydata$Buildings_Commercial_Destroyed_Count.i
  mydata$Buildings_Commercial_Damaged_Count.ni <<- mydata$Buildings_Commercial_Damaged_Count
  mydata$Buildings_Commercial_Damaged_Count <<- mydata$Buildings_Commercial_Damaged_Count.i
  mydata$Buildings_Private_Destroyed_Count.ni <<- mydata$Buildings_Private_Destroyed_Count
  mydata$Buildings_Private_Destroyed_Count <<- mydata$Buildings_Private_Destroyed_Count.i
  mydata$Buildings_Private_Damaged_Count.ni <<- mydata$Buildings_Private_Damaged_Count
  mydata$Buildings_Private_Damaged_Count <<- mydata$Buildings_Private_Damaged_Count.i
  mydata$Public.buildings.destroyed.ni <<- mydata$Public.buildings.destroyed
  mydata$Public.buildings.destroyed <<- mydata$Public.buildings.destroyed.i
  mydata$Public.buildings.damaged.ni <<- mydata$Public.buildings.damaged
  mydata$Public.buildings.damaged <<- mydata$Public.buildings.damaged.i
  mydata$Land_Public_Count.ni <<- mydata$Land_Public_Count
  mydata$Land_Public_Count <<- mydata$Land_Public_Count.i
  mydata$Land_Private_Count.ni <<- mydata$Land_Private_Count
  mydata$Land_Private_Count <<- mydata$Land_Private_Count.i
  mydata$Crops_Destroyed_Count.ni <<- mydata$Crops_Destroyed_Count
  mydata$Crops_Destroyed_Count <<- mydata$Crops_Destroyed_Count.i
  mydata$Livestock_Destroyed_Count.ni <<- mydata$Livestock_Destroyed_Count
  mydata$Livestock_Destroyed_Count <<- mydata$Livestock_Destroyed_Count.i
  mydata$Environmental_Count.ni <<- mydata$Environmental_Count
  mydata$Environmental_Count <<- mydata$Environmental_Count.i
}

## Swaps interpollated costs
swapNormalForInterpollatedCosts <- function() {
  mydata$Insured.Costs.normalised <- mydata$Insured.Costs.normalised.ni
  mydata$Insured.Costs.indexed <- mydata$Insured.Costs.indexed.ni
  mydata$Evacuated <- mydata$Evacuated.ni
  mydata$Homeless <- mydata$Homeless.ni
  mydata$Calls.to.SES <- mydata$Calls.to.SES.ni
  mydata$Estimated.clean.up.costs <- mydata$Estimated.clean.up.costs.ni
  mydata$Buildings_Commercial_Destroyed_Count <- mydata$Buildings_Commercial_Destroyed_Count.ni
  mydata$Buildings_Commercial_Damaged_Count <- mydata$Buildings_Commercial_Damaged_Count.ni
  mydata$Buildings_Private_Destroyed_Count <- mydata$Buildings_Private_Destroyed_Count.ni
  mydata$Buildings_Private_Damaged_Count <- mydata$Buildings_Private_Damaged_Count.ni
  mydata$Public.buildings.destroyed <- mydata$Public.buildings.destroyed.ni
  mydata$Public.buildings.damaged <- mydata$Public.buildings.damaged.ni
  mydata$Land_Public_Count <- mydata$Land_Public_Count.ni
  mydata$Land_Private_Count <- mydata$Land_Private_Count.ni
  mydata$Crops_Destroyed_Count <- mydata$Crops_Destroyed_Count.ni
  mydata$Livestock_Destroyed_Count <- mydata$Livestock_Destroyed_Count.ni
  mydata$Environmental_Count <- mydata$Environmental_Count.ni
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
  mydata$Infrastructure_Public_Damaged_Dollars <- apply(cbind(mydata[c("Infrastructure_Public_Damaged_Count", "Infrastructure_Public_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Infrastructure_Public_Destroyed_Dollars <- apply(cbind(mydata[c("Infrastructure_Public_Destroyed_Count", "Infrastructure_Public_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Infrastructure_Private_Damaged_Dollars <- apply(cbind(mydata[c("Infrastructure_Private_Damaged_Count", "Infrastructure_Private_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Infrastructure_Private_Destroyed_Dollars <- apply(cbind(mydata[c("Infrastructure_Private_Destroyed_Count", "Infrastructure_Private_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Vehicle_Public_Damaged_Dollars <- apply(cbind(mydata[c("Vehicle_Public_Damaged_Count", "Vehicle_Public_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Vehicle_Public_Destroyed_Dollars <- apply(cbind(mydata[c("Vehicle_Public_Destroyed_Count", "Vehicle_Public_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Vehicle_Private_Damaged_Dollars <- apply(cbind(mydata[c("Vehicle_Private_Damaged_Count", "Vehicle_Private_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Vehicle_Private_Destroyed_Dollars <- apply(cbind(mydata[c("Vehicle_Private_Destroyed_Count", "Vehicle_Private_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Buildings_Public_Damaged_Dollars <- apply(cbind(mydata[c("Buildings_Public_Damaged_Count", "Buildings_Public_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Buildings_Public_Destroyed_Dollars <- apply(cbind(mydata[c("Buildings_Public_Destroyed_Count", "Buildings_Public_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Buildings_Private_Damaged_Dollars <- apply(cbind(mydata[c("Buildings_Private_Damaged_Count", "Buildings_Private_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Buildings_Private_Destroyed_Dollars <- apply(cbind(mydata[c("Buildings_Private_Destroyed_Count", "Buildings_Private_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Buildings_Commercial_Damaged_Dollars <- apply(cbind(mydata[c("Buildings_Commercial_Damaged_Count", "Buildings_Commercial_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Buildings_Commercial_Destroyed_Dollars <- apply(cbind(mydata[c("Buildings_Commercial_Destroyed_Count", "Buildings_Commercial_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Land_Public_Dollars <- apply(cbind(mydata[c("Land_Public_Count", "Land_Public_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Land_Private_Dollars <- apply(cbind(mydata[c("Land_Private_Count", "Land_Private_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Crops_Destroyed_Dollars <- apply(cbind(mydata[c("Crops_Destroyed_Count", "Crops_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Livestock_Destroyed_Dollars <- apply(cbind(mydata[c("Livestock_Destroyed_Count", "Livestock_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Environmental_Dollars <- apply(cbind(mydata[c("Environmental_Count", "Environmental_Dollars")], 1), 1, convertSingleCountToDollars)
}

adjustDollarsToCounts <- function() {
  mydata$Infrastructure_Public_Damaged_Count <<- apply(mydata[c("Infrastructure_Public_Damaged_Dollars", "Infrastructure_Public_Damaged_Count", "Year")], 1, dollarsToCount)
  mydata$Infrastructure_Public_Destroyed_Count <<- apply(mydata[c("Infrastructure_Public_Destroyed_Dollars", "Infrastructure_Public_Destroyed_Count", "Year")], 1, dollarsToCount)
  mydata$Infrastructure_Private_Damaged_Count <<- apply(mydata[c("Infrastructure_Private_Damaged_Dollars", "Infrastructure_Private_Damaged_Count", "Year")], 1, dollarsToCount)
  mydata$Infrastructure_Private_Destroyed_Count <<- apply(mydata[c("Infrastructure_Private_Destroyed_Dollars", "Infrastructure_Private_Destroyed_Count", "Year")], 1, dollarsToCount)
  mydata$Vehicle_Public_Damaged_Count <<- apply(mydata[c("Vehicle_Public_Damaged_Dollars", "Vehicle_Public_Damaged_Count", "Year")], 1, dollarsToCount)
  mydata$Vehicle_Public_Destroyed_Count <<- apply(mydata[c("Vehicle_Public_Destroyed_Dollars", "Vehicle_Public_Destroyed_Count", "Year")], 1, dollarsToCount)
  mydata$Vehicle_Private_Damaged_Count <<- apply(mydata[c("Vehicle_Private_Damaged_Dollars", "Vehicle_Private_Damaged_Count", "Year")], 1, dollarsToCount)
  mydata$Vehicle_Private_Destroyed_Count <<- apply(mydata[c("Vehicle_Private_Destroyed_Dollars", "Vehicle_Private_Destroyed_Count", "Year")], 1, dollarsToCount)
  mydata$Buildings_Public_Damaged_Count <<- apply(mydata[c("Buildings_Public_Damaged_Dollars", "Buildings_Public_Damaged_Count", "Year")], 1, dollarsToCount)
  mydata$Buildings_Public_Destroyed_Count <<- apply(mydata[c("Buildings_Public_Destroyed_Dollars", "Buildings_Public_Destroyed_Count", "Year")], 1, dollarsToCount)
  mydata$Buildings_Private_Damaged_Count <<- apply(mydata[c("Buildings_Private_Damaged_Dollars", "Buildings_Private_Damaged_Count", "Year")], 1, dollarsToCount)
  mydata$Buildings_Private_Destroyed_Count <<- apply(mydata[c("Buildings_Private_Destroyed_Dollars", "Buildings_Private_Destroyed_Count", "Year")], 1, dollarsToCount)
  mydata$Buildings_Commercial_Damaged_Count <<- apply(mydata[c("Buildings_Commercial_Damaged_Dollars", "Buildings_Commercial_Damaged_Count", "Year")], 1, dollarsToCount)
  mydata$Buildings_Commercial_Destroyed_Count <<- apply(mydata[c("Buildings_Commercial_Destroyed_Dollars", "Buildings_Commercial_Destroyed_Count", "Year")], 1, dollarsToCount)
  mydata$Land_Public_Count <<- apply(mydata[c("Land_Public_Dollars", "Land_Public_Count", "Year")], 1, dollarsToCount)
  mydata$Land_Private_Count <<- apply(mydata[c("Land_Private_Dollars", "Land_Private_Count", "Year")], 1, dollarsToCount)
  mydata$Crops_Destroyed_Count <<- apply(mydata[c("Crops_Destroyed_Dollars", "Crops_Destroyed_Count", "Year")], 1, dollarsToCount)
  mydata$Livestock_Destroyed_Count <<- apply(mydata[c("Livestock_Destroyed_Dollars", "Livestock_Destroyed_Count", "Year")], 1, dollarsToCount)
  mydata$Environmental_Count <<- apply(mydata[c("Environmental_Dollars", "Environmental_Count", "Year")], 1, dollarsToCount)
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
apply(mydata[c("Infrastructure_Public_Damaged_Dollars", "Infrastructure_Public_Damaged_Count", "Year")], 1, dollarsToCount)


convertSingleCountToDollars <- function(range) {
  count <- as.numeric(range[1])
  dollarValue <- as.numeric(range[2])
  multiplier <- as.numeric(range[3])
  if (is.na(dollarValue) & !is.na(count)) {
    dollarValue <- multiplier * count
  }
  return (dollarValue)
}


# Write mydata back to a file
writeData <- function() {
  write.table(mydata, file = "./output/data.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
}



