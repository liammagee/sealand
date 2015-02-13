
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
  if ( is.na( cpiTest ) )   {
    cpiTest <- cpi2013
  }
  return(cpi2013 / cpiTest)
}
## Generates a gdp ratio based on chain volume measures (based on June 2013)
gdpRatio <- function(baseYear) {
  gdpRow <- 41 + (baseYear - 1967) * 4
  gdpTest <- as.numeric(gdp$Gross.domestic.product..Chain.volume.measures..[gdpRow])
  gdp2013 <- as.numeric(gdp$Gross.domestic.product..Chain.volume.measures..[225])
  if ( is.na( gdpTest ) )   {
    gdpTest <- gdp2013
  }
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
  # mydata <<- read.xls("./data/database.xlsx", 2, perl = perl)
  # # Hack to ignore any rows without a year value - such as rows added for computation
  # mydata <<- mydata[!is.na(mydata$Year), ]
  # cpi <<- read.xls("./data/cpi.xlsx", 2, perl = perl)
  # pop <<- read.xls("./data/pop_consolidate.xlsx", 1, perl = perl)
  # gdp <<- read.xls("./data/5206001_key_aggregates.xlsx", 2, perl = perl)


  # MAC VERSION
  mydata <<- read.xls("./data/database.xlsx", 2)
  # Hack to ignore any rows without a year value - such as rows added for computation
  mydata <<- mydata[!is.na(mydata$Year), ]
  cpi <<- read.xls("./data/cpi.xlsx", 2)
  pop <<- read.xls("./data/pop_consolidate.xlsx", 1)
  gdp <<- read.xls("./data/5206001_key_aggregates.xlsx", 2)
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
    "Buildings_Public_Damaged_Count",
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


# Calculate cost of housing contents
costOfResidentialBuildingContents <- function(events) {
  # Use an average cost, as per  http://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/4102.0Main+Features10Dec+2011#Contents5
  return (indexCosts(c(2010, 61000)))
}

# Calculate cost of housing
costOfResidentialBuilding <- function(events) {
  destroyed <- as.numeric(events$Buildings_Private_Destroyed_Count)
  damaged <- as.numeric(events$Buildings_Private_Damaged_Count)
  destroyed[is.na(destroyed)] <- 0
  damaged[is.na(damaged)] <- 0
  
  destroyedCost = destroyed * 469536.015
  damagedCost = damaged * destroyedCost * 0.5
  contentsCosts <- costOfResidentialBuildingContents(events)
  
  return (destroyedCost + damagedCost + contentsCosts)
}



# Calculate cost of commerial buildings
costOfCommercialBuildings <- function(events) {
  destroyedGeneral <- as.numeric(events$Buildings_Commercial_Destroyed_Count_General)
  damagedGeneral <- as.numeric(events$Buildings_Commercial_Damaged_Count_General)
  destroyedGeneral[is.na(destroyedGeneral)] <- 0
  damagedGeneral[is.na(damagedGeneral)] <- 0
  destroyedIndustrial <- as.numeric(events$Buildings_Commercial_Destroyed_Count_Industrial)
  damagedIndustrial <- as.numeric(events$Buildings_Commercial_Damaged_Count_Industrial)
  destroyedIndustrial[is.na(destroyedIndustrial)] <- 0
  damagedIndustrial[is.na(damagedIndustrial)] <- 0
  destroyedHotels <- as.numeric(events$Buildings_Commercial_Destroyed_Count_Hotels)
  damagedHotels <- as.numeric(events$Buildings_Commercial_Damaged_Count_Hotels)
  destroyedHotels[is.na(destroyedHotels)] <- 0
  damagedHotels[is.na(damagedHotels)] <- 0

  costOfStructures <- destroyedGeneral * 431232 + destroyedIndustrial * 498750 + destroyedHotels * indexCosts(c(2005, 3885000))
  costOfContents <- 0
  
  return (costOfStructures + costOfContents)
}


# Calculate cost of public buildings
costOfPublicBuildings <- function(events) {
  destroyed <- as.numeric(events$Buildings_Public_Destroyed_Count)
  destroyedType <- as.numeric(events$Buildings_Public_Destroyed_Type)
  damaged <- as.numeric(events$Buildings_Public_Damaged_Count)
  damagedType <- as.numeric(events$Buildings_Public_Damaged_Type)
  destroyed[is.na(destroyed)] <- 0
  damaged[is.na(damaged)] <- 0

  costOfStructures <- 0
  costOfContents <- 0
  
  return (costOfStructures + costOfContents)
}


# Calculate cost of infrastructure
costOfInfrastructure <- function(events) {
  destroyedRoadsUrban <- as.numeric(events$Infrastructure_Public_Destroyed_Count_Roads_Urban)
  damagedRoadsUrban <- as.numeric(events$Infrastructure_Public_Damaged_Count_Roads_Urban)
  destroyedRoadsUrban[is.na(destroyedRoadsUrban)] <- 0
  damagedRoadsUrban[is.na(damagedRoadsUrban)] <- 0
  destroyedRoadsRural <- as.numeric(events$Infrastructure_Public_Destroyed_Count_Roads_Rural)
  damagedRoadsRural <- as.numeric(events$Infrastructure_Public_Damaged_Count_Roads_Rural)
  destroyedRoadsRural[is.na(destroyedRoadsRural)] <- 0
  damagedRoadsRural[is.na(damagedRoadsRural)] <- 0
  destroyedBridges <- as.numeric(events$Infrastructure_Public_Destroyed_Count_Bridges)
  damagedBridges <- as.numeric(events$Infrastructure_Public_Damaged_Count_Bridges)
  destroyedBridges[is.na(destroyedBridges)] <- 0
  damagedBridges[is.na(damagedBridges)] <- 0
  destroyedRail <- as.numeric(events$Infrastructure_Public_Destroyed_Count_Rail)
  damagedRail <- as.numeric(events$Infrastructure_Public_Damaged_Count_Rail)
  destroyedRail[is.na(destroyedRail)] <- 0
  damagedRail[is.na(damagedRail)] <- 0
  destroyedPowerPoles <- as.numeric(events$Infrastructure_Public_Destroyed_Count_Power_Poles)
  damagedPowerPoles <- as.numeric(events$Infrastructure_Public_Damaged_Count_Power_Poles)
  destroyedPowerPoles[is.na(destroyedPowerPoles)] <- 0
  damagedPowerPoles[is.na(damagedPowerPoles)] <- 0

  costs <-  destroyedRoadsUrban * indexCosts(c(2011, 2168975)) + damagedRoadsUrban * 0  +
            destroyedRoadsRural * indexCosts(c(2011, 666600)) + damagedRoadsRural * 0  +
            destroyedBridges * indexCosts(c(2013, 28600000)) + damagedBridges * 0 + 
            destroyedRail * indexCosts(c(2010, 42570000)) + damagedRail * 0 + 
            destroyedPowerPoles * indexCosts(c(2013, 9000)) + damagedPowerPoles * 0

  return (costs)
}


# Calculate cost of transport
costOfVehicles <- function(events) {
  destroyedAircraft <- as.numeric(events$Vehicle_Public_Destroyed_Count_Aircraft)
  damagedAircraft <- as.numeric(events$Vehicle_Public_Damaged_Count_Aircraft)
  destroyedTrains <- as.numeric(events$Vehicle_Public_Destroyed_Count_Train)
  damagedTrains <- as.numeric(events$Vehicle_Public_Damaged_Count_Train)
  destroyedBoats <- as.numeric(events$Vehicle_Private_Destroyed_Count_Boats)
  damagedBoats <- as.numeric(events$Vehicle_Private_Damaged_Count_Boats)
  destroyedCars <- as.numeric(events$Vehicle_Private_Destroyed_Count_Cars)
  damagedCars <- as.numeric(events$Vehicle_Private_Damaged_Count_Cars)
  destroyedCaravans <- as.numeric(events$Vehicle_Private_Destroyed_Count_Caravans)
  damagedCaravans <- as.numeric(events$Vehicle_Private_Damaged_Count_Caravans)

  publicVehicles <- destroyedAircraft * indexCosts(c(2015, 1650000)) + 
                    damagedAircraft * 0 +
                    destroyedTrains  * indexCosts(c(2011, 15800000)) + 
                    damagedTrains  * 0
  privateVehicles <- destroyedBoats * indexCosts(c(2014, 742531)) + 
                    damagedBoats * 0 +
                    destroyedCars * indexCosts(c(2010, 18000)) + 
                    damagedCars * indexCosts(c(2008, 3853)) +
                    destroyedCaravans * indexCosts(c(2014, 30000)) + 
                    damagedCaravans * 0

  total <- publicVehicles + privateVehicles

  return (total)
}


# Get the cost of specific crop types
costOfCropType <- function(cropType) {
  cropTypeCost <- switch(cropType, 
    "Cotton" = indexCosts(c(2011, 460)),
    "Wheat and Barley" = indexCosts(c(2013, 570.67)),
    "Cereal and fruit" = indexCosts(c(2013, 0)), # TBD
    "Pasture and lumber" = indexCosts(c(2013, 0)), # TBD
    "Pasture" = indexCosts(c(2013, 60)),
    "Sugar cane" = indexCosts(c(2013, 40)),
    "Fruit" = indexCosts(c(2013, 40000))
  )
  if (is.null(cropTypeCost)) {
     cropTypeCost <- 0
  }
  return (cropTypeCost)
}

# Calculate cost of agriculture
costOfAgriculture <- function(events) {
  destroyedSheds <- as.numeric(events$Infrastructure_Private_Destroyed_Count)
  damagedSheds <- as.numeric(events$Infrastructure_Private_Damaged_Count)
  fencing <- as.numeric(events$Fencing)
  land <- as.numeric(events$Land_Private_Count)
  crops <- as.numeric(events$Crops_Destroyed_Count)
  cropType <- events$Crops_Destroyed_Type
  cropCosts <- crops * apply(data.matrix(events$Crops_Destroyed_Type), 1, costOfCropType)

  livestock <- as.numeric(events$Livestock_Destroyed_Count)
  cattle <- as.numeric(events$Livestock_Destroyed_Count_Cattle)
  sheepGoats <- as.numeric(events$Livestock_Destroyed_Count_Sheep_Goats)
  poultry <- as.numeric(events$Livestock_Destroyed_Count_Poultry)
  pigs <- as.numeric(events$Livestock_Destroyed_Count_Pigs)
  other <- as.numeric(events$Livestock_Destroyed_Count_Other)
  livestockCosts <- cattle * indexCosts(c(2012, 788)) + 
                    sheepGoats * indexCosts(c(2012, 87)) + 
                    poultry * indexCosts(c(2009, 50)) + 
                    pigs * indexCosts(c(2015, 200)) + 
                    other * indexCosts(c(2007, 771.50))
  
  total <- destroyedSheds * indexCosts(c(2015, 18750)) + 
          damagedSheds * indexCosts(c(2015, 0)) + 
          fencing * indexCosts(c(2014, 11600)) + 
          land * indexCosts(c(2005, 2067))  + # USD
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


# Emergency response
emergencyResponseCosts <- function(events) {
  assistance <- as.numeric(events$Assistance_numbers)
  evacuated <- as.numeric(events$Evacuated)
  homeless <- as.numeric(events$Homeless)
  affected <- as.numeric(events$Affected)

  calls <- costOfPublicServiceCalls(events)
  assistanceCosts <- 0
  evacuatedHotelCosts <- evacuated * indexCosts(c(2012, 168)) * 2 * 0.5 # Assume 2 -night stay, divided by 2 people
  homelessCosts <- 0
  affectedCosts <- 0

  total <- calls + assistanceCosts + evacuatedHotelCosts + homelessCosts + affectedCosts

  return (total)
}


# Residential disruption
householdDisruptionCosts <- function(events) {
  # Get key fields
  destroyed <- as.numeric(events$Buildings_Private_Destroyed_Count)
  damaged <- as.numeric(events$Buildings_Private_Damaged_Count)
  
  # NOTE: COST BASIS FOR DAMAGED & DESTROYED IS THE SAME, AS PER SANDRA'S / DELOITTE'S COMMENTS
  cleanupCosts <- destroyed * indexCosts(c(2012, 5900)) + 
                  damaged * indexCosts(c(2012, 5900))
  # Old calculations
  # cleanup <- 330 * totalAffected
  
  # Accom costs - NOTE THIS IS MUCH TOO CHEAP
  # alternativeAccomPerNight <- indexCosts(c(1999, 26))
  # evacuatedCosts = alternativeAccomPerNight * evacuated * 7
  # homelessCosts = alternativeAccomPerNight * homeless * 70
  # accom = evacuatedCosts + homelessCosts
  
  total <- cleanupCosts

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
  lossOfProductionAndServices <- costOfCommercialBuildings(events)  * 0.1

  # NOTE: COST BASIS FOR DAMAGED & DESTROYED IS THE SAME, AS PER SANDRA'S / DELOITTE'S COMMENTS
  destroyedLoss <- destroyed * indexCosts(c(2011, 3800))
  damagedLoss <- damaged * indexCosts(c(2011, 3800))

  total <- lossOfProductionAndServices + destroyedLoss + damagedLoss
  

  # industrialLoss = total * industrialProportionOfCommercial()
  # commercialLoss =  total - industrialLoss
  
  # disruption = commercialLoss * 0.1 + industrialLoss * 0.65
  
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

  # IGNORED FOR NOW
  # These costs are for FLOODS
  # $25ha floodway areas
  # $10ha low velocity flood events
  # $350 horticultural
  # ASSUMPTION: Taking low value of $15ha
  # cleanupLand <- land * 15
  
  return (disruptionAgriculture + cleanupLivestock + cleanupCrops)
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
  
  destroyedLoss <- destroyed * indexCosts(c(2000, 15000))
  damagedLoss <- 0 * damaged * indexCosts(c(2000, 15000))
  landLoss <- 0 * land

  total <- destroyedLoss + damagedLoss + landLoss
  
  return (total)
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
  events$indirectCost.normalised <- events$indirectCost

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
  events$injuryCosts <- injuryCosts(events)
  deathAndInjuryCosts <- rowSums(subset(events, select = c(deathCosts, injuryCosts)), na.rm = TRUE)

  # Normalised values
  events$deathCosts.normalised <- deathCostsNormalised(events)
  events$injuryCosts.normalised <- injuryCostsNormalised(events)
  deathAndInjuryCostsNormalised <- rowSums(subset(events, select = c(deathCosts.normalised, injuryCosts.normalised)), na.rm = TRUE)

  ecosystemCosts <- ecosystemCosts(events)
  healthImpactCosts <- healthImpactCosts(events)
  memorabiliaCosts <- memorabiliaCosts(events)
  culturalHeritageCosts <- culturalHeritageCosts(events)
  nonDeathAndInjuryIntangibles <- ecosystemCosts + healthImpactCosts + memorabiliaCosts + culturalHeritageCosts

  events$intangibleCost = deathAndInjuryCosts + nonDeathAndInjuryIntangibles

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
  events <- totalCostForEvent(resourceTypeParam)
  # events <- getEvents(resourceTypeParam)
  # events <- directCosts(events)
  # events <- indirectCosts(events)
  # events <- intangibleCosts(events)
  # events$total <- rowSums(subset(events, select = c(directCost, indirectCost, intangibleCost)), na.rm = TRUE)
  # events$total.normalised <- rowSums(subset(events, select = c(directCost.normalised, indirectCost.normalised, intangibleCost.normalised)), na.rm = TRUE)
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
  data <- mydata[c("Evacuated", "Insured.Costs.indexed.i")]
  mydata$Evacuated.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Homeless", "Insured.Costs.indexed.i")]
  mydata$Homeless.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Calls.to.SES", "Insured.Costs.indexed.i")]
  mydata$Calls.to.SES.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Assistance_dollars", "Insured.Costs.indexed.i")]
  mydata$Assistance_dollars.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Infrastructure_Public_Destroyed_Count", "Insured.Costs.indexed.i")]
  mydata$Infrastructure_Public_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Infrastructure_Public_Damaged_Count", "Insured.Costs.indexed.i")]
  mydata$Infrastructure_Public_Damaged_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Infrastructure_Public_Destroyed_Count_Roads_Urban", "Insured.Costs.indexed.i")]
  mydata$Infrastructure_Public_Destroyed_Count_Roads_Urban.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Infrastructure_Public_Damaged_Count_Roads_Urban", "Insured.Costs.indexed.i")]
  mydata$Infrastructure_Public_Damaged_Count_Roads_Urban.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Infrastructure_Public_Destroyed_Count_Roads_Rural", "Insured.Costs.indexed.i")]
  mydata$Infrastructure_Public_Destroyed_Count_Roads_Rural.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Infrastructure_Public_Damaged_Count_Roads_Rural", "Insured.Costs.indexed.i")]
  mydata$Infrastructure_Public_Damaged_Count_Roads_Rural.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Infrastructure_Public_Destroyed_Count_Bridges", "Insured.Costs.indexed.i")]
  mydata$Infrastructure_Public_Destroyed_Count_Bridges.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Infrastructure_Public_Damaged_Count_Bridges", "Insured.Costs.indexed.i")]
  mydata$Infrastructure_Public_Damaged_Count_Bridges.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Infrastructure_Public_Destroyed_Count_Rail", "Insured.Costs.indexed.i")]
  mydata$Infrastructure_Public_Destroyed_Count_Rail.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Infrastructure_Public_Damaged_Count_Rail", "Insured.Costs.indexed.i")]
  mydata$Infrastructure_Public_Damaged_Count_Rail.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Infrastructure_Public_Destroyed_Count_Power_Poles", "Insured.Costs.indexed.i")]
  mydata$Infrastructure_Public_Destroyed_Count_Power_Poles.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Infrastructure_Public_Damaged_Count_Power_Poles", "Insured.Costs.indexed.i")]
  mydata$Infrastructure_Public_Damaged_Count_Power_Poles.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Vehicle_Public_Destroyed_Count", "Insured.Costs.indexed.i")]
  mydata$Vehicle_Public_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Vehicle_Public_Damaged_Count", "Insured.Costs.indexed.i")]
  mydata$Vehicle_Public_Damaged_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Vehicle_Public_Destroyed_Count_Aircraft", "Insured.Costs.indexed.i")]
  mydata$Vehicle_Public_Destroyed_Count_Aircraft.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Vehicle_Public_Damaged_Count_Aircraft", "Insured.Costs.indexed.i")]
  mydata$Vehicle_Public_Damaged_Count_Aircraft.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Vehicle_Public_Destroyed_Count_Train", "Insured.Costs.indexed.i")]
  mydata$Vehicle_Public_Destroyed_Count_Train.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Vehicle_Public_Damaged_Count_Train", "Insured.Costs.indexed.i")]
  mydata$Vehicle_Public_Damaged_Count_Train.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Vehicle_Private_Destroyed_Count", "Insured.Costs.indexed.i")]
  mydata$Vehicle_Private_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Vehicle_Private_Damaged_Count", "Insured.Costs.indexed.i")]
  mydata$Vehicle_Private_Damaged_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Vehicle_Private_Destroyed_Count_Boats", "Insured.Costs.indexed.i")]
  mydata$Vehicle_Private_Destroyed_Count_Boats.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Vehicle_Private_Damaged_Count_Boats", "Insured.Costs.indexed.i")]
  mydata$Vehicle_Private_Damaged_Count_Boats.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Vehicle_Private_Destroyed_Count_Cars", "Insured.Costs.indexed.i")]
  mydata$Vehicle_Private_Destroyed_Count_Cars.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Vehicle_Private_Damaged_Count_Cars", "Insured.Costs.indexed.i")]
  mydata$Vehicle_Private_Damaged_Count_Cars.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Vehicle_Private_Destroyed_Count_Caravans", "Insured.Costs.indexed.i")]
  mydata$Vehicle_Private_Destroyed_Count_Caravans.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Vehicle_Private_Damaged_Count_Caravans", "Insured.Costs.indexed.i")]
  mydata$Vehicle_Private_Damaged_Count_Caravans.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Buildings_Commercial_Destroyed_Count", "Insured.Costs.indexed.i")]
  mydata$Buildings_Commercial_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Buildings_Commercial_Damaged_Count", "Insured.Costs.indexed.i")]
  mydata$Buildings_Commercial_Damaged_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Buildings_Private_Destroyed_Count", "Insured.Costs.indexed.i")]
  mydata$Buildings_Private_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Buildings_Private_Damaged_Count", "Insured.Costs.indexed.i")]
  mydata$Buildings_Private_Damaged_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Buildings_Public_Destroyed_Count", "Insured.Costs.indexed.i")]
  mydata$Buildings_Public_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Buildings_Public_Damaged_Count", "Insured.Costs.indexed.i")]
  mydata$Buildings_Public_Damaged_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Land_Public_Count", "Insured.Costs.indexed.i")]
  mydata$Land_Public_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Land_Private_Count", "Insured.Costs.indexed.i")]
  mydata$Land_Private_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Crops_Destroyed_Count", "Insured.Costs.indexed.i")]
  mydata$Crops_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Livestock_Destroyed_Count", "Insured.Costs.indexed.i")]
  mydata$Livestock_Destroyed_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Livestock_Destroyed_Count_Cattle", "Insured.Costs.indexed.i")]
  mydata$Livestock_Destroyed_Count_Cattle.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Livestock_Destroyed_Count_Sheep_Goats", "Insured.Costs.indexed.i")]
  mydata$Livestock_Destroyed_Count_Sheep_Goats.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Livestock_Destroyed_Count_Poultry", "Insured.Costs.indexed.i")]
  mydata$Livestock_Destroyed_Count_Poultry.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Livestock_Destroyed_Count_Pigs", "Insured.Costs.indexed.i")]
  mydata$Livestock_Destroyed_Count_Pigs.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Livestock_Destroyed_Count_Other", "Insured.Costs.indexed.i")]
  mydata$Livestock_Destroyed_Count_Other.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Environmental_Count", "Insured.Costs.indexed.i")]
  mydata$Environmental_Count.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)
  data <- mydata[c("Fencing", "Insured.Costs.indexed.i")]
  mydata$Fencing.i <<- apply(cbind(data, ratio=ratio(data)), 1, interpollate)

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
  mydata$Assistance_dollars.ni <<- mydata$Assistance_dollars
  mydata$Assistance_dollars <<- mydata$Assistance_dollars.i
  mydata$Infrastructure_Public_Damaged_Count.ni <<- mydata$Infrastructure_Public_Damaged_Count
  mydata$Infrastructure_Public_Damaged_Count <<- mydata$Infrastructure_Public_Damaged_Count.i
  mydata$Infrastructure_Public_Destroyed_Count.ni <<- mydata$Infrastructure_Public_Destroyed_Count
  mydata$Infrastructure_Public_Destroyed_Count <<- mydata$Infrastructure_Public_Destroyed_Count.i
  mydata$Infrastructure_Public_Damaged_Count_Roads_Urban.ni <<- mydata$Infrastructure_Public_Damaged_Count_Roads_Urban
  mydata$Infrastructure_Public_Damaged_Count_Roads_Urban <<- mydata$Infrastructure_Public_Damaged_Count_Roads_Urban.i
  mydata$Infrastructure_Public_Destroyed_Count_Roads_Urban.ni <<- mydata$Infrastructure_Public_Destroyed_Count_Roads_Urban
  mydata$Infrastructure_Public_Destroyed_Count_Roads_Urban <<- mydata$Infrastructure_Public_Destroyed_Count_Roads_Urban.i
  mydata$Infrastructure_Public_Damaged_Count_Roads_Rural.ni <<- mydata$Infrastructure_Public_Damaged_Count_Roads_Rural
  mydata$Infrastructure_Public_Damaged_Count_Roads_Rural <<- mydata$Infrastructure_Public_Damaged_Count_Roads_Rural.i
  mydata$Infrastructure_Public_Destroyed_Count_Roads_Rural.ni <<- mydata$Infrastructure_Public_Destroyed_Count_Roads_Rural
  mydata$Infrastructure_Public_Destroyed_Count_Roads_Rural <<- mydata$Infrastructure_Public_Destroyed_Count_Roads_Rural.i
  mydata$Infrastructure_Public_Damaged_Count_Bridges.ni <<- mydata$Infrastructure_Public_Damaged_Count_Bridges
  mydata$Infrastructure_Public_Damaged_Count_Bridges <<- mydata$Infrastructure_Public_Damaged_Count_Bridges.i
  mydata$Infrastructure_Public_Destroyed_Count_Bridges.ni <<- mydata$Infrastructure_Public_Destroyed_Count_Bridges
  mydata$Infrastructure_Public_Destroyed_Count_Bridges <<- mydata$Infrastructure_Public_Destroyed_Count_Bridges.i
  mydata$Infrastructure_Public_Damaged_Count_Rail.ni <<- mydata$Infrastructure_Public_Damaged_Count_Rail
  mydata$Infrastructure_Public_Damaged_Count_Rail <<- mydata$Infrastructure_Public_Damaged_Count_Rail.i
  mydata$Infrastructure_Public_Destroyed_Count_Rail.ni <<- mydata$Infrastructure_Public_Destroyed_Count_Rail
  mydata$Infrastructure_Public_Destroyed_Count_Rail <<- mydata$Infrastructure_Public_Destroyed_Count_Rail.i
  mydata$Infrastructure_Public_Damaged_Count_Power_Poles.ni <<- mydata$Infrastructure_Public_Damaged_Count_Power_Poles
  mydata$Infrastructure_Public_Damaged_Count_Power_Poles <<- mydata$Infrastructure_Public_Damaged_Count_Power_Poles.i
  mydata$Infrastructure_Public_Destroyed_Count_Power_Poles.ni <<- mydata$Infrastructure_Public_Destroyed_Count_Power_Poles
  mydata$Infrastructure_Public_Destroyed_Count_Power_Poles <<- mydata$Infrastructure_Public_Destroyed_Count_Power_Poles.i
  mydata$Buildings_Commercial_Destroyed_Count.ni <<- mydata$Buildings_Commercial_Destroyed_Count
  mydata$Buildings_Commercial_Destroyed_Count <<- mydata$Buildings_Commercial_Destroyed_Count.i
  mydata$Buildings_Commercial_Destroyed_General_Count.ni <<- mydata$Buildings_Commercial_Destroyed_General_Count
  mydata$Buildings_Commercial_Destroyed_General_Count <<- mydata$Buildings_Commercial_Destroyed_General_Count.i
  mydata$Buildings_Commercial_Destroyed_Industrial_Count.ni <<- mydata$Buildings_Commercial_Destroyed_Industrial_Count
  mydata$Buildings_Commercial_Destroyed_Industrial_Count <<- mydata$Buildings_Commercial_Destroyed_Industrial_Count.i
  mydata$Buildings_Commercial_Destroyed_Hotels_Count.ni <<- mydata$Buildings_Commercial_Destroyed_Hotels_Count
  mydata$Buildings_Commercial_Destroyed_Hotels_Count <<- mydata$Buildings_Commercial_Destroyed_Hotels_Count.i
  mydata$Buildings_Commercial_Damaged_Count.ni <<- mydata$Buildings_Commercial_Damaged_Count
  mydata$Buildings_Commercial_Damaged_Count <<- mydata$Buildings_Commercial_Damaged_Count.i
  mydata$Buildings_Commercial_Damaged_General_Count.ni <<- mydata$Buildings_Commercial_Damaged_General_Count
  mydata$Buildings_Commercial_Damaged_General_Count <<- mydata$Buildings_Commercial_Damaged_General_Count.i
  mydata$Buildings_Commercial_Damaged_Industrial_Count.ni <<- mydata$Buildings_Commercial_Damaged_Industrial_Count
  mydata$Buildings_Commercial_Damaged_Industrial_Count <<- mydata$Buildings_Commercial_Damaged_Industrial_Count.i
  mydata$Buildings_Commercial_Damaged_Hotels_Count.ni <<- mydata$Buildings_Commercial_Damaged_Hotels_Count
  mydata$Buildings_Commercial_Damaged_Hotels_Count <<- mydata$Buildings_Commercial_Damaged_Hotels_Count.i
  mydata$Buildings_Private_Destroyed_Count.ni <<- mydata$Buildings_Private_Destroyed_Count
  mydata$Buildings_Private_Destroyed_Count <<- mydata$Buildings_Private_Destroyed_Count.i
  mydata$Buildings_Private_Damaged_Count.ni <<- mydata$Buildings_Private_Damaged_Count
  mydata$Buildings_Private_Damaged_Count <<- mydata$Buildings_Private_Damaged_Count.i
  mydata$Buildings_Private_Destroyed_Count.ni <<- mydata$Buildings_Private_Destroyed_Count
  mydata$Buildings_Private_Destroyed_Count <<- mydata$Buildings_Private_Destroyed_Count.i
  mydata$Buildings_Private_Damaged_Count.ni <<- mydata$Buildings_Private_Damaged_Count
  mydata$Buildings_Private_Damaged_Count <<- mydata$Buildings_Private_Damaged_Count.i

  mydata$Vehicle_Public_Damaged_Count.ni <<- mydata$Vehicle_Public_Damaged_Count
  mydata$Vehicle_Public_Damaged_Count <<- mydata$Vehicle_Public_Damaged_Count.i
  mydata$Vehicle_Public_Damaged_Count_Aircraft.ni <<- mydata$Vehicle_Public_Damaged_Count_Aircraft
  mydata$Vehicle_Public_Damaged_Count_Aircraft <<- mydata$Vehicle_Public_Damaged_Count_Aircraft.i
  mydata$Vehicle_Public_Damaged_Count_Train.ni <<- mydata$Vehicle_Public_Damaged_Count_Train
  mydata$Vehicle_Public_Damaged_Count_Train <<- mydata$Vehicle_Public_Damaged_Count_Train.i
  mydata$Vehicle_Public_Destroyed_Count.ni <<- mydata$Vehicle_Public_Destroyed_Count
  mydata$Vehicle_Public_Destroyed_Count <<- mydata$Vehicle_Public_Destroyed_Count.i
  mydata$Vehicle_Public_Destroyed_Count_Aircraft.ni <<- mydata$Vehicle_Public_Destroyed_Count_Aircraft
  mydata$Vehicle_Public_Destroyed_Count_Aircraft <<- mydata$Vehicle_Public_Destroyed_Count_Aircraft.i
  mydata$Vehicle_Public_Destroyed_Count_Train.ni <<- mydata$Vehicle_Public_Destroyed_Count_Train
  mydata$Vehicle_Public_Destroyed_Count_Train <<- mydata$Vehicle_Public_Destroyed_Count_Train.i

  mydata$Vehicle_Private_Damaged_Count.ni <<- mydata$Vehicle_Private_Damaged_Count
  mydata$Vehicle_Private_Damaged_Count <<- mydata$Vehicle_Private_Damaged_Count.i
  mydata$Vehicle_Private_Damaged_Count_Boats.ni <<- mydata$Vehicle_Private_Damaged_Count_Boats
  mydata$Vehicle_Private_Damaged_Count_Boats <<- mydata$Vehicle_Private_Damaged_Count_Boats.i
  mydata$Vehicle_Private_Damaged_Count_Cars.ni <<- mydata$Vehicle_Private_Damaged_Count_Cars
  mydata$Vehicle_Private_Damaged_Count_Cars <<- mydata$Vehicle_Private_Damaged_Count_Cars.i
  mydata$Vehicle_Private_Damaged_Count_Caravans.ni <<- mydata$Vehicle_Private_Damaged_Count_Caravans
  mydata$Vehicle_Private_Damaged_Count_Caravans <<- mydata$Vehicle_Private_Damaged_Count_Caravans.i
  mydata$Vehicle_Private_Destroyed_Count.ni <<- mydata$Vehicle_Private_Destroyed_Count
  mydata$Vehicle_Private_Destroyed_Count <<- mydata$Vehicle_Private_Destroyed_Count.i
  mydata$Vehicle_Private_Destroyed_Count_Boats.ni <<- mydata$Vehicle_Private_Destroyed_Count_Boats
  mydata$Vehicle_Private_Destroyed_Count_Boats <<- mydata$Vehicle_Private_Destroyed_Count_Boats.i
  mydata$Vehicle_Private_Destroyed_Count_Cars.ni <<- mydata$Vehicle_Private_Destroyed_Count_Cars
  mydata$Vehicle_Private_Destroyed_Count_Cars <<- mydata$Vehicle_Private_Destroyed_Count_Cars.i
  mydata$Vehicle_Private_Destroyed_Count_Caravans.ni <<- mydata$Vehicle_Private_Destroyed_Count_Caravans
  mydata$Vehicle_Private_Destroyed_Count_Caravans <<- mydata$Vehicle_Private_Destroyed_Count_Caravans.i

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
  mydata$Insured.Costs.normalised <<- mydata$Insured.Costs.normalised.ni
  mydata$Insured.Costs.indexed <<- mydata$Insured.Costs.indexed.ni
  mydata$Evacuated <<- mydata$Evacuated.ni
  mydata$Homeless <<- mydata$Homeless.ni
  mydata$Calls.to.SES <<- mydata$Calls.to.SES.ni
  mydata$Assistance_dollars <<- mydata$Assistance_dollars.ni
  mydata$Buildings_Commercial_Destroyed_Count <<- mydata$Buildings_Commercial_Destroyed_Count.ni
  mydata$Buildings_Commercial_Damaged_Count <<- mydata$Buildings_Commercial_Damaged_Count.ni
  mydata$Buildings_Private_Destroyed_Count <<- mydata$Buildings_Private_Destroyed_Count.ni
  mydata$Buildings_Private_Damaged_Count <<- mydata$Buildings_Private_Damaged_Count.ni
  mydata$Public.buildings.destroyed <<- mydata$Public.buildings.destroyed.ni
  mydata$Public.buildings.damaged <<- mydata$Public.buildings.damaged.ni
  mydata$Land_Public_Count <<- mydata$Land_Public_Count.ni
  mydata$Land_Private_Count <<- mydata$Land_Private_Count.ni
  mydata$Crops_Destroyed_Count <<- mydata$Crops_Destroyed_Count.ni
  mydata$Livestock_Destroyed_Count <<- mydata$Livestock_Destroyed_Count.ni
  mydata$Environmental_Count <<- mydata$Environmental_Count.ni
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
  mydata$Infrastructure_Public_Damaged_Dollars <- apply(cbind(mydata[c("Infrastructure_Public_Damaged_Count", "Infrastructure_Public_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Infrastructure_Public_Destroyed_Dollars <- apply(cbind(mydata[c("Infrastructure_Public_Destroyed_Count", "Infrastructure_Public_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Infrastructure_Private_Damaged_Dollars <- apply(cbind(mydata[c("Infrastructure_Private_Damaged_Count", "Infrastructure_Private_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Infrastructure_Private_Destroyed_Dollars <- apply(cbind(mydata[c("Infrastructure_Private_Destroyed_Count", "Infrastructure_Private_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Vehicle_Public_Damaged_Dollars <- apply(cbind(mydata[c("Vehicle_Public_Damaged_Count", "Vehicle_Public_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Vehicle_Public_Destroyed_Dollars <- apply(cbind(mydata[c("Vehicle_Public_Destroyed_Count", "Vehicle_Public_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Vehicle_Private_Damaged_Dollars <- apply(cbind(mydata[c("Vehicle_Private_Damaged_Count", "Vehicle_Private_Damaged_Dollars")], 1), 1, convertSingleCountToDollars)
  mydata$Vehicle_Private_Destroyed_Dollars <- apply(cbind(mydata[c("Vehicle_Private_Destroyed_Count", "Vehicle_Private_Destroyed_Dollars")], 1), 1, convertSingleCountToDollars)
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
  mydata$Infrastructure_Public_Damaged_Count <<- apply(mydata[c("Infrastructure_Public_Damaged_Dollars", "Infrastructure_Public_Damaged_Count", "Year")], 1, dollarsToCount)
  mydata$Infrastructure_Public_Destroyed_Count <<- apply(mydata[c("Infrastructure_Public_Destroyed_Dollars", "Infrastructure_Public_Destroyed_Count", "Year")], 1, dollarsToCount)
  mydata$Infrastructure_Private_Damaged_Count <<- apply(mydata[c("Infrastructure_Private_Damaged_Dollars", "Infrastructure_Private_Damaged_Count", "Year")], 1, dollarsToCount)
  mydata$Infrastructure_Private_Destroyed_Count <<- apply(mydata[c("Infrastructure_Private_Destroyed_Dollars", "Infrastructure_Private_Destroyed_Count", "Year")], 1, dollarsToCount)
  mydata$Vehicle_Public_Damaged_Count <<- apply(mydata[c("Vehicle_Public_Damaged_Dollars", "Vehicle_Public_Damaged_Count", "Year")], 1, dollarsToCount)
  mydata$Vehicle_Public_Destroyed_Count <<- apply(mydata[c("Vehicle_Public_Destroyed_Dollars", "Vehicle_Public_Destroyed_Count", "Year")], 1, dollarsToCount)
  mydata$Vehicle_Private_Damaged_Count <<- apply(mydata[c("Vehicle_Private_Damaged_Dollars", "Vehicle_Private_Damaged_Count", "Year")], 1, dollarsToCount)
  mydata$Vehicle_Private_Destroyed_Count <<- apply(mydata[c("Vehicle_Private_Destroyed_Dollars", "Vehicle_Private_Destroyed_Count", "Year")], 1, dollarsToCount)
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
  
  swapInterpollatedForNormalCosts()
  
  
  # Repeats logic from totalCostForEvent(), getEvents()
  mydata$Deaths <<- as.numeric(mydata$Deaths)
  mydata$Injuries <<- as.numeric(mydata$Injuries)
  mydata$Deaths.normalised <<- as.numeric(mydata$Deaths.normalised)
  mydata$Injuries.normalised <<- as.numeric(mydata$Injuries.normalised)
  # xsub <- mydata[,6:24] 
  # xsub[is.na(xsub)] <- 0 
  # mydata[,6:24]<-xsub

  mydata <<- computedDirectCosts(mydata)
  # mydata <- directCosts(mydata)
  mydata <<- indirectCosts(mydata)
  mydata <<- intangibleCosts(mydata)
  mydata$total <<- rowSums(subset(mydata, select = c(directCost, indirectCost, intangibleCost)), na.rm = TRUE)
  mydata$total.normalised <<- rowSums(subset(mydata, select = c(directCost.normalised, indirectCost.normalised, intangibleCost.normalised)), na.rm = TRUE)


  swapNormalForInterpollatedCosts()
  write.table(mydata, file = "./output/data.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
}



