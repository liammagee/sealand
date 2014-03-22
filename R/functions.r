
# Imports
library(gdata)
help(read.xls)

# Global options
options(stringsAsFactors=F)
options(scipen=999)


# Functions

## Ignores "NA" values for sum
safeSum <- function(value) sum(na.omit(value))
## Parses a currency value in the form "$1,000,000"
parseCurrency <- function(value) as.numeric(gsub(",", "", sub("\\$","", value)))
## Returns the correct financial year for a month and year
financialYear <- function(range) {
  month <- range[1]
  year <- range[2]
  firstMonths = c("January", "February", "March", "April", "May", "June")
  finYear <- if (is.element(month, firstMonths)) as.numeric(year) else as.numeric(year) + 1
  return(finYear)
}
## Generates an CPI indexed cost (based on June 2013)
indexCosts <- function(range) {
  finYear <- range[1]
  insuredCost <- range[2]
  cpiRow <- 85 + (finYear - 1967) * 4
  cpiTest <- as.numeric(cpi$Index.Numbers....All.groups.CPI....Australia[cpiRow])
  cpi2013 <- as.numeric(cpi$Index.Numbers....All.groups.CPI....Australia[269])
  return(insuredCost * (cpi2013 / cpiTest))
}
## Load data
loadData <- function() {
  mydata <<- read.xls("./data/report_v5.xlsx", 2)
  cpi <<- read.xls("./data/cpi.xlsx", 2)
}

## Generate computed columns
computeColumns <- function() {

  # ... for cleaned up costs 
  mydata$Normalised.Costs <<- apply(data.matrix(mydata[,20]), 1, parseCurrency)
  
  # ... for financial years
  mydata$Fin.Years <<- apply(mydata[c("Month", "Year")], 1, financialYear)
  
  # ... for CPI-indexed insured costs
  mydata$Indexed.Insured.Costs <<- apply(mydata[c("Fin.Years", "Normalised.Costs")], 1, indexCosts)
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
  return(indexCosts(c(2006, 2400000)))
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
  0.2
}

# Total cost for evant
totalCostForEvent <- function() {
  events <- mydata[c("Year", "Indexed.Insured.Costs", "Calls.to.SES", "Deaths", "Injuries")]
  events$Deaths <- as.numeric(events$Deaths)
  events$Injuries <- as.numeric(events$Injuries)
  xsub <- events[,1:5] 
  xsub[is.na(xsub)] <- 0 
  events[,1:5]<-xsub
  
  events$directCost <- with(events, Indexed.Insured.Costs)
  events$indirectCost <- with(events, Calls.to.SES * 10)
  events$deathCosts <- with(events, Deaths * costOfLife())
  events$injuryCosts <- with(events, 
                             Injuries  * proportionOfHospitalisedInjury() * costOfHospitalisedInjury() +
                               Injuries * (1 - proportionOfHospitalisedInjury()) * costOfNonHospitalisedInjury())
  events$intangibleCost <- events$deathCosts + events$injuryCosts
  events$total <- events$directCost + events$indirectCost + events$intangibleCost
  return(events) 
}

# Global procedures
loadData()

computeColumns()


# Global variable declarations

## Normalise variables



# Annual CPI (sourced from ABS - http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/6401.0Jun%202013#Time, Table 2)
baseYear <- 85
cpiJune67 = cpi$Index.Numbers....All.groups.CPI....Australia[baseYear]
cpiJune67

# Loop in multiples of 4 for each year's figure
lastYear <- baseYear + 46 * 4
cpiJune13 = cpi$Index.Numbers....All.groups.CPI....Australia[lastYear]
cpiJune13

lastYear <- baseYear + 32 * 4
cpiJune99 = cpi$Index.Numbers....All.groups.CPI....Australia[lastYear]
cpiJune99


costsByYear <- with(totalCostForEvent(), aggregate(total, by=list(Year), FUN=safeSum))
costsByYear

# write.csv(mydata, file = "MyData.csv")
heading = "Total Costs" 
plot(costsByYear, type="l", main=heading) 
lines(costsByYear, type="l")


