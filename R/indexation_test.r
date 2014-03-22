
# Global procedures

## Load the data
loadData()

## Generate computed columns
computeColumns()


# Annual CPI (sourced from ABS - http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/6401.0Jun%202013#Time, Table 2)
baseYear <- 85
cpiJune67 = cpi$Index.Numbers....All.groups.CPI....Australia[baseYear]
cpiJune67

# # Loop in multiples of 4 for each year's figure
lastYear <- baseYear + 46 * 4
cpiJune13 = cpi$Index.Numbers....All.groups.CPI....Australia[lastYear]
cpiJune13

lastYear <- baseYear + 32 * 4
cpiJune99 = cpi$Index.Numbers....All.groups.CPI....Australia[lastYear]
cpiJune99
