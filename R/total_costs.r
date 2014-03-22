
# Sources
source("R/functions.r", TRUE)

# Load the data
loadData()

# Generate computed columns
computeColumns()

# Store the total costs by year
costsByYear <- with(totalCostForEvent(), aggregate(total, by=list(Year), FUN=safeSum))

# write.csv(mydata, file = "MyData.csv")
heading = "Total Costs" 
plot(costsByYear, type="l", main=heading) 
lines(costsByYear, type="l")


