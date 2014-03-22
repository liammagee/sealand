
# Sources
source("R/functions.r", TRUE)

# Load the data
loadData()

# Generate computed columns
computeColumns()

# Store the total costs by year
costsByYear <- with(totalCostForEvent(), aggregate(total, by=list(Year), FUN=safeSum))

# Plot a basic graph of costs
pdf(file="./figs/total_costs.pdf")
heading = "Total Costs"
plot(costsByYear, type="l", main=heading) 
lines(costsByYear, type="l")
dev.off()

