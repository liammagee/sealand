
# Sources
source("R/functions.r", TRUE)

# Load the data
loadData()

# Generate computed columns
computeColumns()

# Store the total costs by year
totalCosts <- totalCostForEvent()
totalCostsByType <- with(totalCosts, aggregate(total, by=list(resourceType), FUN=safeSum))
directCostsByType <- with(totalCosts, aggregate(directCost, by=list(resourceType), FUN=safeSum))
indirectCostsByType <- with(totalCosts, aggregate(indirectCost, by=list(resourceType), FUN=safeSum))
intangibleCostsByType <- with(totalCosts, aggregate(intangibleCost, by=list(resourceType), FUN=safeSum))

# Plot a basic graph of costs
#pdf(file="./figs/total_costs.pdf")

x_range <- range(0, totalCostsByType[2])

# Generate bar chart
x <- as.matrix(totalCostsByType)
barplot(as.vector(as.numeric(x[,2]), names.arg = c(x[,1]))

        
# Add title
title("Total Costs", col.main = "blue")

# Label the x and y axes with dark green text
title(xlab="Resource Types", col.lab=rgb(0,0.5,0))
title(ylab="(2013 Dollars in $millions)", col.lab=rgb(0,0.5,0))

#dev.off()

