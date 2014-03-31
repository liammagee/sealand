
# Sources
source("R/functions.r", TRUE)

# Load the data
loadData()

# Generate computed columns
computeColumns()


popRatio(1974)
cpiRatio(1974)
cpiRatio(1974)

popRatio(1980)
popRatio(1981)
popRatio(2013)
popRatio(1967)

directCosts(getEvents())



# Store the total costs by year
totalCosts <- totalCostForEvent()
directCostsByYear <- with(totalCosts, aggregate(directCost, by=list(Year), FUN=safeSum))
normalisedDirectCostsByYear <- with(totalCosts, aggregate(normalisedDirectCost, by=list(Year), FUN=safeSum))
with(totalCosts, c(Year, normalisedDirectCost))
totalCosts[c(1, 2, 5, 6)]

# Plot a basic graph of costs
#pdf(file="./figs/total_costs.pdf")

# Calculate range from 0 to max value of costs
c_range <- range(0, directCostsByYear, normalisedDirectCostsByYear)

y_range <- range(totalCosts$Year)


plot(directCostsByYear, type="o", col="blue", ylim=c_range, axes=FALSE, ann=FALSE) 
lines(normalisedDirectCostsByYear, type="o", pch=22, lty=2, col="red")

axis(1, las=1, at=seq(y_range[1], y_range[2], by=1))

# Make y axis with horizontal labels that display ticks at 
billions <- 1000000000 * 0:(c_range[2] / 1000000000)
axis(2, las=1, at=billions, labels=format(billions / 1000000, big.mark = ","))

# Create a legend at (1, g_range[2]) that is slightly smaller 
# (cex) and uses the same line colors and points used by 
# the actual plots 
legend(x="1970", y=5000000000 legend=c("Direct", "Normalised"), col=c("blue", "red"))
lty=c(1,1), # gives the legend appropriate symbols (lines)
lwd=c(2.5,2.5),col=c("blue", "red", "green", "grey")) # gives the legend lines the correct color and width

# Add title
title("Total Costs", col.main = "blue")

# Label the x and y axes with dark green text
title(xlab="Years", col.lab=rgb(0,0.5,0))
title(ylab="(2013 Dollars in $millions)", col.lab=rgb(0,0.5,0))

#dev.off()


