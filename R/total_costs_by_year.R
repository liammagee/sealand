
# Sources
source("R/functions.r", TRUE)

# Load the data
loadData()

# Generate computed columns
computeColumns()

# Store the total costs by year
totalCosts <- totalCostForEvent()
totalCostsByYear <- with(totalCosts, aggregate(total, by=list(Year), FUN=safeSum))
directCostsByYear <- with(totalCosts, aggregate(directCost, by=list(Year), FUN=safeSum))
indirectCostsByYear <- with(totalCosts, aggregate(indirectCost, by=list(Year), FUN=safeSum))
intangibleCostsByYear <- with(totalCosts, aggregate(intangibleCost, by=list(Year), FUN=safeSum))

# Plot a basic graph of costs
#pdf(file="./figs/total_costs.pdf")

# Calculate range from 0 to max value of costs
c_range <- range(0, totalCostsByYear, directCostsByYear, indirectCostsByYear, intangibleCostsByYear)

y_range <- range(totalCosts$Year)


plot(totalCostsByYear, type="o", col="blue", ylim=c_range, axes=FALSE, ann=FALSE) 
lines(directCostsByYear, type="o", pch=22, lty=2, col="red")
lines(indirectCostsByYear, type="o", col="green")
lines(intangibleCostsByYear, type="o", col="grey")

axis(1, las=1, at=seq(y_range[1], y_range[2], by=1))

# Make y axis with horizontal labels that display ticks at 
billions <- 1000000000 * 0:(g_range[2] / 1000000000)
axis(2, las=1, at=billions, labels=format(billions / 1000000, big.mark = ","))

# Create a legend at (1, g_range[2]) that is slightly smaller 
# (cex) and uses the same line colors and points used by 
# the actual plots 
legend(x="1970", y=5000000000 legend=c("Total","Direct", "Indirect", "Intangible"), col=c("blue", "red", "green", "grey"))
lty=c(1,1), # gives the legend appropriate symbols (lines)
lwd=c(2.5,2.5),col=c("blue", "red", "green", "grey")) # gives the legend lines the correct color and width

# Add title
title("Total Costs", col.main = "blue")

# Label the x and y axes with dark green text
title(xlab="Years", col.lab=rgb(0,0.5,0))
title(ylab="(2013 Dollars in $millions)", col.lab=rgb(0,0.5,0))

#dev.off()

