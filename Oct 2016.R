#########################################################################
# Graphing Forecasts in ggplot2 and Some Cool Ways to Polish Your Plots #
# Kimberly Coffey - #####################################################
#########################################################################

# Needed packages: forecast, ggplot2, and scales

# data() # list of pre-loaded datasets
data.ts <- wineind # wine sales over time; data are in forecast() package and are already formatted as a .ts object

# create data frame; normally you'll receive data in this form
date <- as.character(seq(from=as.Date("1980/1/15"), by="1 months", length.out=176))
value <- as.numeric(data.ts)
data <- as.data.frame(cbind(date, value), stringsAsFactors=FALSE)
data$date <- as.Date(data$date) # make sure dates are recognized as dates...
data$value <- as.numeric(data$value) # ... and numerical values as numbers
str(data)

library(ggplot2)
graph <- ggplot(data, aes(x=date, y=value))
graph <- graph + geom_line()   # connect data points with a line
graph

# Create a time series object; only need to do this if data aren't already a .ts object
# data.ts <- ts(data$value, start=c(1980, 1), end=c(1994, 8), frequency=12)
data.ts

library(forecast)
ets.model <- ets(data.ts)
ets.fcasts <- forecast(ets.model, h=12, level=c(80,95))
plot(ets.fcasts)
ets.fcasts

# make forecast object into a data frame
ets.fcasts <- as.data.frame(ets.fcasts)
ets.fcasts$date <- as.Date(as.yearmon(row.names(ets.fcasts))) + 14
names(ets.fcasts) <- c('forecast','lo80','hi80','lo95','hi95','date')

# merge original and forecasted data together by date
# there will be missing data - this is exactly what we want!
graph.data <- merge(data, ets.fcasts, by="date", all = TRUE)

# Graph actual historical data with forecasts and associated confidence intervals
graph <- ggplot(graph.data, aes(x=date, y=value))
graph <- graph + geom_line()   # graph a line
graph

# order matters here b/c objects are graphed on top of each other!
graph <- graph + geom_ribbon(aes(x = graph.data$date, ymin = graph.data$lo95, ymax = graph.data$hi95), fill="lightgrey") # 95% CI
graph <- graph + geom_ribbon(aes(x = graph.data$date, ymin = graph.data$lo80, ymax = graph.data$hi80), fill="darkgrey") # 80% CI
graph <- graph + geom_line(aes(x = graph.data$date, y = graph.data$forecast), colour="blue") # create another line with forecasts

graph


###############################################
# A few finishing touches to polish the graph #
###############################################

# ggplot2 color options: http://sape.inf.usi.ch/quick-reference/ggplot2/colour
library(scales)
graph <- ggplot(graph.data, aes(x=date, y=value))
graph <- graph + geom_line()   # graph a line

graph <- graph + geom_ribbon(aes(x = graph.data$date, ymin = graph.data$lo95, ymax = graph.data$hi95), fill="yellow1")
graph <- graph + geom_ribbon(aes(x = graph.data$date, ymin = graph.data$lo80, ymax = graph.data$hi80), fill="orange")
graph <- graph + geom_line(aes(x = graph.data$date, y = graph.data$forecast), colour="red")
graph

graph <- graph + scale_y_continuous(labels = dollar) # format y-axis as currency in dollars
graph <- graph + xlab("Date") + ylab("Wine sales") # Add x and y-axis labels
graph <- graph + theme(axis.title = element_text(face="bold")) # print the axis labels in bold
graph <- graph + ggtitle("Historical Data with Forecasts\nand Confidence Intervals") # Graph title
graph <- graph + theme(plot.title = element_text(face="bold", size=18, colour="black")) # print the graph title larger, in bold, and in black
graph


################
# Export graph #
################

# Can also do this manually with the "Export" menu in the "Plots" tab

ggsave("wine_forecasts.png", width = 7, height = 5, units="cm")
