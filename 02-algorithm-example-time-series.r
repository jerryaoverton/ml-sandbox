#Hypothesis: Close prices for the S&P 500 follows a rythmic, predictable pattern

library(prophet)
library(dplyr)

#Read, from Yahoo! Finance, the S&P 500 Index data for the last month
data <- read.csv('SP500Data.csv')
head(data)

#Read the close price for the the S&P 500 Index for the last month
cols <- c("Date", "Close")

#Create the data set needed to fit a model
spx_history <- data[,cols]
spx_history$Date <- as.Date(spx_history$Date, "%m/%d/%Y")
names(spx_history)[names(spx_history) == 'Date'] <- 'ds'
names(spx_history)[names(spx_history) == 'Close'] <- 'y'

head(spx_history)

#Fit a time-series model
model <- prophet(spx_history)

#Make a prediction
future <- make_future_dataframe(model, periods = 10)
forecast <- predict(model, future)
plot(model, forecast)

#EXERCISE: How plausible is the hypothesis given the evidence?

#EXERCISE: Take the algorithm above and hack it. Propose a new hypothesis and extend this algorithm to investigate.
