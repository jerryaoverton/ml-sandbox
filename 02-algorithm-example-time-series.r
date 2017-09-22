
#INSTRUCTIONS (please read all steps before starting): 

#STEP 1/4: Start by making your own copy: File-> Make a Copy...
#STEP 2/4: Rename the file: File -> Rename.
#STEP 3/4: Rename the file using the convention: [emailID]-time-series-algorithm. For example: joverton-time-series-algorithm
#STEP 4/4: Clear any previous output: Kernel -> Restart & Clear the Output

#Hypothesis: Close prices for the S&P 500 follows a rythmic, predictable pattern

library(prophet)
library(dplyr)

#Read, from Yahoo! Finance, the S&P 500 Index data for the last month
urlfile <- "http://chart.finance.yahoo.com/table.csv?s=^GSPC&a=1&b=28&c=2016&d=1&e=28&f=2017&g=d&ignore=.csv"
data <- read.csv(urlfile)
head(data)

#Read the close price for the the S&P 500 Indes for the last month
cols <- c("Date", "Close")

#Create the data set needed to fit a model
spx_history <- data[,cols]
names(spx_history)[names(spx_history) == 'Date'] <- 'ds'
names(spx_history)[names(spx_history) == 'Close'] <- 'y'

head(spx_history)

#Fit a time-series model
model <- prophet(spx_history)

#Make a prediction
future <- make_future_dataframe(model, periods = 90)
forecast <- predict(model, future)
plot(model, forecast)

#EXERCISE: How plausible is the hypothesis given the evidence?

#EXERCISE: Take the algorithm above and hack it. Propose a new hypothesis and extend this algorithm to investigate.
