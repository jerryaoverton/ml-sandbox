#Staring hypothisis: S&P 500 Close values can be predicted by social mood on a given day.

#Read, from Yahoo! Finance, the S&P 500 Index data for Jan - Aug 2016
urlfile <- 'http://chart.finance.yahoo.com/table.csv?s=^GSPC&a=1&b=01&c=2016&d=9&e=30&f=2016&g=d&ignore=.csv'
data<-read.csv(urlfile)

#Display a sample of the data
str(data)

#For POC purposes Consumer Complaints will be used as a measure of social mood -- more complaints = unhappier population
#Read, from Data.gov, the Consumer Complaint Database

#Note: This may take a few minutes to load
complaint_data <- read.csv("https://data.consumerfinance.gov/api/views/s6ew-h6mp/rows.csv?accessType=DOWNLOAD")

#Display the structure of the data
str(complaint_data)

#Date values in S&P Data and Complaints data have been read as Factor and are in different formats. Need to be converted to Date.
#Convert Date to date to get standard date format
complaint_data$Date.received <- as.Date(complaint_data$Date.received,"%m/%d/%Y")

#Subset data to Jan - Sept 2016 to match data range of S&P 500 data
d1 <- as.Date("2016-01-01")
d2 <- as.Date("2016-08-31")

complaint_Aug16 <- complaint_data[complaint_data$Date.received %in% d1:d2,]

#Return Date to factor, because Jupyter has issues displaying data with date class and I like seeing the dates
complaint_Aug16$Date.received <- as.factor(complaint_Aug16$Date.received)

#Display the structure of the data
str(complaint_Aug16)

#Rename complaint_data Date.recieved to Date so the field name is common between the two data sets
colnames(complaint_Aug16)[1] <- "Date"

#Group number of complaints per day
library(dplyr)
by_ComplaintDate <- group_by(complaint_Aug16, Date)
complaint_count <- summarise(by_ComplaintDate, Complaints = n())


head(complaint_count)

#Read the close price for the the S&P 500 Indes for the June - Sept 2016
spx_history <- select(data,Date,Close)

#Display a sample of the data
head(spx_history)

#Merge the Complaints data and S&P 500 data
complaints_spx <- merge(complaint_count,spx_history, by = "Date")

#Remove any NAs
complaints_spx <-complaints_spx[complete.cases(complaints_spx),]

#Convert back to Date, so dates plot correctly
complaints_spx$Date <- as.Date(complaints_spx$Date,)

#Display a sample of the data
tail(complaints_spx)

#Plot the relationship between Complaints and S&P Close
library(lattice) #graphing package
xyplot(Complaints ~ Close, data = complaints_spx, type = c("p","r"),
       xlab = "S&P Close", 
       ylab = "No. Complaints Filed", 
       main = "Consumer Complaints vs S&P 500"
      )

#There appears to be a slight relationship, but it is the opposite of what I expected
#Keep going with the linear regression idea because I've come this far...
complaint_spxMod1 = lm(Complaints ~ Close, data = complaints_spx)

#I could use to review what all these stats are telling me, but I think the synopsis is no significant relationship
summary(complaint_spxMod1)

#There is either minimal relationship between the data sets or simple linear regression was a poor model choice
#Plot the data of the individual data sets and inspect for any trend relationship
library(ggplot2)

complaints_plot <- ggplot(complaints_spx, aes(Date, Complaints)) + 
                    geom_line() +
                    ggtitle("Consumer Complaints for Jan - Aug 2016") +
                    xlab("Date") + ylab("No Complaints Filed")
                    scale_x_date(labels=date_format("%Y-%m-%d"))

close_plot <- ggplot(complaints_spx, aes(Date, Close)) + 
                    geom_line() +
                    ggtitle("S&P Close for Jan - Aug 2016") +
                    xlab("Date") + ylab("Close")
                    scale_x_date(labels=date_format("%Y-%m-%d")) 
complaints_plot + geom_smooth(method = "lm", se = FALSE)

close_plot + geom_smooth(method = "lm", se = FALSE)


