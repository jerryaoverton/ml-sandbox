
#INSTRUCTIONS (please read all steps before starting): 

#STEP 1/3: Start by making your own copy: File-> Make a Copy...
#STEP 2/3: Rename the file: File -> Rename.
#STEP 3/3: Rename the file using the convention: [emailID]-datapipeline. For example: joverton-datapipeline

#Access and collect the data
#Bring in Medicare.gov data on the hospital readmission rates of different procedures
urlfile <- 'https://data.medicare.gov/api/views/9n3s-kdb3/rows.csv?accessType=DOWNLOAD'
data<-read.csv(urlfile)

data[1:3,1:7]

#Verify the format of the data

#In this example, the data is considered valid if it contains 3 key columns
data_is_valid <- "Hospital.Name" %in% colnames(data) && 
    "State" %in% colnames(data) && 
    "Excess.Readmission.Ratio" %in% colnames(data)

ifelse(data_is_valid, "Valid Data", "Invalid Data")

#EXERCISE: Change the conditions for determining data validity

#Monitor the pipeline
monitored <- c("Hospital.Name", "State", "Excess.Readmission.Ratio")
summary(data[monitored])

#Ingest the data

#Only a small subset of the available data will be brought in for analysis
filter <- c("Hospital.Name", "State", "Measure.Name", "Excess.Readmission.Ratio")
data.ingested <- data[,filter]
data.ingested[1:3,]

#EXERCISE: Ingest a different subset of the data

#Clean the data
library(reshape)

#Remove rows with blank excess readmission ratios
data.cleaned <- data.ingested[complete.cases(data.ingested),]

#Convert the excess readmission ratios to a numeric format
data.cleaned$Excess.Readmission.Ratio <- as.numeric(data.cleaned$Excess.Readmission.Ratio)

#Map the data to a format such that each hospital is listed with excess readmission ratios for each measure
data.cleaned <- cast(data.cleaned, Hospital.Name ~ Measure.Name, value = 'Excess.Readmission.Ratio', mean)
data.cleaned[1:3,]

#EXERCISE: Create your own procedure for cleaning the data

#Deliver the data for consumption
#Display the distribution of excess stays for hip and knee replacement procedures

boxplot(data.cleaned[,2:6], las=2, main="Distribution of Excess Readmission Ratio by Procedure")

#EXERCISE: Define a different method of consuming the data


