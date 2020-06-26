#Importing data into R environment.
#Provide the trend chart for the number of complaints at monthly and daily granularity levels.
#Provide a table with the frequency of complaint types. -Which complaint types are maximum i.e., around internet, network issues, or across any other domains.
#Create a new categorical variable with value as Open and Closed. Open & Pending is to be categorized as Open and Closed & Solved is to be categorized as Closed.
#Provide state wise status of complaints in a stacked bar chart. Use the categorized variable from Q3. Provide insights on: -Which state has the maximum complaints -Which state has the highest percentage of unresolved complaints
#Provide the percentage of complaints resolved till date, which were received through theInternet and customer care calls.

# Including required Packages
install.packages("stringi")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
library(stringi)
library(lubridate)
library(dplyr)
library(ggplot2)

#Loading Dataset:
comcast_data<- read.csv("d:/dataset/Comcast_telecom_complaints_data.csv",header = TRUE)
#Manupulating column names
names(comcast_data)<- stri_replace_all(regex =  "\\.",replacement = "",str =names(comcast_data))
head(comcast_data)

#Finding NA`s in dataset:
na_vector <- is.na(comcast_data)
length(na_vector[na_vector==T])

#Processing date.
comcast_data$Date<- dmy(comcast_data$Date)

#Extracting monthly and daily
monthly_count<- summarise(groups(comcast_data,Month = as.Date(month(Date))),Count = n())
daily_count<- summarise(groups(comcast_data,Date = as.Date(month(days))),Count = n())
monthly_count<- arrange(monthly_count,Month)
#making count of monthly and daily complaints 

#Comparing Monthly and daily Complaints
ggplot(data = monthly_count,aes(Month,Count,label = Count))+
  geom_line()+
  geom_point(size = 0.8)+
  geom_text()+
  scale_x_continuous(breaks = monthly_count$Month)+
  labs(title = "Monthly Ticket Count",x= "Months",y ="No. of Tickets")

# As you can see there is a increases in tickets in the month of April,May and this also increased in the month of June, so there might be some reseon due to that that they recived high amount of tickets.
ggplot(data = daily_count,aes(as.POSIXct(Date),Count))+
  geom_line()+
  geom_point(size = 1)+
  scale_x_datetime(breaks = "2 weeks",date_labels = "%d/%m")+
  labs(title = "Daily Ticket Count",x= "Days",y ="No. of Tickets")
