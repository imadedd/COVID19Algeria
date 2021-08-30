# Read CSV files
library(dplyr)
library(readr)

# Libraries to plot by month
library(ggplot2)

readdailyupdates <- read_csv(file ="/home/imad/R Projects/coronaDZ/data/CasesDailyUpdates.csv")
readPerProvince<- read_csv(file = "/home/imad/R Projects/coronaDZ/data/CasesPerProvince.csv")
View(readdailyupdates)
View(readPerProvince)

# Subset columns from files
DailyUpdates <- subset(readdailyupdates, select = c(1:2))
PerProvince <- subset(readPerProvince, select = c(1:3))
View(DailyUpdates)
View(PerProvince)

# Count number of total cases, total deaths and total recovered
library(COUNT)
print(sum(DailyUpdates$`Case nature`=="New"))
print(NewCases <- length(which(DailyUpdates$"Case nature" == "New")))
print(DeathCases <- length((which(DailyUpdates$"Case nature" == "Death"))))
print(RecoveredCases <- length(which(DailyUpdates$"Case nature" == "Recovered")))

    # Get number of cases per day
print(NewCases2 <- length(which(DailyUpdates$"Case nature" == "New" & DailyUpdates$"Case Date" == "28 May 2021")))
print(DeaCases2 <- length(which(DailyUpdates$"Case nature" == "Death" & DailyUpdates$"Case Date" == "28 May 2021")))
print(RecCases2 <- length(which(DailyUpdates$"Case nature" == "Recovered" & DailyUpdates$"Case Date" == "28 May 2021")))

# Get overall number of cases per x day
library(lubridate)
library(dplyr)
print(sum(DailyUpdates$`Case nature`=="New"))
group_by(DailyUpdates, DailyUpdates$`Case Date`)    %>%sum(DailyUpdates$`Case nature`=="New")
View(group_by(DailyUpdates, DailyUpdates$`Case Date`, sum(DailyUpdates$`Case nature`=="New")))

options(max.print=10)
print(DailyUpdates$"Case Date" <- dmy(DailyUpdates$"Case Date"))


#View(DailyUpdates$"Case Date" <- as.Date(DailyUpdates$"Case Date", format ="%d %B %Y"))
#DailyUpdates$`Case Date` <- as.Date(DailyUpdates$`Case Date`, format ="%d-%B-%Y")
aggregate(DailyUpdates$`Case nature`, by=list(DailyUpdates$`Case Date`),length(which(DailyUpdates$"Case nature"=="New"))
aggregate(DailyUpdates$"Case nature", by=list(DailyUpdates$"Case Date"), FUN=length(which(DailyUpdates$"Case nature"=="New"))

aggregate(DailyUpdates$`Case nature`, by=list(DailyUpdates$`Case Date`), sum(DailyUpdates$"Case nature" == "Recovered"))

# Another way to calculate the number of recovered cases
RecoveredCases2 <- print(sum(DailyUpdates$"Case nature" == "Recovered"))

# Get cumulative infected cases per day per province
View(ConfirmedAlger <- subset(PerProvince, PerProvince$"Province"=="Alger", select = c(2:3)))
print(ConfirmedSetif <- subset(PerProvince, PerProvince$"Province"=="Setif", select = c(2:3)))
print(ConfirmedBBA <- subset(PerProvince, PerProvince$"Province"=="Bordj Bou Arreridj", select = c(2:3)))

# Barplot th enuber of cumulative cases

# Barplot th enuber of cumulative deaths

# Barplot th enuber of cumulative deaths

#Barplot the cumulative number of confirmed cases per province
barplot( ConfirmedBBA$`Confirmed cases`, main = "Cumulative number of confirmed cases in BBA", xlab = "Confirmed cases",col="Green",space = 0.5)
library(hablar)
ConvertedDates <- as.Date(ConfirmedAlger$Date,"%d %m %y")

ggplot(data = ConfirmedAlger, aes(x = `ConvertedDates`, y = `Confirmed cases`)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Cumulative confirmed cases in Algiers",
       subtitle = "2020",
       x = "Date", y = "Cases") +  scale_x_date(labels = date_format("%Y-%m"),breaks = "1 month")



Warning messages:
barplot(ConfirmedSetif$`Confirmed cases`, main = "Cumulative number of confirmed cases in Sétif", xlab = "Confirmed cases",col="Grey",space = 0.5)
barplot(ConfirmedBlida$`Confirmed cases`, main = "Cumulative number of confirmed cases in Blida", xlab = "Confirmed cases",col="Blue",space = 0.5)

#Get the last total of infected cases per province
print(tail(subset(PerProvince, PerProvince$"Province"=="Alger", select = c(2:3)),1))

# Calculate new cases per province

# Graph of new cases per provinces

# DEath rate in Algeria, USA, Spain, Italy, Egypt and other counntries

betterDates <- as.Date(ConfirmedBBA$Date,format = "%d %m %Y")

ConfirmedBBA$Date <- as.Date(ConfirmedBBA$Date,"%d %m %y")

View(betterDates)
