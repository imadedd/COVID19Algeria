# Read CSV files
library(dplyr)
library(readr)
readdailyupdates <- read_csv(file ="/home/imad/R Projects/coronaDZ/data/CasesDailyUpdates.csv")
readPerProvince<- read_csv(file = "/home/imad/R Projects/coronaDZ/data/CasesPerProvince.csv")
View(readdailyupdates)
View(readPerProvince)

# Subset columns from files
DailyUpdates <- subset(readdailyupdates, select = c(1:2))
PerProvince <- subset(readPerProvince, select = c(1:3))
View(DailyUpdates)
View(PerProvince)

#Count number of total cases, total deaths and total recovered
library(COUNT)
print(NewCases <- length(which(DailyUpdates$"Case nature" == "New")))
print(DeathCases <- length((which(DailyUpdates$"Case nature" == "Death"))))
print(RecoveredCases <- length(which(DailyUpdates$"Case nature" == "Recovered")))

# Get number of cases per day
print(NewCases2 <- length(which(DailyUpdates$"Case nature" == "New" & DailyUpdates$"Case Date" == "07 April 2020")))
print(DeaCases2 <- length(which(DailyUpdates$"Case nature" == "Death" & DailyUpdates$"Case Date" == "07 April 2020")))
print(RecCases2 <- length(which(DailyUpdates$"Case nature" == "Recovered" & DailyUpdates$"Case Date" == "07 April 2020")))


#Another way to calculate the number of recovered cases
RecoveredCases2 <- print(sum(DailyUpdates$"Case nature" == "Recovered"))

# Get cumulative infected cases per day per province
View(ConfirmedAlger <- subset(PerProvince, PerProvince$"Province"=="Alger", select = c(2:3)))
print(ConfirmedSetif <- subset(PerProvince, PerProvince$"Province"=="Setif", select = c(2:3)))
print(ConfirmedBlida <- subset(PerProvince, PerProvince$"Province"=="Blida", select = c(2:3)))

# Barplot th enuber of cumulative cases

# Barplot th enuber of cumulative deaths

# Barplot th enuber of cumulative deaths

#Barplot the cumulative number of confirmed cases per province
barplot( ConfirmedAlger$`Confirmed cases`, main = "Cumulative number of confirmed cases in Algiers", xlab = "Confirmed cases",col="Green",space = 0.5)
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



