library(dplyr)
library(readr)

readVariations <- read_csv(file ="/home/imad/R Projects/coronaDZ/data/CoronaVariantsUpdates.csv")

View(readVariations)

NewCases <- subset(readVariations, select = c(1:2))
NewCasesPerTownLabel <- subset(readVariations, select = c(1:2,8,10))

View(NewCases)
View(NewCasesPerTownLabel)

print(sum(NewCases$`Case nature`=="New"))

print(NewCases2 <- length(which(NewCases$"Case nature" == "New" & NewCases$"Case date" == "06/21/21")))
print(NewCases2 <- length(which(NewCasesPerTownLabel$"Case nature" == "New" & NewCasesPerTownLabel$"Case date" == "07/04/21" & NewCasesPerTownLabel$Province == "Alger" & NewCasesPerTownLabel$Label == "Delta"   )))
