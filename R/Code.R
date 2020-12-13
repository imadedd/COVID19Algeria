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
print(NewCases2 <- length(which(DailyUpdates$"Case nature" == "New" & DailyUpdates$"Case Date" == "14 April 2020")))
print(DeaCases2 <- length(which(DailyUpdates$"Case nature" == "Death" & DailyUpdates$"Case Date" == "14 April 2020")))
print(RecCases2 <- length(which(DailyUpdates$"Case nature" == "Recovered" & DailyUpdates$"Case Date" == "14 April 2020")))


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


# #############################################
############################################## GOOD CODE
#############################################""

# Import XLXS file into R and skeep the first line and use the WLWs first row as column names
library(readxl)
studentAnswers <- read_excel("~/Desktop/R/Codes/AR-Project/studentAnswers.xlsx", skip=1)
View(studentAnswers)
print(names(studentAnswers[,]))
# Calculate the number pf participants
NumberPartcipants<-nrow(studentAnswers)
print(NumberPartcipants)


### Calculate the average age of the participant
# First method 
d<-summary(GetAge)
print(d)

#Second method
mean(studentAnswers$"Edad del participante", na.rm = TRUE)

## Plot the ages
install.packages("plotrix")
library(plotrix)

# Get Mens age
Menage <- studentAnswers[which(studentAnswers$"Sexo:" == "Hombre"),names(studentAnswers) %in% c("Edad del participante")]
print(Menage)
# Get women Age
WomenAge <- studentAnswers[which(studentAnswers$"Sexo:" == "Mujer"),names(studentAnswers) %in% c("Edad del participante")]
print(WomenAge)
#Get ages occurances
agesOccur <- table(studentAnswers$`Edad del participante`)
print(agesOccur)

library(plotrix)
xy.pop<-Menage
xx.pop<-WomenAge
agelabels<-c("14-15","15-17","18-19","20-21","22-23","24-25","26-27",
             "28-29","30-31","32-33","34-35","36-37","38-39","65-69","70-74",
             "75-79","80-44","85+","25","52","65","84","98","74","99","100","105")
mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)
par(mar=pyramid.plot(xy.pop,xx.pop,labels=agelabels,
                     main="Population distribution by age and Sex",lxcol=mcol,rxcol=fcol,
                     gap=1,show.values=TRUE))


install.packages("ggplot2")
library(ggplot2)

### count the number of men and women
install.packages("COUNT")
library(COUNT)
MenNumber <- length(which(studentAnswers$"Sexo:" == "Hombre"))
print(MenNumber)
WomenNumber <- length(which(studentAnswers$"Sexo:" == "Mujer"))
print(WomenNumber)

### Barplot of Number of men and women
PopGrap<-c(MenNumber,WomenNumber)
names(PopGrap)<-c("Men","Women")
barplot(PopGrap)

### Get the name of a column
print(names(studentAnswers[6]))

#print(names(studentAnswers)ed_exp4 <- subset(education, Region == 2, select = c("State","Minor.Population","Education.Expenditures")))

### Calculate means for each question
nrow(studentAnswers)
ncol(studentAnswers)
meanQues1<- colMeans(studentAnswers[,6:26],na.rm = TRUE,  dims = 1)
print(meanQues1)
meanQues2<-colMeans(studentAnswers[,28:29],na.rm = TRUE)
print(meanQues2)
barplot(meanQues1)
barplot(meanQues2)
### Calculate the SD for each question
?sd
#as.numeric(unlist(studentAnswers))
#sd(studentAnswers[,6:26],na.rm = TRUE)
#sd(studentAnswers[5])

apply(studentAnswers[,6:26], 2, sd, na.rm=TRUE)
apply(studentAnswers[,28:29], 2, sd, na.rm=TRUE)
apply(studentAnswers[6], 2, sd, na.rm=TRUE)

#sd(studentAnswers[6], na.rm = TRUE)
#set.seed(42)
#M <- matrix(rnorm(40),ncol=4)
#apply(M, 2, sd)
#sd(studentAnswers[,7],na.rm = TRUE)

### Commands to use beautiful tables 
install.packages("expss")
library(expss)
cro_cpct(studentAnswers$`Marca temporal`, list(total(), studentAnswers$`Edad del participante`, studentAnswers$`CUESTIONARIO FASE 1 [Fue fácil de identificar las piezas que debía utilizar en cada paso.]`))
ass<- cro(studentAnswers$`Marca temporal`, studentAnswers$`Edad del participante`)
print_output(summary(ass))

### Calculate the Cronbach alpha
# Using UMX library
install.packages("umx")
install.packages("psych")
library(psych)
Factor1 <-studentAnswers[6:10]
alpha(Factor1)
Factor2<-studentAnswers[11:15]
alpha(Factor2)
Factor3<-studentAnswers[16:21]
alpha(Factor3)
Factor4<-studentAnswers[22:26]
alpha(Factor4)

#### Calculate satisfaction percentages
satisfactionName<-names(studentAnswers[27])
print(satisfactionName)

MuyBueno <- length(which(studentAnswers$`Satisfacción del usuario [Grado de satisfacción general con la aplicación.]` == "Muy bueno"))
print(MuyBueno)
print(MuyBuenoPerce <-(MuyBueno*100)/NumberPartcipants)

Excelente <- length(which(studentAnswers$`Satisfacción del usuario [Grado de satisfacción general con la aplicación.]` == "Excelente"))
print(Excelente)
print(ExcelentePerce <-(Excelente*100)/NumberPartcipants)

Satisfactorio <- length(which(studentAnswers$`Satisfacción del usuario [Grado de satisfacción general con la aplicación.]` == "Satisfactorio"))
print(Satisfactorio)
print(SatisfactorioPerce<-(Satisfactorio*100)/NumberPartcipants)

Deficiente <- length(which(studentAnswers$`Satisfacción del usuario [Grado de satisfacción general con la aplicación.]` == "Deficiente"))
print(Deficiente)
print(DeficientePerce<-(Deficiente*100)/NumberPartcipants)

Medio <- length(which(studentAnswers$`Satisfacción del usuario [Grado de satisfacción general con la aplicación.]` == "Medio"))
print(Medio)
print(MedioPerce<-(Medio*100)/NumberPartcipants)


### Get the medium time to accomplish test
mean(studentAnswers[,28], na.rm=TRUE)
summary(studentAnswers[,28], na.rm=TRUE)


####  Failures because of the application
print(names(studentAnswers[29]))
summary(studentAnswers$`¿Podrías indicar el número de fallos has tenido a causa de la aplicación?`)
