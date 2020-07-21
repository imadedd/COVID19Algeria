require(gdata)
## install support for xlsx files
installXLSXsupport()
excelFile <- ("/home/imad/Desktop/MARIA BLANCA/WorkingWithData/studentAnswers.xlsx")
## note that the perl scripts that gdata uses do not cope well will tilde expansion
## on *nix machines. So use the full path. 
numSheets <- sheetCount(excelFile, verbose=TRUE)
install.packages("readxl")
library(readxl)
readxl() 
devtools::install_github("hadley/readxl")
for ( i in 1:numSheets) {
  mySheet <- read.xls(excelFile, sheet=i)
  write.csv(mySheet, file=paste(i, "csv", sep="."), row.names=FALSE)
  # Sys.setlocale("LC_ALL", "C")
  # x <- read.csv(url, header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
}
print(mySheet) 
install.packages("gdata")
library(gdata)
gdata()
excelFile
install.packages("rio")
convert("/home/imad/Desktop/MARIA BLANCA/WorkingWithData/studentAnswers.xlsx", "/home/imad/Desktop/MARIA BLANCA/WorkingWithData/studentAnswers.csv")

library("rio")
xls <- dir(pattern = "xlsx")
created <- mapply(convert, xls, gsub("xlsx", "csv", xls))
unlink(xls)


# Create a vector of Excel files to read
files.to.read = list.files(pattern="xlsx")


# Read each file and write it to csv
lapply(files.to.read, function(f) {
  df = read.xlsx(f, sheet=1)
  write.csv(df, gsub("/home/imad/Desktop/MARIA BLANCA/WorkingWithData/studentAnswers.xlsx", "csv", f), row.names=FALSE)
})

install.packages("readxl")
install.packages("xlsx")
install.packages("rio")
xls <- dir(pattern = "/home/imad/Desktop/MARIA BLANCA/WorkingWithData/studentAnswers.xlsx")
created <- mapply(convert, xls, gsub("xlsx", "csv", xls))
unlink(xls) # delete xlsx files

#############################################
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