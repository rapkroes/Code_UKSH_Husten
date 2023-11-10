#delete data (remove objects from a specified environment)
rm(list=ls())
# Clear all plots
try(dev.off(dev.list()["RStudioGD"]),silent=TRUE)
try(dev.off(),silent=TRUE)


pathr <- "E:/Volume_D/REST/PROJEKTE_AllgMed/04_aktuelle_Projekte/Zi_Versorgungsforschung/06_Teilprojekte/03_COPD_Husten/!Raphael/R/"
pathdata <- "N:/StudentischeHilfskraefte/_Kroes (Christoph)/Routinedaten/01_Husten/DB/"
pathroot <- "E:/Volume_D/REST/PROJEKTE_AllgMed/04_aktuelle_Projekte/Zi_Versorgungsforschung/06_Teilprojekte/03_COPD_Husten/!Raphael/R/"

# make sure working directory is root
setwd(pathroot)



#---------- load tables ---------------------

#load IPC
IPC <- read.csv(paste(pathdata,"DB_IPC23_v02.csv",sep = ""), sep = ";" , header = TRUE, stringsAsFactors = FALSE)

#load IPC
Diag <- read.csv(paste(pathdata,"DB_Diag3_v02.csv",sep = ""), sep = ";" , header = TRUE, stringsAsFactors = FALSE)


#create data frame for panel data

merged.data<- merge.datasets(list(IPC,Diag))
View(merged.data)

filtered.data<- query.data(merged.data,ipc2.code = "R05")
View(filtered.data)

# filtered.data$icd10<- as.factor(filtered.data$icd10)

filtered.data$icd10_rough_I<- filtered.data$icd10
filtered.data$icd10_rough_II<- filtered.data$icd10
filtered.data$icd10_rough_III<- filtered.data$icd10
for(i in 1:dim(filtered.data)[1]){
  filtered.data$icd10_rough_I[i] <- substr(filtered.data$icd10[i],1,1)
  filtered.data$icd10_rough_II[i] <- substr(filtered.data$icd10[i],1,2)
  filtered.data$icd10_rough_III[i] <- substr(filtered.data$icd10[i],1,3)
}










