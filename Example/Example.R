#Calling the library
library(readxl)

##########################################
#Loading three data files
mydata1 <- read_excel("LC.xlsx", sheet = 1)
mydata2 <- read_excel("LC.xlsx", sheet = 2)
data<- read_excel("LC.xlsx")


# Install package
devtools::install_github("Ataher76/aLBI", force = T)

# calling the first function
library(aLBI)

#detach("package:aLBI", unload = T)

EstimatedParameters <- FishPar(data = mydata1, resample = 1000, progress = F )

#Extracting estimated length parameters
estimatedParameters$estimated_length_par
#Extracting estimated froese parameters
estimatedParameters$estimated_froese_par
#Extracting estimated frequency parameters
estimatedParameters$estimated_freq_par
#Extracting estimated LM_ratio
estimatedParameters$LM_ratio
#Extracting estimated Pobj
estimatedParameters$Pobj


#Loading the cope and pund table

cpdataTable <- read_excel("PT.xlsx")
#Calling the second function
StockStatus <- FishSS(data = cpdataTable,
       LM_ratio = 0.7,
       Pobj = 97,
       Pmat = 29.91881 ,
       Popt = 25.46208
)

# Extracting stock status

StockStatus$StockStatus
StockStatus$Target_Cols

