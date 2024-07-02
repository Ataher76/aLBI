#Install devtools package by following

install.packages("devtools")

# unload if this package is in use previously
detach("package:aLBI", unload = T)

# or remove the existing package by following code
remove.packages("aLBI")

# Install the latest package
devtools::install_github("Ataher76/aLBI", force = T)


#Enter one or more numbers, or an empty line to skip updates: 1

# calling the library
library(aLBI)

# see the documentation of the first function
?FishPar

#or
help(FishPar)

# see the documentation of the secon function
?FishSS

# or
help(FishSS)

# estimating lenght parameters
estimatedParameters <- FishPar(data = lenfreq01, resample = 1000, progress = F)

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




#Calling the second function
StockStatus <- FishSS(data = CPdata,
                      LM_ratio = 0.7,
                      Pobj = 97,
                      Pmat = 29.91881 ,
                      Popt = 25.46208
)

# Extracting stock status

StockStatus$StockStatus
StockStatus$Target_Cols

