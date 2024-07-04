## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE # Ensure caching is appropriately set
)

options(repos = c(CRAN = "https://cran.rstudio.com"))

## ----installing_package-------------------------------------------------------
# Install devtools if you haven't already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}


## ----package_management-------------------------------------------------------
# Unload the aLBI package if it is already loaded
if ("package:aLBI" %in% search()) {
  detach("package:aLBI", unload = TRUE)
}

# Remove the aLBI package if it is installed
if ("aLBI" %in% rownames(installed.packages())) {
  remove.packages("aLBI")
}


## ----calling_library----------------------------------------------------------
# Install the Fish StockAssessment package
devtools::install_github("Ataher76/aLBI")

# Load the aLBI package
library(aLBI)


## ----data_preparation---------------------------------------------------------
library(readxl)
# Load your length-frequency data from the system file
lenfreq_path <- system.file("exdata", "LC.xlsx", package = "aLBI")
print(lenfreq_path)  # Check the generated path

if (lenfreq_path == "") {
  stop("The required file LC.xlsx is missing. Please check the inst/extdata directory.")
}

# load the lenght frequency data
lenfreq_data <- readxl::read_excel(lenfreq_path)
print(lenfreq_data)  # check the data 
# replace with your data directory

## ----FishPar_Example----------------------------------------------------------
# Running the FishPar function
results <- FishPar(data = lenfreq_data, resample = 1000, progress = FALSE)

# Viewing the results
results$estimated_length_par
results$estimated_froese_par
results$estimated_freq_par
results$forese_ind_vs_target
results$LM_ratio
results$Pobj


## ----FishSS_Example-----------------------------------------------------------
# Load the stock status criteria data
cpdata_path <- system.file("exdata", "cpdata.xlsx", package = "aLBI")
print(cpdata_path) #check if the path exist

if (cpdata_path == "") {
  stop("The required file cpdata.xlsx is missing. Please check the inst/extdata directory.")
}
# loading the cope and punt table
cpdata <- readxl::read_excel(cpdata_path)
print(cpdata)

# Running the FishSS function
stock_status <- FishSS(data = cpdata,
                       LM_ratio = results$LM_ratio,
                       Pobj = results$Pobj,
                       Pmat = results$estimated_froese_par[1, 2],
                       Popt = results$estimated_froese_par[2, 2])

# Viewing the stock status
stock_status


