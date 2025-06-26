## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE # Ensure caching is appropriately set
)

options(repos = c(CRAN = "https://cran.rstudio.com"))

## ----installing_package-------------------------------------------------------
# You can install the package using the following commands in your R session:
# Install devtools if you haven't already
# install.packages("devtools")
# # Install dplyr if you haven't already
# install.packages("dplyr")
# # Install readxl if you haven't already
# install.packages("readxl")
# install aLBI package from CRAN
# install.packages("aLBI")
# Install the most updated version of aLBI package from GitHub
# devtools::install_github("Ataher76/aLBI")


## ----package_management-------------------------------------------------------
# Check if required packages are installed and load them
# Check if required packages are installed and load them
required_packages <- c("aLBI", "readxl", "dplyr", "devtools")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    warning(paste("Package", pkg, "is required but not installed."))
  } else {
    library(pkg, character.only = TRUE)
  }
}


# Load the aLBI package
library(aLBI)
library(readxl)
library(dplyr)
library(devtools)

## ----data_preparation---------------------------------------------------------
library(readxl)
# Load your length data from the system file
lenfreq_path <- system.file("exdata", "ExData.xlsx", package = "aLBI")
print(lenfreq_path)  # Check the generated path

if (lenfreq_path == "") {
  stop("The required file ExData.xlsx is missing. Please check the inst/extdata directory.")
}

# load the lenght frequency data
lenght_data <- readxl::read_excel(lenfreq_path)
print(lenght_data)  # check the 
# replace with your data directory



## ----FrequencyTable_Output----------------------------------------------------
# Running the FrequencyTable function
freqTable <- FrequencyTable(data = lenght_data, bin_width = NULL, Lmax = NULL)

# Viewing the results
freqTable$lfqTable  # Display the frequency table
freqTable$lfreq     # Display the summarized frequencies with upper length ranges


## ----data_example-------------------------------------------------------------
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

