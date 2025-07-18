% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FrequencyTable.R
\name{FrequencyTable}
\alias{FrequencyTable}
\title{FrequencyTable Generate a Frequency Distribution Table for Fish Length Data}
\usage{
FrequencyTable(
  data,
  bin_width = NULL,
  Lmax = NULL,
  output_file = "FrequencyTable_Output.xlsx"
)
}
\arguments{
\item{data}{A numeric vector or data frame containing fish length measurements. If a data frame is provided, the first numeric column is used.}

\item{bin_width}{Numeric value specifying the bin width for class intervals. If NULL (default), bin width is calculated using Wang's formula.}

\item{Lmax}{Numeric value for the maximum observed fish length. Required only if `bin_width` is NULL and Wang's formula is used. Defaults to NULL.}

\item{output_file}{Character string specifying the output Excel file name. Defaults to "FrequencyTable_Output.xlsx".}
}
\value{
A list containing two data frames:
\describe{
  \item{lfqTable}{Frequency table with length ranges and their frequencies.}
  \item{lfreq}{Table with upper limits of bins and their frequencies.}
}
}
\description{
Creates a frequency distribution table for fish length data using either a custom bin width or Wang's formula for automatic bin width calculation. The bin width is rounded to the nearest integer if calculated. The results are saved to an Excel file and returned as a list of data frames.
}
\examples{
# Load required package
library(dplyr)

# Generate random fish length data
set.seed(123)
fish_lengths <- runif(200, min = 5, max = 70)

# Create frequency table with automatic bin width
FrequencyTable(data = fish_lengths, output_file = tempfile(fileext = ".xlsx"))

# Create frequency table with custom bin width and output file
FrequencyTable(data = fish_lengths, bin_width = 5, output_file = tempfile(fileext = ".xlsx"))

}
