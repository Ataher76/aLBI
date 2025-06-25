#' Generate a Frequency Distribution Table for Fish Length Data
#'
#' @name FrequencyTable
#' @description This function creates a frequency distribution table for fish length data, using
#' either a custom bin width or Wang's formula to calculate the ideal bin width. If the calculated
#' bin width is a fraction, it is rounded to the nearest integer. The output is saved to an Excel file.
#'
#' @param data A numeric vector or data frame containing fish length measurements. If a data frame is
#' provided, the first numeric column will be selected.
#' @param bin_width (Optional) A numeric value specifying the bin width for class intervals. If not
#' provided, the bin width is automatically calculated using Wang's formula.
#' @param Lmax (Optional) The maximum observed length of fish. Required only if the maximum length is not provided
#' and bin width is calculated using Wang's formula.
#' @param output_file (Optional) A character string specifying the name of the output Excel file (default: "FrequencyTable_Output.xlsx").
#'
#' @return A list containing two data frames:
#' \item{lfqTable}{A frequency table with the length range and frequency.}
#' \item{lfreq}{A table with the upper limits of bins and their frequencies.}
#'
#' @examples
#' # Generate random fish length data
#' set.seed(123)
#' fish_lengths <- data.frame(Length = runif(2000, min = 5, max = 70))
#'
#' # Create a frequency table using Wang's formula (default) and save to Excel
#' FrequencyTable(data = fish_lengths$Length)
#'
#' # Create a frequency table with a custom bin width and custom output file
#' FrequencyTable(data = fish_lengths$Length, bin_width = 5, output_file = "Custom_Output.xlsx")
#'
#' @export
#' @importFrom stats na.omit
#' @importFrom dplyr %>% group_by summarise ungroup mutate
#' @importFrom openxlsx write.xlsx
utils::globalVariables(c("Length_Range", "Frequency", "Length"))

FrequencyTable <- function(data, bin_width = NULL, Lmax = NULL, output_file = "FrequencyTable_Output.xlsx") {
  # Load required package (dplyr) if not already loaded
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    message("Installing and loading dplyr package...")
    install.packages("dplyr")
    library(dplyr)
  }

  # Validate input
  if (!is.numeric(data) && !is.data.frame(data)) {
    stop("Data must be a numeric vector or a data frame containing numeric values.")
  }

  # If data is a dataframe, select the first numeric column and omit NA values
  if (is.data.frame(data)) {
    numeric_cols <- sapply(data, is.numeric)
    if (!any(numeric_cols)) {
      stop("No numeric column found in the data frame.")
    }
    data <- data[[which(numeric_cols)[1]]]  # Select the first numeric column
  }
  data <- stats::na.omit(data)  # Remove NA values

  # Calculate the range
  min_length <- min(data, na.rm = TRUE)
  max_length <- max(data, na.rm = TRUE)
  range_data <- max_length - min_length

  # Determine bin width if not provided
  if (is.null(bin_width)) {
    if (is.null(Lmax)) {
      Lmax <- max_length
      message("Lmax not provided. Using maximum observed length: ", Lmax)
    }
    actual_bin_width <- (0.23 * (Lmax^0.6))  # Calculate bin width using Wang's formula
    rounded_bin_width <- ifelse(actual_bin_width %% 1 < 0.5, floor(actual_bin_width), ceiling(actual_bin_width))  # Round to nearest integer
    message("Calculated actual bin width using Wang's formula: ", round(actual_bin_width, 2),
            ", and the nearest round bin width: ", rounded_bin_width)
    bin_width <- rounded_bin_width  # Use the rounded value for binning
  } else {
    message("Using custom bin width: ", bin_width)
  }

  # Generate bin edges
  breaks <- seq(floor(min_length), ceiling(max_length) + bin_width, by = bin_width)

  # Create frequency table
  freq_table <- data.frame(
    Length_Range = cut(data, breaks = breaks, include.lowest = TRUE, right = FALSE),
    Frequency = 1
  ) %>%
    dplyr::group_by(Length_Range) %>%
    dplyr::summarise(Frequency = sum(Frequency), .groups = "drop")

  # Extract upper limit from each bin
  lfreq <- freq_table %>%
    dplyr::mutate(
      Length = as.numeric(sapply(strsplit(as.character(Length_Range), ","), function(x) {
        gsub("[^0-9.]", "", x[2])  # Extract upper limit
      }))
    ) %>%
    dplyr::select(Length, Frequency)

  # Save to Excel file
  if (file.exists(output_file)) {
    warning("Overwriting existing file: ", output_file, ". Remove or rename the file to avoid this.")
  }
  openxlsx::write.xlsx(list(lfqTable = freq_table, lfreq = lfreq), file = output_file, rowNames = FALSE)

  # Return the results as a list
  output <- list(lfqTable = freq_table, lfreq = lfreq)
  return(output)
}
