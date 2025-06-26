#' @importFrom dplyr group_by summarise mutate select %>% n
#' @importFrom openxlsx write.xlsx
#' @importFrom stats na.omit
utils::globalVariables(c("Length_Range", "Length", "Frequency"))

#' Generate a Frequency Distribution Table for Fish Length Data
#'
#' @name FrequencyTable
#' @description Creates a frequency distribution table for fish length data using either a custom bin width or Wang's formula for automatic bin width calculation. The bin width is rounded to the nearest integer if calculated. The results are saved to an Excel file and returned as a list of data frames.
#'
#' @param data A numeric vector or data frame containing fish length measurements. If a data frame is provided, the first numeric column is used.
#' @param bin_width Numeric value specifying the bin width for class intervals. If NULL (default), bin width is calculated using Wang's formula.
#' @param Lmax Numeric value for the maximum observed fish length. Required only if `bin_width` is NULL and Wang's formula is used. Defaults to NULL.
#' @param output_file Character string specifying the output Excel file name. Defaults to "FrequencyTable_Output.xlsx".
#'
#' @return A list containing two data frames:
#' \describe{
#'   \item{lfqTable}{Frequency table with length ranges and their frequencies.}
#'   \item{lfreq}{Table with upper limits of bins and their frequencies.}
#' }
#'
#' @examples
#' # Load required package
#' library(dplyr)
#'
#' # Generate random fish length data
#' set.seed(123)
#' fish_lengths <- runif(200, min = 5, max = 70)
#'
#' # Create frequency table with automatic bin width
#' FrequencyTable(data = fish_lengths, output_file = tempfile(fileext = ".xlsx"))
#'
#' # Create frequency table with custom bin width and output file
#' FrequencyTable(data = fish_lengths, bin_width = 5, output_file = tempfile(fileext = ".xlsx"))
#'
#' @export
FrequencyTable <- function(data, bin_width = NULL, Lmax = NULL, output_file = "FrequencyTable_Output.xlsx") {
  # Validate input
  if (!is.numeric(data) && !is.data.frame(data)) {
    stop("Input 'data' must be a numeric vector or a data frame with at least one numeric column.")
  }

  # Extract numeric data if data frame
  if (is.data.frame(data)) {
    numeric_cols <- sapply(data, is.numeric)
    if (!any(numeric_cols)) {
      stop("No numeric columns found in the data frame.")
    }
    data <- data[[which(numeric_cols)[1]]]
  }

  # Remove NA values
  data <- stats::na.omit(data)
  if (length(data) == 0) {
    stop("No valid (non-NA) data provided.")
  }

  # Calculate range
  min_length <- min(data)
  max_length <- max(data)

  # Determine bin width
  if (is.null(bin_width)) {
    if (is.null(Lmax)) {
      Lmax <- max_length
      message("Lmax not provided. Using maximum observed length: ", round(Lmax, 2))
    }
    bin_width <- round(0.23 * (Lmax^0.6))  # Wang's formula, rounded to nearest integer
    message("Calculated bin width using Wang's formula: ", bin_width)
  } else {
    if (!is.numeric(bin_width) || bin_width <= 0) {
      stop("bin_width must be a positive numeric value.")
    }
    message("Using custom bin width: ", bin_width)
  }

  # Generate bin edges
  breaks <- seq(floor(min_length), ceiling(max_length) + bin_width, by = bin_width)

  # Create frequency table
  lfqTable <- data.frame(
    Length_Range = cut(data, breaks = breaks, include.lowest = TRUE, right = FALSE)
  ) %>%
    dplyr::group_by(Length_Range) %>%
    dplyr::summarise(Frequency = dplyr::n(), .groups = "drop")

  # Extract upper limits for lfreq table
  lfreq <- lfqTable %>%
    dplyr::mutate(
      Length = as.numeric(sub(".*,(\\d+\\.?\\d*)\\)", "\\1", as.character(Length_Range)))
    ) %>%
    dplyr::select(Length, Frequency)

  # Save to Excel
  if (file.exists(output_file)) {
    warning("Overwriting existing file: ", output_file)
  }
  openxlsx::write.xlsx(list(lfqTable = lfqTable, lfreq = lfreq), file = output_file, rowNames = FALSE)

  # Return results
  return(list(lfqTable = lfqTable, lfreq = lfreq))
}
