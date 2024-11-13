#' Generate a Frequency Distribution Table for Fish Length Data
#' @name FrequencyTable
#' @description This function creates a frequency distribution table for fish length data, allowing
#' users to specify or calculate the ideal bin width based on Sturges' formula. The function returns
#' a data frame containing the upper boundary of each length class and its associated frequency.
#'
#' @param data A numeric vector or data frame containing fish length measurements. If a data frame is
#' provided, the first numeric column will be selected.
#' @param bin_width (Optional) A numeric value specifying the bin width for class intervals. If not
#' provided, the bin width is automatically calculated using Sturges' (1926) formula.
#'
#' @return A data frame with two columns: \code{Upper_Length} representing the upper boundary of each
#' length class, and \code{Frequency} representing the count of observations within each class.
#'
#' @details The ideal bin width is calculated if not provided, based on Sturges' formula:
#' \deqn{\text{Bin Width} = \frac{\text{Range}}{\text{Number of Classes}}} where the number of classes
#' is determined as \eqn{1 + 3.322 \log_{10}(N)} for a dataset of size \eqn{N}.
#'
#' @importFrom stats na.omit
#' @importFrom dplyr %>% group_by summarise ungroup mutate
#'
#' @examples
#' # Generate random fish length data
#' set.seed(123)
#' fish_lengths <- data.frame(Length = runif(2000, min = 5, max = 70))
#'
#' # Create a frequency table with an automatically calculated bin width
#' FrequencyTable(data = fish_lengths$Length)
#'
#' # Specify a bin width of 5 and generate a frequency table
#' FrequencyTable(data = fish_lengths$Length, bin_width = 5)
#' utils::data("ExData", package = "aLBI")
#'
#' @export
#'   # Suppress global variable warnings
utils::globalVariables(c("Length_Range", "Frequency", "Length"))


FrequencyTable <- function(data, bin_width = NULL) {
  # Load necessary datasets within the function
  utils::data("ExData", package = "aLBI")

  # If data is a dataframe, select the first numeric column and omit NA values
  if (is.data.frame(data)) {
    data <- data[[1]]  # Select the first numeric column
    data <- stats::na.omit(data)  # Remove NA values (import from stats)
  }

  # Calculate the range
  min_length <- min(data, na.rm = TRUE)
  max_length <- max(data, na.rm = TRUE)
  range_data <- max_length - min_length

  # Calculate ideal bin width if not provided
  if (is.null(bin_width)) {
    num_classes <- ceiling(1 + 3.322 * log10(length(data))) # Sturges' formula
    bin_width <- ceiling(range_data / num_classes)
    cat("Calculated ideal bin width:", bin_width, "\n")
  }

  # Generate bin edges
  breaks <- seq(floor(min_length), ceiling(max_length) + bin_width, by = bin_width)

  # Create frequency table
  freq_table <- data.frame(
    Length_Range = cut(data, breaks = breaks, include.lowest = TRUE, right = FALSE),
    Frequency = 1
  ) %>%
    dplyr::group_by(Length_Range) %>%
    dplyr::summarise(Frequency = sum(Frequency)) %>%
    dplyr::ungroup()

  # Extract upper limit from each bin
  lfreq <- freq_table %>%
    dplyr::mutate(
      Length = as.numeric(sapply(strsplit(as.character(Length_Range), ","), function(x) {
        # Remove brackets and whitespace, and take the upper limit
        gsub("[^0-9.]", "", x[2])
      }))
    ) %>%
    dplyr::select(Length, Frequency)

  output <- list(lfqTable = freq_table, lfreq = lfreq)
  return(output)
}
