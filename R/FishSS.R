#' @title Assess Stock Status and Classify Fish selectivity
#' @description This function assesses stock status and classifies fish selectivity based on the provided parameters.
#' @param data A data frame containing the necessary columns for stock status calculation from Cope & Punt (2009)
#' @param LM_ratio A numeric value representing the length at maturity ratio.
#' @param Pmat A numeric value representing the percentage of mature fish.
#' @param Popt A numeric value representing the percentage of optimally sized fish.
#' @param Pmega A numeric value representing the percentage megaspawner.
#' @return A list containing the selected columns, the target value, the closest value index, the calculated stock status, and the fish classification.
#' @examples
#' utils::data("CPdata", package = "aLBI")
#' FishSS(CPdata, 0.75, 100, 30, 25)
#' @export
#'
#'
FishSS <- function(data, LM_ratio, Pmat, Popt, Pmega) {

  # Step 1: Calculate the objective probability (Pobj)
  # Pmat, Popt, and Pmega are assumed to be percentages (0-100)
  Pobj <- Pmat + Popt + Pmega

  # Step 2: Select the appropriate columns for stock status calculation
  # This section implements the logic for stock status assessment
  if (Pobj <= 100 && LM_ratio <= 0.75) {
    p <- cbind(data[, c("Tx", "A", "C")])
  } else if (Pobj <= 100 && LM_ratio >= 0.9) {
    p <- cbind(data[, c("Tx", "B", "D")])
  } else if (Pobj > 100 && Pobj < 200 && LM_ratio <= 0.75) {
    p <- cbind(data[, c("Tx", "E", "G")])
  } else if (Pobj > 100 && Pobj < 200 && LM_ratio >= 0.9) {
    p <- cbind(data[, c("Tx", "F", "H")])
  } else if (Pobj >= 200) {
    p <- cbind(data[, c("Tx", "I", "J")])
  } else {
    # If no condition is met, return an error or warning
    warning("Your LM_ratio doesn't fall in the appropriate condition. The value should be <= 0.75 or >= 0.9. Returning NULL.")
    return(NULL)
  }

  # Step 3: Determine the target value (Tr)
  # Corrected logic to check the first value of the second column
  if (p[[1, 2]] > 0) {
    Tr <- Popt
  } else {
    Tr <- Pmat
  }

  # Step 4: Find the closest target value and calculate stock status
  difference <- abs(p[[1]] - Tr)
  closest_vi <- which.min(difference)
  TSB40 <- p[[2]][closest_vi]
  LSB25 <- p[[3]][closest_vi]
  stock_status <- c(TSB40 = TSB40, LSB25 = LSB25)


  # Step 5: Perform fish selectivity classification based on the flowchart
  # This section implements the fish classification algorithm
  if (Pobj < 100) {
    if ((Popt + Pmega) == 0) {
      selectivity <- "Fish small, immature"
    } else {
      selectivity <- "Fish small and optimally-sized or all but biggest"
    }
  } else if (Pobj >= 100 && Pobj < 200) {
    selectivity <- "Fish maturity ogive"
  } else if (Pobj >= 200) {
    if (Popt < 100) {
      selectivity <- "Fish optimally-sized and bigger"
    } else if (Popt >= 100) {
      selectivity <- "Fish optimally-sized"
    }
  } else {
    selectivity <- "selectivity not found"
  }

  # Step 6: Return all results in a single, comprehensive list
  return(list(
    #Target_Cols = as.data.frame(p),
    #Target_value = Tr,
    #Closest_value = closest_vi,
    StockStatus = stock_status,
    Selectivity = selectivity
  ))
}


