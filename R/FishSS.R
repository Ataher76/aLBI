#' Assess Stock Status Based on Calculated Parameters
#'
#' @description This function assesses the stock status based on parameters calculated by the FishPar function.
#' @param data A data frame containing the necessary columns for stock status calculation.
#' @param LM_ratio A numeric value representing the length at maturity ratio.
#' @param Pobj A numeric value representing the percentage objective.
#' @param Pmat A numeric value representing the percentage of mature fish.
#' @param Popt A numeric value representing the percentage of optimally sized fish.
#' @return A numeric vector containing TSB40 and LSB25.
#' @examples
#' utils::data("CPdata", package = "aLBI")
#' FishSS(CPdata, 0.75, 100, 30, 25)
#' @export
FishSS <- function(data, LM_ratio, Pobj, Pmat, Popt) {
  # Function code here...
}
# Second function that will calculate the stock status in respect to cope and punt
FishSS <- function(data,
                   LM_ratio, Pobj, Pmat, Popt ){
  # Load necessary datasets within the function
  utils::data("CPdata", package = "aLBI")
  # this function will pick the columns by following LM_ration and Pobj
  if(Pobj <= 100 && LM_ratio <= 0.75){
    p <- cbind(data[, c("Tx", "A", "C")])

  } else if(Pobj <= 100 && LM_ratio >= 0.9){
    p <- cbind(data[, c("Tx", "B", "D")])

  } else if(Pobj > 100 && Pobj < 200 && LM_ratio <= 0.75){
    p <- cbind(data[, c("Tx", "E", "G")])

  } else if(Pobj > 100 && Pobj < 200 && LM_ratio >= 0.9)
    p <- cbind(data[, c("Tx", "F", "H")])

  else if( Pobj >= 200)
    p <- cbind(data[, c("Tx", "I", "J")])
  else{
    warning("Your LM_ration doesn't fall in the appropriate condition. The value should be <= 0.75 and >= 0.9")
  }

  #This condition will pick the target value Tx from the target column
  for(i in p[[1,2]])
    if ( i > 0){
      (Tr <- Popt)
    }
  else{
    (Tr <- Pmat)
  }

  # finding the result
  difference <- abs(p[[1]] - Tr)
  closest_vi <- which.min(difference)
  TSB40 <- p[[2]][closest_vi]
  LSB25 <- p[[3]][closest_vi]
  result <- c(TSB40 = TSB40 , LSB25 = LSB25)

  #return()
  # selected columns

  Target_cols <- as.data.frame(p)

  #return(list(columns = p, target = Tr, result = result))
  return(list(
    Target_Cols = Target_cols,
    Target_value = Tr,
    Colesest_value = closest_vi,
    StockStatus = result
  ))
}

