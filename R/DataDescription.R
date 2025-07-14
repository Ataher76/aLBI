#' CPdata: Example dataset for aLBI package
#'
#' This dataset contains description of CPdata.
#'
#' @name CPdata
#' @docType data
#' @usage data(CPdata)
#' @format A data frame with 11 columns:
#' \describe{
#'   \item{\code{A}}{Probability values}
#'   \item{\code{B}}{Probability values}
#'   \item{\code{C}}{Probability values}
#'   \item{\code{D}}{Probability values}
#'   \item{\code{E}}{Probability values}
#'   \item{\code{F}}{Probability values}
#'   \item{\code{G}}{Probability values}
#'   \item{\code{H}}{Probability values}
#'   \item{\code{I}}{Probability values}
#'   \item{\code{J}}{Probability values}
#'   \item{\code{Tx}}{Target column compared with LM_ratio to pick probability values}
#' }
#' @source A decision table described by Cope and Punt (2009)
#' @examples
#' data(CPdata, package = "aLBI")
#' head(CPdata)
NULL

#' lenfreq01: Example dataset for aLBI package
#'
#' This dataset contains description of lenfreq01.
#'
#' @name lenfreq01
#' @docType data
#' @usage data(lenfreq01)
#' @format A data frame with 2 columns:
#' \describe{
#'   \item{\code{Frequency}}{Observed individuals in each length class}
#'   \item{\code{Length}}{Upper value of each length class (cm)}
#' }
#' @source Data collected for fish stock assessment studies
#' @examples
#' data(lenfreq01, package = "aLBI")
#' head(lenfreq01)
NULL

#' lenfreq02: Example dataset for aLBI package
#'
#' This dataset contains description of lenfreq02.
#'
#' @name lenfreq02
#' @docType data
#' @usage data(lenfreq02)
#' @format A data frame with 2 columns:
#' \describe{
#'   \item{\code{Frequency}}{Observed individuals in each length class}
#'   \item{\code{LengthClass}}{Upper value of each length class (cm)}
#' }
#' @source Data collected for fish stock assessment studies
#' @examples
#' data(lenfreq02, package = "aLBI")
#' head(lenfreq02)
NULL

#' ExData: Example raw length dataset for aLBI package
#'
#' This dataset contains description of ExData.
#'
#' @name ExData
#' @docType data
#' @usage data(ExData)
#' @format A data frame with 1 column:
#' \describe{
#'   \item{\code{Length}}{Sampled length data (cm)}
#' }
#' @source Data collected for fish stock assessment studies
#' @examples
#' data(ExData, package = "aLBI")
#' head(ExData)
NULL

#' LWdata: Example length-weight dataset for aLBI package
#'
#' This dataset contains length and weight measurements for fish.
#'
#' @name LWdata
#' @docType data
#' @usage data(LWdata)
#' @format A data frame with 2 columns:
#' \describe{
#'   \item{\code{Length}}{Length of sampled fish (cm)}
#'   \item{\code{Weight}}{Weight of sampled fish (g)}
#' }
#' @source Data collected for fish stock assessment studies
#' @examples
#' data(LWdata, package = "aLBI")
#' head(LWdata)
NULL
