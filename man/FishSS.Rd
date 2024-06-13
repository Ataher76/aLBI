\name{FishSS}
\alias{FishSS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FishSS(data, LM_ratio, Pobj, Pmat, Popt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{LM_ratio}{
%%     ~~Describe \code{LM_ratio} here~~
}
  \item{Pobj}{
%%     ~~Describe \code{Pobj} here~~
}
  \item{Pmat}{
%%     ~~Describe \code{Pmat} here~~
}
  \item{Popt}{
%%     ~~Describe \code{Popt} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, LM_ratio, Pobj, Pmat, Popt) 
{
    if (Pobj <= 100 && LM_ratio <= 0.75) {
        p <- cbind(data[, c("Tx", "A", "C")])
    }
    else if (Pobj <= 100 && LM_ratio >= 0.9) {
        p <- cbind(data[, c("Tx", "B", "D")])
    }
    else if (Pobj > 100 && Pobj < 200 && LM_ratio <= 0.75) {
        p <- cbind(data[, c("Tx", "E", "G")])
    }
    else if (Pobj > 100 && Pobj < 200 && LM_ratio >= 0.9) 
        p <- cbind(data[, c("Tx", "F", "H")])
    else if (Pobj >= 200) 
        p <- cbind(data[, c("Tx", "I", "J")])
    else {
        warning("Your LM_ration doesn't fall in the appropriate condition. The value should be <= 0.75 and >= 0.9")
    }
    for (i in p[[1, 2]]) if (i > 0) {
        (Tr <- Popt)
    }
    else {
        (Tr <- Pmat)
    }
    difference <- abs(p[[1]] - Tr)
    closest_vi <- which.min(difference)
    TSB40 <- p[[2]][closest_vi]
    LSB25 <- p[[3]][closest_vi]
    result <- c(TSB40 = TSB40, LSB25 = LSB25)
    Target_cols <- as.data.frame(p)
    return(list(Target_Cols = Target_cols, Target_value = Tr, 
        Colesest_value = closest_vi, StockStatus = result))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
