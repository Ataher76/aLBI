\name{FishPar}
\alias{FishPar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FishPar(data, resample, progress)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{resample}{
%%     ~~Describe \code{resample} here~~
}
  \item{progress}{
%%     ~~Describe \code{progress} here~~
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
function (data, resample, progress) 
{
    if (!is.data.frame(data)) {
        stop("Input is not a data frame.")
    }
    if (is.list(data)) {
        data <- as.data.frame(data)
    }
    colnames(data)[1:2] <- c("Length", "Frequency")
    if (ncol(data) != 2) {
        stop("The dataframe must have exactly two columns as Length and Frequency")
    }
    data <- data[complete.cases(data), ]
    CalParCI <- function(data, Lmax) {
        Linf <- Lmax/0.95
        Lmat <- 10^(0.8979 * log10(Linf) - 0.0782)
        Lopt <- 10^(1.053 * log10(Lmat) - 0.0565)
        Lopt_p10 <- Lopt + Lopt/10
        Lopt_m10 <- Lopt - Lopt/10
        return(c(Lmax, Linf, Lmat, Lopt, Lopt_p10, Lopt_m10))
    }
    parameter_estimates <- matrix(NA, nrow = resample, ncol = 6)
    if (progress) {
        for (i in 1:resample) {
            bootstrap_sample <- data[sample(nrow(data), replace = TRUE), 
                ]
            Lmax_bootstrap <- max(bootstrap_sample[[1]])
            parameter_estimates[i, ] <- CalParCI(bootstrap_sample, 
                Lmax_bootstrap)
            cat("Progress: ", round(i/resample * 100), "\%\n")
        }
    }
    else {
        for (i in 1:resample) {
            bootstrap_sample <- data[sample(nrow(data), replace = TRUE), 
                ]
            Lmax_bootstrap <- max(bootstrap_sample[[1]])
            parameter_estimates[i, ] <- CalParCI(bootstrap_sample, 
                Lmax_bootstrap)
        }
    }
    mean_estimates <- apply(parameter_estimates, 2, mean)
    lower_bound <- apply(parameter_estimates, 2, function(x) quantile(x, 
        probs = 0.025))
    upper_bound <- apply(parameter_estimates, 2, function(x) quantile(x, 
        probs = 0.975))
    sumT <- sum(data[[2]])
    Lmax <- mean_estimates[1]
    Linf <- mean_estimates[2]
    Lmat <- mean_estimates[3]
    Lopt <- mean_estimates[4]
    Lopt_p10 <- mean_estimates[5]
    Lopt_m10 <- mean_estimates[6]
    sum_mat <- sum(ifelse(data[[1]] >= Lmat & data[[1]] <= Lmax, 
        data[[2]], 0))
    Pmat <- (sum_mat/sumT) * 100
    sum_opt <- sum(ifelse(data[[1]] >= Lopt_m10 & data[[1]] <= 
        Lopt_p10, data[[2]], 0))
    Popt <- (sum_opt/sumT) * 100
    sum_mega <- sum(ifelse(data[[1]] >= Lopt_p10 & data[[1]] <= 
        Lmax, data[[2]], 0))
    Pmega <- (sum_mega/sumT) * 100
    Pobj <- sum(Pmat + Popt + Pmega)
    LM_ratio <- (Lmat/Lopt)
    par(mfrow = c(2, 3))
    parameter_names <- c("Lmax", "Linf", "Lmat", "Lopt", "Lopt_p10", 
        "Lopt_m10")
    for (i in 1:6) {
        hist(parameter_estimates[, i], main = parameter_names[i], 
            xlab = parameter_names[i], ylab = "Frequency", col = "lightblue")
        abline(v = mean_estimates[i], col = "red", lwd = 2)
        segments(lower_bound[i], 0, lower_bound[i], max(hist(parameter_estimates[, 
            i], plot = FALSE)$counts), col = "black", lwd = 2, 
            lty = "dotted")
        segments(upper_bound[i], 0, upper_bound[i], max(hist(parameter_estimates[, 
            i], plot = FALSE)$counts), col = "black", lwd = 2, 
            lty = "dotted")
    }
    for (i in 1:6) {
        dens <- density(parameter_estimates[, i])
        plot(dens, main = parameter_names[i], col = "blue", lwd = 1.5, 
            xlim = c(min(parameter_estimates[, i]), max(parameter_estimates[, 
                i])))
        abline(v = mean_estimates[i], col = "red", lwd = 2)
        segments(lower_bound[i], 0, lower_bound[i], max(dens$y), 
            col = "black", lwd = 2, lty = "dotted")
        segments(upper_bound[i], 0, upper_bound[i], max(dens$y), 
            col = "black", lwd = 2, lty = "dotted")
    }
    estimated_length_par <- data.frame(Parameter = parameter_names, 
        Mean_estimate = mean_estimates, Lower_CI = lower_bound, 
        Upper_CI = upper_bound)
    long_df <- data.frame(Parameter = character(), Interval = character(), 
        Value = numeric(), stringsAsFactors = FALSE)
    df_list <- list(estimated_length_par$Parameter, estimated_length_par$Parameter, 
        estimated_length_par$Parameter)
    names(df_list) <- c("Mean_estimate", "Lower_CI", "Upper_CI")
    for (i in names(df_list)) {
        long_df <- rbind(long_df, data.frame(Parameter = df_list[[i]], 
            Interval = rep(i, nrow(estimated_length_par)), Value = estimated_length_par[[i]], 
            stringsAsFactors = FALSE))
    }
    long_df$Parameter <- factor(long_df$Parameter, levels = parameter_names)
    par(mfrow = c(1, 1))
    boxplot(Value ~ Parameter, data = long_df, main = "Estimated Length Parameters", 
        xlab = "Length Parameters", ylab = "Length (cm)", col = "lightblue", 
        border = "black", notch = FALSE, ylim = range(c(long_df$Value)))
    par(mfrow = c(1, 1))
    barplot(data[[2]] ~ data[[1]], main = "Lenght Frequency Plot", 
        xlab = "Length Class", ylab = "Frequency", ylim = c(0, 
            max(data[[2]] * 1.2)), col = "#69b3a2")
    values <- loess(data[[2]] ~ data[[1]])
    lines(predict(values), col = "red", lwd = 2)
    legend("topright", legend = c("Observed frequency", "Modelled frequency"), 
        col = c("#69b3a2", "red"), pch = c(15, NA), lty = c(NA, 
            1), cex = 1, lwd = 2, seg.len = 1.5)
    forese_ind_vs_target <- data.frame(Parameters = c("Pmat", 
        "Popt", "Pmega"), Froese_catch = c(Pmat, Popt, Pmega), 
        Froese_tar = c(100, 100, 20))
    barplot(rbind(forese_ind_vs_target[[3]], forese_ind_vs_target[[2]]), 
        beside = TRUE, names.arg = forese_ind_vs_target[[1]], 
        col = c("#69b3a2", "#404080"), main = "Target vs Catch", 
        xlab = "Froese Length Indicators", ylab = "Percentage (\%)")
    legend("topright", legend = c("Target", "Catch"), fill = c("#69b3a2", 
        "#404080"))
    par(mfrow = c(2, 3))
    for (i in 1:6) {
        dens <- density(parameter_estimates[, i])
        plot(dens, main = parameter_names[i], col = "blue", lwd = 1.5, 
            xlim = c(min(parameter_estimates[, i]), max(parameter_estimates[, 
                i])))
        abline(v = mean_estimates[i], col = "red", lwd = 2)
        segments(lower_bound[i], 0, lower_bound[i], max(dens$y), 
            col = "black", lwd = 2, lty = "dotted")
        segments(upper_bound[i], 0, upper_bound[i], max(dens$y), 
            col = "black", lwd = 2, lty = "dotted")
    }
    for (i in 1:6) {
        plot(data[[1]], data[[2]], type = "l", lwd = 1.8, main = parameter_names[i], 
            xlab = "Length Class (cm)", ylab = "Frequency", ylim = c(0, 
                max(data[[2]] + 100)), xlim = c(0, max(data[[1]] + 
                2)))
        box(col = "white")
        axis(side = 1, lwd = 1.5)
        axis(side = 2, lwd = 1.5)
        abline(v = mean_estimates[i], col = "red", lwd = 2)
        segments(lower_bound[i], 0, lower_bound[i], max(hist(parameter_estimates[, 
            i], plot = FALSE)$counts), col = "black", lwd = 2, 
            lty = "dotted")
        segments(upper_bound[i], 0, upper_bound[i], max(hist(parameter_estimates[, 
            i], plot = FALSE)$counts), col = "black", lwd = 2, 
            lty = "dotted")
        if (i == 4) {
            rect(xleft = Lopt_m10, ybottom = 0, xright = Lopt_p10, 
                ytop = max(data[[2]]), col = "#69b3a2")
            text(x = Lopt, y = max(data[[2]]), labels = "Optimum\nsize", 
                col = "red3", cex = 0.8)
        }
        text(x = mean(c(5, Lmat)), y = 100, labels = "Juveniles", 
            col = "red3", cex = 0.8)
        text(x = mean(c(Lopt_m10, Lmax)), y = 100, labels = "Mega-\nspawners", 
            col = "red3", cex = 0.8)
    }
    estimated_froese_par <- data.frame(Parameter = c("Pmat", 
        "Popt", "Pmega"), Estimate = c(Pmat, Popt, Pmega))
    estimated_freq_par <- data.frame(Parameter = c("sumT", "sum_mat", 
        "sum_opt", "sum_mega"), Estimate = c(sumT, sum_mat, sum_opt, 
        sum_mega))
    return(list(estimated_length_par = estimated_length_par, 
        estimated_froese_par = estimated_froese_par, estimated_freq_par = estimated_freq_par, 
        forese_ind_vs_target = forese_ind_vs_target, LM_ratio = LM_ratio, 
        Pobj = Pobj))
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
