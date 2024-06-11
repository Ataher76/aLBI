
# this is the first function

########################
FishPar <- function(data, resample, progress) {
  # Check if it is a valid dataframe
  if (!is.data.frame(data)) {
    stop("Input is not a data frame.")
  }

  # Check if data frame is a list and convert into a data frame
  if (is.list(data)) {
    data <- as.data.frame(data)
  }

  colnames(data)[1:2] <- c("Length", "Frequency")

  # Check if the dataframe has exactly two columns
  if (ncol(data) != 2) {
    stop("The dataframe must have exactly two columns as Length and Frequency")
  }

  # cleaning the data or removing the NA values
  data <- data[complete.cases(data)]

  # Define a function to calculate parameters for confidence intervals
  CalParCI <- function(data, Lmax) {
    # Calculate Linf
    Linf <- Lmax / 0.95

    # Calculate Lmat
    Lmat <- 10^(0.8979 * log10(Linf) - 0.0782)

    # Calculate Lopt
    Lopt <- 10^(1.053 * log10(Lmat) - 0.0565)

    # Calculate Lopt_p10
    Lopt_p10 <- Lopt + Lopt / 10

    # Calculate Lopt_m10
    Lopt_m10 <- Lopt - Lopt / 10

    return(c(Lmax, Linf, Lmat, Lopt, Lopt_p10, Lopt_m10))
  }

  # Store parameter estimates from bootstrap samples
  parameter_estimates <- matrix(NA, nrow = resample, ncol = 6)

  # Perform bootstrap resampling
  if (progress) {
    for (i in 1:resample) {
      # Generate bootstrap sample by sampling with replacement from the original data
      bootstrap_sample <- data[sample(nrow(data), replace = TRUE), ]

      # Calculate Lmax for the bootstrap sample
      Lmax_bootstrap <- max(bootstrap_sample[[1]])

      # Calculate parameters for the bootstrap sample
      parameter_estimates[i, ] <- CalParCI(bootstrap_sample, Lmax_bootstrap)

      # Display progress
      cat("Progress: ", round(i / resample * 100), "%\n")
    }
  } else {
    for (i in 1:resample) {
      # Generate bootstrap sample by sampling with replacement from the original data
      bootstrap_sample <- data[sample(nrow(data), replace = TRUE), ]

      # Calculate Lmax for the bootstrap sample
      Lmax_bootstrap <- max(bootstrap_sample[[1]])

      # Calculate parameters for the bootstrap sample
      parameter_estimates[i, ] <- CalParCI(bootstrap_sample, Lmax_bootstrap)
    }
  }

  # Calculate mean of bootstrap estimates
  mean_estimates <- apply(parameter_estimates, 2, mean)
  # Calculate the mean of the parameter estimates
  #mean_estimates <- colMeans(parameter_estimates)

  # Calculate confidence intervals using percentile method
  lower_bound <- apply(parameter_estimates, 2, function(x) quantile(x, probs = 0.025))
  upper_bound <- apply(parameter_estimates, 2, function(x) quantile(x, probs = 0.975))

  # Calculate sumT
  sumT <- sum(data[[2]])

  #Some note and extracted length parameters

  Lmax <- mean_estimates[1]
  Linf <- mean_estimates[2]
  Lmat <- mean_estimates[3]
  Lopt <- mean_estimates[4]
  Lopt_p10 <- mean_estimates[5]
  Lopt_m10 <- mean_estimates[6]

  # Calculate sum_mat and Pmat
  sum_mat <- sum(ifelse(data[[1]] >= Lmat & data[[1]] <= Lmax, data[[2]], 0))
  Pmat <- (sum_mat / sumT) * 100

  # Calculate sum_opt and Popt
  sum_opt <- sum(ifelse(data[[1]] >= Lopt_m10 & data[[1]] <= Lopt_p10, data[[2]], 0))
  Popt <- (sum_opt / sumT) * 100

  # Calculate sum_mega and Pmega
  sum_mega <- sum(ifelse(data[[1]] >= Lopt_p10 & data[[1]] <= Lmax, data[[2]], 0))
  Pmega <- (sum_mega / sumT) * 100

  #Calculating Pobj
  Pobj <- sum(Pmat + Popt + Pmega)

  # Calculating the length at maturity ratio from the mean_estimated
  LM_ratio <-  (Lmat / Lopt)

  #Close the plot window
  #dev.off()


  # Plotting the frequency of bootstrapped resampled data for each parameter
  # Save the plots to a directory

  #pdf("plots_histograms.pdf")  # Open a pdf device to save the histograms

  par(mfrow=c(2, 3)) # Setting up a 2x3 grid for the plots

  parameter_names <- c("Lmax", "Linf", "Lmat", "Lopt", "Lopt_p10", "Lopt_m10")


  for (i in 1:6) {
    hist(parameter_estimates[,i], main=parameter_names[i], xlab=parameter_names[i], ylab="Frequency", col="lightblue" )
    abline(v = mean_estimates[i], col = "red", lwd = 2)
    #segments(mean_estimates[i], 0, mean_estimates[i], max(hist(parameter_estimates[, i], plot = FALSE)$counts), col = "red2", lwd = 2)
    segments(lower_bound[i], 0, lower_bound[i], max(hist(parameter_estimates[, i], plot = FALSE)$counts), col = "black", lwd = 2, lty = "dotted" )
    segments(upper_bound[i], 0, upper_bound[i], max(hist(parameter_estimates[, i], plot = FALSE)$counts), col = "black", lwd = 2, lty = "dotted" )

  }

  #dev.off()  # Close the pdf device
  # Density plots
  # Save the plots to a directory

  #pdf("plots_density.pdf")  # Open a pdf device to save the histograms


  #par(mfrow=c(2, 3)) # Setting up another 2x3 grid for the density plots

  for (i in 1:6) {
    dens <- density(parameter_estimates[,i])
    plot(dens, main=parameter_names[i], col="blue", lwd=1.5, xlim=c(min(parameter_estimates[,i]), max(parameter_estimates[,i])))
    abline(v = mean_estimates[i], col = "red", lwd = 2)
    #segments(lower_bound[i], 0, mean_estimates[i], max(dens$y), col = "yellow3", lwd = 1.5 )
    segments(lower_bound[i], 0, lower_bound[i], max(dens$y), col = "black", lwd = 2, lty = "dotted" )
    segments(upper_bound[i], 0, upper_bound[i], max(dens$y), col = "black", lwd = 2, lty = "dotted" )

  }


  # Create data frames for parameter estimates and confidence intervals
  estimated_length_par <- data.frame(
    Parameter = parameter_names,
    Mean_estimate = mean_estimates,
    Lower_CI = lower_bound,
    Upper_CI = upper_bound
  )

  #Creating boxplot with this dataframe

  # Reshape the data into long format
  # Create an empty dataframe to store the long format
  long_df <- data.frame(Parameter = character(),
                        Interval = character(),
                        Value = numeric(),
                        stringsAsFactors = FALSE)

  # Create a list to store each subset of the dataframe
  df_list <- list(estimated_length_par$Parameter,
                  estimated_length_par$Parameter,
                  estimated_length_par$Parameter)
  names(df_list) <- c("Mean_estimate", "Lower_CI", "Upper_CI")

  # Populate the long dataframe
  for (i in names(df_list)) {
    long_df <- rbind(long_df, data.frame(Parameter = df_list[[i]],
                                         Interval = rep(i, nrow(estimated_length_par)),
                                         Value = estimated_length_par[[i]],
                                         stringsAsFactors = FALSE))
  }

  #Converting the order of my parameters as i want
  long_df$Parameter <- factor(long_df$Parameter, levels = parameter_names)

  par(mfrow = c(1,1))
  # Create boxplot
  boxplot(Value ~ Parameter,
          data = long_df,
          main = "Estimated Length Parameters",
          xlab = "Length Parameters",
          ylab = "Length (cm)",
          col = "lightblue",
          border = "black",
          notch = FALSE,
          #names = parameter_names,
          ylim = range(c(long_df$Value))

  )

  #1. First graph of frequency distribution for length frequency data
  par( mfrow = c(1,1))

  barplot(data[[2]] ~ data[[1]],
          main = "Lenght Frequency Plot",
          xlab = "Length Class",
          ylab = "Frequency",
          ylim = c(0 , max(data[[2]]*1.2)),
          col= "#69b3a2")

  #geom_histogram(aes(x = mult, y = ..density..), fill = "lightblue")
  values <- loess(data[[2]] ~ data[[1]])
  lines(predict(values), col = "red", lwd = 2)
  legend( "topright", legend = c("Observed frequency", "Modelled frequency"), col = c("#69b3a2" , "red"),
          pch = c(15, NA), lty = c(NA, 1), cex = 1 , lwd = 2, seg.len = 1.5 )


  #Creating the data frame froese indicator vs target

  forese_ind_vs_target <- data.frame(
    Parameters = c("Pmat", "Popt", "Pmega"),
    Froese_catch = c(Pmat, Popt, Pmega),
    Froese_tar = c(100, 100, 20)
  )

  #2.Making the second barplot of Froese indicators and target

  barplot(rbind(forese_ind_vs_target[[3]], forese_ind_vs_target[[2]]), beside = TRUE,
          names.arg = forese_ind_vs_target[[1]], col = c("#69b3a2" , "#404080"),
          main = "Target vs Catch", xlab = "Froese Length Indicators",
          ylab = "Percentage (%)"
  )
  legend( "topright", legend = c("Target", "Catch"), fill = c("#69b3a2" , "#404080"))


  #3. Main graph with frequency distribution and corresponding lengths
  par(mfrow = c(2,3))

  for (i in 1:6) {
    dens <- density(parameter_estimates[,i])
    plot(dens, main=parameter_names[i], col="blue", lwd=1.5, xlim=c(min(parameter_estimates[,i]), max(parameter_estimates[,i])))
    abline(v = mean_estimates[i], col = "red", lwd = 2)
    #segments(lower_bound[i], 0, mean_estimates[i], max(dens$y), col = "yellow3", lwd = 1.5 )
    segments(lower_bound[i], 0, lower_bound[i], max(dens$y), col = "black", lwd = 2, lty = "dotted" )
    segments(upper_bound[i], 0, upper_bound[i], max(dens$y), col = "black", lwd = 2, lty = "dotted" )

  }
  for(i in 1:6){

    plot(data[[1]], data[[2]], type = "l", lwd = 1.8, main = parameter_names[i] ,
         xlab = "Length Class (cm)", ylab = "Frequency",
         ylim = c(0, max(data[[2]]+100)), xlim = c(0, max(data[[1]]+2)))
    box(col = "white")
    axis(side = 1, lwd = 1.5)
    axis(side = 2, lwd = 1.5)

    abline(v = mean_estimates[i], col = "red", lwd = 2)
    #segments(mean_estimates[i], 0, mean_estimates[i], max(hist(parameter_estimates[, i], plot = FALSE)$counts), col = "red2", lwd = 2)
    segments(lower_bound[i], 0, lower_bound[i], max(hist(parameter_estimates[, i], plot = FALSE)$counts), col = "black", lwd = 2, lty = "dotted" )
    segments(upper_bound[i], 0, upper_bound[i], max(hist(parameter_estimates[, i], plot = FALSE)$counts), col = "black", lwd = 2, lty = "dotted" )
    # Setting the Optimum sized fish as a rectangle box
    if( i == 4){
      rect(xleft = Lopt_m10, ybottom = 0, xright = Lopt_p10, ytop = max(data[[2]]), col = "#69b3a2")

      #Adding text as Lopt
      #text(x = Lopt, y = max(data[[2]]+20), labels = "Lopt", col = "red3", cex = 1)
      #Adding the Optimum size text in Lopt rect
      text(x = Lopt, y = max(data[[2]]), labels = "Optimum\nsize", col = "red3", cex = 0.8)
    }
    #Adding text for Juveniles
    text(x = mean(c(5, Lmat)), y = 100, labels = "Juveniles", col = "red3", cex = 0.8)
    #Adding text for Mega spawners
    text(x = mean(c(Lopt_m10 , Lmax)), y = 100, labels = "Mega-\nspawners", col = "red3", cex = 0.8)
  }

  #estimated froese parameters
  estimated_froese_par <- data.frame(
    Parameter = c("Pmat", "Popt", "Pmega"),
    Estimate = c(Pmat, Popt, Pmega)
  )

  estimated_freq_par <- data.frame(
    Parameter = c("sumT", "sum_mat", "sum_opt", "sum_mega"),
    Estimate = c(sumT, sum_mat, sum_opt, sum_mega)
  )


  # Return the data frames
  return(list(
    estimated_length_par = estimated_length_par,
    estimated_froese_par = estimated_froese_par,
    estimated_freq_par = estimated_freq_par,
    forese_ind_vs_target = forese_ind_vs_target,
    LM_ratio = LM_ratio,
    Pobj = Pobj
  ))
}

