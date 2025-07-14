# Declare global variables at the top of the file
utils::globalVariables(c("fit", "lwr", "upr"))

#'@title Plot and Model Length-Weight Relationships with Optional Log Transformation
#'
#' @description This function visualizes and models the relationship between length and weight
#' (or any two continuous variables) using linear regression. It supports both
#' standard and log-transformed models, producing a ggplot2-based plot with a fitted
#' line, optional confidence interval shading, and annotations for the regression
#' equation, R^2, and p-value. When save_output is TRUE, the plot and model summary
#' are saved to the working directory as a PDF and text file, respectively.
#'
#' @param data A data frame with at least two columns: the first for length, the second for weight.
#' @param log_transform Logical. Whether to apply a log-log transformation to the variables. Default is \code{TRUE}.
#' @param point_col Color of the data points. Default is \code{"black"}.
#' @param line_col Color of the regression line. Default is \code{"red"}.
#' @param shade_col Color for the confidence interval ribbon. Default is \code{"red"}.
#' @param point_size Size of the data points. Default is \code{2}.
#' @param line_size Size of the regression line. Default is \code{1}.
#' @param alpha Transparency for the confidence interval ribbon. Default is \code{0.2}.
#' @param main Title of the plot. Default is \code{"Length-Weight Relationship"}.
#' @param xlab Optional. Custom x-axis label. If \code{NULL}, a label is generated based on \code{log_transform}.
#' @param ylab Optional. Custom y-axis label. If \code{NULL}, a label is generated based on \code{log_transform}.
#' @param save_output Logical. Whether to save the plot as a PDF and the model summary as a text file. Default is \code{TRUE}.
#'
#' @return A list containing:
#' \item{model}{The fitted \code{lm} object}
#' \item{intercept}{The estimated intercept (back-transformed if log_transform = TRUE)}
#' \item{slope}{The estimated slope}
#' \item{r_squared}{R-squared value}
#' \item{correlation_r}{Correlation coefficient (r)}
#' \item{p_value}{P-value for slope}
#' \item{plot}{The \code{ggplot} object for further customization}
#'
#' @examples
#' data(LWdata, package = "aLBI")
#' result <- LWR(LWdata, log_transform = TRUE, save_output = FALSE)
#' print(result$plot)
#'
#' @importFrom stats coef lm predict
#' @importFrom utils globalVariables
#' @import ggplot2
#' @export
LWR <- function(data,
                log_transform = TRUE,
                point_col = "black",
                line_col = "red",
                shade_col = "red",
                point_size = 2,
                line_size = 1,
                alpha = 0.2,
                main = "Length-Weight Relationship",
                xlab = NULL,
                ylab = NULL,
                save_output = TRUE) {
  # Load required package
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Please install it using install.packages('ggplot2').")
  }

  # Input validation
  if (!is.data.frame(data) || ncol(data) < 2) {
    stop("Data must be a data frame with at least two columns.")
  }
  if (anyNA(data[[1]]) || anyNA(data[[2]])) {
    stop("Data contains missing values in the first two columns.")
  }
  if (log_transform && (any(data[[1]] <= 0) || any(data[[2]] <= 0))) {
    stop("Log-transformation requires positive values for length and weight.")
  }

  # Prepare data
  x_raw <- data[[1]]
  y_raw <- data[[2]]

  if (log_transform) {
    x <- log(x_raw)
    y <- log(y_raw)
    xlab <- xlab %||% paste0("log(", names(data)[1] %||% "Length", ")")
    ylab <- ylab %||% paste0("log(", names(data)[2] %||% "Weight", ")")
  } else {
    x <- x_raw
    y <- y_raw
    xlab <- xlab %||% (names(data)[1] %||% "Length")
    ylab <- ylab %||% (names(data)[2] %||% "Weight")
  }

  # Fit linear model
  model <- lm(y ~ x)
  a <- if (log_transform) round(exp(coef(model)[1]), 4) else round(coef(model)[1], 4)
  b <- round(coef(model)[2], 4)
  p_value <- signif(summary(model)$coefficients[2, 4], 4)
  r_squared <- round(summary(model)$r.squared, 4)
  correlation_r <- round(sqrt(r_squared) * sign(coef(model)[2]), 4)

  # Prepare data for plotting
  plot_data <- data.frame(x = x, y = y)
  x_seq <- seq(min(x), max(x), length.out = 100)
  newdata <- data.frame(x = x_seq)
  preds <- predict(model, newdata, interval = "confidence")
  pred_data <- data.frame(x = x_seq, fit = preds[, "fit"], lwr = preds[, "lwr"], upr = preds[, "upr"])

  # Create ggplot
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = plot_data, ggplot2::aes(x = x, y = y),
                        color = point_col, size = point_size) +
    ggplot2::geom_line(data = pred_data, ggplot2::aes(x = x, y = fit),
                       color = line_col, size = line_size) +
    ggplot2::labs(title = main, x = xlab, y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  # Add confidence interval ribbon
  if (alpha > 0) {
    p <- p + ggplot2::geom_ribbon(data = pred_data, ggplot2::aes(x = x, ymin = lwr, ymax = upr),
                                  fill = shade_col, alpha = alpha)
  }

  # Add annotations
  eqn <- if (log_transform) {
    paste0("W = ", a, " L^", b)
  } else {
    paste0("y = ", a, " + ", b, "x")
  }
  r_text <- paste0("R^2 = ", r_squared)
  p_text <- paste0("p = ", p_value)

  p <- p + ggplot2::annotate("text", x = min(x) + 0.05 * (max(x) - min(x)),
                             y = max(y) - 0.05 * (max(y) - min(y)),
                             label = paste(eqn, r_text, p_text, sep = "\n"),
                             hjust = 0, vjust = 1, size = 4)

  # Save outputs if save_output is TRUE
  if (save_output) {
    # Check if working directory is writable
    if (!file.access(getwd(), 2) == 0) {
      warning("Working directory '", getwd(), "' is not writable. Outputs will not be saved.")
    } else {
      # Try saving plot with error handling
      tryCatch({
        ggplot2::ggsave(filename = "LWR_plot.pdf", plot = p,
                        path = getwd(), width = 7, height = 5, device = "pdf")
      }, error = function(e) {
        warning("Failed to save plot to '", file.path(getwd(), "LWR_plot.pdf"), "': ", e$message)
      })

      # Try saving model summary with error handling
      tryCatch({
        sink(file = file.path(getwd(), "LWR_summary.txt"))
        cat("Length-Weight Relationship Model Summary\n")
        cat("======================================\n")
        cat("Equation:", eqn, "\n")
        cat("Intercept:", a, "\n")
        cat("Slope:", b, "\n")
        cat("R-squared:", r_squared, "\n")
        cat("Correlation coefficient (r):", correlation_r, "\n")
        cat("P-value:", p_value, "\n\n")
        cat("Full Model Summary:\n")
        print(summary(model))
        sink()
      }, error = function(e) {
        warning("Failed to save summary to '", file.path(getwd(), "LWR_summary.txt"), "': ", e$message)
      })
    }
  }

  # Return results
  return(list(
    model = model,
    intercept = a,
    slope = b,
    r_squared = r_squared,
    correlation_r = correlation_r,
    p_value = p_value,
    plot = p
  ))
}
