#' Create a quick Kaplan-Meier figure with number-at-risk chart
#'
#' Create a grid of survival ggplot2 figures quickly, with little ability to modify individual components
#'
#' @param formula A formula object, which must have a `Surv` object as the response on the left of the `~` operator and the grouping term (often the treatment) on the right.
#' @param data A data frame in which to interpret the variables named in the formula
#' @param timepts A numeric vector of time points to show on the x-axis
#' @param colors Custom colors for each group line
#' @param ribbon Logical to show a semi-transparent confidence band ribbon
#' @param heights A vector of two numeric values that indicate the ratio of the first row to the second
#'
#' @return A grid figure
#'
#' @export
#' @examples
#' quickSurvFig(survival::Surv(stime, scens) ~ treatment, data = mice)
#' quickSurvFig(survival::Surv(stime, scens) ~ treatment,
#'   data = mice,
#'   timepts = c(30, 45, 60, 75, 90),
#'   colors = c("black", "blue", "red", "green"),
#'   ribbon = FALSE,
#'   heights = c(4, 1)
#' )
quickSurvFig <- function(formula, data, timepts = NULL, colors = NULL, ribbon = TRUE, heights = c(2.5, 1)) {
  topFigure <- makeKMFig(formula = formula, data = data, timepts = timepts, colors = colors, ribbon = ribbon)
  bottomChart <- makeNumRiskFig(formula = formula, data = data, timepts = timepts, colors = colors)
  combineSurvFigs(topFigure, bottomChart, heights)
}
