#' Combine components of a survival figure
#'
#' Create a grid of survival ggplot2 figures
#'
#' You likely won't use this function unless you are modifying the individual objects that
#' are returned from \code{makeKMFig} and \code{makeNumRiskFig}
#'
#' @param KMFig A ggplot2 object that serves as the Kaplan-Meier figure
#' @param NumRiskFig A ggplot2 object that serves as the number at risk chart
#' @param heights A vector of two numeric values that indicate the ratio of the first row to the second
#'
#' @return A grid figure
#'
#' @export
#' @importFrom survival survfit.formula
#' @importFrom ggplot2 ggplotGrob
#' @importFrom grid unit.pmax
#' @importFrom gridExtra grid.arrange
#' @examples
#'
#' topFigure <- makeKMFig(
#'   formula = survival::Surv(stime, scens) ~ treatment,
#'   data = mice, ribbon = TRUE
#' )
#' bottomChart <- makeNumRiskFig(
#'   formula = survival::Surv(stime, scens) ~ treatment,
#'   data = mice
#' )
#' combineSurvFigs(topFigure, bottomChart)
combineSurvFigs <- function(KMFig, NumRiskFig, heights = c(2.5, 1)) {
  ggg1 <- ggplotGrob(KMFig)
  ggg2 <- ggplotGrob(NumRiskFig)
  maxWidth <- grid::unit.pmax(ggg1$widths, ggg2$widths)
  ggg2$widths <- as.list(maxWidth)
  ggg1$widths <- as.list(maxWidth)

  gridExtra::grid.arrange(ggg1, ggg2, heights = heights, ncol = 1)
}
