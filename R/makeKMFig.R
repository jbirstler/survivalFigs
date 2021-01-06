#' Make a survival curve figure as a ggplot2 object
#'
#' @param formula A formula object, which must have a `Surv` object as the response on the left of the `~` operator and the grouping term (often the treatment) on the right.
#' @param data A data frame in which to interpret the variables named in the formula
#' @param timepts A numeric vector of time points to show on the x-axis
#' @param colors Custom colors for each group line
#'
#' @return A ggplot2 object of a Kaplan-Meier curve
#'
#' @export
#' @import ggplot2
#' @importFrom survival survfit.formula
#' @importFrom scales percent
#' @importFrom pammtools geom_stepribbon
#' @examples
#' makeKMFig(formula = survival::Surv(stime, scens) ~ treatment, data = exampleData)
#' 
#' makeKMFig(formula = survival::Surv(stime, scens) ~ treatment, data = exampleData) +
#'     labs(y = "OS", color = "Treatment")
#' 
#' makeKMFig(formula = survival::Surv(stime, scens) ~ treatment, data = exampleData, ribbon = TRUE) +
#'     scale_color_brewer(palette = "Set1") +
#'     scale_fill_brewer(palette = "Set1")
makeKMFig <- function(formula, data, timepts, colors, ribbon = FALSE) {
  ggdf <- makeKMdf(formula = formula, data = data)
  
  if (missing(timepts)) {
    timepts <- round(seq(from = 0, to = max(ggdf$time), length.out = 10))
  }
  if (!missing(colors))  stopifnot(length(colors) == length(unique(ggdf$group)))

  gg1 <- ggplot(data = ggdf, aes(x = time, y = surv, ymin = low, ymax = upp)) +
    geom_step(aes(color = group)) +
    geom_point(data = ggdf[ggdf$n.censor == 1,],
               aes(color = group),
               shape = 3, show.legend = FALSE) +
    theme_bw() +
    theme(legend.position = c(0, 0),
          legend.justification = c(0, 0),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.key.height = unit(0.4, "cm"),
          panel.border = element_blank(),
          axis.line = element_line(),
          panel.grid = element_blank()) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_x_continuous(breaks = timepts, limits = c(0, max(timepts))) +
    labs(y = "Survival probability", x = "Days", color = "Group", fill = "Group")
  if (ribbon) {
    gg1 <- gg1 + 
      pammtools::geom_stepribbon(alpha = 0.2, aes(fill = group))
  }
  if (!missing(colors)) {
    gg1 <- gg1 +
      scale_color_manual(values = colors, limits = sort(unique(ggdf$group))) +
      scale_fill_manual(values = colors, limits = sort(unique(ggdf$group)))
  }
  gg1
}