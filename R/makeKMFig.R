#' Make a survival curve figure as a ggplot2 object
#'
#' @param formula A formula object, which must have a `Surv` object as the response on the left of the `~` operator and the grouping term (often the treatment) on the right.
#' @param data A data frame in which to interpret the variables named in the formula
#' @param timepts A numeric vector of time points to show on the x-axis
#' @param colors Custom colors for each group line
#' @param ribbon Logical to show a semi-transparent confidence band ribbon
#'
#' @return A ggplot2 object of a Kaplan-Meier curve
#'
#' @export
#' @import ggplot2
#' @importFrom survival survfit.formula
#' @importFrom scales percent
#' @importFrom pammtools geom_stepribbon
#' @examples
#' makeKMFig(formula = survival::Surv(stime, scens) ~ treatment, data = mice)
#'
#' makeKMFig(formula = survival::Surv(stime, scens) ~ treatment, data = mice) +
#'   ggplot2::labs(y = "OS", color = "Treatment", fill = "Treatment")
#'
#' makeKMFig(formula = survival::Surv(stime, scens) ~ treatment, data = mice, ribbon = FALSE) +
#'   ggplot2::scale_color_brewer(palette = "Set1")
makeKMFig <- function(formula, data, timepts = NULL, colors = NULL, ribbon = TRUE) {
  ggdf <- makeKMdf(formula = formula, data = data)

  if (is.null(timepts)) {
    timepts <- round(seq(from = 0, to = max(ggdf$time), length.out = 10))
  }
  if (!is.null(colors)) {
    stopifnot(length(colors) == length(unique(ggdf$group)))
  }

  gg1 <- ggplot(data = ggdf, aes(x = .data$time, y = .data$surv, ymin = .data$low, ymax = .data$upp)) +
    geom_step(aes(color = .data$group)) +
    geom_point(data = ggdf[ggdf$n.censor == 1, ], aes(color = .data$group), shape = 3, show.legend = FALSE) +
    theme_bw() +
    theme(
      legend.position = c(0, 0), legend.justification = c(0, 0), legend.background = element_blank(), legend.key = element_blank(),
      legend.key.height = unit(0.4, "cm"), panel.border = element_blank(), axis.line = element_line(), panel.grid = element_blank()
    ) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_x_continuous(breaks = timepts, limits = c(0, max(ggdf$time))) +
    labs(y = "Survival probability", x = "Time", color = as.character(formula[3]), fill = as.character(formula[3]))
  if (ribbon) {
    gg1 <- gg1 + pammtools::geom_stepribbon(alpha = 0.2, aes(fill = .data$group))
  }
  if (!is.null(colors)) {
    gg1 <- gg1 + scale_color_manual(values = colors, limits = sort(unique(ggdf$group))) + scale_fill_manual(
      values = colors,
      limits = sort(unique(ggdf$group))
    )
  }
  gg1
}
