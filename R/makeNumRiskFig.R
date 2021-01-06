#' Make a chart of number at risk as a ggplot2 figure
#'
#' @param formula A formula object, which must have a `Surv` object as the response on the left of the `~` operator and the grouping term (often the treatment) on the right.
#' @param data A data frame in which to interpret the variables named in the formula
#' @param timepts A numeric vector of time points to show on the x-axis
#' @param colors Custom colors for each group line
#'
#' @return A ggplot2 object number at risk chart
#'
#' @export
#' @import ggplot2
#' @importFrom survival survfit.formula
#' @importFrom scales percent
#' @importFrom pammtools geom_stepribbon
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by slice arrange mutate ungroup
#' @examples
#' makeNumRiskFig(formula = survival::Surv(stime, scens) ~ treatment, data = exampleData)
#' 
#' makeNumRiskFig(formula = survival::Surv(stime, scens) ~ treatment, data = exampleData) +
#'     labs(x = "Patients at risk")
#' 
#' makeNumRiskFig(formula = survival::Surv(stime, scens) ~ treatment,
#'     data = exampleData,
#'     timepts = c(30, 45, 60, 75, 90)) +
#'     scale_color_brewer(palette = "Set1")
#'     
#' makeNumRiskFig(formula = survival::Surv(stime, scens) ~ treatment,
#'     data = exampleData,
#'     timepts = c(30, 45, 60, 75, 90),
#'     colors = c("black", "blue", "red", "green"))
makeNumRiskFig <- function(formula, data, timepts, colors) {
  ggdf <- makeKMdf(formula = formula, data = data)
  
  if (missing(timepts)) {
    timepts <- round(seq(from = 0, to = max(ggdf$time), length.out = 10))
  }
  
  if (!missing(colors))  stopifnot(length(colors) == length(unique(ggdf$group)))

  tabdf <- ggdf %>% group_by(group) %>%
    arrange(group, time) %>%
    slice(sapply(timepts, function(x) {which.max(ifelse(time > x, -Inf, time - x))})) %>%
    mutate(time = timepts) %>%
    mutate(label = paste0(n.risk - n.event)) %>% #, "\n", sprintf("%.1f", surv*100), "%"
    ungroup
  
  gg1 <- ggplot(data = tabdf, aes(x = time, y = as.numeric(group), label = label, color = group)) +
    geom_text(size = 3, show.legend = FALSE) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 9),
          axis.ticks = element_blank(),
          axis.text.x = element_blank()) +
    scale_x_continuous(breaks = timepts, limits = c(0, max(timepts))) +
    scale_y_continuous(trans = "reverse", expand = c(0, 0.5),
                       breaks = 1:length(levels(tabdf$group)),
                       labels = levels(tabdf$group)) +
    labs(x = "Number at risk")
  
  if (!missing(colors)) {
    gg1 <- gg1 +
      scale_color_manual(values = colors, limits = sort(unique(ggdf$group)))
  }
  gg1
}


