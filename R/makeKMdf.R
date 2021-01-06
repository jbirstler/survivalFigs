#' Make a data frame for a Kaplan-Meier curve
#'
#' Create a data frame for plotting a Kaplan-Meier curve by pulling components from \code{\link[survival:survfit.formula]{survival:survfit.formula}}
#'
#' @param formula A formula object, which must have a `Surv` object as the response on the left of the `~` operator and the grouping term (often the treatment) on the right.
#' @param data A data frame in which to interpret the variables named in the formula
#'
#' @return A data frame with columns `group`, `time`, `n.risk`, `n.event`, `n.censor`, `surv`, `low`, and `upp`
#'
#' @export
#' @importFrom survival survfit.formula
#' @examples
#' makeKMdf(formula = Surv(stime, scens) ~ treatment, data = exampleData)

makeKMdf <- function(formula, data) {
  fit <- survival::survfit.formula(formula = formula, data = data)
  groups <- sub("^.*=", "", names(fit$strata))
  atDay0 <-  data.frame(
    stringsAsFactors = FALSE,
    group = groups,
    time = 0,
    n.risk = fit$n,
    n.event = 0,
    n.censor = 0,
    surv = 1,
    low = 1,
    upp = 1
  )
  afterDay0 <- data.frame(
    stringsAsFactors = FALSE,
    group = rep(groups, fit$strata),
    time = fit$time,
    n.risk = fit$n.risk,
    n.event = fit$n.event,
    n.censor = fit$n.censor,
    surv = fit$surv,
    low = fit$lower,
    upp = fit$upper
  )
  dat <- rbind(atDay0, afterDay0)
  dat$group <- factor(dat$group, levels = groups)
  return(dat)
}
