plot.ml_g_fit <- function(x, ...) {
  require(ggplot2)
  require(gridExtra)
  e.hat <- residuals(x)
  e.hat.ss <- residuals(x, type="ss")
  y.hat <- fitted(x)
  n <- nrow(x$X)
  pp1 <- qplot(y.hat, x$y, alpha = I(0.2),        ### Plot 1
            ylab = "Observations", xlab = "Fitted Values") + 
         geom_abline(intercept = 0, slope = 1) +
         geom_smooth(aes(x = y.hat, y = x$y), se = FALSE)
  pp2 <- qplot(y.hat, e.hat, alpha = I(0.2),      ### Plot 2
               ylab = "Residuals", xlab = "Fitted Values") + 
         geom_abline(intercept = 0, slope = 0) +
         geom_smooth(aes(x = y.hat, y = e.hat), se = FALSE)
  pp3 <- qplot(y.hat, abs(sqrt(e.hat.ss)),         ### Plot 3
               alpha = I(0.2),
               ylab = "Sqrt (Abs( Stand. Res.))",
               xlab = "Fitted Values") + 
         geom_smooth(aes(x = y.hat, y = abs(sqrt(e.hat.ss))),
               se = FALSE)
  pp4 <- qplot(sort(e.hat.ss), qnorm((1:n)/(n+1)), ### Plot 4
               alpha = I(0.2),
               xlab = "Standardized Studentized Residuals",
               ylab = "Normal Quantiles")
  grid.arrange(pp1, pp2, pp3, pp4, ncol=2)
}
