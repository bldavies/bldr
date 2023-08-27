#' Simulate a Wiener process
#'
#' \code{sample_wiener} generates a sample path of a Wiener process.
#'
#' @param mu Drift parameter. Default is 0.
#' @param sigma Variance parameter. Default is 1.
#' @param t_max Time horizon. Default is 1.
#' @param dt Time step. Default is 0.001.
#'
#' @return Data frame with columns
#' \describe{
#' \item{t}{Time}
#' \item{W}{Simulated value}
#' }
#'
#' @export
sample_wiener <- function(mu = 0, sigma = 1, t_max = 1, dt = 1e-3) {
  t <- seq(0, t_max, by = dt)
  dW <- c(0, rnorm(length(t) - 1, mean = mu * dt, sd = sigma * sqrt(dt)))
  return (data.frame(t = t, W = cumsum(dW)))
}
