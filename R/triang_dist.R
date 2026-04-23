#' Check triangular distribution parameters
#'
#' @param min Lower limit.
#' @param max Upper limit.
#' @param mode Mode of the distribution.
#'
#' @return No return value. Stops with an error if parameters are invalid.
#' @keywords internal
check_tri_params <- function(min, max, mode) {
  if (any(min >= max, na.rm = TRUE)) {
    stop("`min` must be smaller than `max`.")
  }

  if (any(mode < min | mode > max, na.rm = TRUE)) {
    stop("`mode` must be between `min` and `max`.")
  }
}

#' Density of the triangular distribution
#'
#' @param x Vector of values.
#' @param min Lower limit.
#' @param max Upper limit.
#' @param mode Mode of the distribution.
#'
#' @return Numeric vector of density values.
#' @export
dtriang <- function(x, min, max, mode) {

  check_tri_params(min, max, mode)

  f <- numeric(length(x))

  for (i in 1:length(x)) {

    if (x[i] < min || x[i] > max) {
      f[i] <- 0

    } else if (x[i] <= mode) {
      f[i] <- 2 * (x[i] - min) / ((max - min) * (mode - min))

    } else {
      f[i] <- 2 * (max - x[i]) / ((max - min) * (max - mode))
    }
  }

  return(f)
}

#' Distribution function of the triangular distribution
#'
#' @param q Vector of values.
#' @param min Lower limit.
#' @param max Upper limit.
#' @param mode Mode of the distribution.
#'
#' @return Numeric vector of cumulative probabilities.
#' @export
ptriang <- function(q, min, max, mode) {

  check_tri_params(min, max, mode)

  x <- numeric(length(q))

  for (i in 1:length(q)) {

    if (q[i] < min) {
      x[i] <- 0

    } else if (q[i] > max) {
      x[i] <- 1

    } else if (q[i] <= mode) {
      x[i] <- (q[i] - min)^2 / ((max - min) * (mode - min))

    } else {
      x[i] <- 1 - (max - q[i])^2 / ((max - min) * (max - mode))
    }
  }

  return(x)
}

#' Quantile function of the triangular distribution
#'
#' @param p Vector of probabilities.
#' @param min Lower limit.
#' @param max Upper limit.
#' @param mode Mode of the distribution.
#'
#' @return Numeric vector of quantiles.
#' @export
qtriang <- function(p, min, max, mode) {

  check_tri_params(min, max, mode)

  if (any(p < 0 | p > 1)) {
    stop("p debe estar entre 0 y 1")
  }

  q <- numeric(length(p))

  for (i in 1:length(p)) {

    fc <- (mode - min) / (max - min)

    if (p[i] <= fc) {
      q[i] <- min + sqrt(p[i] * (max - min) * (mode - min))

    } else {
      q[i] <- max - sqrt((1 - p[i]) * (max - min) * (max - mode))
    }
  }

  return(q)
}

#' Random generation from the triangular distribution
#'
#' @param n Number of observations.
#' @param min Lower limit.
#' @param max Upper limit.
#' @param mode Mode of the distribution.
#'
#' @return Numeric vector of random values.
#' @importFrom stats runif
#' @export
rtriang <- function(n, min, max, mode) {

  check_tri_params(min, max, mode)

  if (n <= 0) {
    stop("n debe ser positivo")
  }

  x <- runif(n)
  qtriang(x, min, max, mode)
}
