check_tri_params <- function(min, max, mode) {
  if (any(min >= max, na.rm = TRUE)) {
    stop("`min` must be smaller than `max`.")
  }

  if (any(mode < min | mode > max, na.rm = TRUE)) {
    stop("`mode` must be between `min` and `max`.")
  }
}


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
