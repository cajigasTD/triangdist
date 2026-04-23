check_tri_params <- function(min, max, mode) {
  if (any(min >= max, na.rm = TRUE)) {
    stop("`min` must be smaller than `max`.")
  }

  if (any(mode < min | mode > max, na.rm = TRUE)) {
    stop("`mode` must be between `min` and `max`.")
  }
}
