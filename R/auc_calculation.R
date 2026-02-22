#' Calculate AUC using trapezoidal rule
#'
#' @param time Numeric vector
#' @param concentration Numeric vector
#' @return Numeric AUC value
#' @export
calculate_auc <- function(time, concentration) {

  if (length(time) != length(concentration)) {
    stop("time and concentration must have same length.")
  }

  if (any(diff(time) < 0)) {
    stop("time must be increasing.")
  }

  auc <- sum(diff(time) *
               (head(concentration, -1) + tail(concentration, -1)) / 2)

  return(auc)
}
