#' Calculate elimination half-life
#'
#' @param k Elimination rate constant
#' @return Half-life
#' @export
calculate_half_life <- function(k) {

  if (!is.numeric(k) || k <= 0) {
    stop("k must be positive.")
  }

  log(2) / k
}
