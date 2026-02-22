#' Calculate clearance
#'
#' @param k Elimination rate constant
#' @param V Volume of distribution
#' @return Clearance
#' @export
calculate_clearance <- function(k, V) {

  if (k <= 0 || V <= 0) {
    stop("k and V must be positive.")
  }

  k * V
}
