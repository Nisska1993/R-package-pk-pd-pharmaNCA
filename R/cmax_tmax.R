#' Calculate Cmax and Tmax
#'
#' @param time Time vector
#' @param concentration Concentration vector
#' @return List with Cmax and Tmax
#' @export
calculate_cmax_tmax <- function(time, concentration) {

  idx <- which.max(concentration)

  list(
    Cmax = concentration[idx],
    Tmax = time[idx]
  )
}
