#' Simulate oral PK model (1-compartment)
#'
#' @param dose Dose
#' @param V Volume
#' @param k Elimination rate
#' @param ka Absorption rate
#' @param F Bioavailability (0-1)
#' @param time Time vector
#' @export
simulate_oral_pk <- function(dose, V, k, ka, F = 1, time) {

  if (ka == k) {
    stop("ka and k must be different.")
  }

  conc <- (F * dose * ka) / (V * (ka - k)) *
    (exp(-k * time) - exp(-ka * time))

  data.frame(time = time, concentration = conc)
}
