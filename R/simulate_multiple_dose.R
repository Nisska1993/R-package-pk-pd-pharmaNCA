#' Simulate multiple IV bolus dosing
#'
#' @param dose Dose
#' @param V Volume
#' @param k Elimination rate
#' @param tau Dosing interval
#' @param n_doses Number of doses
#' @param time Time vector
#' @export
simulate_multiple_dose <- function(dose, V, k, tau, n_doses, time) {

  conc <- rep(0, length(time))

  for (i in 0:(n_doses - 1)) {
    dose_time <- i * tau
    conc <- conc + ifelse(
      time >= dose_time,
      (dose / V) * exp(-k * (time - dose_time)),
      0
    )
  }

  data.frame(time = time, concentration = conc)
}
