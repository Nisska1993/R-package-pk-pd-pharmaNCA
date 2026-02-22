#' Simulate 1-compartment IV bolus PK model
#'
#' @param dose Numeric. Administered dose (must be > 0).
#' @param V Numeric. Volume of distribution (must be > 0).
#' @param k Numeric. Elimination rate constant (must be > 0).
#' @param time Numeric vector of time points (must be >= 0).
#'
#' @return An object of class "pk_sim" containing concentration-time data.
#' @export
simulate_pk <- function(dose, V, k, time) {

  # ---- Validation ----
  if (!is.numeric(dose) || length(dose) != 1 || dose <= 0) {
    stop("`dose` must be a single positive numeric value.")
  }

  if (!is.numeric(V) || length(V) != 1 || V <= 0) {
    stop("`V` must be a single positive numeric value.")
  }

  if (!is.numeric(k) || length(k) != 1 || k <= 0) {
    stop("`k` must be a single positive numeric value.")
  }

  if (!is.numeric(time) || any(time < 0)) {
    stop("`time` must be a numeric vector with non-negative values.")
  }

  # ---- Model ----
  concentration <- (dose / V) * exp(-k * time)

  result <- data.frame(
    time = time,
    concentration = concentration
  )

  structure(
    list(
      params = list(dose = dose, V = V, k = k),
      data = result
    ),
    class = "pk_sim"
  )
}
