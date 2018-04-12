#' Create the Gurkaynak, Sack, and Wright spot rate function
#'
#' Create the spot rate function y_t(n) using equation 22 of
#' Gurkaynak, Sack, and Wright (2006). The returned function is
#' parameterized by `n`, corresponding to the n-year continuously
#' compounded spot rates at any given date t.
#'
#' @export
#'
spot_rate_factory <- function(beta_0, beta_1, beta_2, beta_3, tau_1, tau_2) {

  # Spot rate function based on Equation 22 of
  # Gurkaynak, Sack and Wright (2006)
  spot_rate_n <- function(n) {
    beta_0 +
    beta_1 *  (1 - exp( -n / tau_1)) / (n / tau_1) +
    beta_2 * ((1 - exp( -n / tau_1)) / (n / tau_1) - exp( -n / tau_1)) +
    beta_3 * ((1 - exp( -n / tau_2)) / (n / tau_2) - exp( -n / tau_2))
  }

  spot_rate_n
}

#' Calculate zero coupon bond prices from spot rates
#'
#' Given a spot rate function taking a single argument, `n`, create a function for calculating
#' n-year zero coupon bond prices.
#'
#' @export
#'
zero_bond_price_factory <- function(spot_rate_fn) {

  # Zero coupon bond for 1 dollar is just discounted spot rate
  zero_bond_price_fn <- function(n) {
    exp( - spot_rate_fn(n) * n)
  }

  zero_bond_price_fn
}
