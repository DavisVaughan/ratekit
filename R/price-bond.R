#' Price a coupon bond
#'
#' Price an n-period semi-annual coupon bond.
#'
#' @param n The number of 6 month _periods_ in the bond.
#' @param c The annualized semi-annual coupon rate, as a decimal.
#' @param M The maturity value of the bond.
#' @param y The yield to maturity on the bond, as a decimal.
#'
#' @details
#' Any of these variables can be a vector, and as long as the other variables
#' are either the same length, or length 1, you can calculate multiple bond
#' prices at once.
#'
#' @export
#'
price_bond <- function(n, c, M, y) {

  c <- c / 2

  calc_price <- function(n_i, c_i, M_i, y_i) {
    i <- seq_len(n_i)
    PV_coupons <- sum( c_i * M_i / ((1 + y_i) ^ i) )
    PV_maturity <- M_i / ((1 + y_i) ^ n_i)
    PV_coupons + PV_maturity
  }

  purrr::pmap_dbl(list(n, c, M, y), calc_price)
}
