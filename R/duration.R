#' Calculate the duration of a bond
#'
#' @inheritParams price_bond
#' @param type Either `"modified"` or `"macaulay"`.
#'
#' @export
#'
duration <- function(n, c, M, y, type = "modified") {
  stopifnot(type %in% c("modified", "macaulay"))

  P <- price_bond(n, c, M, y)
  c <- c / 2

  calc_duration <- function(n_i, c_i, M_i, y_i, P_i, type) {
    i <- seq_len(n_i)
    time_weighted_sum <- sum( (i * c_i * M_i) / ((1 + y_i) ^ i) ) + (n_i * M_i) / ((1 + y_i) ^ n_i)
    duration_modified_i <- (1 / (1 + y_i)) * time_weighted_sum * (1 / P_i)
    duration_modified_i

    if(type == "macaulay") {
      duration_modified_i <- duration_modified_i * (1 + y_i)
    }

    duration_modified_i
  }

  purrr::pmap_dbl(list(n, c, M, y, P, type), calc_duration)
}


#' Calculate the weights of a duration matched barbell hedging portfolio
#'
#' Using the duration of a bullet and the durations of two assets, this function
#' calculates the weighting necessary to hedge the risk in the bullet, assuming
#' a flat yield curve. The durations can be Modified or Macaulay.
#'
#' @param duration_bullet The bullet duration
#' @param duration_asset_1 The first hedging asset's duration
#' @param duration_asset_2 The second hedging asset's duration
#'
#' @details
#'
#' Only the weight of asset_1 is returned so that this function can be called
#' easily from a `dplyr` workflow.
#'
#' @return
#' The weight of asset_1. The weight of asset_2 is simply (1 - weight_asset_1).
#'
#' @export
#'
barbell_weights <- function(duration_bullet, duration_asset_1, duration_asset_2) {
  w_1 <- (duration_bullet - duration_asset_2) / (duration_asset_1 - duration_asset_2)
  w_1
}
