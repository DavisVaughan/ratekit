#' Read in only the rates data
#'
#' Functions for reading rates data, with sane defaults for NA handling. [read_rates()]
#' reads only the interest rates data, and [read_rates_meta_data()] reads in
#' only the meta data.
#'
#' @export
#'
read_rates <- function(path,
                       sheet = NULL,
                       range = NULL,
                       col_names = TRUE,
                       col_types = NULL,
                       na = c("", "-999.99"),
                       trim_ws = TRUE,
                       skip = 9,
                       n_max = Inf,
                       guess_max = min(1000, n_max),
                       clean_date_col = TRUE) {

  rates <- readxl::read_xlsx(path, sheet, range, col_names,
                             col_types, na, trim_ws, skip, n_max,
                             guess_max)


  if(clean_date_col) {
    rates <- dplyr::rename(rates, date = X__1)
    rates <- dplyr::mutate(rates, date = as.Date(date))
  }

  rates
}

#' @rdname read_rates
#' @export
#'
read_rates_meta_data <- function(path,
                                 sheet = NULL,
                                 range = "A3:H8",
                                 col_names = TRUE,
                                 col_types = NULL,
                                 na = c("", "N/A"),
                                 trim_ws = TRUE,
                                 skip = 0,
                                 n_max = Inf,
                                 guess_max = min(1000, n_max)) {

  meta <- readxl::read_xlsx(path, sheet, range, col_names,
                            col_types, na, trim_ws, skip, n_max,
                            guess_max)

  # New columns were created b/c of merged cells
  # They all start with X__ and can be safely removed
  dplyr::select(meta, -starts_with("X__"))
}
