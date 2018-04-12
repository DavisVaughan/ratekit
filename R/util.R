#' Create a named list using the quoted names of the list components
#'
#' @export
#'
list_named <- function(...) {
  .dots <- rlang::enquos(...)
  nms <- purrr::map_chr(.dots, ~rlang::quo_name(.x))
  values <- purrr::map(.dots, ~rlang::eval_tidy(.x))
  named_values <- purrr::set_names(values, nms)
  named_values
}
