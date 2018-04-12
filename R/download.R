#' Download the rates data from the Federal Reserve
#'
#' @export
download_rates_xls <- function(url = "https://www.federalreserve.gov/econresdata/researchdata/feds200628.xls", dest = "rate_data.xls", overwrite = FALSE) {

  # If file exists and no overwrite, bail as early as possible
  if(fs::file_exists(dest)) {
    if(!overwrite) {
      stop("Destination file already exists, specify `overwrite = TRUE` if necessary.", call. = FALSE)
    }
  }

  # Download the file first before deleting anything
  # If there are problems with the download, at least we don't overwrite what
  # may already be there
  temp_file <- fs::file_temp()
  download.file(url = url, destfile = temp_file)
  on.exit(unlink(temp_file))

  if(fs::file_exists(dest)) {
    if(overwrite) {
      fs::file_delete(dest)
    }
  }

  fs::file_copy(temp_file, dest)

  wd_col <- crayon::blue(getwd())
  dest_col <- crayon::yellow(glue::glue("/", dest))

  glue::glue("Download successful, check \n{wd_col}{dest_col}\nfor the downloaded file.")
}
