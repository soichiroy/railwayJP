#' @importFrom rlang .data
NULL

.onLoad <- function(libname, pkgname) {
  # Nothing to do on load - data is loaded lazily
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "railwayJP: Plot Japanese Railway Routes\n",
    "Data source: National Land Numerical Information (Railway Data), MLIT\n",
    "Run download_railway_data() to download the required data (~20MB)"
  )
}
