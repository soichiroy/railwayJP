# Package environment for storing loaded data
.pkg_env <- new.env(parent = emptyenv())

#' Get the cache directory for railwayJP data
#'
#' @return Character string with the path to the cache directory
#' @export
#' @examples
#' get_cache_dir()
get_cache_dir <- function() {
  rappdirs::user_cache_dir("railwayJP", "R")
}

#' Get the path to the railway data directory
#'
#' @param cache_dir Optional custom cache directory path
#' @return Character string with the path to the data directory
#' @export
get_data_dir <- function(cache_dir = NULL) {
  if (is.null(cache_dir)) {
    cache_dir <- get_cache_dir()
  }

  file.path(cache_dir, "UTF-8")
}

#' Check if railway data is available locally
#'
#' @param cache_dir Optional custom cache directory path
#' @return Logical indicating if data exists
#' @export
#' @examples
#' has_railway_data()
has_railway_data <- function(cache_dir = NULL) {
  data_dir <- get_data_dir(cache_dir)
  railroad_file <- file.path(data_dir, "N02-22_RailroadSection.shp")
  station_file <- file.path(data_dir, "N02-22_Station.shp")
  file.exists(railroad_file) && file.exists(station_file)
}

#' Download railway data from MLIT
#'
#' Downloads the National Land Numerical Information (Railway Data) from
#' the Ministry of Land, Infrastructure, Transport and Tourism (MLIT).
#'
#' @param cache_dir Optional custom cache directory path. If NULL, uses
#'   the default user cache directory.
#' @param force If TRUE, re-download even if data already exists
#' @param quiet If TRUE, suppress progress messages
#'
#' @return Invisibly returns the path to the downloaded data directory
#' @export
#'
#' @details
#' The data is downloaded from \url{https://nlftp.mlit.go.jp/ksj/} and is
#' provided under the MLIT Open Data License.
#'
#' Attribution: Source: National Land Numerical Information (Railway Data), MLIT
#' (Japanese: "Source: Kokudo Suuchi Jouhou (Tetsudou Data) Kokudo Koutsuu Shou")
#'
#' @examples
#' \dontrun{
#' download_railway_data()
#' }
download_railway_data <- function(cache_dir = NULL, force = FALSE, quiet = FALSE) {
  if (is.null(cache_dir)) {
    cache_dir <- get_cache_dir()
  }

  data_dir <- get_data_dir(cache_dir)

  if (has_railway_data(cache_dir) && !force) {
    if (!quiet) {
      message("Railway data already exists at: ", data_dir)
      message("Use force = TRUE to re-download")
    }
    return(invisible(data_dir))
  }

  # Create cache directory
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Download URL for N02-22 (2022 data)
  # Note: This URL may change - check https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N02-v3_1.html
  url <- "https://nlftp.mlit.go.jp/ksj/gml/data/N02/N02-22/N02-22_GML.zip"
  zip_file <- file.path(cache_dir, "N02-22_GML.zip")

 if (!quiet) {
    message("Downloading railway data from MLIT...")
    message("URL: ", url)
    message("This may take a few minutes...")
  }

  # Download
  tryCatch({
    utils::download.file(
      url,
      zip_file,
      mode = "wb",
      quiet = quiet
    )
  }, error = function(e) {
    stop(
      "Failed to download railway data. Please check your internet connection.\n",
      "You can also download manually from:\n",
      "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N02-v3_1.html\n",
      "Error: ", e$message
    )
  })

  if (!quiet) {
    message("Extracting data...")
  }

  # Extract
  utils::unzip(zip_file, exdir = cache_dir)

  # Clean up zip file
  unlink(zip_file)

  if (!quiet) {
    message("Railway data installed successfully at: ", data_dir)
    message("\nData attribution:")
    message("Source: National Land Numerical Information (Railway Data), MLIT")
    message("URL: https://nlftp.mlit.go.jp/ksj/")
  }

  invisible(data_dir)
}

#' Load railway data
#'
#' Loads the railway section and station data. If data is not available locally,
#' prompts the user to download it.
#'
#' @param cache_dir Optional custom cache directory path
#' @param force_reload If TRUE, reload data even if already loaded in memory
#'
#' @return A list with two elements: `railroad` and `stations`, both sf objects
#' @export
#'
#' @examples
#' \dontrun{
#' data <- load_railway_data()
#' data$railroad
#' data$stations
#' }
load_railway_data <- function(cache_dir = NULL, force_reload = FALSE) {
  # Check if already loaded
  if (!force_reload &&
      exists("railroad_data", envir = .pkg_env) &&
      exists("station_data", envir = .pkg_env)) {
    return(list(
      railroad = get("railroad_data", envir = .pkg_env),
      stations = get("station_data", envir = .pkg_env)
    ))
  }

  # Check if data exists
  if (!has_railway_data(cache_dir)) {
    stop(
      "Railway data not found.\n",
      "Please run download_railway_data() first, or download manually from:\n",
      "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N02-v3_1.html\n",
      "Expected location: ", get_data_dir(cache_dir)
    )
  }

  data_dir <- get_data_dir(cache_dir)

  message("Loading railway data...")

  railroad_data <- sf::st_read(
    file.path(data_dir, "N02-22_RailroadSection.shp"),
    options = "ENCODING=UTF-8",
    quiet = TRUE
  )

  station_data <- sf::st_read(
    file.path(data_dir, "N02-22_Station.shp"),
    options = "ENCODING=UTF-8",
    quiet = TRUE
  )

  # Cache in package environment
  assign("railroad_data", railroad_data, envir = .pkg_env)
  assign("station_data", station_data, envir = .pkg_env)

  message("Data loaded: ",
          nrow(railroad_data), " railroad sections, ",
          nrow(station_data), " stations")

  list(
    railroad = railroad_data,
    stations = station_data
  )
}

#' Get railroad data from package cache
#'
#' @return sf object with railroad sections
#' @keywords internal
get_railroad_data <- function() {
  if (!exists("railroad_data", envir = .pkg_env)) {
    load_railway_data()
  }
  get("railroad_data", envir = .pkg_env)
}

#' Get station data from package cache
#'
#' @return sf object with stations
#' @keywords internal
get_station_data <- function() {
  if (!exists("station_data", envir = .pkg_env)) {
    load_railway_data()
  }
  get("station_data", envir = .pkg_env)
}

#' Clear cached data from memory
#'
#' @export
#' @examples
#' clear_railway_cache()
clear_railway_cache <- function() {
  if (exists("railroad_data", envir = .pkg_env)) {
    rm("railroad_data", envir = .pkg_env)
  }
  if (exists("station_data", envir = .pkg_env)) {
    rm("station_data", envir = .pkg_env)
  }
  if (exists("japan_map", envir = .pkg_env)) {
    rm("japan_map", envir = .pkg_env)
  }
  message("Railway data cache cleared")
  invisible(NULL)
}

#' Get Japan map for plotting
#'
#' @return sf object with Japan prefecture boundaries
#' @keywords internal
get_japan_map <- function() {
  if (!exists("japan_map", envir = .pkg_env)) {
    japan_map <- rnaturalearth::ne_states(country = "Japan", returnclass = "sf")
    assign("japan_map", japan_map, envir = .pkg_env)
  }
  get("japan_map", envir = .pkg_env)
}
