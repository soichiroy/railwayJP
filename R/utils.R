#' List available railway lines
#'
#' Returns a data frame of all railway lines in the dataset.
#'
#' @param pattern Optional regex pattern to filter line names
#'
#' @return A data frame with columns N02_003 (line name) and N02_004 (operator)
#' @export
#'
#' @examples
#' \dontrun{
#' # List all lines
#' list_lines()
#'
#' # Filter for lines containing "中央"
#' list_lines("中央")
#' }
list_lines <- function(pattern = NULL) {
  railroad_data <- get_railroad_data()

  lines <- sf::st_drop_geometry(railroad_data)
  lines <- unique(lines[, c("N02_003", "N02_004")])
  lines <- lines[order(lines$N02_003), ]

  if (!is.null(pattern)) {
    lines <- lines[grepl(pattern, lines$N02_003), ]
  }

  lines
}

#' List stations on a line
#'
#' Returns a data frame of all stations on a specific railway line.
#'
#' @param line_name Line name (N02_003 field)
#' @param operator Optional operator name (N02_004 field) for disambiguation
#'
#' @return A data frame with station information
#' @export
#'
#' @examples
#' \dontrun{
#' # List all stations on the Chuo Line
#' list_stations("中央線")
#'
#' # List stations on JR East's Chuo Line
#' list_stations("中央線", "東日本旅客鉄道")
#' }
list_stations <- function(line_name, operator = NULL) {
  station_data <- get_station_data()

  stations <- sf::st_drop_geometry(station_data)
  stations <- stations[stations$N02_003 == line_name, ]

  if (!is.null(operator)) {
    stations <- stations[stations$N02_004 == operator, ]
  }

  stations <- unique(stations[, c("N02_005", "N02_004", "N02_005c")])
  stations <- stations[order(stations$N02_005c), ]

  stations
}

#' Search for stations by name
#'
#' @param pattern Regex pattern to search for in station names
#'
#' @return A data frame with matching stations
#' @export
#'
#' @examples
#' \dontrun{
#' # Find all stations with "新宿" in the name
#' search_stations("新宿")
#' }
search_stations <- function(pattern) {
  station_data <- get_station_data()

  stations <- sf::st_drop_geometry(station_data)
  stations <- stations[grepl(pattern, stations$N02_005), ]
  stations <- unique(stations[, c("N02_005", "N02_003", "N02_004")])
  stations <- stations[order(stations$N02_005), ]

  stations
}

#' Search for railway lines by name
#'
#' Searches for railway lines matching a pattern. Useful for finding the
#' exact line name to use in plot_railway_segments().
#'
#' @param pattern Regex pattern to search for in line names
#' @param ignore_case If TRUE (default), search is case-insensitive
#'
#' @return A data frame with columns:
#'   \item{line}{Line name (N02_003)}
#'   \item{operator}{Operator name (N02_004)}
#'   \item{n_sections}{Number of track sections}
#'   \item{n_stations}{Number of stations}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Find lines containing "東海道"
#' search_lines("東海道")
#'
#' # Find all Shinkansen lines
#' search_lines("新幹線")
#'
#' # Find lines with "中央" (Chuo)
#' search_lines("中央")
#' }
search_lines <- function(pattern, ignore_case = TRUE) {
  railroad_data <- get_railroad_data()
  station_data <- get_station_data()

  # Get unique lines
  rail_df <- sf::st_drop_geometry(railroad_data)
  sta_df <- sf::st_drop_geometry(station_data)

  # Count sections per line/operator
  section_counts <- as.data.frame(
    table(rail_df$N02_003, rail_df$N02_004),
    stringsAsFactors = FALSE
  )
  names(section_counts) <- c("line", "operator", "n_sections")
  section_counts <- section_counts[section_counts$n_sections > 0, ]

  # Count stations per line/operator
  sta_unique <- unique(sta_df[, c("N02_003", "N02_004", "N02_005")])
  station_counts <- as.data.frame(
    table(sta_unique$N02_003, sta_unique$N02_004),
    stringsAsFactors = FALSE
  )
  names(station_counts) <- c("line", "operator", "n_stations")
  station_counts <- station_counts[station_counts$n_stations > 0, ]

  # Merge
  result <- merge(section_counts, station_counts,
    by = c("line", "operator"), all.x = TRUE
  )
  result$n_stations[is.na(result$n_stations)] <- 0

  # Filter by pattern
  result <- result[grepl(pattern, result$line, ignore.case = ignore_case), ]

  # Sort by line name, then operator

  result <- result[order(result$line, result$operator), ]
  rownames(result) <- NULL

  result
}

#' List all operators
#'
#' @return A data frame with operator names and number of lines/stations
#' @export
#'
#' @examples
#' \dontrun{
#' list_operators()
#' }
list_operators <- function() {
  railroad_data <- get_railroad_data()
  station_data <- get_station_data()

  rail_ops <- sf::st_drop_geometry(railroad_data)
  rail_counts <- as.data.frame(table(rail_ops$N02_004))
  names(rail_counts) <- c("operator", "n_sections")

  sta_ops <- sf::st_drop_geometry(station_data)
  sta_counts <- as.data.frame(table(sta_ops$N02_004))
  names(sta_counts) <- c("operator", "n_stations")

  result <- merge(rail_counts, sta_counts, by = "operator", all = TRUE)
  result <- result[order(-result$n_sections), ]
  result
}

#' Get data info
#'
#' Returns information about the loaded railway data.
#'
#' @return A list with data statistics
#' @export
#'
#' @examples
#' \dontrun{
#' get_data_info()
#' }
get_data_info <- function() {
  railroad_data <- get_railroad_data()
  station_data <- get_station_data()

  list(
    n_railroad_sections = nrow(railroad_data),
    n_stations = nrow(station_data),
    n_lines = length(unique(railroad_data$N02_003)),
    n_operators = length(unique(railroad_data$N02_004)),
    crs = sf::st_crs(railroad_data)$epsg,
    data_year = "2022",
    source = "National Land Numerical Information (Railway Data), MLIT"
  )
}
