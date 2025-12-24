#' Get line aliases data
#'
#' Returns the mapping of marketing/service line names to infrastructure line names.
#'
#' @return A named list where keys are marketing line names and values are
#'   character vectors of corresponding infrastructure line names.
#' @export
#'
#' @examples
#' aliases <- get_line_aliases()
#' aliases[["瀬戸大橋線"]]  # Returns c("宇野線", "本四備讃線", "予讃線")
get_line_aliases <- function() {
  if (!exists("line_aliases", envir = .pkg_env)) {
    alias_file <- system.file("extdata", "line-aliases.json", package = "railwayJP")
    if (alias_file == "") {
      warning("Line aliases file not found")
      return(list())
    }
    alias_data <- jsonlite::fromJSON(alias_file)
    assign("line_aliases", alias_data$aliases, envir = .pkg_env)
  }
  get("line_aliases", envir = .pkg_env)
}

#' Expand line filter using aliases
#'
#' Converts marketing/service line names (e.g., "瀬戸大橋線") to the actual
#' infrastructure line names used in MLIT data (e.g., "宇野線", "本四備讃線", "予讃線").
#'
#' @param lines Character vector of line names to expand
#' @return Character vector with all matching infrastructure line names
#' @export
#'
#' @details
#' Many JR lines have marketing names that differ from their official infrastructure
#' names in the MLIT data. For example:
#' \itemize{
#'   \item 瀬戸大橋線 -> 宇野線 + 本四備讃線 + 予讃線
#'   \item JR京都線 -> 東海道線
#'   \item JR神戸線 -> 東海道線 + 山陽線
#' }
#'
#' This function allows users to use either marketing names or infrastructure names.
#' If a name is not found in the aliases, it is passed through unchanged.
#'
#' @examples
#' \dontrun{
#' # Single alias
#' expand_line_filter("瀬戸大橋線")
#' # Returns: c("宇野線", "本四備讃線", "予讃線")
#'
#' # Mixed input
#' expand_line_filter(c("瀬戸大橋線", "山陽線"))
#' # Returns: c("宇野線", "本四備讃線", "予讃線", "山陽線")
#'
#' # Non-alias passes through
#' expand_line_filter("東海道新幹線")
#' # Returns: "東海道新幹線"
#' }
expand_line_filter <- function(lines) {
  if (is.null(lines) || length(lines) == 0) {
    return(lines)
  }

  aliases <- get_line_aliases()
  expanded <- character(0)

  for (line in lines) {
    if (line %in% names(aliases)) {
      # Expand alias to infrastructure lines
      expanded <- c(expanded, aliases[[line]])
    } else {
      # Pass through unchanged
      expanded <- c(expanded, line)
    }
  }

  unique(expanded)
}

#' List available marketing line names
#'
#' Returns all available marketing line name aliases that can be used with
#' `get_route()` and `plot_railway_segments()`.
#'
#' @return A data frame with columns:
#'   \item{marketing_name}{The marketing/service line name}
#'   \item{infrastructure_lines}{The corresponding infrastructure line names (comma-separated)}
#' @export
#'
#' @examples
#' list_line_aliases()
list_line_aliases <- function() {
  aliases <- get_line_aliases()
  data.frame(
    marketing_name = names(aliases),
    infrastructure_lines = sapply(aliases, paste, collapse = ", "),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
