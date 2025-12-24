#' Railway operator corporate colors
#'
#' A named vector mapping railway operator names (N02_004 field) to their
#' corporate colors.
#'
#' @format A named character vector where names are operator names in Japanese
#'   and values are hex color codes.
#'
#' @examples
#' operator_colors["東日本旅客鉄道"]  # JR East green
#' operator_colors["小田急電鉄"]      # Odakyu blue
#'
#' @export
operator_colors <- c(
  # JR Group
 "東日本旅客鉄道" = "#008C4A",
"西日本旅客鉄道" = "#0072BC",
  "東海旅客鉄道" = "#FF7F00",
  "北海道旅客鉄道" = "#8FC31F",
  "九州旅客鉄道" = "#E60012",
  "四国旅客鉄道" = "#00ACD1",

  # Tokyo Metro & Toei
  "東京地下鉄" = "#00A7DB",
  "東京都交通局" = "#00A968",

  # Major Private Railways (Kanto)
  "小田急電鉄" = "#0078C9",
  "京王電鉄" = "#DD0077",
  "東急電鉄" = "#E60012",
  "西武鉄道" = "#0067C0",
  "東武鉄道" = "#0057C8",
  "京浜急行電鉄" = "#E8334A",
  "京成電鉄" = "#003876",
  "相模鉄道" = "#2663AB",

  # Major Private Railways (Kansai)
  "阪急電鉄" = "#8F1830",
  "阪神電気鉄道" = "#FF6600",
  "近畿日本鉄道" = "#B50040",
  "南海電気鉄道" = "#003399",
  "京阪電気鉄道" = "#66A823",

  # Major Private Railways (Other regions)
  "名古屋鉄道" = "#E60012",
  "西日本鉄道" = "#00A0E9",
  "広島電鉄" = "#E60012",

  # Third Sector / Other
  "北総鉄道" = "#0077B8",
  "つくばエクスプレス" = "#0066CC",
  "りんかい線" = "#00AEEF",
  "ゆりかもめ" = "#00A0E9",
  "多摩都市モノレール" = "#FF6600",
  "箱根登山鉄道" = "#E60012"
)

#' Get color for a railway operator
#'
#' Returns the corporate color for a given railway operator. If the operator
#' is not found in the color mapping, returns the fallback color.
#'
#' @param operator Operator name (N02_004 field value)
#' @param fallback Fallback color if operator not found (default: "#666666")
#'
#' @return A character string with the hex color code
#' @export
#'
#' @examples
#' get_operator_color("東日本旅客鉄道")
#' get_operator_color("Unknown Operator")
#' get_operator_color("Unknown Operator", fallback = "#FF0000")
get_operator_color <- function(operator, fallback = "#666666") {
  if (is.null(operator) || length(operator) == 0) {
    return(fallback)
  }
  # If multiple operators, try to find a color for any of them
  for (op in operator) {
    if (!is.na(op)) {
      color <- operator_colors[op]
      if (!is.na(color)) {
        return(unname(color))
      }
    }
  }
  fallback
}

#' List all available operator colors
#'
#' @return A data frame with operator names and their colors
#' @export
#'
#' @examples
#' list_operator_colors()
list_operator_colors <- function() {
  data.frame(
    operator = names(operator_colors),
    color = unname(operator_colors),
    stringsAsFactors = FALSE
  )
}

#' Default colors for routes when operator color is not available
#'
#' @keywords internal
default_route_colors <- c(
  "#E60012",
  "#00A650",
  "#0066CC",
  "#FF7F00",
  "#9B59B6",
  "#1ABC9C",
  "#E74C3C",
  "#3498DB",
  "#F39C12",
  "#2ECC71"
)

#' Get a default color by index
#'
#' @param index Color index (will be recycled if > 10)
#' @return Hex color code
#' @keywords internal
get_default_color <- function(index) {
  default_route_colors[((index - 1) %% length(default_route_colors)) + 1]
}
