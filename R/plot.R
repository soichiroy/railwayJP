#' Plot railway segments on a map
#'
#' Creates a map showing one or more railway routes across Japan. Each segment
#' can specify start/end stations, intermediate stops (via points), and optional
#' filtering by line name or operator.
#'
#' @param segments A list of route specifications. Each element should be a list with:
#'   \describe{
#'     \item{start}{Start station name (required)}
#'     \item{end}{End station name (required)}
#'     \item{via}{Character vector of intermediate station names (optional)}
#'     \item{line}{Line name(s) to filter by (optional). Can be a character vector
#'       for routes spanning multiple lines.}
#'     \item{operator}{Operator name(s) to filter by (optional). Can be a character
#'       vector for routes spanning multiple operators
#'       (e.g., `c("四国旅客鉄道", "西日本旅客鉄道")`).}
#'     \item{color}{Custom color for this route (optional, uses operator color if available)}
#'     \item{label}{Label for legend (optional)}
#'   }
#' @param output_file Output filename (default: "railway_map.png")
#' @param title Plot title
#' @param subtitle Plot subtitle (if NULL, auto-generated from routes)
#' @param show_all_stations If TRUE, show all stations along routes
#' @param key_stations Character vector of station names to highlight with labels
#' @param show_legend If TRUE, show route legend
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param dpi Output resolution (default: 300)
#' @param padding_x Horizontal padding around routes (in degrees)
#' @param padding_y Vertical padding around routes (in degrees)
#'
#' @return A ggplot2 object (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' # Single route
#' plot_railway_segments(
#'   segments = list(
#'     list(start = "東京", end = "横浜", label = "東海道線")
#'   ),
#'   output_file = "tokyo_yokohama.png"
#' )
#'
#' # Multiple routes with via points
#' plot_railway_segments(
#'   segments = list(
#'     list(
#'       start = "新宿",
#'       end = "長野",
#'       via = c("松本"),
#'       operator = "東日本旅客鉄道",
#'       label = "中央線"
#'     ),
#'     list(
#'       start = "東京",
#'       end = "大阪",
#'       via = c("名古屋"),
#'       label = "東海道新幹線"
#'     )
#'   ),
#'   title = "Railway Routes",
#'   key_stations = c("新宿", "松本", "長野", "東京", "名古屋", "大阪")
#' )
#' }
plot_railway_segments <- function(
  segments,
  output_file = "railway_map.png",
  title = "Railway Routes",
  subtitle = NULL,
  show_all_stations = TRUE,
  key_stations = NULL,
  show_legend = TRUE,
  width = 14,
  height = 10,
  dpi = 300,
  padding_x = 0.5,
  padding_y = 0.3
) {
  # Load data
  railroad_data <- get_railroad_data()
  station_data <- get_station_data()
  japan_map <- get_japan_map()

  all_segments <- list()
  all_stations <- list()
  all_routes <- list()
  segment_info <- data.frame()

  for (i in seq_along(segments)) {
    seg <- segments[[i]]
    line_desc <- if (!is.null(seg$line)) seg$line else "All lines"
    message(paste(
      "\nProcessing segment",
      i,
      ":",
      line_desc,
      seg$start,
      "->",
      seg$end
    ))

    # Get line data (optional filtering by line and operator)
    use_railroad <- railroad_data
    use_stations <- station_data

    if (!is.null(seg$line)) {
      use_railroad <- use_railroad[use_railroad$N02_003 %in% seg$line, ]
      use_stations <- use_stations[use_stations$N02_003 %in% seg$line, ]
    }

    if (!is.null(seg$operator)) {
      use_railroad <- use_railroad[use_railroad$N02_004 %in% seg$operator, ]
      use_stations <- use_stations[use_stations$N02_004 %in% seg$operator, ]
    }

    if (nrow(use_railroad) == 0) {
      warning("No railway data found for specified filters")
      next
    }

    message(paste(
      "  Using",
      nrow(use_railroad),
      "sections,",
      nrow(use_stations),
      "station records"
    ))

    # Get route (with optional via points)
    route_result <- get_route_sections(
      seg$start,
      seg$end,
      seg$via,
      use_railroad,
      use_stations
    )

    if (is.null(route_result)) {
      warning(paste("Could not find route:", seg$start, "->", seg$end))
      next
    }

    # Add segment info
    seg_label <- if (!is.null(seg$label)) {
      seg$label
    } else if (!is.null(seg$line)) {
      paste0(seg$line, " (", seg$start, "->", seg$end, ")")
    } else {
      paste0(seg$start, "->", seg$end)
    }

    # Determine color: user-specified > operator color > default
    seg_color <- if (!is.null(seg$color)) {
      seg$color
    } else if (!is.null(seg$operator)) {
      get_operator_color(seg$operator, get_default_color(i))
    } else {
      # Try to get operator from route result
      route_operators <- unique(route_result$sections$N02_004)
      if (length(route_operators) == 1) {
        get_operator_color(route_operators[1], get_default_color(i))
      } else {
        get_default_color(i)
      }
    }

    route_result$sections$segment_id <- i
    route_result$sections$segment_label <- seg_label

    if (!is.null(route_result$stations)) {
      route_result$stations$segment_id <- i
      route_result$stations$segment_label <- seg_label
    }

    all_segments[[i]] <- route_result$sections
    all_stations[[i]] <- route_result$stations
    all_routes[[i]] <- route_result$route

    segment_info <- rbind(
      segment_info,
      data.frame(
        segment_id = i,
        label = seg_label,
        color = seg_color,
        stringsAsFactors = FALSE
      )
    )

    message(paste(
      "  Result:",
      nrow(route_result$sections),
      "sections,",
      ifelse(is.null(route_result$stations), 0, nrow(route_result$stations)),
      "stations"
    ))
  }

  if (length(all_segments) == 0) {
    stop("No valid segments found")
  }

  # Combine
  combined_railroad <- do.call(rbind, all_segments)
  combined_stations <- do.call(rbind, Filter(Negate(is.null), all_stations))

  # Bounding box
  bbox <- sf::st_bbox(combined_railroad)
  xlim <- c(
    as.numeric(bbox["xmin"]) - padding_x,
    as.numeric(bbox["xmax"]) + padding_x
  )
  ylim <- c(
    as.numeric(bbox["ymin"]) - padding_y,
    as.numeric(bbox["ymax"]) + padding_y
  )

  # Prefecture labels
  pref_centroids <- suppressWarnings(sf::st_centroid(japan_map))
  pref_coords <- sf::st_coordinates(pref_centroids)
  pref_centroids$lon <- pref_coords[, 1]
  pref_centroids$lat <- pref_coords[, 2]
  pref_centroids <- pref_centroids[
    pref_centroids$lon >= xlim[1] &
    pref_centroids$lon <= xlim[2] &
    pref_centroids$lat >= ylim[1] &
    pref_centroids$lat <= ylim[2],
  ]

  color_mapping <- stats::setNames(segment_info$color, segment_info$label)

  # Highlight stations
  if (!is.null(key_stations) && !is.null(combined_stations)) {
    highlight_stations <- combined_stations[combined_stations$N02_005 %in% key_stations, ]
    highlight_stations <- highlight_stations[!duplicated(highlight_stations$N02_005), ]
  } else if (!is.null(combined_stations)) {
    key_station_names <- unique(unlist(lapply(segments, function(s) {
      c(s$start, s$end)
    })))
    highlight_stations <- combined_stations[combined_stations$N02_005 %in% key_station_names, ]
    highlight_stations <- highlight_stations[!duplicated(highlight_stations$N02_005), ]
  } else {
    highlight_stations <- NULL
  }

  if (is.null(subtitle)) {
    subtitle <- paste(
      sapply(segments, function(s) paste(s$start, "->", s$end)),
      collapse = " / "
    )
  }

  # Build plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = japan_map,
      fill = "#f0f4e8",
      color = "#888888",
      linewidth = 0.4
    ) +
    ggplot2::geom_sf_text(
      data = pref_centroids,
      ggplot2::aes(label = .data$name_ja),
      size = 3.5,
      color = "#666666",
      fontface = "bold",
      alpha = 0.7
    ) +
    ggplot2::geom_sf(
      data = combined_railroad,
      ggplot2::aes(color = .data$segment_label),
      linewidth = 1.5
    ) +
    ggplot2::scale_color_manual(values = color_mapping, name = "Route")

  if (show_all_stations && !is.null(combined_stations)) {
    p <- p + ggplot2::geom_sf(data = combined_stations, color = "#333333", size = 1.2)
  }

  if (!is.null(highlight_stations) && nrow(highlight_stations) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = highlight_stations,
        color = "#000000",
        fill = "#FFFFFF",
        shape = 21,
        size = 3,
        stroke = 1.5
      ) +
      ggplot2::geom_sf_label(
        data = highlight_stations,
        ggplot2::aes(label = .data$N02_005),
        size = 3.3,
        nudge_y = 0.04,
        label.padding = ggplot2::unit(0.15, "lines"),
        alpha = 0.9
      )
  }

  p <- p +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = "Data: National Land Numerical Information (Railway Data), MLIT"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5),
      plot.caption = ggplot2::element_text(size = 8, color = "gray50"),
      panel.grid = ggplot2::element_line(color = "gray90"),
      axis.title = ggplot2::element_blank(),
      legend.position = if (show_legend) "bottom" else "none"
    )

  suppressWarnings(
    ggplot2::ggsave(output_file, p, width = width, height = height, dpi = dpi)
  )
  message(paste("\nMap saved to", output_file))

  attr(p, "routes") <- all_routes
  invisible(p)
}
