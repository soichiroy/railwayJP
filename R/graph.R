#' Round coordinates to create snap grid (for matching endpoints)
#'
#' @param x Coordinate value
#' @param precision Number of decimal places
#' @return Rounded coordinate
#' @keywords internal
snap_coord <- function(x, precision = 5) {
  round(x, precision)
}

#' Build track connectivity graph
#'
#' Builds a graph representation of the railway network where nodes are
#' section endpoints and edges are track sections. This allows for efficient
#' pathfinding between stations.
#'
#' @param line_railroad sf object with railway sections
#' @param precision Decimal places for coordinate snapping (default: 5 = ~1m)
#'
#' @return A list with:
#'   \item{endpoints}{List of start/end coordinates for each section}
#'   \item{node_to_sections}{Mapping from coordinate keys to section indices}
#'   \item{section_neighbors}{Adjacency list for sections}
#'   \item{n_sections}{Number of sections}
#'
#' @keywords internal
build_track_graph <- function(line_railroad, precision = 5) {
  n_sections <- nrow(line_railroad)

  # Extract endpoints for each section
  endpoints <- lapply(seq_len(n_sections), function(i) {
    coords <- sf::st_coordinates(line_railroad$geometry[i])
    list(
      start = c(
        snap_coord(coords[1, "X"], precision),
        snap_coord(coords[1, "Y"], precision)
      ),
      end = c(
        snap_coord(coords[nrow(coords), "X"], precision),
        snap_coord(coords[nrow(coords), "Y"], precision)
      )
    )
  })

  # Create node keys from coordinates
  get_key <- function(coord) paste(coord[1], coord[2], sep = ",")

  # Map nodes to sections
  node_to_sections <- list()

  for (i in seq_len(n_sections)) {
    start_key <- get_key(endpoints[[i]]$start)
    end_key <- get_key(endpoints[[i]]$end)

    if (is.null(node_to_sections[[start_key]])) {
      node_to_sections[[start_key]] <- c()
    }
    if (is.null(node_to_sections[[end_key]])) {
      node_to_sections[[end_key]] <- c()
    }

    node_to_sections[[start_key]] <- c(node_to_sections[[start_key]], i)
    node_to_sections[[end_key]] <- c(node_to_sections[[end_key]], i)
  }

  # Build section adjacency (which sections connect to which)
  section_neighbors <- lapply(seq_len(n_sections), function(i) {
    start_key <- get_key(endpoints[[i]]$start)
    end_key <- get_key(endpoints[[i]]$end)

    neighbors <- unique(c(
      node_to_sections[[start_key]],
      node_to_sections[[end_key]]
    ))
    neighbors[neighbors != i] # Exclude self
  })

  list(
    endpoints = endpoints,
    node_to_sections = node_to_sections,
    section_neighbors = section_neighbors,
    n_sections = n_sections
  )
}

#' Find sections near a station
#'
#' @param line_railroad sf object with railway sections
#' @param station_geom sf geometry of the station
#' @param max_dist_m Maximum distance in meters
#'
#' @return Vector of section indices
#' @keywords internal
find_sections_near_station <- function(line_railroad, station_geom, max_dist_m = 500) {
  # Transform to projected CRS (JGD2011 / Japan Plane Rectangular CS XIV)
  proj_crs <- 6690
  rail_proj <- sf::st_transform(line_railroad, proj_crs)
  sta_proj <- sf::st_transform(station_geom, proj_crs)

  distances <- as.numeric(sf::st_distance(rail_proj, sta_proj))
  which(distances <= max_dist_m)
}

#' Find path between two stations using BFS on track sections
#'
#' @param graph Track graph from build_track_graph
#' @param line_railroad sf object with railway sections
#' @param line_stations sf object with stations
#' @param start_station_name Start station name
#' @param end_station_name End station name
#' @param max_dist_m Max distance to consider station near a section
#'
#' @return Vector of section indices forming the path, or NULL if no path found
#' @keywords internal
find_section_path <- function(
  graph,
  line_railroad,
  line_stations,
  start_station_name,
  end_station_name,
  max_dist_m = 500
) {
  # Find stations
  start_sta <- line_stations[line_stations$N02_005 == start_station_name, ]
  end_sta <- line_stations[line_stations$N02_005 == end_station_name, ]

  if (nrow(start_sta) == 0) {
    stop(paste("Start station not found:", start_station_name))
  }
  if (nrow(end_sta) == 0) {
    stop(paste("End station not found:", end_station_name))
  }

  # Find sections near start and end stations
  start_sections <- find_sections_near_station(
    line_railroad,
    sf::st_union(start_sta),
    max_dist_m
  )
  end_sections <- find_sections_near_station(
    line_railroad,
    sf::st_union(end_sta),
    max_dist_m
  )

  if (length(start_sections) == 0) {
    stop(paste("No sections near start station:", start_station_name))
  }
  if (length(end_sections) == 0) {
    stop(paste("No sections near end station:", end_station_name))
  }

  # BFS from start sections to end sections
  n_sections <- graph$n_sections
  visited <- rep(FALSE, n_sections)
  parent <- rep(NA, n_sections)
  queue <- list()

  for (s in start_sections) {
    queue <- c(queue, list(s))
    visited[s] <- TRUE
  }

  found_end <- NA
  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]

    if (current %in% end_sections) {
      found_end <- current
      break
    }

    for (neighbor in graph$section_neighbors[[current]]) {
      if (!visited[neighbor]) {
        visited[neighbor] <- TRUE
        parent[neighbor] <- current
        queue <- c(queue, list(neighbor))
      }
    }
  }

  if (is.na(found_end)) {
    return(NULL)
  }

  # Reconstruct path
  path <- c(found_end)
  current <- found_end
  while (!is.na(parent[current])) {
    current <- parent[current]
    path <- c(current, path)
  }

  path
}

#' Get stations along a path of sections
#'
#' @param line_railroad sf object with railway sections
#' @param line_stations sf object with stations
#' @param section_indices Indices of sections in the path
#' @param max_dist_m Maximum distance to consider a station on the path
#'
#' @return sf object with stations along the path
#' @keywords internal
get_stations_along_path <- function(
  line_railroad,
  line_stations,
  section_indices,
  max_dist_m = 300
) {
  if (length(section_indices) == 0) {
    return(NULL)
  }

  # Get the path geometry
  path_sections <- line_railroad[section_indices, ]
  path_union <- sf::st_union(path_sections)

  # Transform to projected CRS
  proj_crs <- 6690
  path_proj <- sf::st_transform(path_union, proj_crs)
  sta_proj <- sf::st_transform(line_stations, proj_crs)

  # Find stations within distance of path
  distances <- as.numeric(sf::st_distance(sta_proj, path_proj))
  near_idx <- which(distances <= max_dist_m)

  if (length(near_idx) == 0) {
    return(NULL)
  }

  # Return unique stations
  result <- line_stations[near_idx, ]
  result <- result[!duplicated(result$N02_005), ]
  result
}

#' Get route sections between two stations
#'
#' Finds the railway sections connecting two stations, optionally passing
#' through intermediate stations (via points).
#'
#' @param from_station Start station name
#' @param to_station End station name
#' @param via Character vector of intermediate station names (optional)
#' @param use_railroad sf object with railway sections (if NULL, uses package data)
#' @param use_stations sf object with stations (if NULL, uses package data)
#' @param verbose If TRUE, print progress messages
#'
#' @return A list with:
#'   \item{sections}{sf object with the railway sections}
#'   \item{stations}{sf object with stations along the route}
#'   \item{route}{Character vector with station names in order}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple route
#' route <- get_route_sections("東京", "横浜")
#'
#' # Route with intermediate stops (via points)
#' route <- get_route_sections("新宿", "長野", via = c("松本"))
#' }

#' Get route between stations
#'
#' Finds railway routes for one or more segments, with optional filtering by
#' line name or operator. This is useful for verifying routes before plotting.
#' Uses the same segment specification as `plot_railway_segments()`.
#'
#' @param segments A list of route specifications. Each element should be a list with:
#'   \describe{
#'     \item{start}{Start station name (required)}
#'     \item{end}{End station name (required)}
#'     \item{via}{Character vector of intermediate station names (optional)}
#'     \item{line}{Line name(s) to filter by (optional). Can be a single name or
#'       a character vector for routes spanning multiple lines.
#'       Use `search_lines()` to find names.}
#'     \item{operator}{Operator name(s) to filter by (optional). Can be a single
#'       name or a character vector for routes spanning multiple operators
#'       (e.g., `c("四国旅客鉄道", "西日本旅客鉄道")` for JR Shikoku + JR West).}
#'     \item{label}{Label for this segment (optional, for display)}
#'   }
#' @param verbose If TRUE (default), print route details
#'
#' @return A list of route results, one per segment. Each result contains:
#'   \item{label}{Segment label}
#'   \item{start}{Start station name}
#'   \item{end}{End station name}
#'   \item{via}{Intermediate stations (if specified)}
#'   \item{line}{Line filter (if specified)}
#'   \item{operator}{Operator filter (if specified)}
#'   \item{stations}{Character vector of station names along the route, in order}
#'   \item{n_sections}{Number of track sections in the route}
#'   \item{n_stations}{Number of stations along the route}
#'   \item{sections_sf}{sf object with track sections (for advanced use)}
#'   \item{stations_sf}{sf object with stations (for advanced use)}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Single segment
#' get_route(list(
#'   list(start = "東京", end = "横浜")
#' ))
#'
#' # Multiple segments - compare conventional vs Shinkansen
#' routes <- get_route(list(
#'   list(start = "三島", end = "京都", line = "東海道線", label = "従来線"),
#'   list(start = "三島", end = "京都", line = "東海道新幹線", label = "新幹線")
#' ))
#'
#' # Check station counts
#' routes[[1]]$n_stations
#' routes[[2]]$n_stations
#'
#' # Route with via points
#' get_route(list(
#'   list(start = "新宿", end = "長野", via = "松本", operator = "東日本旅客鉄道")
#' ))
#'
#' # Route spanning multiple operators (JR Shikoku + JR West)
#' get_route(list(
#'   list(
#'     start = "松山",
#'     end = "岡山",
#'     operator = c("四国旅客鉄道", "西日本旅客鉄道")
#'   )
#' ))
#' }
get_route <- function(segments, verbose = TRUE) {
  railroad_data <- get_railroad_data()
  station_data <- get_station_data()

  results <- list()

  for (i in seq_along(segments)) {
    seg <- segments[[i]]

    # Validate required fields
    if (is.null(seg$start) || is.null(seg$end)) {
      stop("Segment ", i, " must have 'start' and 'end' fields")
    }

    # Create label
    seg_label <- if (!is.null(seg$label)) {
      seg$label
    } else if (!is.null(seg$line)) {
      paste0(seg$line, " (", seg$start, " -> ", seg$end, ")")
    } else {
      paste0(seg$start, " -> ", seg$end)
    }

    if (verbose) {
      message("\n[", i, "] ", seg_label)
    }

    # Apply filters
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
      warning("Segment ", i, ": No railway data found for specified filters. ",
              "Use search_lines() to find valid line names.")
      results[[i]] <- NULL
      next
    }

    if (verbose) {
      filter_desc <- if (!is.null(seg$line)) {
        paste(seg$line, collapse = ", ")
      } else {
        "All lines"
      }
      if (!is.null(seg$operator)) {
        filter_desc <- paste0(filter_desc, " (", paste(seg$operator, collapse = ", "), ")")
      }
      message("  Filter: ", filter_desc)
      message("  Available: ", nrow(use_railroad), " sections, ",
              nrow(use_stations), " stations")
    }

    # Get route
    route_result <- get_route_sections(
      from_station = seg$start,
      to_station = seg$end,
      via = seg$via,
      use_railroad = use_railroad,
      use_stations = use_stations,
      verbose = verbose
    )

    if (is.null(route_result)) {
      warning("Segment ", i, ": Could not find route from ",
              seg$start, " to ", seg$end)
      results[[i]] <- NULL
      next
    }

    # Store result
    results[[i]] <- list(
      label = seg_label,
      start = seg$start,
      end = seg$end,
      via = seg$via,
      line = seg$line,
      operator = seg$operator,
      stations = route_result$route,
      n_sections = nrow(route_result$sections),
      n_stations = length(route_result$route),
      sections_sf = route_result$sections,
      stations_sf = route_result$stations
    )

    if (verbose) {
      message("  Result: ", results[[i]]$n_sections, " sections, ",
              results[[i]]$n_stations, " stations")
    }
  }

  # Name results by label
  names(results) <- sapply(results, function(r) {
    if (is.null(r)) NA else r$label
  })

  results
}

get_route_sections <- function(
  from_station,
  to_station,
  via = NULL,
  use_railroad = NULL,
  use_stations = NULL,
  verbose = TRUE
) {
  # Use provided data or fall back to package data
  if (is.null(use_railroad)) {
    use_railroad <- get_railroad_data()
  }
  if (is.null(use_stations)) {
    use_stations <- get_station_data()
  }

  if (verbose) message("  Building track graph...")
  graph <- build_track_graph(use_railroad)
  if (verbose) {
    message(paste(
      "    Nodes:",
      length(graph$node_to_sections),
      "Sections:",
      graph$n_sections
    ))
  }

  # Build waypoints list: start -> via points -> end
  waypoints <- c(from_station, via, to_station)
  if (verbose) message(paste("  Waypoints:", paste(waypoints, collapse = " -> ")))

  # Find path for each consecutive pair of waypoints
  all_path_sections <- c()

  for (i in seq_len(length(waypoints) - 1)) {
    wp_from <- waypoints[i]
    wp_to <- waypoints[i + 1]

    if (verbose) message(paste("  Finding path from", wp_from, "to", wp_to, "..."))
    path_sections <- find_section_path(
      graph,
      use_railroad,
      use_stations,
      wp_from,
      wp_to
    )

    if (is.null(path_sections)) {
      warning(paste("No path found between", wp_from, "and", wp_to))
      return(NULL)
    }

    if (verbose) message(paste("    Found", length(path_sections), "sections"))
    all_path_sections <- unique(c(all_path_sections, path_sections))
  }

  if (verbose) message(paste("  Total path sections:", length(all_path_sections)))

  # Get sections and stations
  filtered_sections <- use_railroad[all_path_sections, ]
  filtered_stations <- get_stations_along_path(
    use_railroad,
    use_stations,
    all_path_sections
  )

  # Order stations along the path (approximately by distance from start)
  if (!is.null(filtered_stations) && nrow(filtered_stations) > 0) {
    start_sta <- use_stations[use_stations$N02_005 == from_station, ]
    if (nrow(start_sta) > 0) {
      proj_crs <- 6690
      sta_proj <- sf::st_transform(filtered_stations, proj_crs)
      start_proj <- sf::st_transform(sf::st_union(start_sta), proj_crs)
      distances <- as.numeric(sf::st_distance(sta_proj, start_proj))
      filtered_stations <- filtered_stations[order(distances), ]
    }

    # Truncate at the end station to avoid stations beyond destination
    route_names <- filtered_stations$N02_005
    end_idx <- which(route_names == to_station)
    if (length(end_idx) > 0) {
      end_idx <- end_idx[1]  # Take first occurrence
      route_names <- route_names[1:end_idx]
      filtered_stations <- filtered_stations[1:end_idx, ]
    }
  } else {
    route_names <- waypoints
  }

  if (verbose) message(paste("  Route:", paste(route_names, collapse = " -> ")))

  list(
    sections = filtered_sections,
    stations = filtered_stations,
    route = route_names
  )
}
