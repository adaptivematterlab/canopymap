.osrm_profiles <- c(driving = "car", cycling = "bike", walking = "foot")

compute_distances <- function(data, modality) {
  message("Computing ", modality, " distances...")
  result <- if (modality == "haversine") {
    compute_haversine(data)
  } else if (modality %in% names(.osrm_profiles)) {
    compute_osrm(data, .osrm_profiles[[modality]])
  } else {
    stop("Unknown modality: '", modality, "'. Use: none, haversine, driving, cycling, walking")
  }
  message("Distance matrix complete.")
  result
}

compute_haversine <- function(data) {
  coords <- as.matrix(data[, c("long", "lat")])
  dist_m <- geosphere::distm(coords, fun = geosphere::distHaversine)
  list(distance_km = dist_m / 1000, duration_min = NULL)
}

compute_osrm <- function(data, profile) {
  coords_sf <- sf::st_as_sf(
    data.frame(
      id  = as.character(seq_len(nrow(data))),
      lon = as.numeric(data$long),
      lat = as.numeric(data$lat)
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )
  options(osrm.server = "https://router.project-osrm.org/")
  options(osrm.profile = profile)

  n <- nrow(data)
  store <- new.env()
  store$distances <- matrix(NA_real_, n, n)
  store$durations <- matrix(NA_real_, n, n)
  osrm_table_fill(coords_sf, seq_len(n), seq_len(n), store)

  list(distance_km = store$distances / 1000, duration_min = store$durations)
}

# The public OSRM server caps the number of source x destination coordinates
# per table request ("TooBig" / "Too many table coordinates"). Rather than
# hard-coding that limit, request the full table and, on failure, recursively
# split the larger of the two coordinate sets in half and retry -- this also
# works unchanged against self-hosted servers with a different (or no) cap.
osrm_table_fill <- function(coords_sf, src_idx, dst_idx, store) {
  result <- tryCatch(
    osrm::osrmTable(src = coords_sf[src_idx, ], dst = coords_sf[dst_idx, ], measure = c("duration", "distance")),
    error = function(e) e
  )
  if (!inherits(result, "error")) {
    store$distances[src_idx, dst_idx] <- result$distances
    store$durations[src_idx, dst_idx] <- result$durations
    return(invisible())
  }
  if (!grepl("TooBig", conditionMessage(result), fixed = TRUE) ||
      (length(src_idx) == 1 && length(dst_idx) == 1)) {
    stop(result)
  }
  message("  OSRM table request too large; splitting into smaller batches...")
  if (length(src_idx) >= length(dst_idx) && length(src_idx) > 1) {
    mid <- length(src_idx) %/% 2
    osrm_table_fill(coords_sf, src_idx[seq_len(mid)], dst_idx, store)
    osrm_table_fill(coords_sf, src_idx[-seq_len(mid)], dst_idx, store)
  } else {
    mid <- length(dst_idx) %/% 2
    osrm_table_fill(coords_sf, src_idx, dst_idx[seq_len(mid)], store)
    osrm_table_fill(coords_sf, src_idx, dst_idx[-seq_len(mid)], store)
  }
}

find_edges <- function(dist_matrix, n, coords, max_edge_km = NULL) {
  message("Finding top ", n, " neighbors per node...")

  # Co-located rows (identical long/lat) share a "location"; neighbors are
  # found between locations, not between rows, so stacked addresses don't
  # spend all their edges on each other at distance zero.
  coord_key      <- paste(coords[, 1], coords[, 2])
  uniq_keys      <- unique(coord_key)
  representative <- match(uniq_keys, coord_key)
  loc_dist       <- dist_matrix[representative, representative, drop = FALSE]

  edges <- vector("list", length(uniq_keys) * n)
  idx <- 1
  for (li in seq_along(uniq_keys)) {
    distances <- loc_dist[li, ]
    distances[li] <- Inf
    nearest <- order(distances)[seq_len(min(n, length(distances) - 1))]
    for (lj in nearest) {
      # NA distances come from rows whose coordinates failed to geocode;
      # skip them (along with self, Inf) rather than erroring on the comparison below.
      d <- distances[lj]
      if (!is.finite(d)) next
      if (!is.null(max_edge_km) && d > max_edge_km) next
      edges[[idx]] <- tibble::tibble(from = representative[li], to = representative[lj], distance_km = d)
      idx <- idx + 1
    }
  }
  empty_edges <- tibble::tibble(from = integer(0), to = integer(0), distance_km = numeric(0))
  edges <- if (idx == 1) empty_edges else dplyr::bind_rows(edges[seq_len(idx - 1)])
  if (nrow(edges) == 0) {
    message("Found 0 unique edges.")
    return(edges)
  }
  edges <- dplyr::mutate(edges, key = paste(pmin(from, to), pmax(from, to)))
  edges <- dplyr::distinct(edges, key, .keep_all = TRUE)
  edges <- dplyr::select(edges, -key)
  message("Found ", nrow(edges), " unique edges.")
  edges
}

attach_durations <- function(edges, duration_min) {
  dplyr::rowwise(edges) |>
    dplyr::mutate(duration_min = duration_min[from, to]) |>
    dplyr::ungroup()
}

compute_route_geoms <- function(data, edges, modality) {
  if (modality != "haversine") {
    options(osrm.server = "https://router.project-osrm.org/")
    options(osrm.profile = .osrm_profiles[[modality]])
  }
  geoms <- vector("list", nrow(edges))
  for (k in seq_len(nrow(edges))) {
    i <- edges$from[k]
    j <- edges$to[k]
    message("  Route ", k, "/", nrow(edges), ": ", i, " -> ", j)
    if (modality != "haversine") {
      src_pt <- sf::st_sfc(sf::st_point(c(as.numeric(data$long[i]), as.numeric(data$lat[i]))), crs = 4326)
      dst_pt <- sf::st_sfc(sf::st_point(c(as.numeric(data$long[j]), as.numeric(data$lat[j]))), crs = 4326)
      route  <- osrm::osrmRoute(
        src = sf::st_sf(id = "src", geometry = src_pt),
        dst = sf::st_sf(id = "dst", geometry = dst_pt),
        overview = "full"
      )
      geoms[[k]] <- sf::st_geometry(route)
    } else {
      line <- sf::st_linestring(matrix(
        c(as.numeric(data$display_long[i]), as.numeric(data$display_lat[i]),
          as.numeric(data$display_long[j]), as.numeric(data$display_lat[j])),
        ncol = 2, byrow = TRUE
      ))
      geoms[[k]] <- sf::st_sfc(line, crs = 4326)
    }
  }
  geoms
}

sfc_to_geojson <- function(geoms) {
  combined <- do.call(c, lapply(geoms, function(g) {
    if (inherits(g, "sfc")) g else sf::st_sfc(g, crs = 4326)
  }))
  sf::st_crs(combined) <- 4326
  merged <- sf::st_union(combined)
  fc  <- sf::st_sf(geometry = merged)
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp))
  sf::st_write(fc, tmp, quiet = TRUE, delete_dsn = TRUE)
  paste(readLines(tmp, warn = FALSE), collapse = "\n")
}

make_palette_colors <- function(categories, palette_name) {
  unique_cats <- sort(unique(as.character(categories)))
  pal_fn      <- leaflet::colorFactor(palette_name, domain = unique_cats)
  as.list(stats::setNames(pal_fn(unique_cats), unique_cats))
}
