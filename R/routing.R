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

  dm <- osrm::osrmTable(src = coords_sf, measure = "distance")$distances / 1000
  du <- osrm::osrmTable(src = coords_sf, measure = "duration")$durations

  list(distance_km = dm, duration_min = du)
}

find_edges <- function(dist_matrix, n) {
  message("Finding top ", n, " neighbors per node...")
  edges <- vector("list", nrow(dist_matrix) * n)
  idx <- 1
  for (i in seq_len(nrow(dist_matrix))) {
    distances <- dist_matrix[i, ]
    distances[i] <- Inf
    nearest <- order(distances)[seq_len(min(n, length(distances) - 1))]
    for (j in nearest) {
      edges[[idx]] <- tibble::tibble(from = i, to = j, distance_km = dist_matrix[i, j])
      idx <- idx + 1
    }
  }
  edges <- dplyr::bind_rows(edges)
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
