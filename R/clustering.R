is_imprecise_address <- function(data, address_col) {
  if ("Geo Precision" %in% names(data)) {
    tolower(trimws(as.character(data[["Geo Precision"]]))) %in%
      c("imprecise", "city", "city centroid", "low", "approximate")
  } else {
    !grepl("\\d", sub(",.*", "", data[[address_col]]))
  }
}

#' Compute DBSCAN clusters for the Neighborhoods layer
#'
#' @param data Data frame with `lat`/`long` columns (geocoded, unjittered).
#' @param address_col Name of the column containing addresses.
#' @param category_col Name of the column to color-code markers by. `NULL` for no color-coding.
#' @param cluster_eps Neighbourhood radius in metres for the DBSCAN cluster layer.
#' @param cluster_min_pts Minimum rows required to form a cluster in the DBSCAN cluster layer.
#' @return list(clusters = list of cluster summaries, is_outlier = logical vector aligned to `data` rows)
#' @noRd
compute_clusters <- function(data, address_col, category_col,
                              cluster_eps = 500, cluster_min_pts = 3) {
  n <- nrow(data)
  # Rows with no resolved coordinate (failed geocode) can't be clustered
  # any more than imprecise ones can.
  no_coords <- is.na(data$long) | is.na(data$lat)
  imprecise <- is_imprecise_address(data, address_col) | no_coords

  if (all(imprecise)) {
    return(list(clusters = list(), is_outlier = rep(TRUE, n)))
  }

  precise_idx <- which(!imprecise)
  coords      <- data[precise_idx, c("long", "lat")]
  coord_key   <- paste(coords$long, coords$lat)
  uniq_keys   <- unique(coord_key)
  uniq_coords <- coords[match(uniq_keys, coord_key), ]
  weights     <- as.numeric(table(coord_key)[uniq_keys])

  row_cluster <- rep(NA_integer_, n)

  if (length(uniq_keys) == 1) {
    # A single unique location can't be a cluster of its own under DBSCAN's
    # distance-based definition; treat as unclustered.
    row_cluster[precise_idx] <- 0L
  } else {
    dist_m <- geosphere::distm(as.matrix(uniq_coords[, c("long", "lat")]), fun = geosphere::distHaversine)
    db <- dbscan::dbscan(stats::as.dist(dist_m), eps = cluster_eps, minPts = cluster_min_pts, weights = weights)
    row_cluster[precise_idx] <- db$cluster[match(coord_key, uniq_keys)]
  }

  is_outlier <- imprecise | (!is.na(row_cluster) & row_cluster == 0L)
  row_cluster[!is.na(row_cluster) & row_cluster == 0L] <- NA_integer_

  cluster_ids <- sort(unique(row_cluster[!is.na(row_cluster)]))
  clusters <- lapply(cluster_ids, function(cid) {
    member_rows   <- which(row_cluster == cid)
    member_coords <- data[member_rows, c("long", "lat")]
    centroid_long <- mean(member_coords$long)
    centroid_lat  <- mean(member_coords$lat)
    dists  <- geosphere::distHaversine(
      cbind(member_coords$long, member_coords$lat),
      c(centroid_long, centroid_lat)
    )
    radius <- max(500, max(dists))
    cats   <- if (!is.null(category_col)) as.character(data[[category_col]][member_rows]) else character(0)
    dominant_cat <- if (length(cats) > 0) names(sort(table(cats), decreasing = TRUE))[1] else NA_character_
    list(
      lat      = centroid_lat,
      long     = centroid_long,
      radius   = radius,
      count    = length(member_rows),
      category = dominant_cat
    )
  })

  list(clusters = clusters, is_outlier = is_outlier)
}
