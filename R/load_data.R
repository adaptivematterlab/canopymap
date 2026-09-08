load_org_data <- function(data_source, source_type, address_col, limit = NULL) {
  if (is.character(data_source)) {
    if (source_type == "public") {
      message("Loading public data from Google Sheets...")
      googlesheets4::gs4_deauth()
      data <- googlesheets4::read_sheet(data_source)
    } else if (source_type == "private") {
      message("Loading private data from Google Sheets...")
      googlesheets4::gs4_auth()
      data <- googlesheets4::read_sheet(data_source)
    }
  } else {
    data <- data_source
  }
  message("Loaded ", nrow(data), " rows.")

  if (!is.null(limit)) {
    message("Limiting to first ", limit, " of ", nrow(data), " rows (testing).")
    data <- utils::head(data, limit)
  }

  if (all(c("Latitude", "Longitude") %in% names(data))) {
    message("Latitude/Longitude columns found; skipping geocoding.")
    data$lat  <- data$Latitude
    data$long <- data$Longitude
    data <- dplyr::mutate(data,
      display_lat  = lat  + stats::rnorm(dplyr::n(), 0, 0.001),
      display_long = long + stats::rnorm(dplyr::n(), 0, 0.001)
    )
  } else {
    message("Geocoding addresses...")
    data <- geocode_addresses(data, address_col)
  }
  data
}

.geocode_cache_path <- file.path("data", "geocode-cache.rds")

read_geocode_cache <- function(path = .geocode_cache_path) {
  if (file.exists(path)) readRDS(path) else list()
}

write_geocode_cache <- function(cache, path = .geocode_cache_path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(cache, path)
}

geocode_addresses <- function(data, address_col, cache_path = .geocode_cache_path) {
  addresses <- data[[address_col]]
  cache     <- read_geocode_cache(cache_path)
  lats      <- numeric(length(addresses))
  longs     <- numeric(length(addresses))

  for (i in seq_along(addresses)) {
    addr <- addresses[i]
    cached <- cache[[addr]]
    if (!is.null(cached)) {
      lats[i]  <- cached$lat
      longs[i] <- cached$long
      next
    }
    message("  Geocoding ", i, "/", length(addresses), ": ", addr)
    result <- tidygeocoder::geocode(
      tibble::tibble(addr = addr),
      addr,
      method = "osm"
    )
    lats[i]  <- result$lat
    longs[i] <- result$long
    cache[[addr]] <- list(lat = lats[i], long = longs[i])
    write_geocode_cache(cache, cache_path)
    Sys.sleep(1.1)
  }
  data$lat  <- lats
  data$long <- longs
  data <- dplyr::mutate(data,
    display_lat  = lat  + stats::rnorm(dplyr::n(), 0, 0.001),
    display_long = long + stats::rnorm(dplyr::n(), 0, 0.001)
  )
  message("Geocoding complete.")
  data
}
