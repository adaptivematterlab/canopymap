load_org_data <- function(data_source, address_col) {
  if (is.character(data_source)) {
    message("Loading data from Google Sheets...")
    googlesheets4::gs4_deauth()
    data <- googlesheets4::read_sheet(data_source)
  } else {
    data <- data_source
  }
  message("Loaded ", nrow(data), " rows. Geocoding addresses...")
  geocode_addresses(data, address_col)
}

geocode_addresses <- function(data, address_col) {
  lats  <- numeric(nrow(data))
  longs <- numeric(nrow(data))
  for (i in seq_len(nrow(data))) {
    message("  Geocoding ", i, "/", nrow(data), ": ", data[[address_col]][i])
    result <- tidygeocoder::geocode(
      tibble::tibble(addr = data[[address_col]][i]),
      addr,
      method = "osm"
    )
    lats[i]  <- result$lat
    longs[i] <- result$long
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
