#' Create an interactive organization map
#'
#' @param data_source A Google Sheets URL or a data frame.
#' @param address_col Name of the column containing addresses.
#' @param name_col Name of the column containing organization names.
#' @param category_col Name of the column to color-code markers by. `NULL` for no color-coding.
#' @param website_col Name of the column containing website URLs. `NULL` to disable click-through.
#' @param modality Initial routing modality: `"none"`, `"haversine"`, `"driving"`, `"cycling"`, `"walking"`.
#' @param modality_labels Named character vector of display labels for modalities, e.g. `c(driving = "Roads", haversine = "Shortest Distance")`. Unlisted modalities fall back to title-cased key.
#' @param available_modalities Modalities to pre-compute routes for (enables switching in the UI).
#' @param n_neighbors Number of nearest neighbors to connect with routes.
#' @param logo_dir Path to a directory containing logos named `<name>.png`. `NULL` to disable logos.
#' @param light_palette RColorBrewer palette name for category colors in light mode.
#' @param dark_palette RColorBrewer palette name for category colors in dark mode.
#' @param marker_radius Radius of circle markers in pixels.
#' @param marker_opacity Fill opacity of circle markers (0–1).
#' @param marker_weight Stroke weight of circle markers in pixels.
#' @param route_color Color of route lines as a hex string.
#' @param route_opacity Opacity of route lines (0–1).
#' @param route_weight Weight of route lines in pixels.
#' @param dark Load the map in dark mode by default. Can be toggled in the UI.
#' @return A `leaflet` map object.
#' @export
canopy_map <- function(
  data_source,
  address_col          = "Address",
  name_col             = "Name",
  category_col         = "Category",
  website_col          = "Website",
  modality             = "none",
  available_modalities = modality,
  n_neighbors          = 3,
  logo_dir             = NULL,
  light_palette        = "Set1",
  dark_palette         = "Dark2",
  marker_radius        = 4,
  marker_opacity       = 0.8,
  marker_weight        = 1,
  route_color          = "#666666",
  dark_route_color     = "#aaaaaa",
  route_opacity        = 0.5,
  route_weight         = 2,
  clustering           = FALSE,
  modality_labels      = NULL,
  dark                 = FALSE
) {
  data <- load_org_data(data_source, address_col)

  # Pre-compute route GeoJSON for every requested non-none modality
  routes_geojson <- list()
  for (m in setdiff(unique(available_modalities), "none")) {
    message("Computing routes for modality: ", m, "...")
    dist_result <- compute_distances(data, m)
    edges       <- find_edges(dist_result$distance_km, n_neighbors)
    if (!is.null(dist_result$duration_min)) {
      edges <- attach_durations(edges, dist_result$duration_min)
    }
    routes_geojson[[m]] <- sfc_to_geojson(compute_route_geoms(data, edges, m))
  }

  categories <- if (!is.null(category_col)) data[[category_col]] else character(0)
  light_colors <- if (length(categories) > 0) make_palette_colors(categories, light_palette) else list()
  dark_colors  <- if (length(categories) > 0) make_palette_colors(categories, dark_palette)  else list()

  hover_html <- make_hover_labels(data, name_col, logo_dir)

  marker_data <- list(
    lat      = as.list(data$display_lat),
    lng      = as.list(data$display_long),
    category = as.list(if (!is.null(category_col)) as.character(data[[category_col]]) else rep("", nrow(data))),
    website  = as.list(if (!is.null(website_col)) as.character(data[[website_col]]) else rep("", nrow(data))),
    html     = as.list(hover_html)
  )

  map <- build_map()

  add_control_panel(
    map,
    marker_data      = marker_data,
    light_colors     = light_colors,
    dark_colors      = dark_colors,
    routes_geojson   = routes_geojson,
    current_modality = modality,
    marker_radius    = marker_radius,
    marker_opacity   = marker_opacity,
    marker_weight    = marker_weight,
    route_color      = route_color,
    dark_route_color = dark_route_color,
    route_opacity    = route_opacity,
    route_weight     = route_weight,
    category_col     = category_col,
    clustering       = clustering,
    modality_labels  = as.list(modality_labels),
    dark             = isTRUE(dark)
  )
}
