# canopymap

An R package for creating interactive Leaflet maps of organizations or other nodes, with optional routing connections between nearest neighbors.

## Features

- **Data from anywhere** — pass a Google Sheets URL or a local data frame
- **Automatic geocoding** — addresses are geocoded via OpenStreetMap (no API key required)
- **Routing modes** — connect nearest neighbors by straight-line distance (Haversine) or road network (driving, cycling, walking) via the public OSRM API
- **Category colors** — color-code markers by any column, with separate palettes for light and dark mode
- **Logo tooltips** — show organization logos and names on hover
- **Click-to-website** — clicking a marker opens its website in a new tab
- **Interactive controls** — dark mode, clustering, marker size, and routing modality can all be toggled live in the map UI

## Installation

```r
# install.packages("remotes")
remotes::install_github("your-username/canopymap")
```

## Quick start

```r
library(canopymap)

canopy_map(
  data_source  = "https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID",
  name_col     = "Name",
  address_col  = "Address",
  category_col = "Category",
  website_col  = "Website"
)
```

Your Google Sheet must be publicly readable (File → Share → Anyone with the link).

## Usage

```r
canopy_map(
  data_source          = "https://docs.google.com/spreadsheets/d/...",
  address_col          = "Address",
  name_col             = "Name",
  category_col         = "Category",       # NULL to disable color-coding
  website_col          = "Website",        # NULL to disable click-through
  modality             = "driving",        # initial routing mode
  available_modalities = c("none", "haversine", "driving"),
  modality_labels      = c(none = "None", haversine = "Shortest Distance", driving = "Roads"),
  n_neighbors          = 3,
  logo_dir             = "path/to/logos",  # PNGs named <Name>.png; NULL to disable
  light_palette        = "Set2",           # RColorBrewer palette for light mode
  dark_palette         = "Dark2",          # RColorBrewer palette for dark mode
  marker_radius        = 8,
  marker_opacity       = 0.8,
  marker_weight        = 1,
  route_color          = "#666666",
  dark_route_color     = "#aaaaaa",
  route_opacity        = 0.5,
  route_weight         = 2,
  clustering           = FALSE
)
```

## Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `data_source` | — | Google Sheets URL or data frame |
| `address_col` | `"Address"` | Column containing street addresses |
| `name_col` | `"Name"` | Column containing node names (used for tooltips and logo filenames) |
| `category_col` | `"Category"` | Column to color-code markers by; `NULL` to disable |
| `website_col` | `"Website"` | Column containing URLs; `NULL` to disable click-through |
| `modality` | `"none"` | Initial routing mode shown on load |
| `available_modalities` | same as `modality` | Modalities to pre-compute; enables live switching in the UI |
| `modality_labels` | `NULL` | Named vector of display labels, e.g. `c(driving = "Roads")` |
| `n_neighbors` | `3` | Number of nearest neighbors to connect per node |
| `logo_dir` | `NULL` | Path to directory of `<Name>.png` logo files |
| `light_palette` | `"Set2"` | RColorBrewer palette for light mode |
| `dark_palette` | `"Dark2"` | RColorBrewer palette for dark mode |
| `marker_radius` | `4` | Circle marker radius in pixels |
| `marker_opacity` | `0.8` | Fill opacity (0–1) |
| `marker_weight` | `1` | Stroke weight in pixels |
| `route_color` | `"#666666"` | Route line color in light mode |
| `dark_route_color` | `"#aaaaaa"` | Route line color in dark mode |
| `route_opacity` | `0.5` | Route line opacity (0–1) |
| `route_weight` | `2` | Route line width in pixels |
| `clustering` | `FALSE` | Whether to start with marker clustering enabled |

## Routing modalities

| Key | Description |
|-----|-------------|
| `"none"` | No routes drawn |
| `"haversine"` | Straight-line distance (no external API, works offline) |
| `"driving"` | Road network via [OSRM](http://project-osrm.org/) public API |
| `"cycling"` | Cycling network via OSRM public API |
| `"walking"` | Walking network via OSRM public API |

Pre-computing multiple modalities at startup enables instant switching in the UI without reloading. Note that the OSRM public API has rate limits — for large datasets, consider [hosting your own OSRM instance](https://github.com/Project-OSRM/osrm-backend).

## Data format

The minimum required columns are an address column and a name column. All other columns are optional.

| Column | Purpose |
|--------|---------|
| Address | Street address for geocoding |
| Name | Display name; also used to match logo files |
| Category | Used for color-coding markers |
| Website | Opened when a marker is clicked |

Column names are fully configurable via the `*_col` parameters.

## Logos

Place PNG files in a directory named `<Name>.png`, where `<Name>` exactly matches the value in your name column. Pass the directory path as `logo_dir`. Logos are displayed at 120×60px in the hover tooltip.

## License

MIT

## Acknowledgements

Developed in NYC/BOS with Claude Sonnet 4.6