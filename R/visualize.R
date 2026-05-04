build_map <- function() {
  leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    # Zero-length call loads the markercluster plugin; actual markers are created in JS
    leaflet::addCircleMarkers(
      lng = numeric(0), lat = numeric(0),
      clusterOptions = leaflet::markerClusterOptions()
    )
}

make_hover_labels <- function(data, name_col, logo_dir) {
  lapply(seq_len(nrow(data)), function(i) {
    name     <- data[[name_col]][i]
    logo_tag <- ""
    if (!is.null(logo_dir)) {
      logo_path <- file.path(logo_dir, paste0(name, ".png"))
      if (file.exists(logo_path)) {
        logo_tag <- paste0(
          '<img src="', base64enc::dataURI(file = logo_path, mime = "image/png"),
          '" style="width:120px;height:60px;object-fit:contain;margin-bottom:4px;"><br>'
        )
      } else {
        message("Logo not found: ", logo_path)
      }
    }
    paste0(
      '<div style="text-align:center;min-width:120px;">',
      logo_tag,
      '<b>', name, '</b>',
      '</div>'
    )
  })
}

add_control_panel <- function(map, marker_data, light_colors, dark_colors,
                               routes_geojson, current_modality,
                               marker_radius, marker_opacity, marker_weight,
                               route_color, dark_route_color, route_opacity, route_weight,
                               category_col, clustering, modality_labels) {
  js_data <- list(
    markers         = marker_data,
    lightColors     = light_colors,
    darkColors      = dark_colors,
    routes          = routes_geojson,
    currentModality = current_modality,
    markerRadius    = marker_radius,
    markerOpacity   = marker_opacity,
    markerWeight    = marker_weight,
    routeColor      = route_color,
    darkRouteColor  = dark_route_color,
    routeOpacity    = route_opacity,
    routeWeight     = route_weight,
    categoryCol     = category_col,
    clustering      = clustering,
    modalityLabels  = modality_labels
  )

  htmlwidgets::onRender(map, "
    function(el, x, data) {
      var map             = this;
      var isDark          = false;
      var lightColors     = data.lightColors  || {};
      var darkColors      = data.darkColors   || {};
      var hasColors       = Object.keys(lightColors).length > 0;
      var currentModality = data.currentModality;
      var currentRadius   = data.markerRadius;
      var isClustered     = data.clustering;
      var legendEl        = null;
      var panelDiv        = null;
      var CARD_CSS_LIGHT  = 'background:white;color:#333;border:1px solid rgba(0,0,0,0.2);';
      var CARD_CSS_DARK   = 'background:#1e1e1e;color:#e0e0e0;border:1px solid rgba(255,255,255,0.15);';
      var CARD_CSS_BASE   = 'padding:8px 12px;border-radius:4px;font-family:sans-serif;font-size:12px;line-height:1.8;cursor:default;min-width:150px;';

      // ── Tile layers ────────────────────────────────────────────────────
      var lightTile = L.tileLayer(
        'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png',
        { attribution: '&copy; OpenStreetMap contributors &copy; CARTO', subdomains: 'abcd', maxZoom: 19 }
      );
      var darkTile = L.tileLayer(
        'https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png',
        { attribution: '&copy; OpenStreetMap contributors &copy; CARTO', subdomains: 'abcd', maxZoom: 19 }
      );

      // Replace the R-added tile layer with our controlled one
      map.eachLayer(function(layer) {
        if (layer instanceof L.TileLayer) { map.removeLayer(layer); }
      });
      lightTile.addTo(map);

      // ── Inject CSS ─────────────────────────────────────────────────────
      var sliderStyle = document.createElement('style');
      sliderStyle.id = 'canopy-slider-style';
      document.head.appendChild(sliderStyle);

      function updateSliderTrack(dark) {
        sliderStyle.textContent = dark ? [
          '#cp-size::-webkit-slider-runnable-track { background:#555; border-radius:2px; }',
          '#cp-size::-moz-range-track { background:#555; border-radius:2px; }'
        ].join('') : '';
      }

      var style = document.createElement('style');
      style.textContent = [
        '.canopy-tip { font-size:13px; padding:6px; }',
        '.leaflet-top.leaflet-left { display:flex; flex-direction:column; align-items:flex-start; gap:5px; }',
        '.leaflet-top.leaflet-left .leaflet-control-zoom { order:2; }',
        '.leaflet-top.leaflet-left .canopy-panel { order:1; }',
        '.leaflet-control-zoom { box-shadow:none !important; border:1px solid rgba(0,0,0,0.2) !important; border-radius:4px !important; }',
        '.leaflet-control-zoom a { background:white; color:#333; border-bottom:1px solid rgba(0,0,0,0.1); line-height:26px; }',
        '.leaflet-control-zoom a:last-child { border-bottom:none; }'
      ].join('');
      document.head.appendChild(style);

      // ── Color helpers ──────────────────────────────────────────────────
      function getColor(category) {
        var palette = isDark ? darkColors : lightColors;
        return palette[category] || '#3388ff';
      }

      // ── Create markers and cluster group directly in JS ────────────────
      var clusterGroup = L.markerClusterGroup();
      var allMarkers   = [];

      data.markers.lat.forEach(function(lat, i) {
        var lng = data.markers.lng[i];
        if (lat === null || lat === undefined || lng === null || lng === undefined) return;
        var cat    = data.markers.category[i] || '';
        var color  = getColor(cat);
        var marker = L.circleMarker([lat, lng], {
          radius:      data.markerRadius,
          color:       color,
          fillColor:   color,
          fillOpacity: data.markerOpacity,
          weight:      data.markerWeight
        });

        if (data.markers.html[i]) {
          marker.bindTooltip(data.markers.html[i], { className: 'canopy-tip', sticky: false });
        }

        var url = data.markers.website[i];
        if (url) {
          marker.on('click', function() { window.open(url, '_blank'); });
        }

        allMarkers.push(marker);
        clusterGroup.addLayer(marker);
      });

      if (isClustered) {
        map.addLayer(clusterGroup);
      } else {
        allMarkers.forEach(function(m) { map.addLayer(m); });
      }
      var boundsGroup = isClustered ? clusterGroup : L.featureGroup(allMarkers);
      if (allMarkers.length > 0) {
        map.fitBounds(boundsGroup.getBounds().pad(0.1));
      }

      // ── Route layers ───────────────────────────────────────────────────
      var routeLayers = {};
      Object.keys(data.routes || {}).forEach(function(m) {
        routeLayers[m] = L.geoJSON(JSON.parse(data.routes[m]), {
          style: { color: data.routeColor, weight: data.routeWeight, opacity: data.routeOpacity }
        });
      });
      if (currentModality !== 'none' && routeLayers[currentModality]) {
        routeLayers[currentModality].addTo(map);
      }

      // ── Legend ─────────────────────────────────────────────────────────
      if (hasColors) {
        var LegendControl = L.Control.extend({
          options: { position: 'bottomright' },
          onAdd: function() {
            legendEl = L.DomUtil.create('div', 'leaflet-control canopy-legend');
            legendEl.style.cssText = 'background:white;color:#333;border:1px solid rgba(0,0,0,0.2);padding:8px 12px;border-radius:4px;font-family:sans-serif;font-size:12px;line-height:1.8;display:block;';
            renderLegend();
            return legendEl;
          }
        });
        new LegendControl().addTo(map);
      }

      function renderLegend() {
        if (!legendEl) return;
        var colors = isDark ? darkColors : lightColors;
        legendEl.innerHTML = '<strong>' + (data.categoryCol || 'Category') + '</strong><br>';
        Object.keys(colors).forEach(function(cat) {
          legendEl.innerHTML +=
            '<span style=\"display:inline-block;width:10px;height:10px;border-radius:50%;' +
            'background:' + colors[cat] + ';margin-right:5px;vertical-align:middle;\"></span>' +
            cat + '<br>';
        });
      }

      // ── Update functions ───────────────────────────────────────────────
      function updateColors() {
        allMarkers.forEach(function(marker, i) {
          var color = getColor(data.markers.category[i] || '');
          marker.setStyle({ color: color, fillColor: color });
        });
        renderLegend();
      }

      function updateSize(radius) {
        allMarkers.forEach(function(marker) { marker.setRadius(radius); });
      }

      function toggleClustering(enabled) {
        if (enabled && !isClustered) {
          allMarkers.forEach(function(m) { map.removeLayer(m); });
          allMarkers.forEach(function(m) { clusterGroup.addLayer(m); });
          map.addLayer(clusterGroup);
          isClustered = true;
        } else if (!enabled && isClustered) {
          map.removeLayer(clusterGroup);
          clusterGroup.clearLayers();
          allMarkers.forEach(function(m) { map.addLayer(m); });
          isClustered = false;
        }
      }

      function applyDarkStyles(dark) {
        var cardCss    = (dark ? CARD_CSS_DARK : CARD_CSS_LIGHT) + CARD_CSS_BASE;
        var legendCss  = (dark ? CARD_CSS_DARK : CARD_CSS_LIGHT) +
          'padding:8px 12px;border-radius:4px;font-family:sans-serif;font-size:12px;line-height:1.8;display:block;';
        var border  = dark ? 'rgba(255,255,255,0.15)' : 'rgba(0,0,0,0.2)';
        var bg      = dark ? '#1e1e1e' : 'white';
        var fg      = dark ? '#e0e0e0' : '#333';
        var divider = dark ? 'rgba(255,255,255,0.1)' : 'rgba(0,0,0,0.1)';

        if (panelDiv) panelDiv.style.cssText = cardCss;
        if (legendEl) legendEl.style.cssText = legendCss;
        updateSliderTrack(dark);

        // Custom modality dropdown
        var modalBtn  = document.getElementById('cp-modality-btn');
        var modalList = document.getElementById('cp-modality-list');
        if (modalBtn)  { modalBtn.style.borderColor  = border; modalBtn.style.background  = bg; modalBtn.style.color = fg; }
        if (modalList) { modalList.style.borderColor = border; modalList.style.background = bg; modalList.style.color = fg; }

        // Zoom controls
        var zoomBar = document.querySelector('.leaflet-control-zoom');
        if (zoomBar) zoomBar.style.borderColor = border;
        document.querySelectorAll('.leaflet-control-zoom a').forEach(function(btn, i, arr) {
          btn.style.background   = bg;
          btn.style.color        = fg;
          btn.style.borderBottom = (i < arr.length - 1) ? ('1px solid ' + divider) : 'none';
        });

        renderLegend();
      }

      function toggleDarkMode(dark) {
        isDark = dark;
        if (dark) {
          map.removeLayer(lightTile);
          darkTile.addTo(map);
        } else {
          map.removeLayer(darkTile);
          lightTile.addTo(map);
        }
        // Update route colors
        var rc = dark ? data.darkRouteColor : data.routeColor;
        Object.keys(routeLayers).forEach(function(m) {
          routeLayers[m].setStyle({ color: rc });
        });
        applyDarkStyles(dark);
        updateColors();
      }

      function updateModality(mod) {
        if (currentModality !== 'none' && routeLayers[currentModality]) {
          map.removeLayer(routeLayers[currentModality]);
        }
        currentModality = mod;
        if (currentModality !== 'none' && routeLayers[currentModality]) {
          routeLayers[currentModality].addTo(map);
        }
      }

      // ── Control panel ──────────────────────────────────────────────────
      var availableModalities = ['none'].concat(Object.keys(routeLayers));
      var modalityLabels = data.modalityLabels || {};
      function modalityLabel(m) {
        return modalityLabels[m] || (m.charAt(0).toUpperCase() + m.slice(1));
      }
      var currentModalityLabel = modalityLabel(currentModality);

      var modalityItems = availableModalities.map(function(m) {
        return '<div data-value=\"' + m + '\" style=\"padding:4px 8px;cursor:pointer;\">' + modalityLabel(m) + '</div>';
      }).join('');

      var ControlPanel = L.Control.extend({
        options: { position: 'topleft' },
        onAdd: function() {
          panelDiv = L.DomUtil.create('div', 'leaflet-control canopy-panel');
          var div = panelDiv;
          div.style.cssText = CARD_CSS_LIGHT + CARD_CSS_BASE;

          var btnStyle = 'width:100%;font-size:12px;font-family:sans-serif;border:1px solid rgba(0,0,0,0.2);border-radius:4px;padding:3px 8px;background:white;color:#333;cursor:pointer;display:flex;justify-content:space-between;align-items:center;box-sizing:border-box;';
          var listStyle = 'display:none;position:absolute;top:calc(100% + 2px);left:0;right:0;z-index:9999;background:white;color:#333;border:1px solid rgba(0,0,0,0.2);border-radius:4px;overflow:hidden;box-shadow:0 2px 8px rgba(0,0,0,0.12);font-size:12px;font-family:sans-serif;';
          div.innerHTML = [
            '<div style=\"margin-bottom:6px\">' +
              '<label style=\"cursor:pointer\">' +
                '<input type=\"checkbox\" id=\"cp-dark\" style=\"margin-right:6px\">' +
                'Dark mode' +
              '</label>' +
            '</div>',
            '<div style=\"margin-bottom:6px\">' +
              '<label style=\"cursor:pointer\">' +
                '<input type=\"checkbox\" id=\"cp-cluster\"' + (isClustered ? ' checked' : '') + ' style=\"margin-right:6px\">' +
                'Clustering' +
              '</label>' +
            '</div>',
            '<div style=\"margin-bottom:6px\">' +
              '<div style=\"margin-bottom:3px\">Marker size: <span id=\"cp-size-val\">' + currentRadius + '</span></div>' +
              '<input type=\"range\" id=\"cp-size\" min=\"2\" max=\"15\" value=\"' + currentRadius + '\" style=\"width:100%;margin:0\">' +
            '</div>',
            '<div>' +
              '<div style=\"margin-bottom:3px\">Modality</div>' +
              '<div style=\"position:relative\">' +
                '<div id=\"cp-modality-btn\" style=\"' + btnStyle + '\">' +
                  '<span id=\"cp-modality-label\">' + currentModalityLabel + '</span>' +
                  '<span style=\"font-size:10px;opacity:0.5\">&#9660;</span>' +
                '</div>' +
                '<div id=\"cp-modality-list\" style=\"' + listStyle + '\">' + modalityItems + '</div>' +
              '</div>' +
            '</div>'
          ].join('');

          L.DomEvent.disableClickPropagation(div);
          L.DomEvent.disableScrollPropagation(div);
          return div;
        }
      });
      new ControlPanel().addTo(map);

      // ── Wire events ────────────────────────────────────────────────────
      setTimeout(function() {
        var darkCb    = document.getElementById('cp-dark');
        var sizeSl    = document.getElementById('cp-size');
        var sizeVal   = document.getElementById('cp-size-val');
        var clusterCb = document.getElementById('cp-cluster');
        var modalBtn  = document.getElementById('cp-modality-btn');
        var modalList = document.getElementById('cp-modality-list');
        var modalLabel = document.getElementById('cp-modality-label');

        if (darkCb) darkCb.addEventListener('change', function() {
          toggleDarkMode(this.checked);
        });
        if (sizeSl) sizeSl.addEventListener('input', function() {
          currentRadius = parseInt(this.value);
          sizeVal.textContent = currentRadius;
          updateSize(currentRadius);
        });
        if (clusterCb) clusterCb.addEventListener('change', function() {
          toggleClustering(this.checked);
        });

        // Custom modality dropdown
        if (modalBtn) modalBtn.addEventListener('click', function(e) {
          e.stopPropagation();
          var open = modalList.style.display !== 'none';
          modalList.style.display = open ? 'none' : 'block';
        });
        if (modalList) modalList.querySelectorAll('[data-value]').forEach(function(opt) {
          opt.addEventListener('mouseover', function() { this.style.background = isDark ? '#2a2a2a' : '#f0f0f0'; });
          opt.addEventListener('mouseout',  function() { this.style.background = ''; });
          opt.addEventListener('click', function(e) {
            e.stopPropagation();
            updateModality(this.dataset.value);
            modalLabel.textContent = this.textContent;
            modalList.style.display = 'none';
          });
        });
        document.addEventListener('click', function() {
          if (modalList) modalList.style.display = 'none';
        });
      }, 200);
    }
  ", data = js_data)
}
