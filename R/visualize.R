build_map <- function(carto_api_key = "") {
  key_param <- if (nzchar(carto_api_key)) paste0("?key=", utils::URLencode(carto_api_key, reserved = TRUE)) else ""
  leaflet::leaflet() |>
    leaflet::addTiles(
      urlTemplate = paste0("https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png", key_param),
      attribution = "&copy; OpenStreetMap contributors &copy; CARTO",
      options = leaflet::tileOptions(subdomains = "abcd", maxZoom = 19)
    ) |>
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
                               category_col, clustering, clusters, modality_labels, dark = FALSE,
                               carto_api_key = "") {
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
    clusters        = clusters,
    modalityLabels  = modality_labels,
    dark            = dark,
    cartoApiKey     = carto_api_key
  )

  htmlwidgets::onRender(map, "
    function(el, x, data) {
      var map             = this;
      var isDark          = !!data.dark;
      var lightColors     = data.lightColors  || {};
      var darkColors      = data.darkColors   || {};
      var hasColors       = Object.keys(lightColors).length > 0;
      var currentModality = data.currentModality;
      var currentRadius   = data.markerRadius;
      var isClustered     = data.clustering;
      var panelDiv        = null;
      var clusterCb       = null;
      var modalBtn        = null;
      var modalList       = null;
      var categoryCheckboxes = [];
      var categoryBlock   = null;
      var catAllLink      = null;
      var catNoneLink     = null;
      var CARD_CSS_LIGHT  = 'background:white;color:#333;border:1px solid rgba(0,0,0,0.2);';
      var CARD_CSS_DARK   = 'background:#1e1e1e;color:#e0e0e0;border:1px solid rgba(255,255,255,0.15);';
      var CARD_CSS_BASE   = 'padding:8px 12px;border-radius:4px;font-family:sans-serif;font-size:12px;line-height:1.8;cursor:default;min-width:150px;';

      // ── Tile layers ────────────────────────────────────────────────────
      // CARTO now requires a (free) API key on basemaps.cartocdn.com; it's
      // read from the CARTO_API_KEY env var on the R side and threaded
      // through as data.cartoApiKey. Without it these will show CARTO's
      // \"API KEY REQUIRED\" watermark.
      var cartoKeyParam = data.cartoApiKey ? ('?key=' + encodeURIComponent(data.cartoApiKey)) : '';
      var lightTile = L.tileLayer(
        'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png' + cartoKeyParam,
        { attribution: '&copy; OpenStreetMap contributors &copy; CARTO', subdomains: 'abcd', maxZoom: 19 }
      );
      var darkTile = L.tileLayer(
        'https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png' + cartoKeyParam,
        { attribution: '&copy; OpenStreetMap contributors &copy; CARTO', subdomains: 'abcd', maxZoom: 19 }
      );

      // Replace the R-added tile layer with our controlled one
      map.eachLayer(function(layer) {
        if (layer instanceof L.TileLayer) { map.removeLayer(layer); }
      });
      if (isDark) { darkTile.addTo(map); } else { lightTile.addTo(map); }

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

      map.zoomControl.setPosition('topright');

      var style = document.createElement('style');
      style.textContent = [
        '.canopy-tip { font-size:13px; padding:6px; }',
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

      // ── Category filter state ─────────────────────────────────────────
      // Category list comes from the same palette object used for marker
      // colors (not a hardcoded duplicate). Counts are tallied from the
      // data at render time.
      var activeView      = 'sites';
      var categoryList    = Object.keys(lightColors);
      var categoryCounts  = {};
      var categoryFilter  = {};
      categoryList.forEach(function(cat) {
        categoryCounts[cat] = 0;
        categoryFilter[cat] = true;
      });
      (data.markers.category || []).forEach(function(cat) {
        if (categoryCounts.hasOwnProperty(cat)) categoryCounts[cat]++;
      });

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
        marker._cat = cat;

        if (data.markers.html[i]) {
          marker.bindTooltip(data.markers.html[i], { className: 'canopy-tip', sticky: false });
        }

        var url = data.markers.website[i];
        if (url) {
          marker.on('click', function() { window.open(url, '_blank'); });
        }

        allMarkers.push(marker);
      });

      // Category filtering only applies in the Sites view -- Connections and
      // Neighborhoods are claims about the complete dataset, so they always
      // show every marker regardless of the remembered filter state.
      function visibleMarkers() {
        return allMarkers.filter(function(m) {
          return activeView !== 'sites' || categoryFilter[m._cat] !== false;
        });
      }

      function refreshMarkers() {
        var visible = visibleMarkers();
        allMarkers.forEach(function(m) { map.removeLayer(m); });
        clusterGroup.clearLayers();
        map.removeLayer(clusterGroup);

        if (isClustered) {
          visible.forEach(function(m) { clusterGroup.addLayer(m); });
          map.addLayer(clusterGroup);
        } else {
          visible.forEach(function(m) { map.addLayer(m); });
        }
      }

      refreshMarkers();
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
      // None isn't offered as a Modality choice -- Sites view already covers
      // showing no routes. If the initial modality is unset/invalid, fall back
      // to the first modality that actually has routes computed.
      var availableModalities = Object.keys(routeLayers);
      if (availableModalities.indexOf(currentModality) === -1) {
        currentModality = availableModalities.length > 0 ? availableModalities[0] : 'none';
      }
      // ── Neighborhoods (cluster) layer ─────────────────────────────────
      var neighborhoodsGroup = L.layerGroup();
      (data.clusters || []).forEach(function(c) {
        var color = getColor(c.category || '');
        var circle = L.circle([c.lat, c.long], {
          radius:      c.radius,
          color:       color,
          weight:      1,
          fillColor:   color,
          fillOpacity: 0.15
        });
        circle.bindTooltip(c.count + ' sites &middot; ' + (c.category || ''), { className: 'canopy-tip', sticky: true });
        neighborhoodsGroup.addLayer(circle);
      });
      data.markers.lat.forEach(function(lat, i) {
        if (!data.markers.outlier || !data.markers.outlier[i]) return;
        var lng = data.markers.lng[i];
        if (lat === null || lat === undefined || lng === null || lng === undefined) return;
        neighborhoodsGroup.addLayer(L.circleMarker([lat, lng], {
          radius:      data.markerRadius + 3,
          color:       '#888888',
          weight:      1,
          fillOpacity: 0
        }));
      });

      // ── View caption ───────────────────────────────────────────────────
      var VIEW_CAPTIONS = {
        sites:         'Circles show where organizations are.',
        connections:   'Lines show travel between places.',
        neighborhoods: 'Circles show what is within walking distance of what.'
      };
      var captionEl  = null;
      var CaptionControl = L.Control.extend({
        options: { position: 'bottomleft' },
        onAdd: function() {
          captionEl = L.DomUtil.create('div', 'leaflet-control canopy-caption');
          captionEl.style.cssText = CARD_CSS_LIGHT + 'padding:4px 10px;border-radius:4px;font-family:sans-serif;font-size:11px;';
          return captionEl;
        }
      });
      new CaptionControl().addTo(map);

      // ── View toggle (Sites / Connections / Neighborhoods) ───────────────
      function setClusterEnabled(enabled) {
        if (!clusterCb) return;
        clusterCb.disabled = !enabled;
        var label = document.getElementById('cp-cluster-label');
        if (label) {
          label.style.opacity = enabled ? '1' : '0.4';
          label.style.cursor  = enabled ? 'pointer' : 'not-allowed';
        }
      }
      function setModalityEnabled(enabled) {
        if (!modalBtn) return;
        modalBtn.style.pointerEvents = enabled ? 'auto' : 'none';
        modalBtn.style.opacity       = enabled ? '1' : '0.4';
        modalBtn.style.cursor        = enabled ? 'pointer' : 'not-allowed';
        if (!enabled && modalList) modalList.style.display = 'none';
      }
      function setCategoryFilterEnabled(enabled) {
        categoryCheckboxes.forEach(function(cb) { if (cb) cb.disabled = !enabled; });
        if (categoryBlock) categoryBlock.style.opacity = enabled ? '1' : '0.5';
        if (catAllLink)  catAllLink.style.pointerEvents  = enabled ? 'auto' : 'none';
        if (catNoneLink) catNoneLink.style.pointerEvents = enabled ? 'auto' : 'none';
      }
      function applyView(view) {
        activeView = view;
        refreshMarkers();
        setCategoryFilterEnabled(view === 'sites');

        // Route lines are only meaningful in the Connections view.
        if (currentModality !== 'none' && routeLayers[currentModality]) {
          if (view === 'connections') {
            routeLayers[currentModality].addTo(map);
          } else {
            map.removeLayer(routeLayers[currentModality]);
          }
        }
        setModalityEnabled(view === 'connections');

        // Neighborhood circles are only shown in the Neighborhoods view.
        if (view === 'neighborhoods') {
          neighborhoodsGroup.addTo(map);
        } else {
          map.removeLayer(neighborhoodsGroup);
        }

        // Clustering the individual markers doesn't add anything on top of
        // the Neighborhoods circles, so it's turned off and locked while active.
        if (view === 'neighborhoods') {
          if (isClustered) toggleClustering(false);
          if (clusterCb) clusterCb.checked = false;
          setClusterEnabled(false);
        } else {
          setClusterEnabled(true);
        }

        if (captionEl) captionEl.textContent = VIEW_CAPTIONS[view];
      }
      applyView('sites');

      // ── Update functions ───────────────────────────────────────────────
      function updateColors() {
        allMarkers.forEach(function(marker, i) {
          var color = getColor(data.markers.category[i] || '');
          marker.setStyle({ color: color, fillColor: color });
        });
      }

      function updateCategorySwatches() {
        var palette = isDark ? darkColors : lightColors;
        categoryList.forEach(function(cat, i) {
          var swatch = document.getElementById('cp-cat-swatch-' + i);
          if (swatch) swatch.style.background = palette[cat] || '#3388ff';
        });
      }

      function updateSize(radius) {
        allMarkers.forEach(function(marker) { marker.setRadius(radius); });
      }

      function toggleClustering(enabled) {
        isClustered = enabled;
        refreshMarkers();
      }

      function applyDarkStyles(dark) {
        var cardCss    = (dark ? CARD_CSS_DARK : CARD_CSS_LIGHT) + CARD_CSS_BASE;
        var border  = dark ? 'rgba(255,255,255,0.15)' : 'rgba(0,0,0,0.2)';
        var bg      = dark ? '#1e1e1e' : 'white';
        var fg      = dark ? '#e0e0e0' : '#333';
        var divider = dark ? 'rgba(255,255,255,0.1)' : 'rgba(0,0,0,0.1)';

        if (panelDiv) panelDiv.style.cssText = cardCss;
        if (captionEl) captionEl.style.cssText = (dark ? CARD_CSS_DARK : CARD_CSS_LIGHT) + 'padding:4px 10px;border-radius:4px;font-family:sans-serif;font-size:11px;';
        updateSliderTrack(dark);
        updateCategorySwatches();

        // Custom modality dropdown
        var modalBtnEl  = document.getElementById('cp-modality-btn');
        var modalListEl = document.getElementById('cp-modality-list');
        if (modalBtnEl)  { modalBtnEl.style.borderColor  = border; modalBtnEl.style.background  = bg; modalBtnEl.style.color = fg; }
        if (modalListEl) { modalListEl.style.borderColor = border; modalListEl.style.background = bg; modalListEl.style.color = fg; }

        // Custom view dropdown
        var viewBtnEl  = document.getElementById('cp-view-btn');
        var viewListEl = document.getElementById('cp-view-list');
        if (viewBtnEl)  { viewBtnEl.style.borderColor  = border; viewBtnEl.style.background  = bg; viewBtnEl.style.color = fg; }
        if (viewListEl) { viewListEl.style.borderColor = border; viewListEl.style.background = bg; viewListEl.style.color = fg; }

        // Zoom controls
        var zoomBar = document.querySelector('.leaflet-control-zoom');
        if (zoomBar) zoomBar.style.borderColor = border;
        document.querySelectorAll('.leaflet-control-zoom a').forEach(function(btn, i, arr) {
          btn.style.background   = bg;
          btn.style.color        = fg;
          btn.style.borderBottom = (i < arr.length - 1) ? ('1px solid ' + divider) : 'none';
        });
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
        if (activeView === 'connections' && currentModality !== 'none' && routeLayers[currentModality]) {
          map.removeLayer(routeLayers[currentModality]);
        }
        currentModality = mod;
        if (activeView === 'connections' && currentModality !== 'none' && routeLayers[currentModality]) {
          routeLayers[currentModality].addTo(map);
        }
      }

      // ── Control panel ──────────────────────────────────────────────────
      var modalityLabels = data.modalityLabels || {};
      function modalityLabel(m) {
        return modalityLabels[m] || (m.charAt(0).toUpperCase() + m.slice(1));
      }
      var currentModalityLabel = modalityLabel(currentModality);

      var modalityItems = availableModalities.map(function(m) {
        return '<div data-value=\"' + m + '\" style=\"padding:4px 8px;cursor:pointer;\">' + modalityLabel(m) + '</div>';
      }).join('');

      var viewOptions = ['sites', 'connections', 'neighborhoods'];
      var viewLabels  = { sites: 'Sites', connections: 'Connections', neighborhoods: 'Neighborhoods' };
      var viewItems = viewOptions.map(function(v) {
        return '<div data-value=\"' + v + '\" style=\"padding:4px 8px;cursor:pointer;\">' + viewLabels[v] + '</div>';
      }).join('');

      var ControlPanel = L.Control.extend({
        options: { position: 'topleft' },
        onAdd: function() {
          panelDiv = L.DomUtil.create('div', 'leaflet-control canopy-panel');
          var div = panelDiv;
          div.style.cssText = CARD_CSS_LIGHT + CARD_CSS_BASE;

          var btnStyle = 'width:100%;font-size:12px;font-family:sans-serif;border:1px solid rgba(0,0,0,0.2);border-radius:4px;padding:3px 8px;background:white;color:#333;cursor:pointer;display:flex;justify-content:space-between;align-items:center;box-sizing:border-box;';
          var listStyle = 'display:none;position:absolute;top:calc(100% + 2px);left:0;right:0;z-index:9999;background:white;color:#333;border:1px solid rgba(0,0,0,0.2);border-radius:4px;overflow:hidden;box-shadow:0 2px 8px rgba(0,0,0,0.12);font-size:12px;font-family:sans-serif;';
          var dividerHtml = '<div style=\"border-top:1px solid rgba(128,128,128,0.3);margin:8px 0 6px\"></div>';

          var categoryItemsHtml = categoryList.map(function(cat, i) {
            var color = isDark ? darkColors[cat] : lightColors[cat];
            return '<label style=\"display:flex;align-items:center;cursor:pointer;padding:1px 0;\">' +
              '<input type=\"checkbox\" id=\"cp-cat-cb-' + i + '\" checked style=\"margin-right:6px\">' +
              '<span id=\"cp-cat-swatch-' + i + '\" style=\"display:inline-block;width:10px;height:10px;border-radius:50%;background:' + color + ';margin-right:6px;flex-shrink:0;\"></span>' +
              '<span>' + cat + ' (' + categoryCounts[cat] + ')</span>' +
            '</label>';
          }).join('');

          var categoryBlockHtml = '<div id=\"cp-category-block\" title=\"Filtering applies to the Sites view. Connections and Neighborhoods are computed from all sites.\">' +
            '<div style=\"display:flex;justify-content:space-between;margin-bottom:3px\">' +
              '<span>Category</span>' +
              '<span><a href=\"#\" id=\"cp-cat-all\" style=\"color:inherit;margin-right:6px\">All</a><a href=\"#\" id=\"cp-cat-none\" style=\"color:inherit\">None</a></span>' +
            '</div>' +
            '<div>' + categoryItemsHtml + '</div>' +
          '</div>';

          var panelParts = [
            '<div style=\"margin-bottom:6px\">' +
              '<div style=\"margin-bottom:3px\">View</div>' +
              '<div style=\"position:relative\">' +
                '<div id=\"cp-view-btn\" style=\"' + btnStyle + '\">' +
                  '<span id=\"cp-view-label\">' + viewLabels[activeView] + '</span>' +
                  '<span style=\"font-size:10px;opacity:0.5\">&#9660;</span>' +
                '</div>' +
                '<div id=\"cp-view-list\" style=\"' + listStyle + '\">' + viewItems + '</div>' +
              '</div>' +
            '</div>',
            '<div style=\"margin-bottom:6px\">' +
              '<div style=\"margin-bottom:3px\">Modality</div>' +
              '<div style=\"position:relative\">' +
                '<div id=\"cp-modality-btn\" style=\"' + btnStyle + '\">' +
                  '<span id=\"cp-modality-label\">' + currentModalityLabel + '</span>' +
                  '<span style=\"font-size:10px;opacity:0.5\">&#9660;</span>' +
                '</div>' +
                '<div id=\"cp-modality-list\" style=\"' + listStyle + '\">' + modalityItems + '</div>' +
              '</div>' +
            '</div>',
            '<div style=\"margin-bottom:6px\">' +
              '<label id=\"cp-cluster-label\" style=\"cursor:pointer\">' +
                '<input type=\"checkbox\" id=\"cp-cluster\"' + (isClustered ? ' checked' : '') + ' style=\"margin-right:6px\">' +
                'Group nearby markers' +
              '</label>' +
            '</div>',
            dividerHtml
          ];
          if (hasColors) {
            panelParts.push(categoryBlockHtml);
            panelParts.push(dividerHtml);
          }
          panelParts.push(
            '<div style=\"margin-bottom:6px\">' +
              '<div style=\"margin-bottom:3px\">Marker size: <span id=\"cp-size-val\">' + currentRadius + '</span></div>' +
              '<input type=\"range\" id=\"cp-size\" min=\"2\" max=\"15\" value=\"' + currentRadius + '\" style=\"width:100%;margin:0\">' +
            '</div>',
            '<div>' +
              '<label style=\"cursor:pointer\">' +
                '<input type=\"checkbox\" id=\"cp-dark\" style=\"margin-right:6px\">' +
                'Dark mode' +
              '</label>' +
            '</div>'
          );
          div.innerHTML = panelParts.join('');

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
        clusterCb     = document.getElementById('cp-cluster');
        modalBtn      = document.getElementById('cp-modality-btn');
        modalList     = document.getElementById('cp-modality-list');
        var modalLabel = document.getElementById('cp-modality-label');
        var viewBtn   = document.getElementById('cp-view-btn');
        var viewList  = document.getElementById('cp-view-list');
        var viewLabel = document.getElementById('cp-view-label');
        categoryBlock = document.getElementById('cp-category-block');
        catAllLink    = document.getElementById('cp-cat-all');
        catNoneLink   = document.getElementById('cp-cat-none');
        categoryCheckboxes = categoryList.map(function(cat, i) {
          return document.getElementById('cp-cat-cb-' + i);
        });

        // applyView() ran once already (before these elements existed) to set
        // up the initial map layers; re-sync the Clustering/Modality/Category
        // lock visuals now that clusterCb/modalBtn/categoryBlock are wired up.
        setModalityEnabled(activeView === 'connections');
        setClusterEnabled(activeView !== 'neighborhoods');
        setCategoryFilterEnabled(activeView === 'sites');

        categoryList.forEach(function(cat, i) {
          var cb = categoryCheckboxes[i];
          if (cb) cb.addEventListener('change', function() {
            categoryFilter[cat] = this.checked;
            refreshMarkers();
          });
        });
        if (catAllLink) catAllLink.addEventListener('click', function(e) {
          e.preventDefault();
          categoryList.forEach(function(cat, i) {
            categoryFilter[cat] = true;
            if (categoryCheckboxes[i]) categoryCheckboxes[i].checked = true;
          });
          refreshMarkers();
        });
        if (catNoneLink) catNoneLink.addEventListener('click', function(e) {
          e.preventDefault();
          categoryList.forEach(function(cat, i) {
            categoryFilter[cat] = false;
            if (categoryCheckboxes[i]) categoryCheckboxes[i].checked = false;
          });
          refreshMarkers();
        });

        if (darkCb) {
          darkCb.checked = isDark;
          darkCb.addEventListener('change', function() {
            toggleDarkMode(this.checked);
          });
        }
        if (isDark) { applyDarkStyles(true); updateColors(); }
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

        // Custom view dropdown
        if (viewBtn) viewBtn.addEventListener('click', function(e) {
          e.stopPropagation();
          var open = viewList.style.display !== 'none';
          viewList.style.display = open ? 'none' : 'block';
        });
        if (viewList) viewList.querySelectorAll('[data-value]').forEach(function(opt) {
          opt.addEventListener('mouseover', function() { this.style.background = isDark ? '#2a2a2a' : '#f0f0f0'; });
          opt.addEventListener('mouseout',  function() { this.style.background = ''; });
          opt.addEventListener('click', function(e) {
            e.stopPropagation();
            applyView(this.dataset.value);
            viewLabel.textContent = this.textContent;
            viewList.style.display = 'none';
          });
        });

        document.addEventListener('click', function() {
          if (modalList) modalList.style.display = 'none';
          if (viewList) viewList.style.display = 'none';
        });
      }, 200);
    }
  ", data = js_data)
}
