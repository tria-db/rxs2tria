function(el, x, data) {
  var hot = this.hot;

  // layout of the flags table header and columns, depending on the space:
  // - the text column (data.text_col, comment) gets a share of the width
  //   (data.text_share, clamped to data.text_min / data.text_max)
  // - fixed-width columns (data.fixed_widths, e.g. affected_tissue) keep
  //   their width, or their name width if wider with horizontal headers
  // - all other (checkbox) columns share the rest equally, at least
  //   data.min_width
  // Headers are rotated (class hot-rotated, see custom-theme.scss) when the
  // checkbox column width is narrower than the longest checkbox column name.
  // The table is rebuilt on every image change, so the decision is cached and
  // only recomputed when the available width or the column names change.
  var cache = window.hotLayoutCache || (window.hotLayoutCache = {});

  function textWidth(text, font) {
    var canvas = textWidth.canvas ||
      (textWidth.canvas = document.createElement("canvas"));
    var ctx = canvas.getContext("2d");
    ctx.font = font;
    return ctx.measureText(text).width;
  }

  function computeLayout() {
    var width = el.clientWidth;
    var names = hot.getColHeader();
    var key = width + "|" + names.join(",");
    if (cache.key === key) return cache.layout;

    var th = el.querySelector(".ht_master thead th");
    var font = th ? getComputedStyle(th).font : "13px sans-serif";
    var rowHeader = el.querySelector(".ht_master tbody th"); // a year cell
    var rowHeaderWidth = rowHeader ? rowHeader.offsetWidth : 50;
    var slack = 2; // borders and rounding; the text column takes the rest
    var padding = 12;

    var nameWidths = names.map(function (name) {
      return Math.ceil(textWidth(name, font)) + padding;
    });
    var isCheckbox = names.map(function (name) {
      return name !== data.text_col && !(name in data.fixed_widths);
    });
    var nCheckbox = isCheckbox.filter(Boolean).length;
    var maxCheckboxName = Math.max.apply(null, nameWidths.filter(function (w, i) {
      return isCheckbox[i];
    }).concat(0));

    var available = width - rowHeaderWidth - slack;
    var textWidthPx = Math.min(data.text_max,
      Math.max(data.text_min, Math.round(available * data.text_share)));
    var fixedWidths = {};
    var fixedTotal = 0;
    names.forEach(function (name, i) {
      if (name in data.fixed_widths) {
        fixedWidths[name] = Math.max(data.fixed_widths[name], nameWidths[i]);
        fixedTotal += fixedWidths[name];
      }
    });
    var checkboxWidth = nCheckbox > 0 ?
      Math.floor((available - textWidthPx - fixedTotal) / nCheckbox) : 0;
    var rotated = checkboxWidth < maxCheckboxName;

    if (rotated) {
      // fixed columns at their set width, names are rotated
      fixedTotal = 0;
      Object.keys(fixedWidths).forEach(function (name) {
        fixedWidths[name] = data.fixed_widths[name];
        fixedTotal += fixedWidths[name];
      });
      checkboxWidth = nCheckbox > 0 ? Math.max(data.min_width,
        Math.floor((available - textWidthPx - fixedTotal) / nCheckbox)) : 0;
    }

    var colWidths = names.map(function (name) {
      if (name === data.text_col) return textWidthPx; // stretched, see below
      if (name in fixedWidths) return fixedWidths[name];
      return checkboxWidth;
    });

    var layout = {
      rotated: rotated,
      settings: {
        colWidths: colWidths,
        stretchH: "last", // text column (last) takes up rounding remainders
        columnHeaderHeight: rotated ? Math.max.apply(null, nameWidths) : 26
      }
    };

    cache.key = key;
    cache.layout = layout;
    return layout;
  }

  function applyLayout() {
    if (el.clientWidth === 0) return; // hidden, e.g. on another tab
    var layout = computeLayout();
    el.classList.toggle("hot-rotated", layout.rotated);
    hot.updateSettings(layout.settings);
  }

  applyLayout();

  // recompute when the available width changes (window size, sidebar)
  if (el._layoutObserver) el._layoutObserver.disconnect();
  var lastWidth = el.clientWidth;
  el._layoutObserver = new ResizeObserver(function () {
    if (el.clientWidth !== lastWidth) {
      lastWidth = el.clientWidth;
      applyLayout();
    }
  });
  el._layoutObserver.observe(el);
}
