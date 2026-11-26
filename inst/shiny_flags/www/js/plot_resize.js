// resizable plot container (.plot-resizable, see flags_server.R main_plot_ui):
// keep the plotly plot in step with the container height and report a height
// the user dragged to (differs from data-default-height) to Shiny
$(function () {
  var observer = new ResizeObserver(function (entries) {
    entries.forEach(function (entry) {
      var box = entry.target;
      var height = Math.round(box.getBoundingClientRect().height);
      if (height === 0) return; // hidden, e.g. on another tab

      var plot = box.querySelector(".js-plotly-plot");
      if (plot && window.Plotly) Plotly.Plots.resize(plot);

      var defaultHeight = parseInt(box.dataset.defaultHeight, 10);
      if (Math.abs(height - defaultHeight) > 2) {
        Shiny.setInputValue(box.dataset.inputId, height);
      }
    });
  });

  // the container is rerendered with the plot, so observe each new one
  $(document).on("shiny:value", function (e) {
    if (e.name !== "flags-main_plot_ui") return;
    setTimeout(function () {
      document.querySelectorAll(".plot-resizable").forEach(function (box) {
        observer.observe(box);
      });
    }, 0);
  });
});
