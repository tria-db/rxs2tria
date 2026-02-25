function(el, x, inputName) {
  // Function to extract trace information
  function extractTraceInfo() {
    var out = {};
    el.data.forEach(function(trace, traceindex) {
      var fullTrace = el._fullData[traceindex];
      out[fullTrace.name] = {
        curveNumber: traceindex,
        opacity: fullTrace.opacity !== undefined ? fullTrace.opacity : 1,
        visible: fullTrace.visible !== undefined ? fullTrace.visible : true,
        meta: trace.meta !== undefined ? trace.meta : (fullTrace.meta !== undefined ? fullTrace.meta : null)
      };
    });
    return out;
  }

  // Helper to update Shiny input
  function updateShinyInput() {
    Shiny.setInputValue(inputName, extractTraceInfo(), {priority: 'event'});
  }

  // Remove old listeners if they exist
  if (el._traceTrackerHandlers) {
    el.removeListener('plotly_afterplot', el._traceTrackerHandlers.afterplot);
    el.removeListener('plotly_restyle', el._traceTrackerHandlers.restyle);
  }

  // Define handlers
  var afterplotHandler = function() {
    console.log('Plot rendered, updating trace info');
    updateShinyInput();
  };

  var restyleHandler = function(evtData) {
    console.log('Plot restyled');
    updateShinyInput();
  };

  // Store references for cleanup on next render
  el._traceTrackerHandlers = {
    afterplot: afterplotHandler,
    restyle: restyleHandler
  };

  // Attach listeners
  el.on('plotly_afterplot', afterplotHandler);
  el.on('plotly_restyle', restyleHandler);
}
