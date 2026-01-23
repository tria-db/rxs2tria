function(el, x, inputName){

    // Function to extract trace information
    function extractTraceInfo() {
      var out = {};
      // Use both el.data (original) and el._fullData (processed)
      el.data.forEach(function(trace, traceindex) {
        var fullTrace = el._fullData[traceindex];
        out[fullTrace.name] = {
          curveNumber:  traceindex,
          opacity:  fullTrace.opacity !== undefined ? fullTrace.opacity : 1,
          visible: fullTrace.visible !== undefined ? fullTrace.visible : true,
          meta: trace.meta !== undefined ? trace.meta : (fullTrace.meta !== undefined ? fullTrace.meta : null)
        };
      });
      return out;
    }

    // Helper to update Shiny input
    function updateShinyInput() {
      Shiny.setInputValue(inputName, extractTraceInfo());
    }

    // Initial state (after plot is fully rendered)
    el.on('plotly_afterplot', function() {
      console.log('Plot rendered, updating trace info');
      updateShinyInput();
    });

    // When traces are restyled (legend clicks, opacity changes)
    el.on('plotly_restyle', function(evtData) {
      console.log('Plot restyled');
      updateShinyInput();
    });
  }
  