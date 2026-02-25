 function(el, x) {
  el.on('plotly_hover', function(d) {
    var point = d.points[0];
    // check if the hovered trace is wp 'orgline'
    var traceData = el.data[point.curveNumber];
    var wproles = ['selwp', 'otherwp'];

    if (traceData.meta && wproles.includes(traceData.meta.role)) {
      // find the corresponding point on the sample depth trace
      var sdCurveNumber = el.data.findIndex(
        trace => trace.meta && trace.meta.role === 'sampledepth'
      );
      var xValue = point.x;
      var xData = el.data[sdCurveNumber].x;
      var dsPtNum = xData.indexOf(xValue);

      if (dsPtNum !== -1) {
        // re-do the hover on both subplots with the identified points
        Plotly.Fx.hover(el.id, [
          { curveNumber:point.curveNumber, pointNumber:point.pointNumber },
          { curveNumber:sdCurveNumber, pointNumber:dsPtNum }
        ], ['xy','xy2']);
      }
    }
  });
}
