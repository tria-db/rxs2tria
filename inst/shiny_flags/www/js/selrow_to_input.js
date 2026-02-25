function(el, x, data) {
  var hot = this.hot;
  
  // set inital highlighted row
  hot._highlightedRow = data.selrow_idx;

  // remove previously attached hook if it exists
  if (hot._selectionHook) {
    hot.removeHook('afterSelection', hot._selectionHook);
  }

  // Define and store the hook
  hot._selectionHook = function(r, c, r2, c2) {
    Shiny.setInputValue(data.input_id, r + 1, {priority: 'event'});
  };

  hot.addHook('afterSelection', hot._selectionHook);
  hot.render();
}
