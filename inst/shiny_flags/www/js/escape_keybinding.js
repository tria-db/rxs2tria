function(el, x) {
  var hot = this.hot;

  // remove previously attached hook if it exists
  if (hot._escapeKeyHook) {
    hot.removeHook('afterDocumentKeyDown', hot._escapeKeyHook);
  }

  // define escape key hook
  hot._escapeKeyHook = function(event) {
    if (event.key === 'Escape' || event.keyCode === 27) {
      // deselect all cells
      hot.deselectCell();
      // blur active element to remove focus
      if (document.activeElement) {
        document.activeElement.blur();
      }
      event.preventDefault();
      event.stopImmediatePropagation();
      return false;
    }
  };

  // add hook
  hot.addHook('afterDocumentKeyDown', hot._escapeKeyHook);
}
