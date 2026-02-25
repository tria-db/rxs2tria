$(document).ready(function () {
  var interactiveSelectors = [
    "input",
    "textarea",
    "select",
    "button",
    "a",
  ];

  var interactiveContainers = [
    ".handsontable",
    ".shiny-input-container",
    ".btn",
    '[role="button"]',
  ];

  // helper: enter key should only be caught if not currently
  // focused on interactive element
  function isInteractiveElement(el) {
    if (el === document.body) return false;

    var tag = el.tagName.toLowerCase();
    if (interactiveSelectors.indexOf(tag) !== -1) return true;

    for (var i = 0; i < interactiveContainers.length; i++) {
      if (el.closest(interactiveContainers[i])) return true;
    }

    return false;
  }

  // keybindings
  $(document).on("keydown", function (e) {
    // Arrow keys (left/right): trigger prev/next ring buttons
    // NOTE: seems that hot automatically overwrites when table 
    // in focus?
    if (e.key === "ArrowLeft") {
      e.preventDefault();
      console.log("Left arrow caught!");
      Shiny.setInputValue("flags-prev_ring", Math.random());
    } else if (e.key === "ArrowRight") {
      e.preventDefault();
      Shiny.setInputValue("flags-next_ring", Math.random());
    }

    // Enter key: trigger event (to select cell in hot)
    // iff not in an interactive element
    if (e.key === "Enter" || e.keyCode === 13) {
      if (!isInteractiveElement(document.activeElement)) {
        Shiny.setInputValue("flags-enter_key", Math.random(), {
          priority: "event",
        });
        e.preventDefault();
      }
    }
  });
});