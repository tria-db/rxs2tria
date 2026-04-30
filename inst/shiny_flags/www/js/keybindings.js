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
    ".selectize-control",
    ".selectize-dropdown",
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

  // clean up on unload so listeners don't bleed into next app on same port
  $(window).on("unload", function () {
    $(document).off("keydown.flags");
  });

  // keybindings
  $(document).on("keydown.flags", function (e) {
    // Arrow keys: left/right = prev/next image, up/down = prev/next ring
    // (mirrors HOT navigation direction; HOT handles its own arrow keys when focused)
    if (e.key === "ArrowLeft" || e.key === "ArrowRight" || e.key === "ArrowUp" || e.key === "ArrowDown") {
      if (!isInteractiveElement(document.activeElement)) {
        e.preventDefault();
        if (e.key === "ArrowLeft") {
          Shiny.setInputValue("flags-prev_img", Math.random());
        } else if (e.key === "ArrowRight") {
          Shiny.setInputValue("flags-next_img", Math.random());
        } else if (e.key === "ArrowUp") {
          Shiny.setInputValue("flags-prev_ring", Math.random());
        } else {
          Shiny.setInputValue("flags-next_ring", Math.random());
        }
      }
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