// unsaved changes indicator (see flags_server.R): while there are unsaved
// edits, a dot on the save button (.has-unsaved), a "● " title prefix and a
// leave-page guard
$(function () {
  var baseTitle = document.title;
  var unsaved = false;

  window.addEventListener("beforeunload", function (e) {
    if (!unsaved) return;
    e.preventDefault();
    e.returnValue = ""; // older browsers
  });

  Shiny.addCustomMessageHandler("unsaved", function (msg) {
    unsaved = msg.unsaved;
    $("#" + msg.id).toggleClass("has-unsaved", unsaved);
    document.title = (unsaved ? "● " : "") + baseTitle;
  });
});
