(function() {
  let isDirty = false;

  Shiny.addCustomMessageHandler('dirty-state', function(msg) {
    isDirty = !!(msg && msg.is_dirty);
  });

  window.addEventListener('beforeunload', function(e) {
    if (!isDirty) return;
    e.preventDefault();
    e.returnValue = '';
  });
})();
