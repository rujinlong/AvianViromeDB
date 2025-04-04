$( document ).ready(function() {
  Shiny.addCustomMessageHandler('fun', function(arg) {

  })
});

// AvianViromeDB custom event handlers

// Custom input binding for special inputs
Shiny.inputBindings.register(new function() {
  return {
    find: function(scope) {
      return $(scope).find('.custom-input');
    },
    getValue: function(el) {
      return $(el).val();
    },
    setValue: function(el, value) {
      $(el).val(value);
    },
    subscribe: function(el, callback) {
      $(el).on('change.custom-input', function(e) {
        callback();
      });
    },
    unsubscribe: function(el) {
      $(el).off('.custom-input');
    }
  };
}());
