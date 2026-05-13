HTMLWidgets.widget({
  name: "statgl_table",
  type: "output",
  factory: function (el, width, height) {
    return {
      renderValue: function (x) {
        el.innerHTML = x.html;
      },
      resize: function (width, height) {
        // The table sizing is CSS-driven (bootstrap + .hide_mobile media
        // query); no JS-side resize logic needed.
      }
    };
  }
});
