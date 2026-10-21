// Holds `plausible()` calls until the Plausible script loads. On load, the
// script sends each call in `window.plausible.q`.
window.plausible =
  window.plausible ||
  function () {
    (window.plausible.q = window.plausible.q || []).push(arguments);
  };
