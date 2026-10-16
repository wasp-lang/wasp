// Queues custom events until the Plausible script loads and sends them.
// https://plausible.io/docs/custom-event-goals
window.plausible =
  window.plausible ||
  function () {
    (window.plausible.q = window.plausible.q || []).push(arguments);
  };
