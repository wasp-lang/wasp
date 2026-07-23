// Scrolls the docs sidebar so the currently-active page is visible on load.
//
// When a user opens a docs page from an external link (e.g.
// wasp.sh/docs/advanced/email), the sidebar is rendered but the
// "Where am I?" indicator is often scrolled out of view — you only find
// your location after manually scrolling the sidebar. This script finds
// the active sidebar link and scrolls the nearest scrollable sidebar
// container so it's visible. Runs only on docs pages (where the sidebar
// is rendered); no-op elsewhere.
//
// Fixes issue #4507.

(function () {
  function scrollActiveIntoView() {
    const doc = document;
    // Docusaurus marks the active sidebar link with `aria-current="page"`
    // and adds the `menu__link--active` class to the link. We look for the
    // *parent* `<li>` (or the link itself) and scroll its nearest
    // scrollable ancestor.
    const activeLink = doc.querySelector(
      '.theme-doc-sidebar-container a.menu__link--active[aria-current="page"]',
    );
    if (!activeLink) return;

    // Prefer the parent <li> for the scroll target — the link itself is
    // short, and the highlight background usually extends to the <li>.
    const scrollTarget = activeLink.closest("li") || activeLink;

    // Find the nearest scrollable ancestor (the sidebar pane, not the
    // page). Docusaurus uses `.theme-doc-sidebar-container` as the
    // scrollable element on the doc layout.
    const scrollContainer = scrollTarget.closest(
      ".theme-doc-sidebar-container",
    );
    if (!scrollContainer) return;

    // Compute the target's offset relative to the scroll container and
    // scroll just enough to bring it into view, with a small margin so it
    // doesn't sit flush against the top edge.
    const containerRect = scrollContainer.getBoundingClientRect();
    const targetRect = scrollTarget.getBoundingClientRect();
    const offset = targetRect.top - containerRect.top;
    const MARGIN = 16;

    // If the active item is already comfortably visible, do nothing.
    if (offset >= MARGIN && offset + targetRect.height <= containerRect.height - MARGIN) {
      return;
    }

    // Otherwise, scroll so the item sits MARGIN pixels from the top of
    // the visible sidebar area.
    scrollContainer.scrollBy({
      top: offset - MARGIN,
      behavior: "instant", // no animation on initial load
    });
  }

  // Run after the DOM is ready and the sidebar has been hydrated.
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", scrollActiveIntoView);
  } else {
    scrollActiveIntoView();
  }
  // Also run after the initial Docusaurus route transition (client-side
  // navigation): the sidebar DOM changes when the user clicks a link.
  // We watch the document for the active class to appear.
  const observer = new MutationObserver(() => {
    const active = document.querySelector(
      '.theme-doc-sidebar-container a.menu__link--active[aria-current="page"]',
    );
    if (active) {
      // Debounce: the DOM may still be settling for a few frames.
      clearTimeout(observer._t);
      observer._t = setTimeout(scrollActiveIntoView, 50);
    }
  });
  observer.observe(document.body, {
    subtree: true,
    childList: true,
    attributes: true,
    attributeFilter: ["class", "aria-current"],
  });
})();
