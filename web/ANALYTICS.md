# Website analytics

The website sends a few custom events to Plausible (every visitor, no cookies) and to PostHog (only after a visitor accepts cookies).

- `src/lib/analytics.ts`: `track()`, and the types of every event and property value. Start here.
- `src/clientModules/track.ts`: one listener for the whole site. It counts code block copies, text copied by hand, and clicks on Discord, GitHub and Open SaaS links.
- `src/lib/analytics-classifier.ts`: decides which event a copied code block is.
- `static/scripts/plausible-queue.js`: holds events until the Plausible script loads.

In the markup, `data-track-placement` on a container sets where on the page its events happen. `data-track-event` sets the event of an element, for example `"Get Started: Click"`, and `data-track-kind` sets its `kind`.

To test, run `npm start`: each event prints `[analytics] <event> <props>` in the browser console. In a production build, run `localStorage.setItem("wasp-analytics-debug", "true")` first.
