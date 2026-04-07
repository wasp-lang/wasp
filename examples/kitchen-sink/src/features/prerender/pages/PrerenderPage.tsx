import { useSyncExternalStore } from "react";
import { FeatureContainer } from "../../../components/FeatureContainer";

export function PrerenderPage() {
  return (
    <FeatureContainer>
      <p data-testid="prerender-route">
        This route has <code>prerender: true</code>.
      </p>

      <p data-testid="prerender-with-useisclient">
        This content is rendered on the {useIsClient() ? "client" : "server"}.
      </p>

      {typeof window === "undefined" ? (
        <script>{`window.WAS_PRERENDERED = true`}</script>
      ) : null}
    </FeatureContainer>
  );
}

function useIsClient() {
  return useSyncExternalStore(
    isClientSubscribe,
    () => true,
    () => false,
  );
}

// This is a no-op subscribe function that doesn't actually subscribe to
// anything, but it satisfies the requirements of useSyncExternalStore. It has
// to be on the top-level so that it doesn't change between renders.
function isClientSubscribe() {
  return () => {};
}
