import { FeatureContainer } from "../../../components/FeatureContainer";

export function HydrationMismatchPage() {
  const isServer = typeof window === "undefined";

  return (
    <FeatureContainer>
      <p>
        This route has <code>prerender: true</code> and should trigger a
        hydration mismatch warning.
      </p>

      <p>
        Rendered on the
        <span data-testid="render-location">
          {isServer ? "server" : "client"}
        </span>
        .
      </p>
    </FeatureContainer>
  );
}
