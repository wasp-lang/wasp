import { FeatureContainer } from "../../../components/FeatureContainer";

export function HydrationMismatchPage() {
  const isServer = typeof window === "undefined";

  return (
    <FeatureContainer>
      <p data-testid="hydration-mismatch-content">
        Rendered on the {isServer ? "server" : "client"}.
      </p>
    </FeatureContainer>
  );
}
