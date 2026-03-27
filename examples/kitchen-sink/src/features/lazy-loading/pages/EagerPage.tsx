import { FeatureContainer } from "../../../components/FeatureContainer";

if (typeof window !== "undefined") {
  (window as any).__EAGER_PAGE_LOADED__ = true;
  console.log("Eager page loaded");
}

export function EagerPage() {
  return (
    <FeatureContainer>
      <div data-testid="lazy-no-page">Eager page (lazy: false)</div>
    </FeatureContainer>
  );
}
