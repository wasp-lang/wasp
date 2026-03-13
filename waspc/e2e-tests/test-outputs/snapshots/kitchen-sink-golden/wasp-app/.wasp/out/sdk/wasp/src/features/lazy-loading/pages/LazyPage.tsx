import { FeatureContainer } from "../../../components/FeatureContainer";

(window as any).__LAZY_PAGE_LOADED__ = true;
console.log("Lazy page loaded");

export function LazyPage() {
  return (
    <FeatureContainer>
      <div data-testid="lazy-yes-page">Lazy page (lazy: true)</div>
    </FeatureContainer>
  );
}
