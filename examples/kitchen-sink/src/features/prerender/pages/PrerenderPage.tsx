import { FeatureContainer } from "../../../components/FeatureContainer";

export function PrerenderPage() {
  return (
    <FeatureContainer>
      <div data-testid="prerender-page">
        This page is prerendered at build time (prerender: true)
      </div>
    </FeatureContainer>
  );
}
