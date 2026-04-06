import { createElement } from "react";
import { FeatureContainer } from "../../../components/FeatureContainer";

const IS_BROWSER = typeof window !== "undefined";

export function PrerenderPage() {
  return (
    <FeatureContainer>
      <div data-testid="prerender-page">
        This page is prerendered at build time (prerender: true)
      </div>

      {createElement(
        "div",
        { suppressHydrationWarning: true },
        ...(IS_BROWSER
          ? []
          : [
              "You should only see this message if the page was prerendered at build time.",
            ]),
      )}
    </FeatureContainer>
  );
}
