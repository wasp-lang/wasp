import aiReadyDark from "../client/static/assets/aiready-dark.webp";
import aiReady from "../client/static/assets/aiready.webp";
import { HighlightedFeature } from "./components/HighlightedFeature";

export function AIReady() {
  return (
    <HighlightedFeature
      name="Example Feature Highlight"
      description="Yo! Use this component to show off the most important features in your app."
      highlightedComponent={<AIReadyExample />}
      direction="row-reverse"
    />
  );
}

function AIReadyExample() {
  return (
    <div className="w-full">
      <img
        src={aiReady}
        alt="AI Ready"
        loading="lazy"
        className="dark:hidden"
      />
      <img
        src={aiReadyDark}
        alt="AI Ready"
        loading="lazy"
        className="hidden dark:block"
      />
    </div>
  );
}
