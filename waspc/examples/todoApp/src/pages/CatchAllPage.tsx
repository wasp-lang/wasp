import { useLocation } from "react-router-dom";
import { FeatureContainer } from "../components/FeatureContainer";

export function CatchAllPage() {
  const location = useLocation();

  return (
    <FeatureContainer>
      <div className="grid place-items-center card">
        <h1 className="text-2xl font-bold mb-4" data-testid="not-found-title">
          Not found
        </h1>
        <p className="text-gray-500" data-testid="not-found-message">
          We couldn't find anything at the{" "}
          <code className="text-gray-700 font-mono bg-gray-200 rounded px-2">
            {location.pathname}
          </code>{" "}
          location.
        </p>
      </div>
    </FeatureContainer>
  );
}
