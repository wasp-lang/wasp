import { useLocation } from "react-router-dom";
import { FeatureContainer } from "../components/FeatureContainer";

export function CatchAllPage() {
  const location = useLocation();

  return (
    <FeatureContainer>
      <div className="card grid place-items-center">
        <h1 className="mb-4 text-2xl font-bold" data-testid="not-found-title">
          Not found
        </h1>
        <p className="text-gray-500" data-testid="not-found-message">
          We couldn't find anything at the{" "}
          <code className="rounded bg-gray-200 px-2 font-mono text-gray-700">
            {location.pathname}
          </code>{" "}
          location.
        </p>
      </div>
    </FeatureContainer>
  );
}
