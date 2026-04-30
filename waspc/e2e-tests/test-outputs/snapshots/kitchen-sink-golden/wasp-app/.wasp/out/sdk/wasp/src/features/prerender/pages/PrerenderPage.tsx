import { useEffect, useState } from "react";
import { FeatureContainer } from "../../../components/FeatureContainer";

export function PrerenderPage() {
  const [isClient, setIsClient] = useState(false);
  useEffect(() => setIsClient(true), []);

  return (
    <FeatureContainer>
      <p>
        This route has <code>prerender: true</code>.
      </p>

      <p>
        This content has been rendered on the{" "}
        <span data-testid="render-location">
          {isClient ? "client" : "server"}
        </span>
        .
      </p>
    </FeatureContainer>
  );
}
