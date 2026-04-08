import { useEffect, useState } from "react";
import { FeatureContainer } from "../../../components/FeatureContainer";

export function PrerenderPage() {
  const [isClient, setIsClient] = useState(false);
  useEffect(() => setIsClient(true), []);

  return (
    <FeatureContainer>
      <p data-testid="prerender-route">
        This route has <code>prerender: true</code>.
      </p>

      <p data-testid="prerender-with-useisclient">
        This content is rendered on the {isClient ? "client" : "server"}.
      </p>
    </FeatureContainer>
  );
}
