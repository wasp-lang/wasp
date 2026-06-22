import { useEffect, useState } from "react";
import { useParams } from "react-router";
import { FeatureContainer } from "../../../components/FeatureContainer";

export function PrerenderInstancesPage() {
  const { slug } = useParams<{ slug: string }>();
  const [isClient, setIsClient] = useState(false);
  useEffect(() => setIsClient(true), []);

  return (
    <FeatureContainer>
      <p>
        This dynamic route prerenders specific instances by listing concrete
        paths in <code>prerender</code>.
      </p>

      <p>
        Slug: <span data-testid="slug">{slug}</span>
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
