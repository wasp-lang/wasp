import { useEffect } from "react";
import { useRouteError } from "react-router";

import { handleClientError } from "../errors.js";

import { FullPageWrapper } from "./FullPageWrapper";
import { ViteErrorDisplay } from "./ViteErrorDisplay";

export function DefaultRootErrorBoundary() {
  const error = useRouteError();
  // React may render this more than once per failure, so we report from an
  // effect, which only runs for the render React commits.
  useEffect(() => {
    handleClientError(error, { source: "render" });
  }, [error]);
  return (
    <FullPageWrapper>
      <div>
        There was an error rendering this page. Check the browser console for
        more information.
        {import.meta.env.DEV && <ViteErrorDisplay outerError={error} />}
      </div>
    </FullPageWrapper>
  );
}
