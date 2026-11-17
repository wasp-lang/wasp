import { useEffect } from "react";
import { useRouteError } from "react-router";

import { handleClientError } from "../errors.js";

import { FullPageWrapper } from "./FullPageWrapper";
import { ViteErrorDisplay } from "./ViteErrorDisplay";

export function DefaultRootErrorBoundary() {
  const error = useRouteError();
  // An effect makes sure we report the error at most once per committed render.
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
