import { useEffect } from "react";
import { useRouteError } from "react-router";

import { handleClientError } from "../errors.js";

import { FullPageWrapper } from "./FullPageWrapper";
import { ViteErrorDisplay } from "./ViteErrorDisplay";

export function DefaultRootErrorBoundary() {
  const error = useRouteError();
  // Rendering has to stay free of side effects, because React can call a
  // component more than once before it commits: always under `StrictMode` in
  // development, and in production whenever it discards a render. An effect
  // runs once per committed render.
  // https://react.dev/learn/keeping-components-pure
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
