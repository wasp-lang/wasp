import { useRouteError } from "react-router";

import { FullPageWrapper } from "./FullPageWrapper";
import { ViteErrorDisplay } from "./ViteErrorDisplay";

export function DefaultRootErrorBoundary() {
  const error = useRouteError();
  console.error(error);
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
