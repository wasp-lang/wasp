import { useRouteError } from 'react-router'

import { reportUnexpectedClientError } from "../../errors";
import { FullPageWrapper } from './FullPageWrapper'

export function DefaultRootErrorBoundary() {
  const error = useRouteError()
  reportUnexpectedClientError(error, { source: "pageRender" });
  return (
    <FullPageWrapper>
      <div>
        There was an error rendering this page. Check the browser console for
        more information.
      </div>
    </FullPageWrapper>
  )
}
