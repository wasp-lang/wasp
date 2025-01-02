import { useRouteError } from 'react-router-dom'

import { FullPageWrapper } from './FullPageWrapper'

export function DefaultRootErrorBoundary() {
  const error = useRouteError()
  console.error(error)
  return (
    <FullPageWrapper>
      <div>
        There was an error rendering this page. Check the browser console for
        more information.
      </div>
    </FullPageWrapper>
  )
}
