import { useRouteError } from 'react-router'

import { reportClientError } from '../../errors'
import { FullPageWrapper } from './FullPageWrapper'

export function DefaultRootErrorBoundary() {
  const error = useRouteError()
  reportClientError(error, 'page rendering')
  return (
    <FullPageWrapper>
      <div>
        There was an error rendering this page. Check the browser console for
        more information.
      </div>
    </FullPageWrapper>
  )
}
