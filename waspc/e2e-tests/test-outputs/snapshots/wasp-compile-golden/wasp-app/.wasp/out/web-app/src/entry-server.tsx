import React from 'react'
import { createMemoryRouter, matchRoutes, RouterProvider } from 'react-router'
import { renderToString } from 'react-dom/server'
import { QueryClientProvider } from '@tanstack/react-query'

import {
  initializeQueryClient,
  queryClientInitialized,
} from 'wasp/client/operations'

import { appRoutes, baseDir } from './routes'

initializeQueryClient()

function stripBase(pathname: string): string | null {
  if (baseDir === '/' || baseDir === '') {
    return pathname
  }
  const normalizedBase = baseDir.endsWith('/')
    ? baseDir.slice(0, -1)
    : baseDir
  if (pathname === normalizedBase) {
    return '/'
  }
  if (pathname.startsWith(normalizedBase + '/')) {
    return pathname.slice(normalizedBase.length)
  }
  return null
}

export function isSsrRoute(url: string): boolean {
  const pathname = new URL(url, 'http://localhost').pathname
  const strippedPathname = stripBase(pathname)
  if (!strippedPathname) {
    return false
  }

  const matches = matchRoutes(appRoutes, strippedPathname)
  return matches?.some((match) => match.route.handle?.ssr) ?? false
}

export async function render(url: string): Promise<string> {
  const queryClient = await queryClientInitialized
  queryClient.clear()

  const router = createMemoryRouter(appRoutes, {
    initialEntries: [url],
    basename: baseDir,
  })

  const app = (
    <QueryClientProvider client={queryClient}>
      <RouterProvider router={router} />
    </QueryClientProvider>
  )

  return renderToString(app)
}

export { baseDir }
