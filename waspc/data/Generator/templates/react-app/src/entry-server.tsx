import React from 'react'
import { createMemoryRouter, matchRoutes, RouterProvider } from 'react-router'
import { renderToString } from 'react-dom/server'
import { QueryClientProvider } from '@tanstack/react-query'
import { HelmetProvider } from 'react-helmet-async'
import type { HelmetServerState } from 'react-helmet-async'

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
  return getRouteMatchInfo(url).ssr
}

export function getRouteMatchInfo(url: string): {
  matched: boolean
  ssr: boolean
  outsideBase: boolean
} {
  const pathname = new URL(url, 'http://localhost').pathname
  const strippedPathname = stripBase(pathname)
  if (!strippedPathname) {
    return { matched: false, ssr: false, outsideBase: true }
  }

  const matches = matchRoutes(appRoutes, strippedPathname)
  return {
    matched: matches !== null,
    ssr: matches?.some((match) => match.route.handle?.ssr) ?? false,
    outsideBase: false,
  }
}

export async function render(url: string): Promise<{
  appHtml: string
  headHtml: string
}> {
  const queryClient = await queryClientInitialized
  queryClient.clear()

  const router = createMemoryRouter(appRoutes, {
    initialEntries: [url],
    basename: baseDir,
  })

  const helmetContext: { helmet?: HelmetServerState } = {}
  const app = (
    <HelmetProvider context={helmetContext}>
      <QueryClientProvider client={queryClient}>
        <RouterProvider router={router} />
      </QueryClientProvider>
    </HelmetProvider>
  )

  const appHtml = renderToString(app)
  const headHtml = renderHelmetToString(helmetContext.helmet)
  return { appHtml, headHtml }
}

function renderHelmetToString(helmet?: HelmetServerState): string {
  if (!helmet) {
    return ''
  }

  return [
    helmet.title?.toString(),
    helmet.base?.toString(),
    helmet.meta?.toString(),
    helmet.link?.toString(),
    helmet.script?.toString(),
    helmet.style?.toString(),
    helmet.noscript?.toString(),
  ]
    .filter(Boolean)
    .join('')
}

export { baseDir }
