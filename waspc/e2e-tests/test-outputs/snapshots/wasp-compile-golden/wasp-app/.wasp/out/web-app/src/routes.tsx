import React from 'react'
import type { RouteObject } from 'react-router'
import { routes } from 'wasp/client/router'

import { MainPage } from '../../../../src/MainPage'

import { DefaultRootErrorBoundary } from './components/DefaultRootErrorBoundary'

export const baseDir: string = '/'

export const routeNameToRouteComponent = {
  RootRoute: MainPage,
} as const;

export const routeNameToSsr = {
  RootRoute: false,
} as const;

type RouteName = keyof typeof routeNameToRouteComponent

const waspDefinedRoutes: RouteObject[] = [
]

const userDefinedRoutes: RouteObject[] = Object.entries(routes).map(([routeKey, route]) => {
  const name = routeKey as RouteName
  return {
    path: route.to,
    Component: routeNameToRouteComponent[name],
    handle: { ssr: routeNameToSsr[name] ?? false },
  }
})

export const appRoutes: RouteObject[] = [
  {
    path: '/',
    ErrorBoundary: DefaultRootErrorBoundary,
    children: [
      ...waspDefinedRoutes,
      ...userDefinedRoutes,
    ],
  },
]
