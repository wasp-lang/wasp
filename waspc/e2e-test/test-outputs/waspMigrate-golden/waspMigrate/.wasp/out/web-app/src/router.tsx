import React from 'react'
import { createBrowserRouter, RouterProvider } from 'react-router-dom'


import { MainPage } from '../../../../src/MainPage'


import { routes } from 'wasp/client/router'

export const routeNameToRouteComponent = {
  RootRoute: MainPage,
} as const;

const waspDefinedRoutes = [
]
const userDefinedRoutes = Object.entries(routes).map(([routeKey, route]) => {
  return {
    path: route.to,
    Component: routeNameToRouteComponent[routeKey],
  }
})

const browserRouter = createBrowserRouter([{
  path: '/',
  children: [
    ...waspDefinedRoutes,
    ...userDefinedRoutes,
  ],
}])


export const router = <RouterProvider router={browserRouter} />
