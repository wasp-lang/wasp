import React from 'react'
import { createBrowserRouter, RouterProvider } from 'react-router'

import { appRoutes, baseDir } from './routes'

const browserRouter = createBrowserRouter(appRoutes, {
  basename: baseDir,
})

export const router = <RouterProvider router={browserRouter} />
