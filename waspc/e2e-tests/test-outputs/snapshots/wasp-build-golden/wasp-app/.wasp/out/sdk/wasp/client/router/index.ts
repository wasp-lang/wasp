import {
  interpolatePath,
  type RouteDefinitionsToRoutes,
  type OptionalRouteOptions,
  type ParamValue,
  type ExpandRouteOnOptionalStaticSegments,
} from '@wasp.sh/lib-sdk-core'

// PUBLIC API
export const routes = {
  RootRoute: {
    to: "/",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/",
        undefined,
        options?.search,
        options?.hash
      ),
  },
} as const;

// PRIVATE API
export type Routes = RouteDefinitionsToRoutes<typeof routes>

// PUBLIC API
export { Link } from './Link'
// PUBLIC API
export { NavLink } from './NavLink'
