import { type RouteDefinitionsToRoutes } from '@wasp/lib-sdk-core'
import { routes } from './routes'

// PUBLIC API
export { routes }

// PRIVATE API
export type Routes = RouteDefinitionsToRoutes<typeof routes>

// PUBLIC API
export { Link, type LinkProps } from './Link'
// PUBLIC API
export { NavLink, type NavLinkProps } from './NavLink'
