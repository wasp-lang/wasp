{{={= =}=}}
import { interpolatePath } from './linkHelpers'
import type {
  RouteDefinitionsToRoutes,
  OptionalRouteOptions,
  ParamValue,
} from './types'

export const routes = {
  {=# routes =}
  {= name =}: {
    to: "{= urlPath =}",
    {=#  hasUrlParams =}
    build: (
      options: {
        params: {{=# urlParams =}{= name =}{=# isOptional =}?{=/ isOptional =}: ParamValue;{=/ urlParams =}}
      } & OptionalRouteOptions,
    ) => interpolatePath("{= urlPath =}", options.params, options?.search, options?.hash),
    {=/ hasUrlParams =}
    {=^ hasUrlParams =}
    build: (
      options?: OptionalRouteOptions,
    ) => interpolatePath("{= urlPath =}", undefined, options?.search, options?.hash),
    {=/ hasUrlParams =}
  },
  {=/ routes =}
} as const;

export type Routes = RouteDefinitionsToRoutes<typeof routes>

export { Link } from './Link'
