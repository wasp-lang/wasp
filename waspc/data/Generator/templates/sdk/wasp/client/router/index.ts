{{={= =}=}}
import { interpolatePath } from './linkHelpers'
import type {
  RouteDefinitionsToRoutes,
  OptionalRouteOptions,
  ParamValue,
  ExpandRouteOnOptionalStaticSegments,
} from './types'

// PUBLIC API
export const routes = {
  {=# routes =}
  {= name =}: {
    to: "{= urlPath =}",
    build: (
      options{=^ hasUrlParams =}?{=/ hasUrlParams =}:
      OptionalRouteOptions
      {=#  hasUrlParams =}& {
        params: {{=# urlParams =}{= name =}{=# isOptional =}?{=/ isOptional =}: ParamValue;{=/ urlParams =}}
      }{=/ hasUrlParams =}
      {=# hasOptionalStaticSegments =}& { path: ExpandRouteOnOptionalStaticSegments<"{= urlPath =}"> }{=/ hasOptionalStaticSegments =}
    ) => interpolatePath({=# hasOptionalStaticSegments =}options.path{=/ hasOptionalStaticSegments =}{=^ hasOptionalStaticSegments =}"{= urlPath =}"{=/ hasOptionalStaticSegments =}, {=^ hasUrlParams =}undefined{=/ hasUrlParams =}{=# hasUrlParams =}options.params{=/ hasUrlParams =}, options?.search, options?.hash),
  },
  {=/ routes =}
} as const;

// PRIVATE API
export type Routes = RouteDefinitionsToRoutes<typeof routes>

// PUBLIC API
export { Link } from './Link'
