{{={= =}=}}
import { interpolatePath } from '../../../core/client/router/linkHelpers.js'
import type {
  RouteDefinitionsToRoutes,
  OptionalRouteOptions,
  ParamValue,
  ExpandRouteOnOptionalStaticSegments,
} from '../../../core/client/router/types.js'

// PUBLIC API
export const routes = {
  {=# routes =}
  {= name =}: {
    to: "{= urlPath =}",
    {=# hasUrlParams =}
    build: (
      options: OptionalRouteOptions
      & { params: {{=# urlParams =}"{= name =}"{=# isOptional =}?{=/ isOptional =}: ParamValue;{=/ urlParams =}}}
      {=# hasOptionalStaticSegments =}
      & { path: ExpandRouteOnOptionalStaticSegments<"{= urlPath =}"> }
      {=/ hasOptionalStaticSegments =}
    ) => interpolatePath(
        {=# hasOptionalStaticSegments =}options.path,{=/ hasOptionalStaticSegments =}
        {=^ hasOptionalStaticSegments =}"{= urlPath =}",{=/ hasOptionalStaticSegments =}
        options.params,
        options?.search,
        options?.hash
      ),
    {=/ hasUrlParams =}
    {=^ hasUrlParams =}
    build: (
      options?:
      OptionalRouteOptions
      {=# hasOptionalStaticSegments =}
      & { path: ExpandRouteOnOptionalStaticSegments<"{= urlPath =}"> }
      {=/ hasOptionalStaticSegments =}
    ) => interpolatePath(
        {=# hasOptionalStaticSegments =}options.path,{=/ hasOptionalStaticSegments =}
        {=^ hasOptionalStaticSegments =}"{= urlPath =}",{=/ hasOptionalStaticSegments =}
        undefined,
        options?.search,
        options?.hash
      ),
    {=/ hasUrlParams =}
  },
  {=/ routes =}
} as const;

// PRIVATE API
export type Routes = RouteDefinitionsToRoutes<typeof routes>

// PUBLIC API
export { Link } from './Link'
