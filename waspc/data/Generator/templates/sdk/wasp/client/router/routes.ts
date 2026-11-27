{{={= =}=}}
// The app's routes. No React here, so code that doesn't render (like the
// server) can import them.
import {
  interpolatePath,
  type OptionalRouteOptions,
  type ParamValue,
  type ExpandRouteOnOptionalStaticSegments,
} from '@wasp/lib-sdk-core'

// PUBLIC API
export const routes = {
  {=# routes =}
  {= name =}: {
    to: {=& urlPath =},
    {=# hasUrlParams =}
    build: (
      options: OptionalRouteOptions
      & { params: {{=# urlParams =}{=& name =}{=# isOptional =}?{=/ isOptional =}: ParamValue;{=/ urlParams =}}}
      {=# hasOptionalStaticSegments =}
      & { path: ExpandRouteOnOptionalStaticSegments<{=& urlPath =}> }
      {=/ hasOptionalStaticSegments =}
    ) => interpolatePath(
        {=# hasOptionalStaticSegments =}options.path,{=/ hasOptionalStaticSegments =}
        {=^ hasOptionalStaticSegments =}{=& urlPath =},{=/ hasOptionalStaticSegments =}
        options.params,
        options?.search,
        options?.hash
      ),
    {=/ hasUrlParams =}
    {=^ hasUrlParams =}
    build: (
      options{=^ hasOptionalStaticSegments =}?{=/ hasOptionalStaticSegments =}:
      OptionalRouteOptions
      {=# hasOptionalStaticSegments =}
      & { path: ExpandRouteOnOptionalStaticSegments<{=& urlPath =}> }
      {=/ hasOptionalStaticSegments =}
    ) => interpolatePath(
        {=# hasOptionalStaticSegments =}options.path,{=/ hasOptionalStaticSegments =}
        {=^ hasOptionalStaticSegments =}{=& urlPath =},{=/ hasOptionalStaticSegments =}
        undefined,
        options?.search,
        options?.hash
      ),
    {=/ hasUrlParams =}
  },
  {=/ routes =}
} as const;
