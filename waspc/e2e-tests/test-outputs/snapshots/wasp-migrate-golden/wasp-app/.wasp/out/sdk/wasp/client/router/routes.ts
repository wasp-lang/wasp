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
