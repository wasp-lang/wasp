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
  TaskRoute: {
    to: "/tasks/:id",
    build: (
      options: OptionalRouteOptions
      & { params: {"id": ParamValue;}}
    ) => interpolatePath(
        
        "/tasks/:id",
        options.params,
        options?.search,
        options?.hash
      ),
  },
  LoginRoute: {
    to: "/login",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/login",
        undefined,
        options?.search,
        options?.hash
      ),
  },
  SignupRoute: {
    to: "/signup",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/signup",
        undefined,
        options?.search,
        options?.hash
      ),
  },
  McpConsentRoute: {
    to: "/connect",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/connect",
        undefined,
        options?.search,
        options?.hash
      ),
  },
  McpConfirmRoute: {
    to: "/confirm",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/confirm",
        undefined,
        options?.search,
        options?.hash
      ),
  },
  ConnectionsRoute: {
    to: "/connections",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/connections",
        undefined,
        options?.search,
        options?.hash
      ),
  },
} as const;
