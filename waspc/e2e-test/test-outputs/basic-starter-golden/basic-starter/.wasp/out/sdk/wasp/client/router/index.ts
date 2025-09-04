import { interpolatePath } from './linkHelpers'
import type {
  RouteDefinitionsToRoutes,
  OptionalRouteOptions,
  ParamValue,
  ExpandRouteOnOptionalStaticSegments,
} from './types'

// PUBLIC API
export const routes = {
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
  RequestPasswordResetRoute: {
    to: "/request-password-reset",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/request-password-reset",
        undefined,
        options?.search,
        options?.hash
      ),
  },
  PasswordResetRoute: {
    to: "/password-reset",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/password-reset",
        undefined,
        options?.search,
        options?.hash
      ),
  },
  EmailVerificationRoute: {
    to: "/email-verification",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/email-verification",
        undefined,
        options?.search,
        options?.hash
      ),
  },
  TasksRoute: {
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
