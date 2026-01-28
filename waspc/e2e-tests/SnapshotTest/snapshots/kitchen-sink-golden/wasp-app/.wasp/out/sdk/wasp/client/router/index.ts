import { interpolatePath } from './linkHelpers'
import type {
  RouteDefinitionsToRoutes,
  OptionalRouteOptions,
  ParamValue,
  ExpandRouteOnOptionalStaticSegments,
} from './types'

// PUBLIC API
export const routes = {
  HomeRoute: {
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
  CatchAllRoute: {
    to: "*",
    build: (
      options: OptionalRouteOptions
      & { params: {"*": ParamValue;}}
    ) => interpolatePath(
        
        "*",
        options.params,
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
    to: "/email-verification-",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/email-verification-",
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
  ProfileRoute: {
    to: "/profile",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/profile",
        undefined,
        options?.search,
        options?.hash
      ),
  },
  ManualSignupRoute: {
    to: "/manual-signup",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/manual-signup",
        undefined,
        options?.search,
        options?.hash
      ),
  },
  CustomSignupRoute: {
    to: "/custom-signup",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/custom-signup",
        undefined,
        options?.search,
        options?.hash
      ),
  },
  TasksRoute: {
    to: "/tasks",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/tasks",
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
  SerializationRoute: {
    to: "/serialization",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/serialization",
        undefined,
        options?.search,
        options?.hash
      ),
  },
  JobsRoute: {
    to: "/jobs",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/jobs",
        undefined,
        options?.search,
        options?.hash
      ),
  },
  ApisRoute: {
    to: "/apis",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/apis",
        undefined,
        options?.search,
        options?.hash
      ),
  },
  CrudListRoute: {
    to: "/crud",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/crud",
        undefined,
        options?.search,
        options?.hash
      ),
  },
  CrudDetailRoute: {
    to: "/crud/:id",
    build: (
      options: OptionalRouteOptions
      & { params: {"id": ParamValue;}}
    ) => interpolatePath(
        
        "/crud/:id",
        options.params,
        options?.search,
        options?.hash
      ),
  },
  StreamingRoute: {
    to: "/streaming",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/streaming",
        undefined,
        options?.search,
        options?.hash
      ),
  },
  ChatRoute: {
    to: "/chat",
    build: (
      options?:
      OptionalRouteOptions
    ) => interpolatePath(
        
        "/chat",
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
