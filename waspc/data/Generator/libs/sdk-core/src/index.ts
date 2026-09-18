export {
  createProviderId,
  getProviderData,
  getProviderDataWithPassword,
  normalizeProviderUserId,
  providerDataHasPasswordField,
  type EmailProviderData,
  type OAuthProviderData,
  type PossibleProviderData,
  type ProviderId,
  type ProviderName,
  type UsernameProviderData,
} from "./auth/providerData.js";
export {
  SessionResponseSchema,
  SuccessResponseSchema,
} from "./auth/responseSchemas.js";

export {
  PASSWORD_FIELD,
  ensurePasswordIsPresent,
  ensureTokenIsPresent,
  ensureValidEmail,
  ensureValidPassword,
  ensureValidUsername,
  throwValidationError,
} from "./auth/validation.js";
export { type ClientConfig } from "./config.js";
export { defineEnvValidationSchema } from "./env/schema.js";
export {
  ensureEnvSchema,
  formatZodEnvError,
  getValidatedEnvOrError,
} from "./env/validation.js";
export { HttpMethod, type Route } from "./http.js";
export { HttpError } from "./HttpError.js";
export { Job, SubmittedJob } from "./jobs.js";
export {
  makeOperationRoute,
  type OperationRoute,
} from "./operations/routes.js";
export {
  type Action,
  type GenericBackendOperation,
  type GenericOperationRpc,
  type OperationRpcFor,
  type Query,
  type QueryFunction,
  type QueryMetadata,
} from "./operations/rpc.js";
export {
  type FromRegister,
  type FromRegisterPath,
  type Register,
} from "./register.js";
export { interpolatePath } from "./router/interpolatePath.js";
export {
  type ExpandRouteOnOptionalStaticSegments,
  type OptionalRouteOptions,
  type ParamValue,
  type Params,
  type RouteDefinitionsToRoutes,
  type Search,
} from "./router/types.js";
export { type CustomSerializableJSONValue } from "./serialization/register.js";
export {
  type JSONArray,
  type JSONObject,
  type JSONValue,
  type Payload,
  type SerializableJSONValue,
  type SuperJSONArray,
  type SuperJSONObject,
  type SuperJSONValue,
} from "./serialization/types.js";
export { colorize } from "./utils/ansiColors.js";
export { isNotNull } from "./utils/predicates.js";
export {
  type Exact,
  type Expand,
  type IfAny,
  type Tail,
  type _Awaited,
  type _Parameters,
  type _ReturnType,
} from "./utils/types.js";
export { getOrigin, stripTrailingSlash } from "./utils/url.js";
export {
  isValidAbsoluteURL,
  throwIfNotValidAbsoluteURL,
} from "./utils/validators.js";
