export { DefaultRootErrorBoundary } from "./app/DefaultRootErrorBoundary.js";
export { FullPageWrapper } from "./app/FullPageWrapper.js";
export { Loader } from "./app/Loader.js";
export { MessageError, MessageLoading } from "./app/Message.js";
export { apiEventsEmitter } from "./auth/events.js";
export {
  clearSessionId,
  getSessionId,
  handleApiError,
  initSession,
  removeLocalUserData,
  setSessionId,
} from "./auth/session.js";
export {
  makeUseActionFor,
  makeUseQueryFor,
  type OperationInput,
  type OperationOutput,
  type UseActionFor,
  type UseQueryFor,
} from "./crud/operations.js";
export { useEffectOnce } from "./hooks.js";
export { type ActionFor } from "./operations/actions.js";
export {
  useAction,
  useQuery,
  type ActionOptions,
  type OptimisticUpdateDefinition,
} from "./operations/hooks.js";
export {
  addResourcesUsedByQuery,
  getActiveOptimisticUpdates,
  invalidateAndRemoveQueries,
  registerActionDone,
  registerActionInProgress,
} from "./operations/internal/resources.js";
export { makeUpdateHandlersMap } from "./operations/internal/updateHandlersMap.js";
export {
  buildAndRegisterQuery,
  makeQueryCacheKey,
  type QueryFor,
} from "./operations/queries.js";
export {
  configureQueryClient,
  initializeQueryClient,
  queryClientInitialized,
} from "./operations/queryClient.js";

export { storage, type DataStore } from "./storage.js";
