import type {
  GenericBackendOperation,
  OperationRpcFor,
} from "../../operations/rpc.js";

// PRIVATE API
export type ActionFor<BackendAction extends GenericBackendOperation> =
  OperationRpcFor<BackendAction>;
