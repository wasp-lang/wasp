import type {
  GenericBackendOperation,
  OperationRpcFor,
} from "../../operations/rpc.js";

export type ActionFor<BackendAction extends GenericBackendOperation> =
  OperationRpcFor<BackendAction>;
