import type { QueryKey } from "@tanstack/react-query";
import type { SpecificOptimisticUpdateDefinition } from "../hooks.js";

type UpdateHandlerTuple = SpecificOptimisticUpdateDefinition<any>;
type UpdateHandler = UpdateHandlerTuple["updateQuery"];

export type UpdateHandlersMap = {
  add: (queryKey: QueryKey, updateQuery: UpdateHandler) => void;
  remove: (queryKeyToRemove: QueryKey) => void;
  getUpdateHandlers: (queryKey: QueryKey) => UpdateHandler[];
};

export function makeUpdateHandlersMap(
  calculateHash: (queryKey: QueryKey) => string,
): UpdateHandlersMap {
  const updateHandlers = new Map<string, UpdateHandlerTuple[]>();

  function getHandlerTuples(queryKeyHash: string) {
    return updateHandlers.get(queryKeyHash) || [];
  }

  function add(queryKey: QueryKey, updateQuery: UpdateHandler) {
    const queryKeyHash = calculateHash(queryKey);
    const handlers = getHandlerTuples(queryKeyHash);
    updateHandlers.set(queryKeyHash, [...handlers, { queryKey, updateQuery }]);
  }

  function getUpdateHandlers(queryKey: QueryKey) {
    const queryKeyHash = calculateHash(queryKey);
    return getHandlerTuples(queryKeyHash).map(({ updateQuery }) => updateQuery);
  }

  function remove(queryKeyToRemove: QueryKey) {
    const queryKeyHash = calculateHash(queryKeyToRemove);
    const filteredHandlers = getHandlerTuples(queryKeyHash).filter(
      ({ queryKey }) => queryKey !== queryKeyToRemove,
    );

    if (filteredHandlers.length > 0) {
      updateHandlers.set(queryKeyHash, filteredHandlers);
    } else {
      updateHandlers.delete(queryKeyHash);
    }
  }

  return {
    add,
    remove,
    getUpdateHandlers,
  };
}
