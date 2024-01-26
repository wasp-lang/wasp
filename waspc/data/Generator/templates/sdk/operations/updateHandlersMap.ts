export function makeUpdateHandlersMap(calculateHash: (queryKey: string[]) => string): {
  add: (queryKey: string[], updateQuery: UpdateQueryFn) => void,
  remove: (queryKey: string[]) => void,
  getUpdateHandlers: (queryKey: string[]) => UpdateQueryFn[]
} {
  const updateHandlers: Map<string, UpdateHandler[]> = new Map()

  function getHandlerTuples(queryKeyHash: string): UpdateHandler[] {
    return updateHandlers.get(queryKeyHash) || [];
  }

  function add(queryKey: string[], updateQuery: UpdateQueryFn) {
    const queryKeyHash = calculateHash(queryKey)
    const handlers = getHandlerTuples(queryKeyHash);
    updateHandlers.set(queryKeyHash, [...handlers, { queryKey, updateQuery }])
  }

  function getUpdateHandlers(queryKey: string[]): UpdateQueryFn[] {
    const queryKeyHash = calculateHash(queryKey)
    return getHandlerTuples(queryKeyHash).map(({ updateQuery }) => updateQuery)
  }

  function remove(queryKeyToRemove: string[]): void {
    const queryKeyHash = calculateHash(queryKeyToRemove)
    const filteredHandlers = getHandlerTuples(queryKeyHash).filter(
      ({ queryKey }) => queryKey !== queryKeyToRemove
    )

    if (filteredHandlers.length > 0) {
      updateHandlers.set(queryKeyHash, filteredHandlers)
    } else {
      updateHandlers.delete(queryKeyHash)
    }
  }

  return {
    add,
    remove,
    getUpdateHandlers,
  }
}

export type UpdateQueryFn = (...args: any[]) => any;

type UpdateHandler = {
  queryKey: string[];
  updateQuery: UpdateQueryFn;
}
