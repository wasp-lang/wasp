export function makeOptimisticUpdatesMap(calculateHash) {
  const updateHandlers = new Map()

  function _getUpdateHandlers(queryKeyHash) {
    return updateHandlers.get(queryKeyHash) || [];
  }

  function add(queryKey, updateQueryFn) {
    const queryKeyHash = calculateHash(queryKey)
    const handlers = _getUpdateHandlers(queryKeyHash);
    updateHandlers.set(queryKeyHash, [...handlers, { queryKey, updateQueryFn }])
  }

  function getUpdateHandlers(queryKey) {
    const queryKeyHash = calculateHash(queryKey)
    return _getUpdateHandlers(queryKeyHash).map(({ updateQueryFn }) => updateQueryFn)
  }

  function remove(queryKeyToRemove) {
    const queryKeyHash = calculateHash(queryKeyToRemove)
    const filteredHandlers = _getUpdateHandlers(queryKeyHash).filter(
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
