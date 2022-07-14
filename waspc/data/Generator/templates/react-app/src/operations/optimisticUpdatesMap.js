const updateHandlers = new Map()
// TODO: remove
window.updateHandlers = updateHandlers

export function makeOptimisticUpdatesMap(calculateHash) {
  function _getUpdateHandlers(queryKeyHash) {
    return updateHandlers.get(queryKeyHash) || [];
  }

  function add(queryKey, updateQueryFn) {
    console.log("Adding handler", { queryKey, updateQueryFn })
    const queryKeyHash = calculateHash(queryKey)
    const handlers = _getUpdateHandlers(queryKeyHash);
    updateHandlers.set(queryKeyHash, [...handlers, { queryKey, updateQueryFn }])
    // TODO: remove
    console.log(updateHandlers)
  }

  function getUpdateHandlers(queryKey) {
    const queryKeyHash = calculateHash(queryKey)
    return _getUpdateHandlers(queryKeyHash).map(({ updateQueryFn }) => updateQueryFn)
  }

  function remove(queryKeyToRemove) {
    console.log("Removing handler for", queryKeyToRemove)
    const queryKeyHash = calculateHash(queryKeyToRemove)
    const filteredHandlers = _getUpdateHandlers(queryKeyHash) 
      .filter(({ queryKey }) => queryKey !== queryKeyToRemove)
    if (filteredHandlers.length > 0) {
      updateHandlers.set(queryKeyHash, filteredHandlers)
    } else {
      updateHandlers.delete(queryKeyHash)
    }
    // TODO: remove
    console.log(updateHandlers)
  }

  return {
    add,
    remove,
    getUpdateHandlers,
  }
}
