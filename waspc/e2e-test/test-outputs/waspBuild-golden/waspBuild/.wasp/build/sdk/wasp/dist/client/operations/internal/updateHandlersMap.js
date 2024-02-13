export function makeUpdateHandlersMap(calculateHash) {
    const updateHandlers = new Map();
    function getHandlerTuples(queryKeyHash) {
        return updateHandlers.get(queryKeyHash) || [];
    }
    function add(queryKey, updateQuery) {
        const queryKeyHash = calculateHash(queryKey);
        const handlers = getHandlerTuples(queryKeyHash);
        updateHandlers.set(queryKeyHash, [...handlers, { queryKey, updateQuery }]);
    }
    function getUpdateHandlers(queryKey) {
        const queryKeyHash = calculateHash(queryKey);
        return getHandlerTuples(queryKeyHash).map(({ updateQuery }) => updateQuery);
    }
    function remove(queryKeyToRemove) {
        const queryKeyHash = calculateHash(queryKeyToRemove);
        const filteredHandlers = getHandlerTuples(queryKeyHash).filter(({ queryKey }) => queryKey !== queryKeyToRemove);
        if (filteredHandlers.length > 0) {
            updateHandlers.set(queryKeyHash, filteredHandlers);
        }
        else {
            updateHandlers.delete(queryKeyHash);
        }
    }
    return {
        add,
        remove,
        getUpdateHandlers,
    };
}
//# sourceMappingURL=updateHandlersMap.js.map