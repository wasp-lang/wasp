const createStorage = typeof window === "undefined" || !window.localStorage
    ? createMemoryDataStore
    : createLocalStorageDataStore;
export const storage = createStorage("wasp");
function createMemoryDataStore(prefix) {
    const store = new Map();
    function getPrefixedKey(key) {
        return `${prefix}:${key}`;
    }
    return {
        getPrefixedKey,
        set(key, value) {
            store.set(getPrefixedKey(key), value);
        },
        get(key) {
            return store.get(getPrefixedKey(key));
        },
        remove(key) {
            store.delete(getPrefixedKey(key));
        },
        clear() {
            store.clear();
        },
    };
}
function createLocalStorageDataStore(prefix) {
    if (!window.localStorage) {
        throw new Error("Local storage is not available.");
    }
    function getPrefixedKey(key) {
        return `${prefix}:${key}`;
    }
    return {
        getPrefixedKey,
        set(key, value) {
            localStorage.setItem(getPrefixedKey(key), JSON.stringify(value));
        },
        get(key) {
            const value = localStorage.getItem(getPrefixedKey(key));
            try {
                return value ? JSON.parse(value) : undefined;
            }
            catch (e) {
                return undefined;
            }
        },
        remove(key) {
            localStorage.removeItem(getPrefixedKey(key));
        },
        clear() {
            Object.keys(localStorage).forEach((key) => {
                if (key.startsWith(prefix)) {
                    localStorage.removeItem(key);
                }
            });
        },
    };
}
//# sourceMappingURL=storage.js.map