function createLocalStorageDataStore(prefix) {
    function getPrefixedKey(key) {
        return `${prefix}:${key}`;
    }
    return {
        getPrefixedKey,
        set(key, value) {
            ensureLocalStorageIsAvailable();
            localStorage.setItem(getPrefixedKey(key), JSON.stringify(value));
        },
        get(key) {
            ensureLocalStorageIsAvailable();
            const value = localStorage.getItem(getPrefixedKey(key));
            try {
                return value ? JSON.parse(value) : undefined;
            }
            catch (e) {
                return undefined;
            }
        },
        remove(key) {
            ensureLocalStorageIsAvailable();
            localStorage.removeItem(getPrefixedKey(key));
        },
        clear() {
            ensureLocalStorageIsAvailable();
            Object.keys(localStorage).forEach((key) => {
                if (key.startsWith(prefix)) {
                    localStorage.removeItem(key);
                }
            });
        },
    };
}
export const storage = createLocalStorageDataStore('wasp');
function ensureLocalStorageIsAvailable() {
    if (!window.localStorage) {
        throw new Error('Local storage is not available.');
    }
}
//# sourceMappingURL=storage.js.map