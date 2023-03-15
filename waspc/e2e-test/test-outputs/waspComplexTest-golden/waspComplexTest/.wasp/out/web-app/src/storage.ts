function createLocalStorageDataStore(prefix: string): DataStore {
    return {
        set(key, value) {
            if (!isLocalStorageAvailable()) {
                return;
            }
            localStorage.setItem(`${prefix}:${key}`, JSON.stringify(value));
        },
        get(key) {
            if (!isLocalStorageAvailable()) {
                return;
            }
            const value = localStorage.getItem(`${prefix}:${key}`);
            try {
                return value ? JSON.parse(value) : null;
            } catch (e: any) {
                return null;
            }
        },
        remove(key) {
            if (!isLocalStorageAvailable()) {
                return;
            }
            localStorage.removeItem(`${prefix}:${key}`);
        },
        clear() {
            if (!isLocalStorageAvailable()) {
                return;
            }
            Object.keys(localStorage).forEach((key) => {
                if (key.startsWith(prefix)) {
                    localStorage.removeItem(key);
                }
            });
        },
    };

    function isLocalStorageAvailable() {
        try {
            return !!window.localStorage;
        } catch (e) {
            return false;
        }
    }
}

export const storage = createLocalStorageDataStore('wasp');

type DataStore = {
    set(key: string, value: any): void;
    get(key: string): any;
    remove(key: string): void;
    clear(): void;
};
