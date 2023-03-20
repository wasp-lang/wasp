export type DataStore = {
    set(key: string, value: unknown): void;
    get(key: string): unknown | undefined;
    remove(key: string): void;
    clear(): void;
};

function createLocalStorageDataStore(prefix: string): DataStore {
    return {
        set(key, value) {
            ensureLocalStorageIsAvailable();
            localStorage.setItem(`${prefix}:${key}`, JSON.stringify(value));
        },
        get(key) {
            ensureLocalStorageIsAvailable();
            const value = localStorage.getItem(`${prefix}:${key}`);
            try {
                return value ? JSON.parse(value) : undefined;
            } catch (e: any) {
                return undefined;
            }
        },
        remove(key) {
            ensureLocalStorageIsAvailable();
            localStorage.removeItem(`${prefix}:${key}`);
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

    function ensureLocalStorageIsAvailable(): void {
        if (!window.localStorage) {
            throw new Error('Local storage is not available.');
        }
    }
}

export const storage = createLocalStorageDataStore('wasp');