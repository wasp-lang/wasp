export type DataStore = {
    getPrefixedKey(key:string): string;
    set(key: string, value: unknown): void;
    get(key: string): unknown;
    remove(key: string): void;
    clear(): void;
};

function createLocalStorageDataStore(prefix: string): DataStore {
    return {
        getPrefixedKey(key) { return `${prefix}:${key}`; },
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
}

export const storage = createLocalStorageDataStore('wasp');

function ensureLocalStorageIsAvailable(): void {
    if (!window.localStorage) {
        throw new Error('Local storage is not available.');
    }
}
