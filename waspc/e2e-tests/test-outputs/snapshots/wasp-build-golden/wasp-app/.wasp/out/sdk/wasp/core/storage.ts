export type DataStore = {
  getPrefixedKey(key: string): string;
  set(key: string, value: unknown): void;
  get(key: string): unknown;
  remove(key: string): void;
  clear(): void;
};

export const storage = (
  typeof window === "undefined" || !window.localStorage
    ? createMemoryDataStore
    : createLocalStorageDataStore
)("wasp");

function createMemoryDataStore(prefix: string): DataStore {
  const store: Map<string, unknown> = new Map();

  function getPrefixedKey(key: string): string {
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

function createLocalStorageDataStore(prefix: string): DataStore {
  if (!window.localStorage) {
    throw new Error("Local storage is not available.");
  }

  function getPrefixedKey(key: string): string {
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
      } catch (e: any) {
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
