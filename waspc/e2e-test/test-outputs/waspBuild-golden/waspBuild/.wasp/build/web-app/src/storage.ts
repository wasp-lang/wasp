export type DataStore = {
  getPrefixedKey(key: string): string
  set(key: string, value: unknown): void
  get(key: string): unknown
  remove(key: string): void
  clear(): void
}

function createLocalStorageDataStore(prefix: string): DataStore {
  function getPrefixedKey(key: string): string {
    return `${prefix}:${key}`
  }

  return {
    getPrefixedKey,
    set(key, value) {
      ensureLocalStorageIsAvailable()
      localStorage.setItem(getPrefixedKey(key), JSON.stringify(value))
    },
    get(key) {
      ensureLocalStorageIsAvailable()
      const value = localStorage.getItem(getPrefixedKey(key))
      try {
        return value ? JSON.parse(value) : undefined
      } catch (e: any) {
        return undefined
      }
    },
    remove(key) {
      ensureLocalStorageIsAvailable()
      localStorage.removeItem(getPrefixedKey(key))
    },
    clear() {
      ensureLocalStorageIsAvailable()
      Object.keys(localStorage).forEach((key) => {
        if (key.startsWith(prefix)) {
          localStorage.removeItem(key)
        }
      })
    },
  }
}

export const storage = createLocalStorageDataStore('wasp')

function ensureLocalStorageIsAvailable(): void {
  if (!window.localStorage) {
    throw new Error('Local storage is not available.')
  }
}
