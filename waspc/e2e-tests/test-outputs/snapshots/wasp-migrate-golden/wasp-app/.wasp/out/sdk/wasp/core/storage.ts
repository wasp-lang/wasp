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
      if (!isLocalStorageAvailable()) return
      localStorage.setItem(getPrefixedKey(key), JSON.stringify(value))
    },
    get(key) {
      if (!isLocalStorageAvailable()) return undefined
      const value = localStorage.getItem(getPrefixedKey(key))
      try {
        return value ? JSON.parse(value) : undefined
      } catch (e: any) {
        return undefined
      }
    },
    remove(key) {
      if (!isLocalStorageAvailable()) return
      localStorage.removeItem(getPrefixedKey(key))
    },
    clear() {
      if (!isLocalStorageAvailable()) return
      Object.keys(localStorage).forEach((key) => {
        if (key.startsWith(prefix)) {
          localStorage.removeItem(key)
        }
      })
    },
  }
}

export const storage = createLocalStorageDataStore('wasp')

function isLocalStorageAvailable(): boolean {
  // In SSR environment, localStorage is not available - gracefully return false
  if (typeof window === 'undefined') {
    return false
  }
  // In browser, throw if localStorage is not available (preserves original behavior)
  if (!window.localStorage) {
    throw new Error('Local storage is not available.')
  }
  return true
}
