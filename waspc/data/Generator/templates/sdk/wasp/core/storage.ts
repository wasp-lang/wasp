export type DataStore = {
  getPrefixedKey(key: string): string
  set(key: string, value: unknown): void
  get(key: string): unknown
  remove(key: string): void
  clear(): void
}

function createStorageDataStore(prefix: string): DataStore {
  function getPrefixedKey(key: string): string {
    return `${prefix}:${key}`
  }

  return {
    getPrefixedKey,
    set(key, value) {
      const storage =getStorage()
      storage?.setItem(getPrefixedKey(key), JSON.stringify(value))
    },
    get(key) {
      const storage = getStorage()
      const value = storage?.getItem(getPrefixedKey(key))
      try {
        return value ? JSON.parse(value) : undefined
      } catch (e: any) {
        return undefined
      }
    },
    remove(key) {
      const storage = getStorage()
      storage?.removeItem(getPrefixedKey(key))
    },
    clear() {
      const storage = getStorage()
      if(!storage) {
        return
      }
      Object.keys(storage).forEach((key) => {
        if (key.startsWith(prefix)) {
          storage.removeItem(key)
        }
      })
    },
  }
}

export const storage = createStorageDataStore('wasp')

// TODO: Make this function work in the server context as well.
function getStorage(): Storage | undefined {
  if (typeof localStorage === 'undefined') {
    return undefined
  }
  return localStorage
}
