const counter = new Map()
// todo: remove
window.counter = counter;

export function makeCounter() {
  function increment(key) {
    const currentCount = counter.get(key) ?? 0;
    counter.set(key, currentCount + 1)
    // todo: remove this
    console.log('increment action counter', key, counter.get(key))
  }

  function decrement(queryCacheKey) {
    const currentCount = counter.get(queryCacheKey) ?? 0;
    counter.set(queryCacheKey, currentCount - 1)
    // todo: remove this
    console.log('decrement action counter', queryCacheKey, counter.get(queryCacheKey))
  }

  function count(queryCacheKey) {
    return counter.get(queryCacheKey) ?? 0
  }

  return {
    increment,
    decrement,
    count
  }
}