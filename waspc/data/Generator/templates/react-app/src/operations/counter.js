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

  function decrement(key) {
    const currentCount = counter.get(key) ?? 0;
    counter.set(key, currentCount - 1)
    // todo: remove this
    console.log('decrement action counter', key, counter.get(key))
  }

  function count(key) {
    return counter.get(key) ?? 0
  }

  return {
    increment,
    decrement,
    count
  }
}