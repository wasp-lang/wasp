const counter = new Map()
// todo: remove
window.counter = counter;

export function makeCounter(calculateHash) {
  function increment(value) {
    const key = calculateHash(value)
    const currentCount = counter.get(key) ?? 0;
    counter.set(key, currentCount + 1)
    // todo: remove this
    console.log('increment action counter', key, counter.get(key))
  }

  function decrement(value) {
    const key = calculateHash(value)

    const currentCount = counter.get(key) ?? 0;
    counter.set(key, currentCount - 1)
    // todo: remove this
    console.log('decrement action counter', key, counter.get(key))
  }

  function count(value) {
    const key = calculateHash(value)
    return counter.get(key) ?? 0
  }

  return {
    increment,
    decrement,
    count
  }
}