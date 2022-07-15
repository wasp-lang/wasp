export function makeCounter(calculateHash) {
  const counter = new Map()

  function increment(value) {
    const key = calculateHash(value)
    const currentCount = counter.get(key) ?? 0
    counter.set(key, currentCount + 1)
  }

  function decrement(value) {
    const key = calculateHash(value)
    const currentCount = counter.get(key) ?? 0
    counter.set(key, currentCount - 1)
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
