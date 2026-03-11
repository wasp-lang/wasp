export async function forEach<T>(
  iterable: AsyncIterable<T>,
  fn: (item: T) => void | Promise<void>,
): Promise<void> {
  for await (const item of iterable) {
    await fn(item);
  }
}
