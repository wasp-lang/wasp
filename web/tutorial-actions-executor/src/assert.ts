export function assertUnreachable(_x: never, message: string): never {
  throw new Error(message);
}
