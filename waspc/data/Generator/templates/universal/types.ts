// This is a helper type used exclusively for DX purposes. It's a No-op for the
// compiler, but expands the type's representatoin in IDEs (i.e., inlines all
// type constructors) to make it more readable for the user.
//
// It expands this SO answer to functions: https://stackoverflow.com/a/57683652
export type Expand<T> = T extends (...args: infer A) => infer R
  ? (...args: A) => R
  : T extends infer O
  ? { [K in keyof O]: O[K] }
  : never

// TypeScript's native Awaited type exhibits strange behavior in VS Code (see
// todo for details). Until it's fixed, we're using our own type for this.
export type _Awaited<T> = T extends Promise<infer V>
  ? _Awaited<V>
  : T

export type _ReturnType<T extends (...args: never[]) => unknown> = 
  T extends (...args: never[]) => infer R ? R : never