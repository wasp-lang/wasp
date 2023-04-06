// This is a helper type used exclusively for DX purposes. It's a No-op for the
// compiler, but expands the type's representatoin in IDEs (i.e., inlines all
// type constructors) to make it more readable for the user.
//
// It expands this SO answer to funcitons: https://stackoverflow.com/a/57683652
export type Expand<T> = T extends (...args: infer A) => infer R
  ? (...args: A) => R
  : T extends infer O
  ? { [K in keyof O]: O[K] }
  : never;
