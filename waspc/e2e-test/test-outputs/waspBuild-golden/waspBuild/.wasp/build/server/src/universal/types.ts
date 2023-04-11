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
// https://github.com/wasp-lang/wasp/pull/1090#discussion_r1159687537 for
// details). Until it's fixed, we're using our own type for this.
//
// TODO: investigate further. This most likely has something to do with an
// unsatisfied 'extends' constraints. A mismatch is probably happening with
// function parameter types and/or return types (check '_ReturnType' below for
// more).
export type _Awaited<T> = T extends Promise<infer V>
  ? _Awaited<V>
  : T

// TypeScript's native ReturnType does not work for functions of type '(...args:
// never[]) => unknown' (and that's what operations currently use).
//
// TODO: investigate how to properly specify the 'extends' constraint for function
// type (i.e., any vs never and unknown) and stick with that. Take DX into
// consideration.
export type _ReturnType<T extends (...args: never[]) => unknown> = 
  T extends (...args: never[]) => infer R ? R : never
