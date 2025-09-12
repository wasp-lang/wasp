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


// Returns elements of an array except the first one.
export type Tail<T extends [unknown, ...unknown[]]> = T extends [unknown, ...infer R] ? R : never;


// Source: https://stackoverflow.com/a/55541672
export type IfAny<Value, Then, Else> = 0 extends (1 & Value) ? Then : Else;

// If users use JS to define their operations and destructure object arguments,
// Wasp's TS helper types can't infer correct types due to this TS bug:
// https://github.com/microsoft/TypeScript/issues/52768 
// The bug breaks the built-in `Parameters` helper, so we introduce a custom version
// that works with Wasp's TS helper types like `EntityMapFor`.
export type _Parameters<T extends (...args: any) => any> = T extends (
  firstParam: infer FirstParam,
  ...rest: infer Rest
) => any
  ? [FirstParam, ...Rest]
  : Parameters<T>;

/**
 * 
 * Exact ensures a type has exactly the properties of another type.
 * Helps with simulating excess property checking for generics in function definitions.
 * 
 * Inspired by: https://stackoverflow.com/questions/49580725/is-it-possible-to-restrict-typescript-object-to-contain-only-properties-define
 * 
 * @template T - The base type that defines the allowed properties
 * @template U - The type to be constrained (must extend T)
 * 
 * @example
 * // User can no longer add properties outside of UserSignupFields to T
 * function defineUserSignupFields<T extends UserSignupFields>(
 *   fields: Exact<UserSignupFields, T>
 * ): T {
 *   return fields
 * }
 */
export type Exact<T, U extends T = T> = U & {
  [P in Exclude<keyof U, keyof T>]: never;
}

