export type Expand<T> = T extends (...args: infer A) => infer R ? (...args: A) => R : T extends infer O ? {
    [K in keyof O]: O[K];
} : never;
export type _Awaited<T> = T extends Promise<infer V> ? _Awaited<V> : T;
export type _ReturnType<T extends (...args: never[]) => unknown> = T extends (...args: never[]) => infer R ? R : never;
export type Tail<T extends [unknown, ...unknown[]]> = T extends [unknown, ...infer R] ? R : never;
export type IfAny<Value, Then, Else> = 0 extends (1 & Value) ? Then : Else;
export type _Parameters<T extends (...args: any) => any> = T extends (firstParam: infer FirstParam, ...rest: infer Rest) => any ? [FirstParam, ...Rest] : Parameters<T>;
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
};
//# sourceMappingURL=types.d.ts.map