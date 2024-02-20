export type Expand<T> = T extends (...args: infer A) => infer R ? (...args: A) => R : T extends infer O ? {
    [K in keyof O]: O[K];
} : never;
export type _Awaited<T> = T extends Promise<infer V> ? _Awaited<V> : T;
export type _ReturnType<T extends (...args: never[]) => unknown> = T extends (...args: never[]) => infer R ? R : never;
