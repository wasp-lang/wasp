export type Expand<T> = T extends (...args: infer A) => infer R ? (...args: A) => R : T extends infer O ? {
    [K in keyof O]: O[K];
} : never;
export type _Awaited<T> = T extends Promise<infer V> ? _Awaited<V> : T;
export type _ReturnType<T extends (...args: never[]) => unknown> = T extends (...args: never[]) => infer R ? R : never;
export type Tail<T extends [unknown, ...unknown[]]> = T extends [unknown, ...infer R] ? R : never;
export type IfAny<Value, Then, Else> = 0 extends (1 & Value) ? Then : Else;
export type _Parameters<T extends (...args: any) => any> = T extends (firstParam: infer FirstParam, ...rest: infer Rest) => any ? [FirstParam, ...Rest] : Parameters<T>;
