// Based on https://egghead.io/blog/using-branded-types-in-typescript
declare const __brand: unique symbol;
type Brand<B> = { [__brand]: B };
export type Branded<T, B> = T & Brand<B>;

export type EnvVars = { [envVarName: string]: string };
