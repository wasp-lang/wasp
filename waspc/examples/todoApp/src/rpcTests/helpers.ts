// All generics in this file are taken from
// https://github.com/type-challenges/type-challenges/blob/9080836b5923f1716815e643342195d23b6a1617/utils/index.d.ts#L7

export type Expect<T extends true> = T;

export type Equal<X, Y> =
  (<T>() => T extends X ? 1 : 2) extends <T>() => T extends Y ? 1 : 2
    ? true
    : false;

export type NotEqual<X, Y> = true extends Equal<X, Y> ? false : true;
