import { Prisma } from "@prisma/client";

export const SERIALIZABLE_OBJECTS_FIXTURE = {
  date: new Date(1746538539344),
  set: new Set(["foo"]),
  map: new Map([["foo", "bar"]]),
  regexp: /foo/,
  nan: NaN,
  inf: Infinity,
  negInf: -Infinity,
  // we ensure that it'd be too big to be represented as a `number`
  decimal: new Prisma.Decimal(Number.MAX_SAFE_INTEGER).mul(2),
  bigint: 2n * BigInt(Number.MAX_SAFE_INTEGER),
};
