// Without this import, Prisma types are resolved incorrectly.
import * as runtime from '@prisma/client/runtime';

// Prisma generated types which we use as default input and output types for CRUD
// operations internally use interfaces for some types.
// Our SuperJSON serialization types throw a type error when used with interfaces
// because interfaces don't have an implicit index signature.
// Read more https://github.com/microsoft/TypeScript/issues/15300#issuecomment-1320528385
// Here's a playground demonstrating the behaviour difference: https://tsplay.dev/interface_vs_type_index

// Luckily, only one interface is problematic for us: `FieldRef` which we can augment
// to have an index signature:
declare module '@prisma/client/runtime/library.js' {
  export interface FieldRef<Model, FieldType> {
    [key: string]: any;
  }
}
