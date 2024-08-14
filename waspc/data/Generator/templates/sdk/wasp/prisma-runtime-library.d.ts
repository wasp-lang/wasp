// Without this import, Prisma types are resolved incorrectly.
import * as runtime from '@prisma/client/runtime';

// Prisma generated types which we use as default input and output types for CRUD
// operations internally use interfaces for some types.
// Our SuperJSON serialization types throw a type error when used with interfaces
// because interfaces don't have a index signature. 
// We augment the Prisma generated types to have an index signature.
// Read more https://github.com/microsoft/TypeScript/issues/15300#issuecomment-1320528385
declare module '@prisma/client/runtime/library.js' {
  export interface FieldRef<Model, FieldType> {
    [key: string]: any;
  }
}
