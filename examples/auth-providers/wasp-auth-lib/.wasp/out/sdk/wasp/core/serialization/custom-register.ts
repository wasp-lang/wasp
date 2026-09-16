import type { WaspInternal_CustomSerializableJSONValue_Register } from "superjson"

/*
  This is a workaround for declaring custom types through side-effects in
  TypeScript. This way a bare import, like `import "./prisma"` will register
  itself as a custom serialization in the type-level, like it does at the
  runtime-level.

  This declaration creates an empty interface. Implementations of custom
  serialization types can extend this interface through augmentation
  (https://www.typescriptlang.org/docs/handbook/declaration-merging.html#merging-interfaces).
  We can later inspect this interface to see which custom types are registered.

  This approach is copied from TanStack Router's `Register` interface
  https://github.com/TanStack/router/blob/v1.124.1/docs/router/framework/react/guide/creating-a-router.md#router-type-safety
*/
declare module "superjson" {
  interface WaspInternal_CustomSerializableJSONValue_Register {}
}

export type CustomSerializableJSONValue = WaspInternal_CustomSerializableJSONValue_Register[keyof WaspInternal_CustomSerializableJSONValue_Register]
