/**
 * This module augments SDK's `Register` type with the user project types.
 */

// The import ensures the module is always loaded into the bundle.
// Otherwise, module augmentation can fail if it wasn't loaded.
import "wasp/types"

declare module "wasp/types" {
  interface Register {
    operations: {
    }
    crudOverrides: {
    }
  }
}
