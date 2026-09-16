/**
 * This module augments SDK's `Register` type with the user project types.
 */

// The import ensures the module is always loaded into the bundle.
// Otherwise, module augmentation can fail if it wasn't loaded.
import "wasp/types"

declare module "wasp/types" {
  interface Register {
    operations: {
      'getMyTasks': typeof import('../../../../../src/operations').getMyTasks
      'getAdminReport': typeof import('../../../../../src/operations').getAdminReport
      'createTask': typeof import('../../../../../src/operations').createTask
    }
    crudOverrides: {
    }
  }
}
