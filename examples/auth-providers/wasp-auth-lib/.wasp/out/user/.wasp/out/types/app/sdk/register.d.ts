/**
 * This module augments SDK's `Register` type with the user project types.
 */
import "wasp/types";
declare module "wasp/types" {
    interface Register {
        operations: {
            'getMyTasks': typeof import('../../../../../src/operations').getMyTasks;
            'createTask': typeof import('../../../../../src/operations').createTask;
        };
        crudOverrides: {};
    }
}
