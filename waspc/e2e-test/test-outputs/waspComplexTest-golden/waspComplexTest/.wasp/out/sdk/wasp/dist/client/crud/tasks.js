import { useAction, useQuery } from "wasp/client/operations";
import { createAction } from "../operations/actions/core.js";
import { createQuery } from "../operations/queries/core.js";
function createCrud() {
    const crudGetQuery = createQuery('tasks/get', ['Task']);
    const crudGetAllQuery = createQuery('tasks/get-all', ['Task']);
    const crudCreateAction = createAction('tasks/create', ['Task']);
    return {
        get: {
            query: crudGetQuery,
            useQuery(args) {
                return useQuery(crudGetQuery, args);
            }
        },
        getAll: {
            query: crudGetAllQuery,
            useQuery() {
                return useQuery(crudGetAllQuery);
            }
        },
        create: {
            action: crudCreateAction,
            useAction() {
                return useAction(crudCreateAction);
            }
        },
    };
}
// PUBLIC API
export const tasks = createCrud();
//# sourceMappingURL=tasks.js.map