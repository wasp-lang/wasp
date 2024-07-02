import { createAction } from "../operations/actions/core.js";
import { createQuery } from "../operations/queries/core.js";
import { makeUseActionFor, makeUseQueryFor } from "./operationsHelpers.js";
function createCrud() {
    const crudGetQuery = createQuery('tasks/get', ['Task']);
    const crudGetAllQuery = createQuery('tasks/get-all', ['Task']);
    const crudCreateAction = createAction('tasks/create', ['Task']);
    return {
        get: {
            query: crudGetQuery,
            useQuery: makeUseQueryFor(crudGetQuery)
        },
        getAll: {
            query: crudGetAllQuery,
            useQuery: makeUseQueryFor(crudGetAllQuery)
        },
        create: {
            action: crudCreateAction,
            useAction: makeUseActionFor(crudCreateAction)
        },
    };
}
// PUBLIC API
export const tasks = createCrud();
//# sourceMappingURL=tasks.js.map