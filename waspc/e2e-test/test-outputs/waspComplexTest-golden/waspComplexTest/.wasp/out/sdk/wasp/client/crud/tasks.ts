import { createAction } from "../operations/actions/core.js";
import { createQuery } from "../operations/queries/core.js";
import { makeUseActionFor, makeUseQueryFor } from "./operationsHelpers.js";
import { 
    GetQueryResolved,
    GetAllQueryResolved,
    CreateActionResolved,
} from 'wasp/server/crud/tasks'

function createCrud() {
    const crudGetQuery = createQuery<GetQueryResolved>(
        'crud/tasks/get',
        ['Task']
    )
    const crudGetAllQuery = createQuery<GetAllQueryResolved>(
        'crud/tasks/get-all',
        ['Task']
    )
    const crudCreateAction = createAction<CreateActionResolved>(
        'crud/tasks/create',
        ['Task']
    )
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
    }
}

// PUBLIC API
export const tasks = createCrud();
