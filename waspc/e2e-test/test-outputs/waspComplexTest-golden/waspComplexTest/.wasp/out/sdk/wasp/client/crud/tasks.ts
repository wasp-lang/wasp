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
        'tasks/get',
        ['Task']
    )
    const crudGetAllQuery = createQuery<GetAllQueryResolved>(
        'tasks/get-all',
        ['Task']
    )
    const crudCreateAction = createAction<CreateActionResolved>(
        'tasks/create',
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
