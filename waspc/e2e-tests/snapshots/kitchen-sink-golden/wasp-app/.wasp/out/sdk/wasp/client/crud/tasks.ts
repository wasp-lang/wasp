import { createAction } from "../operations/actions/core.js";
import { createQuery } from "../operations/queries/core.js";
import { makeUseActionFor, makeUseQueryFor } from "./operationsHelpers.js";
import { 
    GetQueryResolved,
    GetAllQueryResolved,
    CreateActionResolved,
    UpdateActionResolved,
    DeleteActionResolved,
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
    const crudUpdateAction = createAction<UpdateActionResolved>(
        'crud/tasks/update',
        ['Task']
    )
    const crudDeleteAction = createAction<DeleteActionResolved>(
        'crud/tasks/delete',
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
        update: {
            action: crudUpdateAction,
            useAction: makeUseActionFor(crudUpdateAction)
        },
        delete: {
            action: crudDeleteAction,
            useAction: makeUseActionFor(crudDeleteAction)
        },
    }
}

// PUBLIC API
export const tasks = createCrud();
