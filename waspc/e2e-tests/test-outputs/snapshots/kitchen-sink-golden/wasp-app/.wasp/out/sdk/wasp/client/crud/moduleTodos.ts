import { createAction } from "../operations/actions/core.js";
import { createQuery } from "../operations/queries/core.js";
import { makeUseActionFor, makeUseQueryFor } from "./operationsHelpers.js";
import type {
    GetAllQueryResolved,
    UpdateActionResolved,
    DeleteActionResolved,
} from '../../server/crud/moduleTodos.js'

function createCrud() {
    const crudGetAllQuery = createQuery<GetAllQueryResolved>(
        'crud/moduleTodos/get-all',
        ['Task']
    )
    const crudUpdateAction = createAction<UpdateActionResolved>(
        'crud/moduleTodos/update',
        ['Task']
    )
    const crudDeleteAction = createAction<DeleteActionResolved>(
        'crud/moduleTodos/delete',
        ['Task']
    )
    return {
        getAll: {
            query: crudGetAllQuery,
            useQuery: makeUseQueryFor(crudGetAllQuery)
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
export const moduleTodos = createCrud();
