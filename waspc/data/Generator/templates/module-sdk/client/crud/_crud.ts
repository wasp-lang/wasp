{{={= =}=}}
import { createAction } from "../operations/actions/core.js";
import { createQuery } from "../operations/queries/core.js";
import { makeUseActionFor, makeUseQueryFor } from "./operationsHelpers.js";

function createCrud() {
    const crudGetQuery = createQuery<any>(
        '{= name =}/get',
        ['{= entityName =}']
    )
    const crudGetAllQuery = createQuery<any>(
        '{= name =}/getAll',
        ['{= entityName =}']
    )
    const crudCreateAction = createAction<any>(
        '{= name =}/create',
        ['{= entityName =}']
    )
    const crudUpdateAction = createAction<any>(
        '{= name =}/update',
        ['{= entityName =}']
    )
    const crudDeleteAction = createAction<any>(
        '{= name =}/delete',
        ['{= entityName =}']
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

export const {= name =} = createCrud();
