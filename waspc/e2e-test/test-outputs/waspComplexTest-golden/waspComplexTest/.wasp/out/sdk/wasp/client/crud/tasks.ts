import { useAction, useQuery } from "wasp/client/operations";
import { createAction } from "../operations/actions/core.js";
import { createQuery } from "../operations/queries/core.js";
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
            useQuery(args: Parameters<GetQueryResolved>[0]) {
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
    }
}

// PUBLIC API
export const tasks = createCrud();
