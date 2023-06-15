import { createAction } from "../actions/core";
import { useAction } from "../actions";
import { createQuery } from "../queries/core";
import { useQuery } from "../queries";
import { 
    GetQueryResolved,
    GetAllQueryResolved,
    CreateActionResolved,
} from '../../../server/src/crud/tasks'

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

export const tasks = createCrud();
