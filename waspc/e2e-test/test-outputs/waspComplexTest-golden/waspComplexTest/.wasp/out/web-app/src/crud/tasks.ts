import { createAction } from "../actions/core";
import { useAction } from "../actions";
import { createQuery } from "../queries/core";
import { useQuery } from "../queries";
import { Task } from "../entities";

type EntityType = Task
type PrimaryFieldType = Task["id"]
type PrimaryFieldArgs = { id: PrimaryFieldType }
type CreateArgs = Partial<Omit<EntityType, "id">>

function createCrud() {
    const crudGetQuery = createQuery<(args: PrimaryFieldArgs) => Promise<EntityType>>(
        'crud/tasks/get',
        ['Task']
    )
    const crudGetAllQuery = createQuery<() => Promise<EntityType[]>>(
        'crud/tasks/getAll',
        ['Task']
    )
    const crudCreateAction = createAction<(args: CreateArgs) => Promise<void>>(
        'crud/tasks/create',
        ['Task']
    )
    const crudUpdateAction = createAction<(args: CreateArgs & PrimaryFieldArgs) => Promise<void>>(
        'crud/tasks/update',
        ['Task']
    )
    return {
        get: {
            query: crudGetQuery,
            useQuery(args: PrimaryFieldArgs) {
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
        update: {
            action: crudUpdateAction,
            useAction() {
                return useAction(crudUpdateAction);
            }
        },
    }
}

export const tasks = createCrud();
