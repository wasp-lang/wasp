import { GetQueryResolved, GetAllQueryResolved } from 'wasp/server/crud/tasks';
export declare const tasks: {
    get: {
        query: import("../operations/queries/core.js").QueryFor<GetQueryResolved>;
        useQuery: (queryFnArgs?: import(".prisma/client").Prisma.TaskWhereUniqueInput | undefined, options?: any) => import("@tanstack/react-query").UseQueryResult<{
            id: number;
            description: string;
            isDone: boolean;
        } | null, Error>;
    };
    getAll: {
        query: import("../operations/queries/core.js").QueryFor<GetAllQueryResolved>;
        useQuery: (queryFnArgs?: {} | undefined, options?: any) => import("@tanstack/react-query").UseQueryResult<{
            id: number;
            description: string;
            isDone: boolean;
        }[], Error>;
    };
    create: {
        action: (args: (import(".prisma/client").Prisma.Without<import(".prisma/client").Prisma.TaskCreateInput, import(".prisma/client").Prisma.TaskUncheckedCreateInput> & import(".prisma/client").Prisma.TaskUncheckedCreateInput) | (import(".prisma/client").Prisma.Without<import(".prisma/client").Prisma.TaskUncheckedCreateInput, import(".prisma/client").Prisma.TaskCreateInput> & import(".prisma/client").Prisma.TaskCreateInput)) => Promise<{
            id: number;
            description: string;
            isDone: boolean;
        }>;
        useAction: (actionOptions?: {
            optimisticUpdates: import("../operations/hooks.js").OptimisticUpdateDefinition<(import(".prisma/client").Prisma.Without<import(".prisma/client").Prisma.TaskCreateInput, import(".prisma/client").Prisma.TaskUncheckedCreateInput> & import(".prisma/client").Prisma.TaskUncheckedCreateInput) | (import(".prisma/client").Prisma.Without<import(".prisma/client").Prisma.TaskUncheckedCreateInput, import(".prisma/client").Prisma.TaskCreateInput> & import(".prisma/client").Prisma.TaskCreateInput), any>[];
        } | undefined) => (args: (import(".prisma/client").Prisma.Without<import(".prisma/client").Prisma.TaskCreateInput, import(".prisma/client").Prisma.TaskUncheckedCreateInput> & import(".prisma/client").Prisma.TaskUncheckedCreateInput) | (import(".prisma/client").Prisma.Without<import(".prisma/client").Prisma.TaskUncheckedCreateInput, import(".prisma/client").Prisma.TaskCreateInput> & import(".prisma/client").Prisma.TaskCreateInput)) => Promise<{
            id: number;
            description: string;
            isDone: boolean;
        }>;
    };
};
