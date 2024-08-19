import { GetQueryResolved, GetAllQueryResolved } from 'wasp/server/crud/tasks';
export declare const tasks: {
    get: {
        query: import("../operations/queries/core.js").QueryFor<GetQueryResolved>;
        useQuery: (queryFnArgs?: import(".prisma/client").Prisma.TaskWhereUniqueInput, options?: any) => import("@tanstack/react-query").UseQueryResult<import("@prisma/client/runtime/index.js").GetResult<{
            id: number;
            description: string;
            isDone: boolean;
        }, unknown> & {}, Error>;
    };
    getAll: {
        query: import("../operations/queries/core.js").QueryFor<GetAllQueryResolved>;
        useQuery: (queryFnArgs?: {}, options?: any) => import("@tanstack/react-query").UseQueryResult<(import("@prisma/client/runtime/index.js").GetResult<{
            id: number;
            description: string;
            isDone: boolean;
        }, unknown> & {})[], Error>;
    };
    create: {
        action: (args: import(".prisma/client").Prisma.TaskCreateInput) => Promise<import("@prisma/client/runtime/index.js").GetResult<{
            id: number;
            description: string;
            isDone: boolean;
        }, unknown> & {}>;
        useAction: (actionOptions?: {
            optimisticUpdates: import("../operations/hooks.js").OptimisticUpdateDefinition<import(".prisma/client").Prisma.TaskCreateInput, any>[];
        }) => (args: import(".prisma/client").Prisma.TaskCreateInput) => Promise<import("@prisma/client/runtime/index.js").GetResult<{
            id: number;
            description: string;
            isDone: boolean;
        }, unknown> & {}>;
    };
};
