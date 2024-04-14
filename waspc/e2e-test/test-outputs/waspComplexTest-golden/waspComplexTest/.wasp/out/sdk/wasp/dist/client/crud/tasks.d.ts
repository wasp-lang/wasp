import { GetQueryResolved, GetAllQueryResolved } from 'wasp/server/crud/tasks';
export declare const tasks: {
    get: {
        query: import("../operations/queries/core.js").QueryFor<GetQueryResolved>;
        useQuery(args: Parameters<GetQueryResolved>[0]): import("@tanstack/react-query").UseQueryResult<import("@prisma/client/runtime/index.js").GetResult<{
            id: number;
            description: string;
            isDone: boolean;
        }, unknown> & {}, Error>;
    };
    getAll: {
        query: import("../operations/queries/core.js").QueryFor<GetAllQueryResolved>;
        useQuery(): import("@tanstack/react-query").UseQueryResult<(import("@prisma/client/runtime/index.js").GetResult<{
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
        useAction(): (args: import(".prisma/client").Prisma.TaskCreateInput) => Promise<import("@prisma/client/runtime/index.js").GetResult<{
            id: number;
            description: string;
            isDone: boolean;
        }, unknown> & {}>;
    };
};
