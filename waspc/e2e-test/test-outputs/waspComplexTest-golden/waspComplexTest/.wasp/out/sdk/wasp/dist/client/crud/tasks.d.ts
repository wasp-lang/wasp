import { GetQueryResolved } from 'wasp/server/crud/tasks';
export declare const tasks: {
    get: {
        query: (queryCacheKey: string[], args: import(".prisma/client").Prisma.TaskWhereUniqueInput) => Promise<import("@prisma/client/runtime/index.js").GetResult<{
            id: number;
            description: string;
            isDone: boolean;
        }, unknown> & {}>;
        useQuery(args: Parameters<GetQueryResolved>[0]): import("@tanstack/react-query").UseQueryResult<import("@prisma/client/runtime/index.js").GetResult<{
            id: number;
            description: string;
            isDone: boolean;
        }, unknown> & {}, Error>;
    };
    getAll: {
        query: (queryCacheKey: string[], args: {}) => Promise<(import("@prisma/client/runtime/index.js").GetResult<{
            id: number;
            description: string;
            isDone: boolean;
        }, unknown> & {})[]>;
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
