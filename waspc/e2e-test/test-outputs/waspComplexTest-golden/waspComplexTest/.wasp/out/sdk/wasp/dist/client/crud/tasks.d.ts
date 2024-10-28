import { GetQueryResolved, GetAllQueryResolved } from 'wasp/server/crud/tasks';
export declare const tasks: {
    get: {
        query: import("../operations/queries/core.js").QueryFor<GetQueryResolved>;
        useQuery: (queryFnArgs?: ({
            id: number | undefined;
        } & {
            id?: number;
            AND?: import(".prisma/client").Prisma.TaskWhereInput | import(".prisma/client").Prisma.TaskWhereInput[];
            OR?: import(".prisma/client").Prisma.TaskWhereInput[];
            NOT?: import(".prisma/client").Prisma.TaskWhereInput | import(".prisma/client").Prisma.TaskWhereInput[];
            description?: import(".prisma/client").Prisma.StringFilter<"Task"> | string;
            isDone?: import(".prisma/client").Prisma.BoolFilter<"Task"> | boolean;
        }) | ({
            id: number;
        } & {
            id?: number;
            AND?: import(".prisma/client").Prisma.TaskWhereInput | import(".prisma/client").Prisma.TaskWhereInput[];
            OR?: import(".prisma/client").Prisma.TaskWhereInput[];
            NOT?: import(".prisma/client").Prisma.TaskWhereInput | import(".prisma/client").Prisma.TaskWhereInput[];
            description?: import(".prisma/client").Prisma.StringFilter<"Task"> | string;
            isDone?: import(".prisma/client").Prisma.BoolFilter<"Task"> | boolean;
        }) | undefined, options?: any) => import("@tanstack/react-query").UseQueryResult<{
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
        action: (args?: (import(".prisma/client").Prisma.Without<import(".prisma/client").Prisma.TaskCreateInput, import(".prisma/client").Prisma.TaskUncheckedCreateInput> & import(".prisma/client").Prisma.TaskUncheckedCreateInput) | (import(".prisma/client").Prisma.Without<import(".prisma/client").Prisma.TaskUncheckedCreateInput, import(".prisma/client").Prisma.TaskCreateInput> & import(".prisma/client").Prisma.TaskCreateInput) | undefined) => Promise<{
            id: number;
            description: string;
            isDone: boolean;
        }>;
        useAction: (actionOptions?: {
            optimisticUpdates: import("../operations/hooks.js").OptimisticUpdateDefinition<(import(".prisma/client").Prisma.Without<import(".prisma/client").Prisma.TaskCreateInput, import(".prisma/client").Prisma.TaskUncheckedCreateInput> & import(".prisma/client").Prisma.TaskUncheckedCreateInput) | (import(".prisma/client").Prisma.Without<import(".prisma/client").Prisma.TaskUncheckedCreateInput, import(".prisma/client").Prisma.TaskCreateInput> & import(".prisma/client").Prisma.TaskCreateInput), any>[];
        } | undefined) => (args?: (import(".prisma/client").Prisma.Without<import(".prisma/client").Prisma.TaskCreateInput, import(".prisma/client").Prisma.TaskUncheckedCreateInput> & import(".prisma/client").Prisma.TaskUncheckedCreateInput) | (import(".prisma/client").Prisma.Without<import(".prisma/client").Prisma.TaskUncheckedCreateInput, import(".prisma/client").Prisma.TaskCreateInput> & import(".prisma/client").Prisma.TaskCreateInput) | undefined) => Promise<{
            id: number;
            description: string;
            isDone: boolean;
        }>;
    };
};
