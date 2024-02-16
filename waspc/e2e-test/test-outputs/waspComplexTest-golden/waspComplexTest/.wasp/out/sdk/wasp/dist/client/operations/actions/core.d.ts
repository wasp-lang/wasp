export function createAction(relativeActionRoute: any, entitiesUsed: any): {
    (args: any): Promise<unknown>;
    internal: (args: any, specificOptimisticUpdateDefinitions: any) => Promise<unknown>;
};
