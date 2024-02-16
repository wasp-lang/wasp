export function createQuery(relativeQueryPath: any, entitiesUsed: any): (queryKey: any, queryArgs: any) => Promise<any>;
export function addMetadataToQuery(query: any, { relativeQueryPath, queryRoute, entitiesUsed }: {
    relativeQueryPath: any;
    queryRoute: any;
    entitiesUsed: any;
}): void;
