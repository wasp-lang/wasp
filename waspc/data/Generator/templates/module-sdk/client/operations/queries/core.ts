type QueryMetadata = {
  queryCacheKey: string[];
  route: { method: string; path: string };
};

type QueryFunction<Input, Output> =
  0 extends (1 & Input)
    ? ((args?: any) => Promise<Output>)
    : [Input] extends [void]
      ? (() => Promise<Output>)
      : ((args: Input) => Promise<Output>);

export type Query<Input, Output> = QueryFunction<Input, Output> & QueryMetadata;

export type QueryFor<BackendQuery extends (...args: any[]) => any> = Query<
  Parameters<BackendQuery> extends [] ? void : Parameters<BackendQuery>[0],
  Awaited<ReturnType<BackendQuery>>
>;

export function createQuery<BackendQuery extends (...args: any[]) => any>(
  queryPath: string,
  entitiesUsed: string[],
): QueryFor<BackendQuery> {
  const queryFn = ((_args?: any) => {
    throw new Error(
      `Query '${queryPath}' is not available in module dev mode. ` +
        'It will work when the module is used in a host app.',
    );
  }) as any;
  queryFn.queryCacheKey = [queryPath];
  queryFn.route = { method: 'GET', path: `/api/module/${queryPath}` };
  return queryFn;
}

export function useQuery<Input, Output>(
  query: Query<Input, Output>,
  queryFnArgs?: Input,
  options?: any,
): { data: Output | undefined; isLoading: boolean; error: Error | null } {
  throw new Error(
    'useQuery is not available in module dev mode. ' +
      'It will work when the module is used in a host app.',
  );
}
