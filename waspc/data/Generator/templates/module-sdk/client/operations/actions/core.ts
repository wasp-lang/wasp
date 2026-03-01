type ActionFunction<Input, Output> =
  0 extends (1 & Input)
    ? ((args?: any) => Promise<Output>)
    : [Input] extends [void]
      ? (() => Promise<Output>)
      : ((args: Input) => Promise<Output>);

export type ActionFor<BackendAction extends (...args: any[]) => any> = ActionFunction<
  Parameters<BackendAction> extends [] ? void : Parameters<BackendAction>[0],
  Awaited<ReturnType<BackendAction>>
>;

export function createAction<BackendAction extends (...args: any[]) => any>(
  actionPath: string,
  entitiesUsed: string[],
): ActionFor<BackendAction> {
  const actionFn = ((_args?: any) => {
    throw new Error(
      `Action '${actionPath}' is not available in module dev mode. ` +
        'It will work when the module is used in a host app.',
    );
  }) as any;
  return actionFn;
}

export function useAction<Input, Output>(
  actionFn: ActionFor<(...args: any[]) => any>,
  actionOptions?: any,
): typeof actionFn {
  throw new Error(
    'useAction is not available in module dev mode. ' +
      'It will work when the module is used in a host app.',
  );
}
