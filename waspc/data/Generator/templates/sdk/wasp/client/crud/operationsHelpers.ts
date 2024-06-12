import { useAction, useQuery } from "wasp/client/operations";

export function makeUseQueryFor<Input, Output>(
  query: Parameters<typeof useQuery<Input, Output>>[0]
) {
  return (
    args?: Parameters<typeof useQuery<Input, Output>>[1],
    options?: Parameters<typeof useQuery<Input, Output>>[2]
  ) => useQuery<Input, Output>(query, args, options);
}

export function makeUseActionFor<Input = unknown, Output = unknown>(
  action: Parameters<typeof useAction<Input, Output>>[0]
) {
  return (actionOptions?: Parameters<typeof useAction<Input, Output>>[1]) =>
    useAction<Input, Output>(action, actionOptions);
}
