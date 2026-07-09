export type Query<Args = unknown, Result = unknown, Context = unknown> = (
  args: Args,
  context: Context,
) => Result | Promise<Result>;

export type Action<Args = unknown, Result = unknown, Context = unknown> = (
  args: Args,
  context: Context,
) => Result | Promise<Result>;
