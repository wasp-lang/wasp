export type EntityMap = Record<string, any>;

export type ServerContext = {
  entities: EntityMap;
  user?: any;
};

export type OperationFn<
  Args = unknown,
  Result = unknown,
  Context = ServerContext,
> = (args: Args, context: Context) => Result | Promise<Result>;

export type QueryFn<
  Args = unknown,
  Result = unknown,
  Context = ServerContext,
> = OperationFn<Args, Result, Context>;

export type ActionFn<
  Args = unknown,
  Result = unknown,
  Context = ServerContext,
> = OperationFn<Args, Result, Context>;

export type ApiFn<Request = any, Response = any, Context = ServerContext> = (
  req: Request,
  res: Response,
  context: Context,
) => void | Promise<void>;

export type JobFn<
  Input = unknown,
  Output = unknown,
  Context = Pick<ServerContext, "entities">,
> = (input: Input, context: Context) => Promise<Output>;

export type MiddlewareConfig<Handler = any> = Map<string, Handler>;

export type MiddlewareConfigFn<Config = MiddlewareConfig> = (
  config: Config,
) => Config;
