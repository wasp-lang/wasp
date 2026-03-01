export interface PrismaDelegate<T = any> {
  findMany(args?: any): Promise<T[]>;
  findUnique(args: any): Promise<T | null>;
  findUniqueOrThrow(args: any): Promise<T>;
  findFirst(args?: any): Promise<T | null>;
  findFirstOrThrow(args?: any): Promise<T>;
  create(args: any): Promise<T>;
  createMany(args: any): Promise<{ count: number }>;
  update(args: any): Promise<T>;
  updateMany(args: any): Promise<{ count: number }>;
  upsert(args: any): Promise<T>;
  delete(args: any): Promise<T>;
  deleteMany(args?: any): Promise<{ count: number }>;
  count(args?: any): Promise<number>;
}

export type AuthUser = {
  id: number | string;
  identities: Record<string, { id: string } | null>;
  getFirstProviderUserId: () => string | null;
  [key: string]: unknown;
};

// Typed operation context — Entities maps alias names to their types.
// Example: { Todo: { id: number; text: string; isDone: boolean } }
export type OperationContext<
  Entities extends Record<string, any> = Record<string, any>,
> = {
  entities: { [K in keyof Entities]: PrismaDelegate<Entities[K]> };
};

export type AuthOperationContext<
  Entities extends Record<string, any> = Record<string, any>,
> = OperationContext<Entities> & {
  user?: AuthUser;
};

// New typed operation types (3 generics: Entities, Input, Output)
export type Query<
  Entities extends Record<string, any>,
  Input,
  Output,
> = (args: Input, context: OperationContext<Entities>) => Output | Promise<Output>;

export type AuthenticatedQuery<
  Entities extends Record<string, any>,
  Input,
  Output,
> = (args: Input, context: AuthOperationContext<Entities>) => Output | Promise<Output>;

export type Action<
  Entities extends Record<string, any>,
  Input,
  Output,
> = (args: Input, context: OperationContext<Entities>) => Output | Promise<Output>;

export type AuthenticatedAction<
  Entities extends Record<string, any>,
  Input,
  Output,
> = (args: Input, context: AuthOperationContext<Entities>) => Output | Promise<Output>;

// Legacy 2-generic types (backward compatible, untyped entities)
export type UnauthenticatedQueryDefinition<Input, Output> = (
  args: Input,
  context: OperationContext,
) => Output | Promise<Output>;

export type UnauthenticatedActionDefinition<Input, Output> = (
  args: Input,
  context: OperationContext,
) => Output | Promise<Output>;

export type AuthenticatedQueryDefinition<Input, Output> = (
  args: Input,
  context: AuthOperationContext,
) => Output | Promise<Output>;

export type AuthenticatedActionDefinition<Input, Output> = (
  args: Input,
  context: AuthOperationContext,
) => Output | Promise<Output>;

// Re-export HttpError for module convenience
export { HttpError } from 'wasp/server';
