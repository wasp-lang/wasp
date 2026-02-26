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

export type OperationContext = {
  entities: Record<string, PrismaDelegate>;
};

export type AuthUser = {
  id: number | string;
  identities: Record<string, { id: string } | null>;
  getFirstProviderUserId: () => string | null;
  [key: string]: unknown;
};

export type AuthOperationContext = OperationContext & {
  user?: AuthUser;
};

// Simplified versions of the SDK's operation definition types
// (from wasp/server/_types). These use 2 generics instead of 3 because modules
// don't know the app's entities at compile time (entities are a dynamic
// Record<string, PrismaDelegate> rather than a tagged _Entity[] type).
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
