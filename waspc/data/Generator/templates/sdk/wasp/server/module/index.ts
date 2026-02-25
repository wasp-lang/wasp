export interface PrismaDelegate<T = any> {
  findMany(args?: any): Promise<T[]>;
  findUnique(args: any): Promise<T | null>;
  findFirst(args?: any): Promise<T | null>;
  create(args: any): Promise<T>;
  update(args: any): Promise<T>;
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
