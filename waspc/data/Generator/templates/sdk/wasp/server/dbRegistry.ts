type PrismaClient = any;
let _prismaClient: PrismaClient | undefined = undefined;

export function registerPrismaClient(fn: PrismaClient): void {
  _prismaClient = fn;
}

export function getPrismaClient(): PrismaClient {
  if (!_prismaClient) {
    throw new Error(`
      'Internal Wasp error: _prismaClient is not registered.`
    );
  }
  return _prismaClient;
}