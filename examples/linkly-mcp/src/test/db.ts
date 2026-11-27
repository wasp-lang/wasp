import { PrismaClient } from "@prisma/client";
import { randomUUID } from "node:crypto";

// A real Postgres test database, shared by all server unit tests.
export const prisma = new PrismaClient({
  datasources: { db: { url: process.env.DATABASE_URL } },
});

export async function resetDb(): Promise<void> {
  const tables = await prisma.$queryRaw<{ tablename: string }[]>`
    SELECT tablename FROM pg_tables WHERE schemaname = 'public' AND tablename <> '_prisma_migrations'`;
  if (tables.length === 0) return;
  const list = tables.map((t) => `"public"."${t.tablename}"`).join(", ");
  await prisma.$executeRawUnsafe(`TRUNCATE TABLE ${list} CASCADE`);
}

/** Creates a bare User row (no auth identity: services only need the id). */
export async function createTestUser(): Promise<{ id: string }> {
  return prisma.user.create({ data: { id: randomUUID() } });
}
