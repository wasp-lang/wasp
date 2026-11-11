import { type Db } from "@wasp/spec";

import { createPrismaClient } from "./prisma" with { type: "ref" };
import { devSeedSimple, prodSeed } from "./seeds" with { type: "ref" };

export const db: Db = {
  seeds: [devSeedSimple, prodSeed],
  createPrismaClient,
};
