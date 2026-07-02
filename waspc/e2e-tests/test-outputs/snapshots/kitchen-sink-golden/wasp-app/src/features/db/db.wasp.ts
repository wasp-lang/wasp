import { type Db } from "@wasp.sh/spec";

import { setUpPrisma } from "./prisma" with { type: "ref" };
import { devSeedSimple, prodSeed } from "./seeds" with { type: "ref" };

export const db: Db = {
  seeds: [devSeedSimple, prodSeed],
  prismaSetupFn: setUpPrisma,
};
