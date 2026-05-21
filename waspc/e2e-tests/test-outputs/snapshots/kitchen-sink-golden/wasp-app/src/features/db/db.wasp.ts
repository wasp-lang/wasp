import { setUpPrisma } from "./prisma" with { type: "ref" };
import { devSeedSimple, prodSeed } from "./seeds" with { type: "ref" };

export const db = {
  seeds: [devSeedSimple, prodSeed],
  prismaSetupFn: setUpPrisma,
};
