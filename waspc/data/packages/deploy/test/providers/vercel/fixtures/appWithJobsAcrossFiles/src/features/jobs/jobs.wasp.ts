import { job } from "@wasp.sh/spec";

import { mySpecialJob, uppercaseTextJob } from "./bar";

export const jobsSpec = [
  job(uppercaseTextJob, {
    executor: "PgBoss",
    entities: ["UppercaseTextRequest"],
  }),
  job(mySpecialJob, {
    executor: "PgBoss",
    performExecutorOptions: { pgBoss: { retryLimit: 1 } },
    entities: ["Task"],
  }),
];
