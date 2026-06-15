import { action, job, page, query, route, type Spec } from "@wasp.sh/spec";

import {
  mySpecialJob,
  mySpecialScheduledJob,
} from "./bar" with { type: "ref" };
import { JobsPage } from "./pages/JobsPage" with { type: "ref" };
import {
  getTextUppercaseRequests,
  requestUppercaseText,
  uppercaseTextJob,
} from "./uppercaseText" with { type: "ref" };

export const jobsSpec: Spec = [
  route("JobsRoute", "/jobs", page(JobsPage, { authRequired: true })),
  action(requestUppercaseText, { entities: ["UppercaseTextRequest"] }),
  query(getTextUppercaseRequests, { entities: ["UppercaseTextRequest"] }),
  job(uppercaseTextJob, {
    executor: "PgBoss",
    entities: ["UppercaseTextRequest"],
  }),
  job(mySpecialJob, {
    executor: "PgBoss",
    performExecutorOptions: { pgBoss: { retryLimit: 1 } },
    entities: ["Task"],
  }),
  job(mySpecialScheduledJob, {
    executor: "PgBoss",
    schedule: {
      cron: "0 * * * *",
      args: { foo: "bar" },
      executorOptions: { pgBoss: { retryLimit: 2 } },
    },
  }),
];
