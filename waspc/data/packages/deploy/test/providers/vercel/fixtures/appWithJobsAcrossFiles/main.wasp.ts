import { app } from "@wasp.sh/spec";

import { jobsSpec } from "./src/features/jobs/jobs.wasp";

export default app({
  name: "AppWithJobsAcrossFiles",
  wasp: { version: "0.25.0" },
  spec: [...jobsSpec],
});
