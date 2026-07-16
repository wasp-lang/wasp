import { app, job } from "@wasp.sh/spec";

import { sendEmailJob } from "./src/jobs";

export default app({
  name: "AppWithJob",
  wasp: { version: "0.25.0" },
  spec: [
    job(sendEmailJob, {
      executor: "PgBoss",
      schedule: { cron: "0 * * * *" },
    }),
  ],
});
