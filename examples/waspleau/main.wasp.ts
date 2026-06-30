import { app, job, page, query, route } from "@wasp.sh/spec";

import { refreshDashboardData } from "./src/dashboard" with { type: "ref" };
import MainPage from "./src/MainPage" with { type: "ref" };
import serverSetup from "./src/serverSetup" with { type: "ref" };
import { refreshGitHubRepoData } from "./src/workers/github" with { type: "ref" };
import { measureLoadTime } from "./src/workers/loadTime" with { type: "ref" };

export default app({
  name: "waspleau",
  wasp: { version: "0.25.0" },
  title: "Waspleau",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  server: {
    setupFn: serverSetup,
  },
  spec: [
    job(refreshGitHubRepoData, {
      executor: "PgBoss",
      schedule: {
        cron: "*/10 * * * *",
      },
      entities: ["Datum"],
    }),
    job(measureLoadTime, {
      executor: "PgBoss",
      schedule: {
        cron: "*/5 * * * *",
        args: {
          url: "https://wasp.sh",
          name: "wasp.sh Load Time",
        },
      },
      entities: ["Datum"],
    }),
    route("RootsRoute", "/", page(MainPage), { prerender: true }),
    query(refreshDashboardData, { entities: ["Datum"] }),
  ],
});
