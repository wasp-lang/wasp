import cors from "cors";
import { type Application } from "express";
import {
  config,
  env,
  type MiddlewareConfigFn,
  type ServerSetupFn,
} from "wasp/server";
import { mySpecialJob } from "wasp/server/jobs";

import "./rpcTests/operations/server";

let someResource: string | undefined = undefined;

export const getSomeResource = () => someResource;

export const setup: ServerSetupFn = async ({ app }) => {
  addCustomRoute(app);

  await new Promise((resolve) => setTimeout(resolve, 2000));
  someResource = "This resource is now set up.";
  console.log("Custom server setup done!");

  console.log("Kicking off Job...");
  // Or: const submittedJob = await mySpecialJob.delay(10).submit({ something: "here" })
  const submittedJob = await mySpecialJob.submit({ something: "here" });
  console.log(
    submittedJob.jobId,
    submittedJob.job.jobName,
    submittedJob.job.executorName,
  );
  console.log(
    "submittedJob.pgBoss.details()",
    await submittedJob.pgBoss.details(),
  );

  console.log("Env var TEST_ENV_VAR:", env.TEST_ENV_VAR);
};

function addCustomRoute(app: Application) {
  app.get("/customRoute", (_req, res) => {
    res.set("Access-Control-Allow-Origin", "example-cors-override.com");
    res.removeHeader("X-Frame-Options");
    res.send("I am a custom route");
  });
}

export const serverMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {
  // Example of adding an extra domain to CORS.
  middlewareConfig.set(
    "cors",
    cors({ origin: [...config.allowedCORSOrigins, "http://127.0.0.1:3000"] }),
  );
  return middlewareConfig;
};
