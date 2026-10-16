import { setTimeout } from "node:timers/promises";

import { WaspProjectDir } from "../../../common/brandedTypes.js";
import { waspSays } from "../../../common/terminal.js";
import { createCommandWithCwd, tryRunJsonCommand } from "../../../common/zx.js";
import { RailwayCliExe } from "../brandedTypes.js";
import {
  DeploymentStatus,
  RailwayCliService,
  RailwayCliServiceStatusSchema,
} from "../jsonOutputSchemas.js";

const POLL_INTERVAL_MS = 5_000;
const TIMEOUT_MS = 5 * 60 * 1_000;

// Any other status means the deployment is still in progress.
const SUCCESS_STATUS: DeploymentStatus = "SUCCESS";
const FAILURE_STATUSES: DeploymentStatus[] = ["FAILED", "CRASHED"];

export async function waitForServiceDeploymentSuccess(
  service: RailwayCliService,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<void> {
  const deadline = Date.now() + TIMEOUT_MS;
  while (Date.now() < deadline) {
    const status = await getLatestServiceDeploymentStatus(service, options);

    if (status === SUCCESS_STATUS) {
      return;
    }

    if (status !== null && FAILURE_STATUSES.includes(status)) {
      throw new Error(
        `"${service.name}" deployment finished with status "${status}". Check the Railway dashboard for details.`,
      );
    }

    waspSays(
      `Waiting for "${service.name}" deployment... (Status: "${status ?? "UNKNOWN"}")`,
    );

    await setTimeout(POLL_INTERVAL_MS);
  }

  throw new Error(
    `Timed out waiting for "${service.name}" to be deployed. Check the Railway dashboard for details.`,
  );
}

async function getLatestServiceDeploymentStatus(
  service: RailwayCliService,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<DeploymentStatus | null> {
  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );
  const serviceStatus = await tryRunJsonCommand(
    railwayCli,
    ["service", "status", "--service", service.id, "--json"],
    RailwayCliServiceStatusSchema,
  );
  // Treat a failed command as a transient Railway CLI failure, i.e. "not
  // ready yet".
  return serviceStatus?.status ?? null;
}
