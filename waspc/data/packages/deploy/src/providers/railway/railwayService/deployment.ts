import { setTimeout } from "node:timers/promises";

import { WaspProjectDir } from "../../../common/brandedTypes.js";
import { waspSays } from "../../../common/terminal.js";
import { createCommandWithCwd } from "../../../common/zx.js";
import { RailwayCliExe, RailwayServiceName } from "../brandedTypes.js";
import {
  DeploymentStatus,
  RailwayCliProjectStatus,
  RailwayCliProjectStatusSchema,
} from "../jsonOutputSchemas.js";

const POLL_INTERVAL_MS = 5_000;
const TIMEOUT_MS = 5 * 60 * 1_000;

// Any other status means the deployment is still in progress.
const SUCCESS_STATUS: DeploymentStatus = "SUCCESS";
const FAILURE_STATUSES: DeploymentStatus[] = ["FAILED", "CRASHED"];

export async function waitForServiceDeploymentSuccess(
  serviceName: RailwayServiceName,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<void> {
  const deadline = Date.now() + TIMEOUT_MS;
  while (Date.now() < deadline) {
    const status = await getLatestServiceDeploymentStatus(serviceName, options);

    if (status === SUCCESS_STATUS) {
      return;
    }

    if (status !== null && FAILURE_STATUSES.includes(status)) {
      throw new Error(
        `"${serviceName}" deployment finished with status "${status}". Check the Railway dashboard for details.`,
      );
    }

    waspSays(
      `Waiting for "${serviceName}" deployment... (Status: "${status ?? "UNKNOWN"}")`,
    );

    await setTimeout(POLL_INTERVAL_MS);
  }

  throw new Error(
    `Timed out waiting for "${serviceName}" to be deployed. Check the Railway dashboard for details.`,
  );
}

async function getLatestServiceDeploymentStatus(
  serviceName: RailwayServiceName,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<DeploymentStatus | null> {
  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );
  const result = await railwayCli(["status", "--json"], {
    verbose: false,
    nothrow: true,
  });
  if (result.exitCode !== 0) {
    // Treat transient `railway status` failures as "not ready yet".
    return null;
  }

  const projectStatus = RailwayCliProjectStatusSchema.parse(result.json());
  return findServiceDeploymentStatus(projectStatus, serviceName);
}

export function findServiceDeploymentStatus(
  projectStatus: RailwayCliProjectStatus,
  serviceName: string,
): DeploymentStatus | null {
  const serviceInstance = projectStatus.environments.edges
    .flatMap((environment) => environment.node.serviceInstances.edges)
    .map((edge) => edge.node)
    .find((instance) => instance.serviceName === serviceName);

  return serviceInstance?.latestDeployment?.status ?? null;
}
