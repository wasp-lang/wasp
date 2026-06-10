import { setTimeout } from "node:timers/promises";

import { WaspProjectDir } from "../../../common/brandedTypes.js";
import { waspSays } from "../../../common/terminal.js";
import { createCommandWithCwd } from "../../../common/zx.js";
import { RailwayCliExe, RailwayServiceName } from "../brandedTypes.js";
import {
  RailwayCliProjectStatus,
  RailwayCliProjectStatusSchema,
} from "../jsonOutputSchemas.js";

const DEFAULT_POLL_INTERVAL_MS = 3_000;
const DEFAULT_TIMEOUT_MS = 5 * 60 * 1_000;
const PROGRESS_MESSAGE_INTERVAL_MS = 30_000;

// From Railway's GraphQL `DeploymentStatus` enum, any other status
// (BUILDING, DEPLOYING, QUEUED, ...) means the deployment is still in progress.
const SUCCESS_STATUS = "SUCCESS";
const FAILURE_STATUSES = ["FAILED", "CRASHED"];

export async function waitForServiceDeploymentSuccess(
  serviceName: RailwayServiceName,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
  {
    pollIntervalMs = DEFAULT_POLL_INTERVAL_MS,
    timeoutMs = DEFAULT_TIMEOUT_MS,
  }: {
    pollIntervalMs?: number;
    timeoutMs?: number;
  } = {},
): Promise<void> {
  waspSays(`Waiting for the "${serviceName}" service to be deployed...`);

  let lastReportedStatus: string | null = null;
  let lastReportTimestamp = Date.now();

  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const status = await getLatestServiceDeploymentStatus(serviceName, options);

    if (status === SUCCESS_STATUS) {
      waspSays(`Service "${serviceName}" deployed successfully!`);
      return;
    }

    if (status !== null && FAILURE_STATUSES.includes(status)) {
      throw new Error(
        `Service "${serviceName}" deployment finished with status "${status}". Check the Railway dashboard for details.`,
      );
    }

    if (status !== null && status !== lastReportedStatus) {
      waspSays(`Service "${serviceName}" deployment status: ${status}`);
      lastReportedStatus = status;
      lastReportTimestamp = Date.now();
    } else if (
      Date.now() - lastReportTimestamp >=
      PROGRESS_MESSAGE_INTERVAL_MS
    ) {
      waspSays(`Still waiting for the "${serviceName}" service...`);
      lastReportTimestamp = Date.now();
    }

    await setTimeout(pollIntervalMs);
  }

  throw new Error(
    `Timed out waiting for the "${serviceName}" service to be deployed. Check the Railway dashboard for details.`,
  );
}

async function getLatestServiceDeploymentStatus(
  serviceName: RailwayServiceName,
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<string | null> {
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
): string | null {
  const serviceInstance = projectStatus.environments.edges
    .flatMap((environment) => environment.node.serviceInstances.edges)
    .map((edge) => edge.node)
    .find((instance) => instance.serviceName === serviceName);

  return serviceInstance?.latestDeployment?.status ?? null;
}
