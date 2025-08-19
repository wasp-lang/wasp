import { ProcessOutput } from "zx";

import { WaspProjectDir } from "../../../../common/brandedTypes.js";
import { waspSays } from "../../../../common/terminal.js";
import { createCommandWithCwd } from "../../../../common/zx.js";
import {
  ClientServiceName,
  RailwayCliExe,
  ServerServiceName,
} from "../../brandedTypes.js";

export enum ServiceDeploymentStatus {
  SUCCESS = "SUCCESS",
  FAILED_TO_STREAM_LOGS = "FAILED_TO_STREAM_LOGS",
}

export async function deployServiceWithStreamingLogs(
  service: {
    name: ClientServiceName | ServerServiceName;
    dirToDeploy: string;
  },
  options: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<ServiceDeploymentStatus> {
  try {
    const railwayCli = createCommandWithCwd(
      options.railwayExe,
      options.waspProjectDir,
    );
    await railwayCli([
      "up",
      service.dirToDeploy,
      "--service",
      service.name,
      "--no-gitignore",
      // This option fixes a Railway CLI quirk to package the
      // service directory as the root of the deployment.
      "--path-as-root",
      // This option enables service build logs streaming.
      "--ci",
    ]);
    return ServiceDeploymentStatus.SUCCESS;
  } catch (e: unknown) {
    if (isFailedToStreamLogsError(e)) {
      // The deployment most likely didn't fail, we only failed to stream the logs.
      // This can happen with the Railway CLI, and the deployment might still be successful.
      waspSays(
        `Failed to stream build log for service "${service.name}". This sometimes happens with the Railway CLI. Please check the Railway dashboard for build logs.`,
      );
      return ServiceDeploymentStatus.FAILED_TO_STREAM_LOGS;
    }

    throw new Error(
      `Error deploying service "${service.name}", please check the command output.`,
    );
  }
}

function isFailedToStreamLogsError(error: unknown): boolean {
  return (
    error instanceof ProcessOutput &&
    error.stderr.includes("Failed to stream build log")
  );
}
