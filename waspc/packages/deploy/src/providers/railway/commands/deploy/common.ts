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
  FAILED_STREAMING_LOGS = "FAILED_STREAMING_LOGS",
}

export async function deployServiceWithStreamingLogs(
  service: {
    name: ClientServiceName | ServerServiceName;
    artefactsDirectory: string;
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
      service.artefactsDirectory,
      "--service",
      service.name,
      "--no-gitignore",
      "--path-as-root",
      "--ci",
    ]);
    return ServiceDeploymentStatus.SUCCESS;
  } catch (e: unknown) {
    if (isFailedToStreamLogsError(e)) {
      // The deployment didn't fail, but we couldn't stream the logs.
      // This can happen with the Railway CLI, and the deployment might still be successful.
      waspSays(
        `Failed to stream build log for service "${service.name}". This sometimes happens with the Railway CLI. Please check the Railway dashboard for build logs.`,
      );
      return ServiceDeploymentStatus.FAILED_STREAMING_LOGS;
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
