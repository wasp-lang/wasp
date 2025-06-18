import { exit } from "process";

import { ProcessOutput } from "zx";

import { createCommandWithDirectory } from "../../../common/cli.js";
import { WaspProjectDir } from "../../../common/cliArgs.js";
import { waspSays } from "../../../common/terminal.js";
import { RailwayCliExe } from "../CommonOptions.js";
import { ClientServiceName, ServerServiceName } from "../DeploymentInfo.js";

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
    const railwayCli = createCommandWithDirectory(
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
      // Continue with the deployment, but notify the user about the known issue.
      waspSays(
        `Failed to stream build log for service "${service.name}". This sometimes happens with the Railway CLI. Please check the Railway dashboard for build logs.`,
      );
      return ServiceDeploymentStatus.FAILED_STREAMING_LOGS;
    }

    waspSays(
      `Error deploying service "${service.name}", please check the output above.`,
    );
    exit(1);
  }
}

function isFailedToStreamLogsError(error: unknown): boolean {
  return (
    isProcessOutputError(error) &&
    error.stderr.includes("Failed to stream build log")
  );
}

function isProcessOutputError(error: unknown): error is ProcessOutput {
  return error instanceof ProcessOutput;
}
