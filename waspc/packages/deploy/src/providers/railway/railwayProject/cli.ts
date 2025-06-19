import { $ } from "zx";

import { WaspProjectDir } from "../../../common/brandedTypes.js";
import { waspSays } from "../../../common/terminal.js";
import { createCommandWithCwd } from "../../../common/zx.js";
import { RailwayCliExe } from "../brandedTypes.js";
import { SetupOptions } from "../commands/setup/SetupOptions.js";
import { DeploymentInfo } from "../DeploymentInfo.js";
import {
  RailwayCliProjectSchema,
  RailwayProjectListSchema,
} from "../jsonOutputSchemas.js";
import { RailwayProject } from "./RailwayProject.js";

export async function initRailwayProject({
  projectName,
  options,
}: DeploymentInfo<SetupOptions>): Promise<RailwayProject> {
  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );
  await railwayCli(["init", "--name", projectName], {
    // If there are multiple workspaces, the user needs to select **interactively**
    // which one to use. We need to allow users to select the workspace interactively.
    // There is no way to pass it as a command line argument.
    stdio: "inherit",
  });

  // Check if the project was created successfully...
  const newProject = await getRailwayProjectForDirectory(
    options.railwayExe,
    options.waspProjectDir,
  );
  if (newProject === null) {
    throw new Error("Project creation failed.");
  }

  waspSays("Project created successfully!");
  return newProject;
}

export async function linkRailwayProject(
  project: RailwayProject,
  { options }: DeploymentInfo<SetupOptions>,
): Promise<RailwayProject> {
  const railwayCli = createCommandWithCwd(
    options.railwayExe,
    options.waspProjectDir,
  );

  // Railway CLI quirk, if the project has services, we need to specify one of them,
  // otherwise Railway will ask for it interactively. (Even though we are linking
  // a project and not a service.)
  const serviceArg =
    project.services.length > 0 ? ["-s", project.services[0].name] : [];
  await railwayCli(["link", "-p", project.name, ...serviceArg]);

  // Sometimes linking fails silently, so we need to check if the project
  // was linked successfully.
  const linkedProject = await getRailwayProjectForDirectory(
    options.railwayExe,
    options.waspProjectDir,
  );
  if (linkedProject === null) {
    throw new Error("Project linking failed.");
  }

  waspSays("Project linked successfully!");
  return linkedProject;
}

export async function getRailwayProjectForDirectory(
  railwayExe: RailwayCliExe,
  waspProjectDir: WaspProjectDir,
): Promise<RailwayProject | null> {
  const railwayCli = createCommandWithCwd(railwayExe, waspProjectDir);
  const result = await railwayCli(["status", "--json"], {
    verbose: false,
    nothrow: true,
    // Ignoring stdin and stderr to stop error output from Railway CLI
    stdio: ["ignore", "pipe", "ignore"],
  });
  if (result.exitCode !== 0) {
    return null;
  } else {
    return new RailwayProject(
      RailwayCliProjectSchema.parse(JSON.parse(result.stdout)),
    );
  }
}

export async function getProjectById(
  railwayExe: RailwayCliExe,
  remoteProjectId: string,
): Promise<RailwayProject | null> {
  const projects = await getProjects(railwayExe);
  return projects.find((project) => project.id === remoteProjectId) ?? null;
}

export async function getProjectByName(
  railwayExe: RailwayCliExe,
  name: string,
): Promise<RailwayProject | null> {
  const projects = await getProjects(railwayExe);
  return projects.find((project) => project.name === name) ?? null;
}

// TODO: Figure out how to specify the workspace when listing projects.
// This command lists all projects in all the workspaces the user has access to.
async function getProjects(
  railwayExe: RailwayCliExe,
): Promise<RailwayProject[]> {
  const result = await $({
    verbose: false,
  })`${railwayExe} list --json`;

  const projects = RailwayProjectListSchema.parse(JSON.parse(result.stdout));

  return projects.map((cliProject) => {
    return new RailwayProject(cliProject);
  });
}
