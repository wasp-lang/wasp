import { exit } from "process";

import { $ } from "zx";

import { createCommandWithDirectory } from "../../../../common/cli.js";
import { WaspProjectDir } from "../../../../common/cliArgs.js";
import { waspSays } from "../../../../common/terminal.js";
import { RailwayCliExe } from "../../CommonOptions.js";
import { DeploymentInfo } from "../../DeploymentInfo.js";
import {
  type RailwayCliProject,
  RailwayCliProjectSchema,
  RailwayProjectListSchema,
} from "../../jsonOutputSchemas.js";
import { SetupOptions } from "../../setup/SetupOptions.js";

export async function initProject({
  projectName,
  options,
}: DeploymentInfo<SetupOptions>): Promise<RailwayProject> {
  const railwayCli = createCommandWithDirectory(
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
  const newProject = await getProjectForDirectory(
    options.railwayExe,
    options.waspProjectDir,
  );
  if (newProject === null) {
    waspSays("Project creation failed. Exiting...");
    exit(1);
  } else {
    waspSays("Project created successfully!");
  }

  return newProject;
}

export async function linkProject(
  project: RailwayProject,
  { options }: DeploymentInfo<SetupOptions>,
): Promise<RailwayProject> {
  const railwayCli = createCommandWithDirectory(
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
  const linkedProject = await getProjectForDirectory(
    options.railwayExe,
    options.waspProjectDir,
  );
  if (linkedProject === null) {
    waspSays("Project linking failed. Exiting...");
    exit(1);
  } else {
    waspSays("Project linked successfully!");
  }

  return linkedProject;
}

export async function getProjectForDirectory(
  railwayExe: RailwayCliExe,
  waspProjectDir: WaspProjectDir,
): Promise<RailwayProject | null> {
  const railwayCli = createCommandWithDirectory(railwayExe, waspProjectDir);
  const result = await railwayCli(["status", "--json"], {
    verbose: false,
    nothrow: true,
    // Ignoring stdin and stderr to stop error output from Railway CLI
    stdio: ["ignore", "pipe", "ignore"],
  });
  if (result.exitCode !== 0) {
    return null;
  } else {
    const project = RailwayCliProjectSchema.parse(JSON.parse(result.stdout));

    return new RailwayProject(project);
  }
}

// TODO: Figure out how to specify the workspace when listing projects.
// This command lists all projects in all the workspaces the user has access to.
export async function getProjects(
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

export class RailwayProject {
  id: string;
  name: string;
  services: RailwayService[];

  constructor(cliProject: RailwayCliProject) {
    this.id = cliProject.id;
    this.name = cliProject.name;
    this.services = cliProject.services.edges.map((edge) => ({
      id: edge.node.id,
      name: edge.node.name,
    }));
  }

  doesServiceExist(serviceName: string): boolean {
    return this.services.some((s) => s.name === serviceName);
  }
}

export type RailwayService = {
  id: string;
  name: string;
};
