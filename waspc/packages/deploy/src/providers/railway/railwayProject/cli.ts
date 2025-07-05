import { $ } from "zx";

import { WaspProjectDir } from "../../../common/brandedTypes.js";
import { waspSays } from "../../../common/terminal.js";
import { createCommandWithCwd } from "../../../common/zx.js";
import { RailwayCliExe, RailwayProjectName } from "../brandedTypes.js";
import {
  RailwayCliProjectSchema,
  RailwayProjectListSchema,
} from "../jsonOutputSchemas.js";
import { RailwayProject } from "./RailwayProject.js";

export async function initRailwayProject({
  projectName,
  railwayExe,
  waspProjectDir,
}: {
  projectName: RailwayProjectName;
  railwayExe: RailwayCliExe;
  waspProjectDir: WaspProjectDir;
}): Promise<RailwayProject> {
  const railwayCli = createCommandWithCwd(railwayExe, waspProjectDir);
  await railwayCli(["init", "--name", projectName], {
    // If there are multiple workspaces, the user needs to select **interactively**
    // which one to use. We need to allow users to select the workspace interactively.
    // Railway CLI doesn't accept it as a command line argument, only interactively.
    stdio: "inherit",
  });

  // Check if the project was created successfully...
  const newRailwayProject = await getRailwayProjectForDirectory(
    railwayExe,
    waspProjectDir,
  );
  if (newRailwayProject === null) {
    throw new Error("Railway project creation failed.");
  }

  waspSays("Railway project created successfully!");
  return newRailwayProject;
}

export async function linkRailwayProjectToWaspProjectDir(
  project: RailwayProject,
  {
    railwayExe,
    waspProjectDir,
  }: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<RailwayProject> {
  const railwayCli = createCommandWithCwd(railwayExe, waspProjectDir);

  // Railway CLI quirk, if the project has services, we need to specify one of them,
  // otherwise Railway will ask for it interactively. (Even though we are linking
  // a project and not a service.)
  const serviceArg =
    project.services.length > 0 ? ["-s", project.services[0].name] : [];
  await railwayCli(["link", "-p", project.name, ...serviceArg]);

  // Sometimes linking fails silently, so we need to check if the project
  // was linked successfully.
  const linkedRailwayProject = await getRailwayProjectForDirectory(
    railwayExe,
    waspProjectDir,
  );
  if (linkedRailwayProject === null) {
    throw new Error("Railway project linking failed.");
  }

  waspSays("Railway project linked successfully!");
  return linkedRailwayProject;
}

export async function getRailwayProjectForDirectory(
  railwayExe: RailwayCliExe,
  directoryPath: string,
): Promise<RailwayProject | null> {
  const railwayCli = createCommandWithCwd(railwayExe, directoryPath);
  const result = await railwayCli(["status", "--json"], {
    verbose: false,
    nothrow: true,
  });
  if (result.exitCode === 0) {
    return new RailwayProject(RailwayCliProjectSchema.parse(result.json()));
  } else {
    return null;
  }
}

export async function getRailwayProjectById(
  railwayExe: RailwayCliExe,
  id: string,
): Promise<RailwayProject | null> {
  const projects = await getRailwayProjects(railwayExe);
  return projects.find((project) => project.id === id) ?? null;
}

export async function getRailwayProjectByName(
  railwayExe: RailwayCliExe,
  name: string,
): Promise<RailwayProject | null> {
  const projects = await getRailwayProjects(railwayExe);
  return projects.find((project) => project.name === name) ?? null;
}

// TODO: Figure out how to specify the workspace when listing projects.
// This command lists all projects in all the workspaces the user has access to.
async function getRailwayProjects(
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
