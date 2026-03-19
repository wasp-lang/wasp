import { $ } from "zx";

import { WaspProjectDir } from "../../../common/brandedTypes.js";
import { waspSays } from "../../../common/terminal.js";
import { createCommandWithCwd } from "../../../common/zx.js";
import { RailwayCliExe, RailwayProjectName } from "../brandedTypes.js";
import {
  RailwayCliProjectSchema,
  RailwayProjectListSchema,
} from "../jsonOutputSchemas.js";
import {
  createLinkedRailwayProject,
  createUnlinkedRailwayProject,
  LinkedRailwayProject,
  UnlinkedRailwayProject,
} from "./RailwayProject.js";

/**
 * Initializing a Railway project means creating a new project
 * in Railway platform and linking it to the current
 * Wasp project directory.
 */
export async function initRailwayProject({
  projectName,
  railwayExe,
  waspProjectDir,
  workspace,
}: {
  projectName: RailwayProjectName;
  railwayExe: RailwayCliExe;
  waspProjectDir: WaspProjectDir;
  workspace?: string;
}): Promise<LinkedRailwayProject> {
  const railwayCli = createCommandWithCwd(railwayExe, waspProjectDir);

  const workspaceArgs = workspace ? ["--workspace", workspace] : [];

  await railwayCli(["init", "--name", projectName, ...workspaceArgs], {
    // If there are multiple workspaces and the user has not specified which one
    // they want, the CLI will ask for it **interactively**, so we need users to
    // be able to interact with the command.
    stdio: "inherit",
  });

  // Check if the project was created successfully...
  const newRailwayProject = await getLinkedRailwayProjectForDirectory(
    railwayExe,
    waspProjectDir,
  );
  if (newRailwayProject === null) {
    throw new Error("Railway project creation failed.");
  }

  waspSays("Railway project created successfully!");
  return newRailwayProject;
}

/**
 * Linking a Railway project means associating an existing
 * Railway project with the current Wasp project directory.
 */
export async function linkRailwayProjectToWaspProjectDir(
  project: UnlinkedRailwayProject,
  {
    railwayExe,
    waspProjectDir,
  }: {
    railwayExe: RailwayCliExe;
    waspProjectDir: WaspProjectDir;
  },
): Promise<LinkedRailwayProject> {
  const railwayCli = createCommandWithCwd(railwayExe, waspProjectDir);

  // Railway CLI quirk, if the project has services, we need to specify one of them,
  // otherwise Railway will ask for it interactively. (Even though we are linking
  // a project and not a service.)
  const serviceArg =
    project.services.length > 0 ? ["-s", project.services[0].name] : [];
  await railwayCli(["link", "-p", project.name, ...serviceArg]);

  // Sometimes linking fails silently, so we need to check if the project
  // was linked successfully.
  const linkedRailwayProject = await getLinkedRailwayProjectForDirectory(
    railwayExe,
    waspProjectDir,
  );
  if (linkedRailwayProject === null || linkedRailwayProject.id !== project.id) {
    throw new Error("Railway project linking failed.");
  }

  waspSays("Railway project linked successfully!");
  return linkedRailwayProject;
}

export async function getLinkedRailwayProjectForDirectory(
  railwayExe: RailwayCliExe,
  directoryPath: string,
): Promise<LinkedRailwayProject | null> {
  const railwayCli = createCommandWithCwd(railwayExe, directoryPath);
  const result = await railwayCli(["status", "--json"], {
    verbose: false,
    nothrow: true,
  });
  if (result.exitCode === 0) {
    return createLinkedRailwayProject(
      RailwayCliProjectSchema.parse(result.json()),
    );
  } else {
    return null;
  }
}

export async function getUnlinkedRailwayProjectById(
  railwayExe: RailwayCliExe,
  id: string,
): Promise<UnlinkedRailwayProject | null> {
  const projects = await getUnlinkedRailwayProjects(railwayExe);
  return projects.find((project) => project.id === id) ?? null;
}

export async function getUnlinkedRailwayProjectByName(
  railwayExe: RailwayCliExe,
  name: string,
): Promise<UnlinkedRailwayProject | null> {
  const projects = await getUnlinkedRailwayProjects(railwayExe);
  return projects.find((project) => project.name === name) ?? null;
}

// TODO: Figure out how to specify the workspace when listing projects.
// This command lists all projects in all the workspaces the user has access to.
async function getUnlinkedRailwayProjects(
  railwayExe: RailwayCliExe,
): Promise<UnlinkedRailwayProject[]> {
  const result = await $({
    verbose: false,
  })`${railwayExe} list --json`;

  const projects = RailwayProjectListSchema.parse(JSON.parse(result.stdout));

  return projects.map((cliProject) => {
    return createUnlinkedRailwayProject(cliProject);
  });
}
