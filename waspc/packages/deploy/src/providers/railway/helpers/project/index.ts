import { exit } from "process";

import { waspSays } from "../../../../helpers.js";
import { DeploymentInfo } from "../../DeploymentInfo.js";
import { SetupOptions } from "../../setup/SetupOptions.js";
import {
  getProjectForCurrentDir,
  getProjects,
  initProject,
  linkProject,
  RailwayProject,
} from "./cli.js";

export enum ProjectStatus {
  EXISTING_PROJECT_LINKED = "EXISTING_PROJECT_LINKED",
  LINK_PROJECT = "LINK_PROJECT",
  NEW_PROJECT = "NEW_PROJECT",
  EXISTING_PROJECT_DIFFERENT_NAME_ERROR = "EXISTING_PROJECT_DIFFERENT_NAME_ERROR",
  PROJECT_WITH_ID_NOT_FOUND_ERROR = "PROJECT_WITH_ID_NOT_FOUND_ERROR",
  PROJECT_WITH_NAME_EXISTS_ERROR = "PROJECT_WITH_NAME_EXISTS_ERROR",
}

export async function ensureProjectForCurrentDir(
  deploymentInfo: DeploymentInfo<SetupOptions>,
): Promise<RailwayProject> {
  const { baseName, options } = deploymentInfo;

  const { status, project } = await getProject(options, baseName);

  switch (status) {
    case ProjectStatus.EXISTING_PROJECT_LINKED:
      waspSays(
        `Project with name "${baseName}" already linked. Skipping project creation.`,
      );
      return project;

    case ProjectStatus.LINK_PROJECT:
      waspSays(`Linking project with name "${project.name}" to this directory`);
      return linkProject(deploymentInfo, project);

    case ProjectStatus.NEW_PROJECT:
      waspSays(`Setting up Railway project with name "${baseName}"`);
      return initProject(deploymentInfo);

    case ProjectStatus.EXISTING_PROJECT_DIFFERENT_NAME_ERROR:
      waspSays(
        `Project with a different name already linked to this directory: "${project.name}". Run "railway unlink" to unlink it.`,
      );
      return exit(1);

    case ProjectStatus.PROJECT_WITH_ID_NOT_FOUND_ERROR:
      waspSays(
        `Project with ID "${options.existingProjectId}" does not exist.`,
      );
      return exit(1);

    case ProjectStatus.PROJECT_WITH_NAME_EXISTS_ERROR:
      waspSays(
        `Project with name "${project.name}" already exists. Add "--existing-project-id ${project.id}" option to this command to link it or use a different name.`,
      );
      return exit(1);

    default:
      status satisfies never;
      throw new Error(`Unhandled status: ${status}`);
  }
}

async function getProject(options: SetupOptions, baseName: string) {
  const projectLinkedToDir = await getProjectForCurrentDir(options.railwayExe);

  // Project already linked to this directory
  if (projectLinkedToDir !== null) {
    if (projectLinkedToDir.name === baseName) {
      return {
        status: ProjectStatus.EXISTING_PROJECT_LINKED,
        project: projectLinkedToDir,
      } as const;
    } else {
      return {
        status: ProjectStatus.EXISTING_PROJECT_DIFFERENT_NAME_ERROR,
        project: projectLinkedToDir,
      } as const;
    }
  }

  // Trying to use an existing project
  if (options.existingProjectId) {
    const projectById = await getProjectById(
      options.railwayExe,
      options.existingProjectId,
    );
    if (projectById !== null) {
      return {
        status: ProjectStatus.LINK_PROJECT,
        project: projectById,
      } as const;
    } else {
      return {
        status: ProjectStatus.PROJECT_WITH_ID_NOT_FOUND_ERROR,
        project: null,
      } as const;
    }
  }

  // Trying to create a new project
  const projectByName = await getProjectByName(options.railwayExe, baseName);
  if (projectByName !== null) {
    return {
      status: ProjectStatus.PROJECT_WITH_NAME_EXISTS_ERROR,
      project: projectByName,
    } as const;
  } else {
    return {
      status: ProjectStatus.NEW_PROJECT,
      project: null,
    } as const;
  }
}

async function getProjectById(
  railwayExe: string,
  remoteProjectId: string,
): Promise<RailwayProject | null> {
  const projects = await getProjects(railwayExe);
  return projects.find((project) => project.id === remoteProjectId) ?? null;
}

async function getProjectByName(
  railwayExe: string,
  name: string,
): Promise<RailwayProject | null> {
  const projects = await getProjects(railwayExe);
  return projects.find((project) => project.name === name) ?? null;
}
