import { exit } from "process";

import { WaspProjectDir } from "../../../../common/cliArgs.js";
import { waspSays } from "../../../../common/terminal.js";
import { RailwayCliExe } from "../../CommonOptions.js";
import { DeploymentInfo, RailwayProjectName } from "../../DeploymentInfo.js";
import { SetupOptions } from "../../setup/SetupOptions.js";
import {
  getProjectForDirectory,
  getProjects,
  initProject,
  linkProject,
  RailwayProject,
} from "./cli.js";

export enum ProjectStatus {
  EXISTING_PROJECT_ALREADY_LINKED = "EXISTING_PROJECT_ALREADY_LINKED",
  EXISTING_PROJECT_LINK_PROJECT = "EXISTING_PROJECT_LINK_PROJECT",
  CREATE_NEW_PROJECT = "CREATE_NEW_PROJECT",
  ERROR_EXISTING_PROJECT_DIFFERENT_NAME = "ERROR_EXISTING_PROJECT_DIFFERENT_NAME",
  ERROR_PROJECT_WITH_ID_NOT_FOUND = "ERROR_PROJECT_WITH_ID_NOT_FOUND",
  ERROR_PROJECT_WITH_NAME_EXISTS = "ERROR_PROJECT_WITH_NAME_EXISTS",
}

export async function ensureProjectForDirectory(
  waspProjectDirPath: WaspProjectDir,
  deploymentInfo: DeploymentInfo<SetupOptions>,
): Promise<RailwayProject> {
  const { projectName, options } = deploymentInfo;

  const { status, project } = await getProjectStatus(
    projectName,
    waspProjectDirPath,
    options,
  );

  switch (status) {
    case ProjectStatus.EXISTING_PROJECT_ALREADY_LINKED:
      waspSays(
        `Project with name "${projectName}" already linked. Skipping project creation.`,
      );
      return project;

    case ProjectStatus.EXISTING_PROJECT_LINK_PROJECT:
      waspSays(`Linking project with name "${project.name}" to this directory`);
      return linkProject(project, deploymentInfo);

    case ProjectStatus.CREATE_NEW_PROJECT:
      waspSays(`Setting up Railway project with name "${projectName}"`);
      return initProject(deploymentInfo);

    case ProjectStatus.ERROR_EXISTING_PROJECT_DIFFERENT_NAME:
      waspSays(
        `Project with a different name already linked to this directory: "${project.name}". Run "railway unlink" to unlink it.`,
      );
      return exit(1);

    case ProjectStatus.ERROR_PROJECT_WITH_ID_NOT_FOUND:
      waspSays(
        `Project with ID "${options.existingProjectId}" does not exist.`,
      );
      return exit(1);

    case ProjectStatus.ERROR_PROJECT_WITH_NAME_EXISTS:
      waspSays(
        `Project with name "${project.name}" already exists. Add "--existing-project-id ${project.id}" option to this command to link it or use a different name.`,
      );
      return exit(1);

    default:
      status satisfies never;
      throw new Error(`Unhandled status: ${status}`);
  }
}

async function getProjectStatus(
  projectName: RailwayProjectName,
  waspProjectDirPath: WaspProjectDir,
  options: SetupOptions,
): Promise<
  | {
      status: ProjectStatus.EXISTING_PROJECT_ALREADY_LINKED;
      project: RailwayProject;
    }
  | {
      status: ProjectStatus.EXISTING_PROJECT_LINK_PROJECT;
      project: RailwayProject;
    }
  | {
      status: ProjectStatus.CREATE_NEW_PROJECT;
      project: null;
    }
  | {
      status: ProjectStatus.ERROR_EXISTING_PROJECT_DIFFERENT_NAME;
      project: RailwayProject;
    }
  | {
      status: ProjectStatus.ERROR_PROJECT_WITH_ID_NOT_FOUND;
      project: null;
    }
  | {
      status: ProjectStatus.ERROR_PROJECT_WITH_NAME_EXISTS;
      project: RailwayProject;
    }
> {
  const projectLinkedToDir = await getProjectForDirectory(
    options.railwayExe,
    waspProjectDirPath,
  );

  // Project already linked to this directory
  if (projectLinkedToDir !== null) {
    if (projectLinkedToDir.name === projectName) {
      return {
        status: ProjectStatus.EXISTING_PROJECT_ALREADY_LINKED,
        project: projectLinkedToDir,
      };
    } else {
      return {
        status: ProjectStatus.ERROR_EXISTING_PROJECT_DIFFERENT_NAME,
        project: projectLinkedToDir,
      };
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
        status: ProjectStatus.EXISTING_PROJECT_LINK_PROJECT,
        project: projectById,
      };
    } else {
      return {
        status: ProjectStatus.ERROR_PROJECT_WITH_ID_NOT_FOUND,
        project: null,
      };
    }
  }

  // Trying to create a new project
  const projectByName = await getProjectByName(options.railwayExe, projectName);
  if (projectByName !== null) {
    return {
      status: ProjectStatus.ERROR_PROJECT_WITH_NAME_EXISTS,
      project: projectByName,
    };
  } else {
    return {
      status: ProjectStatus.CREATE_NEW_PROJECT,
      project: null,
    };
  }
}

async function getProjectById(
  railwayExe: RailwayCliExe,
  remoteProjectId: string,
): Promise<RailwayProject | null> {
  const projects = await getProjects(railwayExe);
  return projects.find((project) => project.id === remoteProjectId) ?? null;
}

async function getProjectByName(
  railwayExe: RailwayCliExe,
  name: string,
): Promise<RailwayProject | null> {
  const projects = await getProjects(railwayExe);
  return projects.find((project) => project.name === name) ?? null;
}
