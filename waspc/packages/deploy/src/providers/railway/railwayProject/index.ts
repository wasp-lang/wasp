import { WaspProjectDir } from "../../../common/brandedTypes.js";
import { waspSays } from "../../../common/terminal.js";
import { DeploymentInfo } from "../DeploymentInfo.js";
import {
  RailwayCliExe,
  RailwayProjectId,
  RailwayProjectName,
} from "../brandedTypes.js";
import { SetupOptions } from "../commands/setup/SetupOptions.js";
import { RailwayProject } from "./RailwayProject.js";
import {
  getProjectById,
  getProjectByName,
  getRailwayProjectForDirectory,
  initRailwayProject,
  linkRailwayProject,
} from "./cli.js";

enum ProjectStatus {
  EXISTING_PROJECT_ALREADY_LINKED = "EXISTING_PROJECT_ALREADY_LINKED",
  EXISTING_PROJECT_SHOULD_BE_LINKED = "EXISTING_PROJECT_SHOULD_BE_LINKED",
  MISSING_PROJECT = "MISSING_PROJECT",
}

export async function ensureRailwayProjectForDirectory(
  waspProjectDirPath: WaspProjectDir,
  deploymentInfo: DeploymentInfo<SetupOptions>,
): Promise<RailwayProject> {
  const { projectName, options } = deploymentInfo;

  const { status, project } = await getRailwayProjectStatus(
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

    case ProjectStatus.EXISTING_PROJECT_SHOULD_BE_LINKED:
      waspSays(`Linking project with name "${project.name}" to this directory`);
      return linkRailwayProject(project, deploymentInfo);

    case ProjectStatus.MISSING_PROJECT:
      waspSays(`Setting up Railway project with name "${projectName}"`);
      return initRailwayProject(deploymentInfo);

    default:
      status satisfies never;
      throw new Error(`Unhandled status: ${status}`);
  }
}

async function getRailwayProjectStatus(
  projectName: RailwayProjectName,
  waspProjectDirPath: WaspProjectDir,
  options: SetupOptions,
): Promise<
  | {
      status: ProjectStatus.EXISTING_PROJECT_ALREADY_LINKED;
      project: RailwayProject;
    }
  | {
      status: ProjectStatus.EXISTING_PROJECT_SHOULD_BE_LINKED;
      project: RailwayProject;
    }
  | {
      status: ProjectStatus.MISSING_PROJECT;
      project: null;
    }
> {
  const projectLinkedToDir = await getRailwayProjectForDirectory(
    options.railwayExe,
    waspProjectDirPath,
  );

  if (projectLinkedToDir !== null) {
    await assertProvidedProjectNameSameAsExistingProjectName(
      projectLinkedToDir,
      projectName,
    );

    return {
      status: ProjectStatus.EXISTING_PROJECT_ALREADY_LINKED,
      project: projectLinkedToDir,
    };
  }

  if (options.existingProjectId) {
    const existingProject = await getExistingProjectById(
      options.railwayExe,
      options.existingProjectId,
    );

    return {
      status: ProjectStatus.EXISTING_PROJECT_SHOULD_BE_LINKED,
      project: existingProject,
    };
  }

  await assertUniqueProjectName(options.railwayExe, projectName);
  return {
    status: ProjectStatus.MISSING_PROJECT,
    project: null,
  };
}

async function getExistingProjectById(
  railwayExe: RailwayCliExe,
  existingProjectId: RailwayProjectId,
): Promise<RailwayProject> {
  const projectById = await getProjectById(railwayExe, existingProjectId);
  if (projectById === null) {
    throw new Error(`Project with ID "${existingProjectId}" does not exist.`);
  }

  return projectById;
}

async function assertProvidedProjectNameSameAsExistingProjectName(
  project: RailwayProject,
  projectName: RailwayProjectName,
): Promise<void> {
  if (project.name !== projectName) {
    throw new Error(
      `Project with a different name already linked to this directory: "${project.name}". Run "railway unlink" to unlink it.`,
    );
  }
}

async function assertUniqueProjectName(
  railwayExe: RailwayCliExe,
  projectName: RailwayProjectName,
): Promise<void> {
  const project = await getProjectByName(railwayExe, projectName);
  if (project !== null) {
    throw new Error(
      `Project with name "${project.name}" already exists. Add "--existing-project-id ${project.id}" option to this command to link it or use a different name.`,
    );
  }
}
