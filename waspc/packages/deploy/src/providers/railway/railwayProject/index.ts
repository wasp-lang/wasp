import { WaspProjectDir } from "../../../common/brandedTypes.js";
import {
  RailwayCliExe,
  RailwayProjectId,
  RailwayProjectName,
} from "../brandedTypes.js";
import { RailwayProject } from "./RailwayProject.js";
import {
  getRailwayProjectById,
  getRailwayProjectByName,
  getRailwayProjectForDirectory,
} from "./cli.js";

export enum ProjectStatus {
  EXISTING_PROJECT_ALREADY_LINKED = "EXISTING_PROJECT_ALREADY_LINKED",
  EXISTING_PROJECT_SHOULD_BE_LINKED = "EXISTING_PROJECT_SHOULD_BE_LINKED",
  MISSING_PROJECT = "MISSING_PROJECT",
}

export async function getRailwayProjectStatus({
  projectName,
  waspProjectDir,
  railwayExe,
  existingProjectId,
}: {
  projectName: RailwayProjectName;
  existingProjectId?: RailwayProjectId;
  waspProjectDir: WaspProjectDir;
  railwayExe: RailwayCliExe;
}): Promise<
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
    railwayExe,
    waspProjectDir,
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

  if (existingProjectId) {
    const existingProject = await getExistingProjectById(
      railwayExe,
      existingProjectId,
    );

    return {
      status: ProjectStatus.EXISTING_PROJECT_SHOULD_BE_LINKED,
      project: existingProject,
    };
  }

  await assertUniqueProjectName(railwayExe, projectName);
  return {
    status: ProjectStatus.MISSING_PROJECT,
    project: null,
  };
}

async function getExistingProjectById(
  railwayExe: RailwayCliExe,
  existingProjectId: RailwayProjectId,
): Promise<RailwayProject> {
  const projectById = await getRailwayProjectById(
    railwayExe,
    existingProjectId,
  );
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
  const project = await getRailwayProjectByName(railwayExe, projectName);
  if (project !== null) {
    throw new Error(
      `Project with name "${project.name}" already exists. Add "--existing-project-id ${project.id}" option to this command to link it or use a different name.`,
    );
  }
}
