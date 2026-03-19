import { WaspProjectDir } from "../../../common/brandedTypes.js";
import {
  RailwayCliExe,
  RailwayProjectId,
  RailwayProjectName,
} from "../brandedTypes.js";
import {
  createDraftRailwayProject,
  LinkedRailwayProject,
  RailwayProject,
  UnlinkedRailwayProject,
} from "./RailwayProject.js";
import {
  getLinkedRailwayProjectForDirectory,
  getUnlinkedRailwayProjectById,
  getUnlinkedRailwayProjectByName,
} from "./cli.js";

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
}): Promise<RailwayProject> {
  const projectLinkedToDir = await getLinkedRailwayProjectForDirectory(
    railwayExe,
    waspProjectDir,
  );

  if (projectLinkedToDir !== null) {
    await assertProvidedProjectNameSameAsExistingProjectName(
      projectLinkedToDir,
      projectName,
    );

    return projectLinkedToDir;
  }

  if (existingProjectId) {
    const existingProject = await getExistingProjectById(
      railwayExe,
      existingProjectId,
    );

    return existingProject;
  }

  await assertUniqueProjectName(railwayExe, projectName);
  return createDraftRailwayProject();
}

async function getExistingProjectById(
  railwayExe: RailwayCliExe,
  existingProjectId: RailwayProjectId,
): Promise<UnlinkedRailwayProject> {
  const projectById = await getUnlinkedRailwayProjectById(
    railwayExe,
    existingProjectId,
  );
  if (projectById === null) {
    throw new Error(`Project with ID "${existingProjectId}" does not exist.`);
  }

  return projectById;
}

async function assertProvidedProjectNameSameAsExistingProjectName(
  project: LinkedRailwayProject,
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
  const project = await getUnlinkedRailwayProjectByName(
    railwayExe,
    projectName,
  );
  if (project !== null) {
    throw new Error(
      `Project with name "${project.name}" already exists. Add "--existing-project-id ${project.id}" option to this command to link it or use a different name.`,
    );
  }
}
