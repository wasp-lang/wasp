import { type RailwayCliProject } from "../jsonOutputSchemas";

export type RailwayService = {
  id: string;
  name: string;
};

type RailwayProjectData = {
  id: string;
  name: string;
  services: RailwayService[];
  doesServiceExist: (serviceName: string) => boolean;
};

/**
 * A project that does not yet exist on Railway.
 * The user needs to create it (e.g. via `railway init`).
 */
export type DraftRailwayProject = {
  status: "draft";
};

/**
 * A project that exists on Railway but is not linked
 * to the current Wasp project directory.
 */
export type UnlinkedRailwayProject = RailwayProjectData & {
  status: "unlinked";
};

/**
 * A project that exists on Railway and is linked
 * to the current Wasp project directory.
 */
export type LinkedRailwayProject = RailwayProjectData & {
  status: "linked";
};

/**
 * Discriminated union representing the possible states
 * of a Railway project relative to a Wasp project directory.
 *
 * Use `project.status` to narrow the type:
 * - `'draft'`    — no project exists yet
 * - `'unlinked'` — project exists but is not linked to this directory
 * - `'linked'`   — project exists and is linked to this directory
 */
export type RailwayProject =
  | DraftRailwayProject
  | UnlinkedRailwayProject
  | LinkedRailwayProject;

function createProjectData(cliProject: RailwayCliProject): RailwayProjectData {
  return {
    id: cliProject.id,
    name: cliProject.name,
    services: cliProject.services.edges.map((edge) => ({
      id: edge.node.id,
      name: edge.node.name,
    })),
    doesServiceExist(serviceName: string) {
      return this.services.some((s) => s.name === serviceName);
    },
  };
}

export function createLinkedRailwayProject(
  cliProject: RailwayCliProject,
): LinkedRailwayProject {
  return {
    ...createProjectData(cliProject),
    status: "linked",
  };
}

export function createUnlinkedRailwayProject(
  cliProject: RailwayCliProject,
): UnlinkedRailwayProject {
  return {
    ...createProjectData(cliProject),
    status: "unlinked",
  };
}

export function createDraftRailwayProject(): DraftRailwayProject {
  return { status: "draft" };
}
