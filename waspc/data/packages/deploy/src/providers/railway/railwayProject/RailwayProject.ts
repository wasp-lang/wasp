import { type RailwayCliProject } from "../jsonOutputSchemas";

export type RailwayWorkspace = {
  id: string;
  name: string;
};

export type RailwayProject = {
  id: string;
  name: string;
  workspace: RailwayWorkspace;
  services: RailwayService[];
  doesServiceExist: (serviceName: string) => boolean;
};

export type RailwayService = {
  id: string;
  name: string;
};

export function createRailwayProject(
  cliProject: RailwayCliProject,
): RailwayProject {
  return {
    id: cliProject.id,
    name: cliProject.name,
    workspace: {
      id: cliProject.workspace.id,
      name: cliProject.workspace.name,
    },
    services: cliProject.services.edges.map((edge) => ({
      id: edge.node.id,
      name: edge.node.name,
    })),
    doesServiceExist(serviceName: string) {
      return this.services.some((s) => s.name === serviceName);
    },
  };
}
