import { type RailwayCliProject } from "../jsonOutputSchemas.js";

export type RailwayProject = {
  id: string;
  name: string;
  services: RailwayService[];
  findService: (serviceName: string) => RailwayService | undefined;
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
    services: cliProject.services.edges.map((edge) => ({
      id: edge.node.id,
      name: edge.node.name,
    })),
    findService(serviceName: string) {
      return this.services.find((service) => service.name === serviceName);
    },
    doesServiceExist(serviceName: string) {
      return this.findService(serviceName) !== undefined;
    },
  };
}
