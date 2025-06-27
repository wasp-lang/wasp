import { type RailwayCliProject } from "../jsonOutputSchemas";

export class RailwayProject {
  id: string;
  name: string;
  services: RailwayService[];

  constructor(cliProject: RailwayCliProject) {
    this.id = cliProject.id;
    this.name = cliProject.name;
    this.services = cliProject.services.edges.map((edge) => ({
      id: edge.node.id,
      name: edge.node.name,
    }));
  }

  doesServiceExist(serviceName: string): boolean {
    return this.services.some((s) => s.name === serviceName);
  }
}

export type RailwayService = {
  id: string;
  name: string;
};
