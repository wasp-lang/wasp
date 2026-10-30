export function createRailwayProject(cliProject) {
    return {
        id: cliProject.id,
        name: cliProject.name,
        services: cliProject.services.edges.map((edge) => ({
            id: edge.node.id,
            name: edge.node.name,
        })),
        findService(serviceName) {
            return this.services.find((service) => service.name === serviceName);
        },
        doesServiceExist(serviceName) {
            return this.findService(serviceName) !== undefined;
        },
    };
}
//# sourceMappingURL=RailwayProject.js.map