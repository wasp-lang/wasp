function mockService(
  serviceName: string,
  latestDeployment: { status: string } | null,
) {
  return { node: { serviceName, latestDeployment } };
}

export const cliProjectStatus = {
  id: "project-1",
  name: "test-project",
  workspace: { name: "Test" },
  environments: {
    edges: [
      {
        node: {
          id: "env-1",
          name: "production",
          serviceInstances: {
            edges: [
              mockService("Postgres", { status: "SUCCESS" }),
              mockService("test-project-server", null),
            ],
          },
        },
      },
    ],
  },
  services: {
    edges: [
      { node: { id: "svc-1", name: "Postgres" } },
      { node: { id: "svc-2", name: "test-project-server" } },
    ],
  },
};
