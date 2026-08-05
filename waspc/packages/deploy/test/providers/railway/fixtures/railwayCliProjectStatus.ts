function mockService(
  serviceName: string,
  latestDeployment: { status: string } | null,
) {
  return { node: { serviceName, latestDeployment } };
}

// Railway CLI >=4.35 nests service instances under `environments`.
export const cliProjectStatusInNewFormat = {
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

// Railway CLI <=4.11 nests service instances under `services`.
export const cliProjectStatusInOldFormat = {
  id: "project-1",
  name: "test-project",
  environments: {
    edges: [{ node: { id: "env-1", name: "production" } }],
  },
  services: {
    edges: [
      {
        node: {
          id: "svc-1",
          name: "Postgres",
          serviceInstances: {
            edges: [mockService("Postgres", { status: "DEPLOYING" })],
          },
        },
      },
      {
        node: {
          id: "svc-2",
          name: "test-project-server",
          serviceInstances: { edges: [] },
        },
      },
    ],
  },
};
