export const cliProjectWithServices = {
  id: "proj-1",
  name: "my-project",
  services: {
    edges: [
      { node: { id: "svc-1", name: "web-server" } },
      { node: { id: "svc-2", name: "Postgres" } },
    ],
  },
};

export const cliProjectWithoutServices = {
  id: "proj-2",
  name: "empty-project",
  services: { edges: [] },
};
