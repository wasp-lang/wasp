export const cliProjectWithServices = {
  id: "proj-1",
  name: "my-project",
  workspace: {
    id: "ws-1",
    name: "my-workspace",
  },
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
  workspace: {
    id: "ws-1",
    name: "my-workspace",
  },
  services: { edges: [] },
};

export const cliProjectInDifferentWorkspace = {
  id: "proj-3",
  name: "other-project",
  workspace: {
    id: "ws-2",
    name: "other-workspace",
  },
  services: { edges: [] },
};

// Regression fixture: same project name in two different workspaces.
// Ensures --workspace filtering correctly distinguishes between them.
export const cliProjectSameNameDifferentWorkspace = {
  id: "proj-4",
  name: "my-project", // same name as cliProjectWithServices
  workspace: {
    id: "ws-2",
    name: "other-workspace",
  },
  services: { edges: [] },
};
