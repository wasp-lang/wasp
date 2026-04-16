import { beforeEach, describe, expect, test, vi } from "vitest";

const mockTagFn = vi.fn();

vi.mock("zx", () => ({
  $: vi.fn(() => mockTagFn),
}));

import { $ } from "zx";
import {
  getRailwayProjectById,
  getRailwayProjectByName,
} from "../../../src/providers/railway/railwayProject/cli.js";

const mock$ = $ as unknown as ReturnType<typeof vi.fn>;

const mockProjects = [
  {
    id: "proj-1",
    name: "my-project",
    services: { edges: [] },
  },
  {
    id: "proj-2",
    name: "other-project",
    services: { edges: [] },
  },
];

function mockRailwayList(projects: object[] = mockProjects) {
  mockTagFn.mockResolvedValue({
    stdout: JSON.stringify(projects),
  });
}

/** Reconstruct the command string from a tagged template call's arguments. */
function getCommandFromTaggedTemplateCall(call: unknown[]): string {
  const strings = call[0] as string[];
  const values = call.slice(1);
  let cmd = "";
  for (let i = 0; i < strings.length; i++) {
    cmd += strings[i];
    if (i < values.length) {
      cmd += String(values[i]);
    }
  }
  return cmd.trim();
}

describe("getRailwayProjectById", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  test("finds project by id without workspace", async () => {
    mockRailwayList();
    const project = await getRailwayProjectById("railway" as any, "proj-1");
    expect(project).not.toBeNull();
    expect(project!.id).toBe("proj-1");
  });

  test("passes --workspace to railway list when workspace provided", async () => {
    mockRailwayList();
    await getRailwayProjectById("railway" as any, "proj-1", "team-xyz");

    expect(mockTagFn).toHaveBeenCalledTimes(1);
    const cmd = getCommandFromTaggedTemplateCall(mockTagFn.mock.calls[0]);
    expect(cmd).toContain("--workspace");
    expect(cmd).toContain("team-xyz");
  });

  test("does not pass --workspace when workspace is undefined", async () => {
    mockRailwayList();
    await getRailwayProjectById("railway" as any, "proj-1", undefined);

    expect(mockTagFn).toHaveBeenCalledTimes(1);
    const cmd = getCommandFromTaggedTemplateCall(mockTagFn.mock.calls[0]);
    expect(cmd).not.toContain("--workspace");
  });
});

describe("getRailwayProjectByName", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  test("finds project by name without workspace", async () => {
    mockRailwayList();
    const project = await getRailwayProjectByName(
      "railway" as any,
      "my-project",
    );
    expect(project).not.toBeNull();
    expect(project!.name).toBe("my-project");
  });

  test("passes --workspace to railway list when workspace provided", async () => {
    mockRailwayList();
    await getRailwayProjectByName("railway" as any, "my-project", "team-xyz");

    expect(mockTagFn).toHaveBeenCalledTimes(1);
    const cmd = getCommandFromTaggedTemplateCall(mockTagFn.mock.calls[0]);
    expect(cmd).toContain("--workspace");
    expect(cmd).toContain("team-xyz");
  });
});
