export type StarterTemplateName = "minimal" | "basic" | "saas";

export type WaspProjectRelativePath = `/${string}`;
export type RemoteTestsRelativePath = `/${string}`;

export type StarterTests = LocalStarterTests | RemoteStarterTests;

export type LocalStarterTests = {
  templateName: StarterTemplateName;
  waspProjectRelativePath: WaspProjectRelativePath;
};

export type RemoteStarterTests = LocalStarterTests & {
  remoteTestsRelativePath: RemoteTestsRelativePath;
  // TODO: add other properties required for remote tests
};

const minimalStarterTests: LocalStarterTests = {
  templateName: "minimal",
  waspProjectRelativePath: "/",
};

const basicStarterTests: LocalStarterTests = {
  templateName: "basic",
  waspProjectRelativePath: "/",
};

const openSaasStarterTests: RemoteStarterTests = {
  templateName: "saas",
  waspProjectRelativePath: "/app",
  remoteTestsRelativePath: "/e2e-tests",
};

export const STARTER_TESTS: StarterTests[] = [
  minimalStarterTests,
  basicStarterTests,
  openSaasStarterTests,
];
