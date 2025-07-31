export type StarterTemplateName = "minimal" | "basic" | "saas";

export type StarterTests = LocalStarterTests | RemoteStarterTests;

export type LocalStarterTests = {
  templateName: StarterTemplateName;
  appRelativePath: string;
};

export type RemoteStarterTests = LocalStarterTests & {
  remoteTestsRelativePath: string;
  // TODO: add other properties required for remote tests
};

const minimalStarterTests: LocalStarterTests = {
  templateName: "minimal",
  appRelativePath: "/",
};

const basicStarterTests: LocalStarterTests = {
  templateName: "basic",
  appRelativePath: "/",
};

const openSaasStarterTests: RemoteStarterTests = {
  templateName: "saas",
  appRelativePath: "/app",
  remoteTestsRelativePath: "/e2e-tests",
};

export const STARTER_TESTS: StarterTests[] = [
  // minimalStarterTests,
  basicStarterTests,
  // openSaasStarterTests,
];
