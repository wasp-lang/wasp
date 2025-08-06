export type StarterTemplateName = "minimal" | "basic" | "saas";

export type WaspProjectRelativePath = `/${string}`;
export type RemoteTestsRelativePath = `/${string}`;

export type StarterTests = StarterWithoutTests | StarterWithTests;

export type StarterWithoutTests = {
  templateName: StarterTemplateName;
  waspProjectRelativePath: WaspProjectRelativePath;
};

export type StarterWithTests = StarterWithoutTests & {
  includedTestsRelativePath: RemoteTestsRelativePath;
  // TODO: add other properties required for remote tests
};

const minimalStarterTests: StarterWithoutTests = {
  templateName: "minimal",
  waspProjectRelativePath: "/",
};

const basicStarterTests: StarterWithoutTests = {
  templateName: "basic",
  waspProjectRelativePath: "/",
};

const openSaasStarterTests: StarterWithTests = {
  templateName: "saas",
  waspProjectRelativePath: "/app",
  includedTestsRelativePath: "/e2e-tests",
};

export const STARTER_TESTS: StarterTests[] = [
  minimalStarterTests,
  basicStarterTests,
  openSaasStarterTests,
];
