type StarterTemplateName = "minimal" | "basic" | "saas";

type WaspProjectRelativePath = `./${string}`;
type IncludedTestsRelativePath = `./${string}`;

export type StarterHeadlessE2ETests =
  | StarterWithoutHeadlessE2ETests
  | StarterWithHeadlessE2ETests;

export type StarterWithoutHeadlessE2ETests = {
  starterName: StarterTemplateName;
  waspProjectRelativePath: WaspProjectRelativePath;
};

export type StarterWithHeadlessE2ETests = StarterWithoutHeadlessE2ETests & {
  includedTestsRelativePath: IncludedTestsRelativePath;
};

const minimalStarterHeadlessE2ETests: StarterWithoutHeadlessE2ETests = {
  starterName: "minimal",
  waspProjectRelativePath: "./",
};

const basicStarterHeadlessE2ETests: StarterWithoutHeadlessE2ETests = {
  starterName: "basic",
  waspProjectRelativePath: "./",
};

const openSaasStarterHeadlessE2ETests: StarterWithHeadlessE2ETests = {
  starterName: "saas",
  waspProjectRelativePath: "./app",
  includedTestsRelativePath: "./e2e-tests",
};

export const STARTERS_HEADLESS_E2E_TESTS: StarterHeadlessE2ETests[] = [
  minimalStarterHeadlessE2ETests,
  basicStarterHeadlessE2ETests,
  openSaasStarterHeadlessE2ETests,
];
