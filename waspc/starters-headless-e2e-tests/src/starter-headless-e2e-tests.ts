type StarterTemplateName = "minimal" | "basic" | "saas";

type WaspProjectRelativePath = `.${string}`;
type IncludedTestsRelativePath = `.${string}`;

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

export const STARTERS_HEADLESS_E2E_TESTS: StarterHeadlessE2ETests[] = [
  {
    starterName: "minimal",
    waspProjectRelativePath: "./",
  } satisfies StarterWithoutHeadlessE2ETests,
  {
    starterName: "basic",
    waspProjectRelativePath: "./",
  } satisfies StarterWithoutHeadlessE2ETests,
  {
    starterName: "saas",
    waspProjectRelativePath: "./app",
    includedTestsRelativePath: "./e2e-tests",
  } satisfies StarterWithHeadlessE2ETests,
];
