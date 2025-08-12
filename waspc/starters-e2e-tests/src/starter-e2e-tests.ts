type StarterTemplateName = "minimal" | "basic" | "saas";

type WaspProjectRelativePath = `.${string}`;
type IncludedTestsRelativePath = `.${string}`;

export type StarterE2ETests = StarterWithoutE2ETests | StarterWithE2ETests;

export type StarterWithoutE2ETests = {
  starterName: StarterTemplateName;
  waspProjectRelativePath: WaspProjectRelativePath;
};

export type StarterWithE2ETests = StarterWithoutE2ETests & {
  includedTestsRelativePath: IncludedTestsRelativePath;
};

export const STARTERS_E2E_TESTS: StarterE2ETests[] = [
  {
    starterName: "minimal",
    waspProjectRelativePath: "./",
  } satisfies StarterWithoutE2ETests,
  {
    starterName: "basic",
    waspProjectRelativePath: "./",
  } satisfies StarterWithoutE2ETests,
  {
    starterName: "saas",
    waspProjectRelativePath: "./app",
    includedTestsRelativePath: "./e2e-tests",
  } satisfies StarterWithE2ETests,
];
