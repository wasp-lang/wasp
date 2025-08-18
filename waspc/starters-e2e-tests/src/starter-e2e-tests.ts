type StarterTemplateName = "minimal" | "basic" | "saas";

type WaspProjectRelativePath = `.${string}`;

export type StarterE2ETests = {
  starterName: StarterTemplateName;
  waspProjectRelativePath: WaspProjectRelativePath;
};

export const STARTERS_E2E_TESTS: StarterE2ETests[] = [
  {
    starterName: "minimal",
    waspProjectRelativePath: "./",
  },
  {
    starterName: "basic",
    waspProjectRelativePath: "./",
  },
];
