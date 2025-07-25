import { LinkGrid } from "@site/src/components/LinkGrid";

const deploymentMethods = [
  {
    title: "Fly.io",
    description: "One command deployment & redeployment",
    linkTo: "./fly",
  },
  {
    title: "Railway",
    description: "One command deployment & redeployment",
    linkTo: "./railway",
  },
];

export function WaspDeployProvidersGrid() {
  return (
    <LinkGrid
      links={deploymentMethods}
      caption="Click on each provider for more details."
    />
  );
}
