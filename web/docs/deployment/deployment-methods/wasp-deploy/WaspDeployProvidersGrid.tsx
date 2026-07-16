import { LinkGrid } from "@site/src/components/LinkGrid";

const deploymentMethods = [
  {
    title: "Fly.io",
    linkTo: "./fly",
  },
  {
    title: "Railway",
    linkTo: "./railway",
  },
  {
    title: "Vercel",
    linkTo: "./vercel",
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
