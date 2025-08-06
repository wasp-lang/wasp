import { LinkGrid } from "@site/src/components/LinkGrid";

const deploymentMethods = [
  {
    title: "Wasp Deploy",
    description: "One-command deployment & redeployment",
    linkTo: "./wasp-deploy/overview",
  },
  {
    title: "Cloud Providers",
    description: "Deploy your app manually to the cloud",
    linkTo: "./paas",
  },
  {
    title: "Self-hosting",
    description: "Use your own servers to host your app",
    linkTo: "./self-hosted",
  },
];

export function DeploymentOptionsGrid() {
  return (
    <LinkGrid
      links={deploymentMethods}
      caption="Click on each deployment method for more details."
    />
  );
}
