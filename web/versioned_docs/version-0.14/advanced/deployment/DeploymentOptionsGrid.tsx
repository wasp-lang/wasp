import Link from "@docusaurus/Link";
import "./DeploymentOptionsGrid.css";

export function DeploymentOptionsGrid() {
  const deploymentMethods = [
    {
      title: "Using Wasp CLI",
      description: "One command deployment & redeployment",
      linkToDocs: "./cli",
    },
    {
      title: "Deploying Manually",
      description: "Build the app and deploy it manually",
      linkToDocs: "./manually",
    },
  ];
  return (
    <>
      <div className="deployment-methods-grid">
        {deploymentMethods.map((deploymentMethod) => (
          <DeploymentOptionBox
            title={deploymentMethod.title}
            description={deploymentMethod.description}
            linkToDocs={deploymentMethod.linkToDocs}
          />
        ))}
      </div>
      <p className="deployment-methods-info">
        <small>Click on each deployment method for more details.</small>
      </p>
    </>
  );
}

function DeploymentOptionBox({
  linkToDocs,
  title,
  description,
}: {
  linkToDocs: string;
  title: string;
  description: string;
}) {
  return (
    <Link to={linkToDocs} className="deployment-method-box">
      <h3>{title} »</h3>
      <p>{description}</p>
    </Link>
  );
}
