import Link from "@docusaurus/Link";
import "./AuthMethodsGrid.css";

export function AuthMethodsGrid() {
  const authMethods = [
    {
      title: "Email",
      description: "Email verification, password reset, etc.",
      linkToDocs: "./email",
    },
    {
      title: "Username & Password",
      description: "The simplest way to get started",
      linkToDocs: "./username-and-pass",
    },
    {
      title: "Google",
      description: "Users sign in with their Google account",
      linkToDocs: "./social-auth/google",
    },
    {
      title: "Github",
      description: "Users sign in with their Github account",
      linkToDocs: "./social-auth/github",
    },
    {
      title: "Keycloak",
      description: "Users sign in with their Keycloak account",
      linkToDocs: "./social-auth/keycloak",
    },
  ];
  return (
    <>
      <div className="auth-methods-grid">
        {authMethods.map((authMethod) => (
          <AuthMethodBox
            title={authMethod.title}
            description={authMethod.description}
            linkToDocs={authMethod.linkToDocs}
          />
        ))}
      </div>
      <p className="auth-methods-info">
        <small>Click on each auth method for more details.</small>
      </p>
    </>
  );
}

function AuthMethodBox({
  linkToDocs,
  title,
  description,
}: {
  linkToDocs: string;
  title: string;
  description: string;
}) {
  return (
    <Link to={linkToDocs} className="auth-method-box">
      <h3>{title} »</h3>
      <p>{description}</p>
    </Link>
  );
}
