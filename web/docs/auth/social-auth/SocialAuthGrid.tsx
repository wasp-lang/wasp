import React from "react";
import Link from '@docusaurus/Link';
import "./SocialAuthGrid.css";

export function SocialAuthGrid({
  pagePart = "", // e.g. #overrides
}) {
  const authMethods = [
    {
      title: "Google",
      description: "Users sign in with their Google account.",
      linkToDocs: "/docs/auth/social-auth/google" + pagePart,
    },
    {
      title: "Github",
      description: "Users sign in with their Github account.",
      linkToDocs: "/docs/auth/social-auth/github" + pagePart,
    },
  ];
  return (
    <>
      <div className="social-auth-grid">
        {authMethods.map((authMethod) => (
          <AuthMethodBox
            title={authMethod.title}
            description={authMethod.description}
            linkToDocs={authMethod.linkToDocs}
          />
        ))}
      </div>
      <p className="social-auth-info">
        <small>Click on each provider for more details.</small>
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
