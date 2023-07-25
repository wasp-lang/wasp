import React from "react";
import "./AuthMethodsGrid.css";

export function AuthMethodsGrid() {
  const authMethods = [
    {
      title: "Email",
      description: "Email verification, password reset, etc.",
      linkToDocs: "/docs/auth/email",
    },
    {
      title: "Username & Password",
      description: "Simplest way to get started",
      linkToDocs: "/docs/auth/username-and-pass",
    },
    {
      title: "Google",
      description: "Users sign in with their Google account",
      linkToDocs: "/docs/auth/google",
    },
    {
      title: "Github",
      description: "Users sign in with their Github account",
      linkToDocs: "/docs/auth/github",
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
        <small>Click on the auth method to see more details about it.</small>
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
    <a href={linkToDocs} className="auth-method-box">
      <h3>{title} »</h3>
      <p>{description}</p>
    </a>
  );
}
