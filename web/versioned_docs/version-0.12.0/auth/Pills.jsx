import React from "react";
import Link from "@docusaurus/Link";

const PILL_INFO = {
  email: { label: "Email", link: "/docs/auth/email" },
  username: {
    label: "Username & Password",
    link: "/docs/auth/username-and-pass",
  },
  slack: { label: "Slack", link: "/docs/auth/social-auth/slack" },
  discord: { label: "Discord", link: "/docs/auth/social-auth/discord" },
  github: { label: "Github", link: "/docs/auth/social-auth/github" },
  google: { label: "Google", link: "/docs/auth/social-auth/google" },
  keycloak: { label: "Keycloak", link: "/docs/auth/social-auth/keycloak" },
};

function Pill({ label, link }) {
  return (
    <Link
      to={link}
      style={{
        padding: "0.1rem 0.5rem",
        borderRadius: "0.375rem",
        color: "white",
        backgroundColor: "#444",
        textDecoration: "none",
        display: "inline-block",
        marginRight: "0.25rem",
      }}
    >
      {label}
    </Link>
  );
}

export function AuthPills({ providers }) {
  return (
    <span style={{display: 'inline'}}>
      {providers.map((provider) => {
        const pill = PILL_INFO[provider];
        if (!pill) return null;
        return <Pill key={provider} label={pill.label} link={pill.link} />;
      })}
    </span>
  );
}
