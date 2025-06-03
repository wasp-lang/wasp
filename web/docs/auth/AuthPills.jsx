import React from "react";
import Link from "@docusaurus/Link";
import './AuthPills.css'

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
      className={'pills'}
    >
      {label}
    </Link>
  );
}

export function AuthPills({ providers }) {
  const validPills = providers
    .map((provider) => PILL_INFO[provider])
    .filter(Boolean);

  return (
    <span className={'display-inline'}>
      {validPills.map((pill, index) => {
        const isLast = index === validPills.length - 1;
        const isSecondLast = index === validPills.length - 2;

        return (
          <React.Fragment key={pill.link}>
            {isLast && validPills.length > 1 && "and "}
            <Pill label={pill.label} link={pill.link} />
            {!isLast && (isSecondLast ? ", " : ", ")}
          </React.Fragment>
        );
      })}
    </span>
  );
}
