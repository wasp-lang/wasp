import Link from "@docusaurus/Link";
import { useActiveVersion } from "@docusaurus/plugin-content-docs/client";
import "./Pills.css";

export function Pill({ children, linkToPage, style = {} }) {
  // Link within the docs version this page belongs to, not the latest one.
  const { path: versionPath } = useActiveVersion();
  return (
    <Link
      to={versionPath + linkToPage}
      style={{
        padding: "0.1rem 0.5rem",
        borderRadius: "0.375rem",
        color: "var(--auth-pills-color)",
        textDecoration: "none",
        display: "inline-block",
        ...style,
      }}
    >
      {children}
    </Link>
  );
}

export function EmailPill() {
  return (
    <Pill
      style={{
        backgroundColor: "var(--auth-pills-email)",
      }}
      linkToPage="/auth/email"
    >
      Email
    </Pill>
  );
}

export function UsernameAndPasswordPill() {
  return (
    <Pill
      style={{
        backgroundColor: "var(--auth-pills-username-and-pass)",
      }}
      linkToPage="/auth/username-and-pass"
    >
      Username & Password
    </Pill>
  );
}

export function SlackPill() {
  return (
    <Pill
      style={{
        backgroundColor: "var(--auth-pills-slack)",
      }}
      linkToPage="/auth/social-auth/slack"
    >
      Slack
    </Pill>
  );
}

export function DiscordPill() {
  return (
    <Pill
      style={{
        backgroundColor: "var(--auth-pills-discord)",
      }}
      linkToPage="/auth/social-auth/discord"
    >
      Discord
    </Pill>
  );
}

export function GithubPill() {
  return (
    <Pill
      style={{
        backgroundColor: "var(--auth-pills-github)",
      }}
      linkToPage="/auth/social-auth/github"
    >
      Github
    </Pill>
  );
}

export function GooglePill() {
  return (
    <Pill
      style={{
        backgroundColor: "var(--auth-pills-google)",
      }}
      linkToPage="/auth/social-auth/google"
    >
      Google
    </Pill>
  );
}

export function KeycloakPill() {
  return (
    <Pill
      style={{
        backgroundColor: "var(--auth-pills-keycloak)",
      }}
      linkToPage="/auth/social-auth/keycloak"
    >
      Keycloak
    </Pill>
  );
}
