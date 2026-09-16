import Link from "@docusaurus/Link";
import { useActiveVersion } from "@docusaurus/plugin-content-docs/client";
import "./Pills.css";

export function Pill({ children, linkToPage }) {
  // Link within the docs version this page belongs to, not the latest one.
  const { path: versionPath } = useActiveVersion();
  return (
    <Link to={versionPath + linkToPage} className="pills">
      {children}
    </Link>
  );
}

export function EmailPill() {
  return <Pill linkToPage="/auth/email">Email</Pill>;
}

export function UsernameAndPasswordPill() {
  return <Pill linkToPage="/auth/username-and-pass">Username & Password</Pill>;
}

export function SlackPill() {
  return <Pill linkToPage="/auth/social-auth/slack">Slack</Pill>;
}

export function DiscordPill() {
  return <Pill linkToPage="/auth/social-auth/discord">Discord</Pill>;
}

export function GithubPill() {
  return <Pill linkToPage="/auth/social-auth/github">Github</Pill>;
}

export function GooglePill() {
  return <Pill linkToPage="/auth/social-auth/google">Google</Pill>;
}

export function KeycloakPill() {
  return <Pill linkToPage="/auth/social-auth/keycloak">Keycloak</Pill>;
}
