import Link from "@docusaurus/Link";
import styles from "./Pills.module.css";

export function Pill({ children, linkToPage }) {
  return (
    <Link to={linkToPage} className={styles.pills}>
      {children}
    </Link>
  );
}

export function EmailPill() {
  return <Pill linkToPage="/docs/auth/email">Email</Pill>;
}

export function UsernameAndPasswordPill() {
  return (
    <Pill linkToPage="/docs/auth/username-and-pass">Username & Password</Pill>
  );
}

export function SlackPill() {
  return <Pill linkToPage="/docs/auth/social-auth/slack">Slack</Pill>;
}

export function DiscordPill() {
  return <Pill linkToPage="/docs/auth/social-auth/discord">Discord</Pill>;
}

export function GithubPill() {
  return <Pill linkToPage="/docs/auth/social-auth/github">Github</Pill>;
}

export function GooglePill() {
  return <Pill linkToPage="/docs/auth/social-auth/google">Google</Pill>;
}

export function KeycloakPill() {
  return <Pill linkToPage="/docs/auth/social-auth/keycloak">Keycloak</Pill>;
}
