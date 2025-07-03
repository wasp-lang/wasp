import { LinkGrid } from "@site/src/components/LinkGrid";

const authMethods = [
  {
    title: "Email",
    description: "Email verification, password reset, etc.",
    linkTo: "./email",
  },
  {
    title: "Username & Password",
    description: "The simplest way to get started",
    linkTo: "./username-and-pass",
  },
  {
    title: "Google",
    description: "Users sign in with their Google account",
    linkTo: "./social-auth/google",
  },
  {
    title: "Github",
    description: "Users sign in with their Github account",
    linkTo: "./social-auth/github",
  },
  {
    title: "Keycloak",
    description: "Users sign in with their Keycloak account",
    linkTo: "./social-auth/keycloak",
  },
  {
    title: "Slack",
    description: "Users sign in with their Slack account",
    linkTo: "./social-auth/slack",
  },
  {
    title: "Discord",
    description: "Users sign in with their Discord account",
    linkTo: "./social-auth/discord",
  },
];

export function AuthMethodsGrid() {
  return (
    <LinkGrid
      caption="Click on each auth method for more details."
      links={authMethods}
    />
  );
}
