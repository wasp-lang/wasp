import { LinkGrid } from "@site/src/components/LinkGrid";

export function SocialAuthGrid({
  pagePart = "", // e.g. #overrides
}) {
  const authMethods = [
    {
      title: "Google",
      description: "Users sign in with their Google account.",
      linkTo: "./google" + pagePart,
    },
    {
      title: "Github",
      description: "Users sign in with their Github account.",
      linkTo: "./github" + pagePart,
    },
    {
      title: "Keycloak",
      description: "Users sign in with their Keycloak account.",
      linkTo: "./keycloak" + pagePart,
    },
    {
      title: "Slack",
      description: "Users sign in with their Slack account.",
      linkTo: "./slack" + pagePart,
    },
    {
      title: "Discord",
      description: "Users sign in with their Discord account.",
      linkTo: "./discord" + pagePart,
    },
  ];

  return (
    <LinkGrid
      links={authMethods}
      caption="Click on each provider for more details."
    />
  );
}
