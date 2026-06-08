// Markdown renderers for components we don't own the source of in a
// build-importable way: doc-local components (e.g. docs/auth/Pills.jsx) and any
// third-party package components. The registry is keyed by the JSX name, so we
// adapt them here without importing their (React/CSS) implementation.

import type { MarkdownRenderer } from "./doc-component";
import { link, text } from "./md";

// The auth pills (docs/auth/Pills.jsx) each render a link to an auth method.
const pill =
  (label: string, linkTo: string): MarkdownRenderer =>
  () => [link(linkTo, [text(label)])];

export const externalRenderers: Record<string, MarkdownRenderer> = {
  EmailPill: pill("Email", "/docs/auth/email"),
  UsernameAndPasswordPill: pill(
    "Username & Password",
    "/docs/auth/username-and-pass",
  ),
  SlackPill: pill("Slack", "/docs/auth/social-auth/slack"),
  DiscordPill: pill("Discord", "/docs/auth/social-auth/discord"),
  GithubPill: pill("Github", "/docs/auth/social-auth/github"),
  GooglePill: pill("Google", "/docs/auth/social-auth/google"),
  KeycloakPill: pill("Keycloak", "/docs/auth/social-auth/keycloak"),
};
