import type { Meta, StoryObj } from "@storybook/react-vite";
import {
  DiscordIcon,
  GitHubIcon,
  GoogleIcon,
  KeycloakIcon,
  MicrosoftIcon,
  SlackIcon,
} from "../index";

const meta = {
  title: "Auth/Primitives/Provider Icons",
} satisfies Meta;

export default meta;

type Story = StoryObj<typeof meta>;

export const Icons: Story = {
  render: () => (
    <div className="flex flex-wrap gap-4 text-slate-950">
      <IconTile label="Google">
        <GoogleIcon width="1.5rem" height="1.5rem" />
      </IconTile>
      <IconTile label="GitHub">
        <GitHubIcon width="1.5rem" height="1.5rem" />
      </IconTile>
      <IconTile label="Discord">
        <DiscordIcon width="1.5rem" height="1.5rem" />
      </IconTile>
      <IconTile label="Slack">
        <SlackIcon width="1.5rem" height="1.5rem" />
      </IconTile>
      <IconTile label="Microsoft">
        <MicrosoftIcon width="1.5rem" height="1.5rem" />
      </IconTile>
      <IconTile label="Keycloak">
        <KeycloakIcon width="1.5rem" height="1.5rem" />
      </IconTile>
    </div>
  ),
};

function IconTile({
  label,
  children,
}: {
  label: string;
  children: React.ReactNode;
}) {
  return (
    <div className="grid min-w-28 justify-items-center gap-2 rounded border border-slate-200 p-4">
      {children}
      <span className="text-sm text-slate-600">{label}</span>
    </div>
  );
}
