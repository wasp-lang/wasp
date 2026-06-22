import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, fn, waitFor } from "storybook/test";
import {
  providerIconById,
  useOAuthProviderActions,
  type OAuthProvider,
} from "../index";

type StoryArgs = {
  onProviderSelected: (provider: OAuthProvider) => Promise<void> | void;
  shouldFail: boolean;
};

const meta = {
  title: "Auth/Hooks/useOAuthProviderActions",
  component: OAuthProviderActionsHarness,
  args: {
    onProviderSelected: fn(),
    shouldFail: false,
  },
} satisfies Meta<typeof OAuthProviderActionsHarness>;

export default meta;

type Story = StoryObj<typeof meta>;

export const Providers: Story = {
  play: async ({ args, canvas, userEvent }) => {
    await userEvent.click(
      canvas.getByRole("button", { name: "Continue with Google" }),
    );
    await waitFor(() => expect(args.onProviderSelected).toHaveBeenCalled());
  },
};

export const ProviderError: Story = {
  args: {
    shouldFail: true,
  },
  play: async ({ canvas, userEvent }) => {
    await userEvent.click(
      canvas.getByRole("button", { name: "Continue with GitHub" }),
    );
    await canvas.findByRole("alert");
  },
};

function OAuthProviderActionsHarness({
  onProviderSelected,
  shouldFail,
}: StoryArgs) {
  const oauth = useOAuthProviderActions({
    providers: createProviders(shouldFail),
    onProviderSelected,
  });

  return (
    <section className="grid max-w-sm gap-3">
      <h2 className="text-xl font-semibold">OAuth provider actions</h2>
      <div className="grid gap-2">
        {oauth.providers.map((provider) => {
          const Icon = providerIconById[provider.id];
          return (
            <button
              key={provider.id}
              className="inline-flex items-center justify-center gap-2 rounded border px-3 py-2 disabled:opacity-60"
              {...provider.getButtonProps()}
            >
              {Icon && <Icon width="1em" height="1em" />}
              Continue with {provider.label}
            </button>
          );
        })}
      </div>
      {oauth.errorMessage && (
        <p className="text-red-700" role="alert">
          {oauth.errorMessage.title}
        </p>
      )}
    </section>
  );
}

function createProviders(shouldFail: boolean): OAuthProvider[] {
  return [
    { id: "google", label: "Google" },
    {
      id: "github",
      label: "GitHub",
      start() {
        if (shouldFail) {
          throw new Error("Unable to start GitHub login.");
        }
      },
    },
    { id: "discord", label: "Discord" },
  ];
}
