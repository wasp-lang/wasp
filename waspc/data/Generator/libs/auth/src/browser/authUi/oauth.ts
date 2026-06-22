import {
  useState,
  type ComponentPropsWithoutRef,
  type MouseEvent,
} from "react";
import { getDefaultErrorMessage, type ErrorMessage } from "../messages";

export type OAuthProviderId =
  | "discord"
  | "github"
  | "google"
  | "keycloak"
  | "microsoft"
  | "slack"
  | (string & {});

export type OAuthProvider = {
  id: OAuthProviderId;
  label: string;
  href?: string;
  start?: (provider: OAuthProvider) => Promise<void> | void;
};

export type OAuthProviderAction = Omit<OAuthProvider, "start"> & {
  isLoading: boolean;
  isDisabled: boolean;
  start: () => Promise<void>;
  getButtonProps: () => ComponentPropsWithoutRef<"button">;
  getLinkProps: () => ComponentPropsWithoutRef<"a">;
};

export type UseOAuthProviderActionsOptions = {
  providers: OAuthProvider[];
  onProviderSelected?: (provider: OAuthProvider) => Promise<void> | void;
  getErrorMessage?: (error: unknown) => ErrorMessage;
};

export function useOAuthProviderActions({
  providers,
  onProviderSelected,
  getErrorMessage = getDefaultErrorMessage,
}: UseOAuthProviderActionsOptions) {
  const [activeProviderId, setActiveProviderId] =
    useState<OAuthProviderId | null>(null);
  const [errorMessage, setErrorMessage] = useState<ErrorMessage | null>(null);
  const isLoading = activeProviderId !== null;

  async function startProvider(provider: OAuthProvider) {
    if (activeProviderId !== null) {
      return;
    }

    setActiveProviderId(provider.id);
    setErrorMessage(null);

    try {
      await onProviderSelected?.(provider);
      await provider.start?.(provider);
    } catch (error: unknown) {
      setErrorMessage(getErrorMessage(error));
    } finally {
      setActiveProviderId(null);
    }
  }

  function getProviderAction(provider: OAuthProvider): OAuthProviderAction {
    const shouldHandleClick = Boolean(provider.start || onProviderSelected);
    const isProviderLoading = activeProviderId === provider.id;

    function onClick(
      event: MouseEvent<HTMLButtonElement> | MouseEvent<HTMLAnchorElement>,
    ) {
      if (shouldHandleClick) {
        event.preventDefault();
      }
      void startProvider(provider);
    }

    return {
      ...provider,
      isLoading: isProviderLoading,
      isDisabled: isLoading,
      start: () => startProvider(provider),
      getButtonProps() {
        return {
          type: "button",
          disabled: isLoading,
          "aria-busy": isProviderLoading || undefined,
          onClick,
        };
      },
      getLinkProps() {
        return {
          href: provider.href,
          "aria-busy": isProviderLoading || undefined,
          ...(isLoading ? { "aria-disabled": true } : {}),
          ...(shouldHandleClick ? { onClick } : {}),
        };
      },
    };
  }

  return {
    providers: providers.map(getProviderAction),
    activeProviderId,
    errorMessage,
    isLoading,
    clearError() {
      setErrorMessage(null);
    },
    startProvider,
  };
}
