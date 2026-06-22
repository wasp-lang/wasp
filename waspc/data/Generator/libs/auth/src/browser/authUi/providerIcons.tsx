import type { JSX, SVGProps } from "react";
import type { OAuthProviderId } from "./oauth";

export type ProviderIconProps = SVGProps<SVGSVGElement> & {
  title?: string;
};

export function GoogleIcon({ title, ...props }: ProviderIconProps) {
  return (
    <svg
      {...getSvgAccessibilityProps(title)}
      fill="currentColor"
      viewBox="0 0 24 24"
      {...props}
    >
      {title && <title>{title}</title>}
      <path d="M11.99 13.9v-3.72h9.36c.14.63.25 1.22.25 2.05C21.6 17.94 17.77 22 12 22 6.48 22 2 17.52 2 12S6.48 2 12 2c2.7 0 4.96.99 6.69 2.61l-2.84 2.76c-.72-.68-1.98-1.48-3.85-1.48-3.31 0-6.01 2.75-6.01 6.12s2.7 6.12 6.01 6.12c3.83 0 5.24-2.65 5.5-4.22h-5.51z" />
    </svg>
  );
}

export function GitHubIcon({ title, ...props }: ProviderIconProps) {
  return (
    <svg
      {...getSvgAccessibilityProps(title)}
      fill="currentColor"
      viewBox="0 0 20 20"
      {...props}
    >
      {title && <title>{title}</title>}
      <path
        fillRule="evenodd"
        d="M10 0C4.477 0 0 4.484 0 10.017c0 4.425 2.865 8.18 6.839 9.504.5.092.682-.217.682-.483 0-.237-.008-.868-.013-1.703-2.782.605-3.369-1.343-3.369-1.343-.454-1.158-1.11-1.466-1.11-1.466-.908-.62.069-.608.069-.608 1.003.07 1.531 1.032 1.531 1.032.892 1.53 2.341 1.088 2.91.832.092-.647.35-1.088.636-1.338-2.22-.253-4.555-1.113-4.555-4.951 0-1.093.39-1.988 1.029-2.688-.103-.253-.446-1.272.098-2.65 0 0 .84-.27 2.75 1.026A9.564 9.564 0 0 1 10 4.844c.85.004 1.705.115 2.504.337 1.909-1.296 2.747-1.027 2.747-1.027.546 1.379.203 2.398.1 2.651.64.7 1.028 1.595 1.028 2.688 0 3.848-2.339 4.695-4.566 4.942.359.31.678.921.678 1.856 0 1.338-.012 2.419-.012 2.747 0 .268.18.58.688.482A10.019 10.019 0 0 0 20 10.017C20 4.484 15.522 0 10 0z"
        clipRule="evenodd"
      />
    </svg>
  );
}

export function DiscordIcon({ title, ...props }: ProviderIconProps) {
  return (
    <svg
      {...getSvgAccessibilityProps(title)}
      fill="currentColor"
      viewBox="0 0 16 16"
      {...props}
    >
      {title && <title>{title}</title>}
      <path d="M13.545 2.907a13.227 13.227 0 0 0-3.257-1.011.05.05 0 0 0-.052.025c-.141.25-.297.577-.406.833a12.19 12.19 0 0 0-3.658 0 8.258 8.258 0 0 0-.412-.833.051.051 0 0 0-.052-.025c-1.125.194-2.22.534-3.257 1.011a.041.041 0 0 0-.021.018C.356 6.024-.213 9.047.066 12.032c.001.014.01.028.021.037a13.276 13.276 0 0 0 3.995 2.02.05.05 0 0 0 .056-.019c.308-.42.582-.863.818-1.329a.05.05 0 0 0-.028-.07 8.875 8.875 0 0 1-1.248-.595.05.05 0 0 1-.005-.085c.084-.063.168-.129.248-.195a.05.05 0 0 1 .051-.007c2.619 1.196 5.454 1.196 8.041 0a.052.052 0 0 1 .053.007c.08.066.164.132.248.195a.051.051 0 0 1-.004.085 8.254 8.254 0 0 1-1.249.594.05.05 0 0 0-.027.071c.24.465.515.909.817 1.329a.05.05 0 0 0 .056.019 13.235 13.235 0 0 0 4.001-2.02.049.049 0 0 0 .021-.037c.334-3.451-.559-6.449-2.366-9.106a.034.034 0 0 0-.02-.019zM5.347 10.214c-.789 0-1.438-.724-1.438-1.612 0-.889.637-1.613 1.438-1.613.807 0 1.45.73 1.438 1.613 0 .888-.637 1.612-1.438 1.612zm5.316 0c-.788 0-1.438-.724-1.438-1.612 0-.889.637-1.613 1.438-1.613.807 0 1.451.73 1.438 1.613 0 .888-.631 1.612-1.438 1.612z" />
    </svg>
  );
}

export function SlackIcon({ title, ...props }: ProviderIconProps) {
  return (
    <svg
      {...getSvgAccessibilityProps(title)}
      fill="currentColor"
      viewBox="0 0 24 24"
      {...props}
    >
      {title && <title>{title}</title>}
      <path d="M13 10a2 2 0 1 0 4 0V5a2 2 0 1 0-4 0v5zM5 8a2 2 0 1 0 0 4h5a2 2 0 1 0 0-4H5zm10 5a2 2 0 1 0 0 4h5a2 2 0 1 0 0-4h-5zm-5 9a2 2 0 0 1-2-2v-5a2 2 0 1 1 4 0v5a2 2 0 0 1-2 2zM8 5a2 2 0 1 1 4 0v2h-2a2 2 0 0 1-2-2zM3 15a2 2 0 1 0 4 0v-2H5a2 2 0 0 0-2 2zm14 5a2 2 0 1 1-4 0v-2h2a2 2 0 0 1 2 2zm5-10a2 2 0 1 0-4 0v2h2a2 2 0 0 0 2-2z" />
    </svg>
  );
}

export function MicrosoftIcon({ title, ...props }: ProviderIconProps) {
  return (
    <svg
      {...getSvgAccessibilityProps(title)}
      fill="currentColor"
      viewBox="0 0 24 24"
      {...props}
    >
      {title && <title>{title}</title>}
      <path d="M2 3h9v9H2zm9 19H2v-9h9zM21 3v9h-9V3zm0 19h-9v-9h9z" />
    </svg>
  );
}

export function KeycloakIcon({ title, ...props }: ProviderIconProps) {
  return (
    <svg
      {...getSvgAccessibilityProps(title)}
      fill="currentColor"
      viewBox="0 0 559 466"
      {...props}
    >
      {title && <title>{title}</title>}
      <path d="M553.125 116.4h-80.6c-1.5 0-3-.8-3.7-2.1L404.125 2.1c-.8-1.3-2.2-2.1-3.8-2.1h-264c-1.5 0-3 .8-3.7 2.1L65.325 118.5.525 230.7c-.7 1.3-.7 2.9 0 4.3l64.8 112.2 67.2 116.5c.7 1.3 2.2 2.2 3.7 2.1h264.1c1.5 0 3-.8 3.8-2.1l64.8-112.2c.7-1.3 2.2-2.2 3.7-2.1h80.6c2.7 0 4.8-2.2 4.8-4.8V121.3c-.1-2.7-2.3-4.9-4.9-4.9zM244.425 351.5l-20.3 35c-.3.5-.8 1-1.3 1.3-.6.3-1.2.5-1.9.5h-40.3c-1.4 0-2.7-.7-3.3-2l-60.1-104.3-5.9-10.3-21.6-36.9c-.3-.5-.5-1.1-.4-1.8 0-.6.2-1.3.5-1.8l21.7-37.6 65.9-114c.7-1.2 2-2 3.3-2h40.2c.7 0 1.4.2 2.1.5.5.3 1 .7 1.3 1.3l20.3 35.2c.6 1.2.5 2.7-.2 3.8l-65.1 112.8c-.3.5-.4 1.1-.4 1.6 0 .6.2 1.1.4 1.6l65.1 112.7c.9 1.5.8 3.1 0 4.4zm202.1-116.7l-21.6 36.9-5.9 10.3-60.1 104.3c-.7 1.2-1.9 2-3.3 2h-40.3c-.7 0-1.3-.2-1.9-.5-.5-.3-1-.7-1.3-1.3l-20.3-35c-.9-1.3-.9-2.9-.1-4.2l65.1-112.7c.3-.5.4-1.1.4-1.6 0-.6-.2-1.1-.4-1.6l-65.1-112.8c-.7-1.2-.8-2.6-.2-3.8l20.3-35.2c.3-.5.8-1 1.3-1.3.6-.4 1.3-.5 2.1-.5h40.4c1.4 0 2.7.7 3.3 2l65.9 114 21.7 37.6c.3.6.5 1.2.5 1.8 0 .4-.2 1-.5 1.6z" />
    </svg>
  );
}

export const providerIconById: Partial<
  Record<OAuthProviderId, (props: ProviderIconProps) => JSX.Element>
> = {
  discord: DiscordIcon,
  github: GitHubIcon,
  google: GoogleIcon,
  keycloak: KeycloakIcon,
  microsoft: MicrosoftIcon,
  slack: SlackIcon,
};

function getSvgAccessibilityProps(title: string | undefined) {
  return title ? { role: "img" as const } : { "aria-hidden": true as const };
}
