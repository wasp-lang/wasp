import classNames from "classnames";
import { ComponentType, ReactNode, SVGProps, useState } from "react";

import InlineCode from "../InlineCode";
import SectionContainer from "../Layouts/SectionContainer";
import SectionLabel from "../Layouts/SectionLabel";
import TextLink from "../TextLink";
import {
  DigitalOceanLogo,
  DockerLogo,
  FlyIoLogo,
  GoogleCloudLogo,
  HetznerLogo,
  RailwayLogo,
} from "./deployTargetLogos";

const badges = ["MIT License", "No Provider Lock-In", "Self-Host Anywhere"];

type Logo = ComponentType<SVGProps<SVGSVGElement>>;

// The `oneCmd` targets are real single-command `wasp deploy` destinations.
// The `byo` ("bring your own") targets reuse the generic `wasp build` output
// (a Dockerfile + static files) that ships to any host.
type Terminal =
  | { kind: "oneCmd"; command: string; success: string }
  | { kind: "byo"; host: string };

interface DeployTarget {
  key: string;
  name: string;
  sub: string;
  logo: Logo;
  oneCmd?: boolean;
  terminal: Terminal;
}

const deployTargets: DeployTarget[] = [
  {
    key: "fly",
    name: "Fly.io",
    sub: "managed",
    logo: FlyIoLogo,
    oneCmd: true,
    terminal: {
      kind: "oneCmd",
      command: "wasp deploy fly launch my-app my-app-db",
      success: "server + client + Postgres live on Fly",
    },
  },
  {
    key: "railway",
    name: "Railway",
    sub: "managed",
    logo: RailwayLogo,
    oneCmd: true,
    terminal: {
      kind: "oneCmd",
      command: "wasp deploy railway launch my-app",
      success: "full stack deployed to Railway",
    },
  },
  {
    key: "gcp",
    name: "Google Cloud",
    sub: "bring your own",
    logo: GoogleCloudLogo,
    terminal: { kind: "byo", host: "Google Cloud" },
  },
  {
    key: "digitalocean",
    name: "DigitalOcean",
    sub: "bring your own",
    logo: DigitalOceanLogo,
    terminal: { kind: "byo", host: "DigitalOcean" },
  },
  {
    key: "hetzner",
    name: "Hetzner",
    sub: "bring your own",
    logo: HetznerLogo,
    terminal: { kind: "byo", host: "Hetzner" },
  },
  {
    key: "docker",
    name: "Self-host",
    sub: "docker, anywhere",
    logo: DockerLogo,
    terminal: { kind: "byo", host: "your own box" },
  },
];

const NoLockIn = () => {
  const [activeKey, setActiveKey] = useState(deployTargets[0].key);
  const activeTarget =
    deployTargets.find((t) => t.key === activeKey) ?? deployTargets[0];

  return (
    <SectionContainer>
      <SectionLabel text="no lock-in" />

      <h2 className="mb-4 flex flex-wrap items-center gap-4 font-mono text-2xl font-extrabold tracking-tight text-wasp-black lg:text-4xl">
        <Padlock />
        No lock-in. No catch.
      </h2>
      <p className="max-w-2xl text-pretty font-mono text-sm leading-relaxed text-wasp-g6 lg:text-base">
        Open source, standalone framework. Ship the same app to a one-command
        host, or to your own box.
      </p>

      <div className="mt-6 flex flex-wrap gap-2">
        {badges.map((badge) => (
          <span
            key={badge}
            className="border-2 border-wasp-black bg-wasp-yellow-light px-3 py-1 font-mono text-xs font-semibold uppercase tracking-wide text-wasp-g7"
          >
            {badge}
          </span>
        ))}
      </div>

      <div className="mt-8 grid grid-cols-1 gap-6 lg:mt-10 lg:grid-cols-2 lg:items-start">
        <TargetTerminal target={activeTarget} />

        <div>
          <div
            role="group"
            aria-label="Pick a deployment target"
            className="grid grid-cols-2 gap-[2px] border-2 border-wasp-black bg-wasp-black sm:grid-cols-3"
          >
            {deployTargets.map((target) => (
              <Tile
                key={target.key}
                target={target}
                isActive={target.key === activeKey}
                onSelect={() => setActiveKey(target.key)}
              />
            ))}
          </div>
          <p className="mt-4 font-mono text-xs leading-relaxed text-wasp-g6">
            Two are one-command (<InlineCode>wasp deploy</InlineCode>). The rest:{" "}
            <Strong>wasp build</Strong> hands you a Dockerfile + static files to
            run{" "}
            <TextLink to="/docs/deployment/deployment-methods/self-hosted">
              anywhere
            </TextLink>
            .
          </p>
        </div>
      </div>

      <p className="mt-8 max-w-3xl font-mono text-[11px] leading-relaxed text-wasp-g4">
        Logos and brand names belong to their respective owners and indicate
        supported deployment targets only; no partnership or endorsement is
        implied.
      </p>
    </SectionContainer>
  );
};

const Tile = ({
  target,
  isActive,
  onSelect,
}: {
  target: DeployTarget;
  isActive: boolean;
  onSelect: () => void;
}) => {
  const Logo = target.logo;
  return (
    <button
      type="button"
      aria-pressed={isActive}
      onClick={onSelect}
      className={classNames(
        "relative flex cursor-pointer items-center gap-3 border-0 px-3.5 py-4 text-left font-mono transition-colors duration-100",
        isActive
          ? "z-10 bg-wasp-yellow"
          : "bg-wasp-white hover:bg-wasp-yellow-light",
      )}
    >
      {target.oneCmd && (
        <span className="absolute right-0 top-0 bg-wasp-purple px-1.5 py-0.5 text-[8.5px] font-bold leading-none tracking-wide text-wasp-white">
          1 cmd
        </span>
      )}
      <span className="flex h-[22px] w-[22px] shrink-0 items-center justify-center text-wasp-black">
        <Logo className="h-full w-full" />
      </span>
      <span className="flex min-w-0 flex-col leading-tight">
        <span className="text-[13px] font-semibold uppercase tracking-wide text-wasp-black">
          {target.name}
        </span>
        <span className="mt-0.5 text-[9.5px] uppercase tracking-wider text-wasp-g5">
          {target.sub}
        </span>
      </span>
    </button>
  );
};

// The terminal deliberately uses Wasp's dark code palette (yellow prompt on
// `code-bg-purple-dark`) rather than <CodeHighlight>, whose theme is hardcoded
// to the light docs code block. The lavender text shades below have no wasp-*
// token equivalent, so they're set as one-off values, scoped to this terminal.
const TargetTerminal = ({ target }: { target: DeployTarget }) => (
  <div className="border-2 border-wasp-black bg-wasp-code-bg-purple-dark font-mono text-[13.5px] leading-[1.8] text-[#e7e1f3]">
    <div className="flex items-center justify-between border-b border-[#463c61] bg-[#332b47] px-3 py-2">
      <span className="text-[11px] uppercase tracking-[0.1em] text-[#8a7fb0]">
        wasp-cli
      </span>
      <span className="flex gap-1.5" aria-hidden="true">
        <span className="h-[9px] w-[9px] bg-[#4d4368]" />
        <span className="h-[9px] w-[9px] bg-[#4d4368]" />
        <span className="h-[9px] w-[9px] bg-[#4d4368]" />
      </span>
    </div>
    <div
      aria-live="polite"
      className="min-h-[196px] px-[18px] pb-[22px] pt-[18px]"
    >
      {target.terminal.kind === "oneCmd" ? (
        <OneCmdOutput
          command={target.terminal.command}
          success={target.terminal.success}
        />
      ) : (
        <ByoOutput host={target.terminal.host} />
      )}
    </div>
  </div>
);

const OneCmdOutput = ({
  command,
  success,
}: {
  command: string;
  success: string;
}) => (
  <>
    <div>
      <Prompt />
      <Arg>{command}</Arg> <Cursor />
    </div>
    <div className="mt-3.5">
      <Check /> {success}
    </div>
  </>
);

const ByoOutput = ({ host }: { host: string }) => (
  <>
    <div>
      <Prompt />
      <Arg>wasp build</Arg>
    </div>
    <div className="text-[#9186b0]">
      <Check indent /> .wasp/out/Dockerfile
    </div>
    <div className="text-[#9186b0]">
      <Check indent /> .wasp/out/web-app/build/{"  "}
      <span className="text-[#7d729c]">(static files)</span>
    </div>
    <div className="mt-3">
      <span className="text-wasp-yellow"># </span>
      <span className="text-[#9186b0]">ship it to {host}, however you like</span>
    </div>
    <div className="mt-1">
      <Prompt />
      <Arg>docker build . &amp;&amp; deploy</Arg> <Cursor />
    </div>
  </>
);

const Prompt = () => <span className="text-wasp-yellow">$ </span>;

const Arg = ({ children }: { children: ReactNode }) => (
  <span className="text-[#f1ecfb]">{children}</span>
);

const Check = ({ indent = false }: { indent?: boolean }) => (
  <span className="font-bold text-wasp-yellow">
    {indent ? "  ✓" : "✓"}
  </span>
);

const Cursor = () => (
  <span
    aria-hidden="true"
    className="inline-block h-4 w-2 translate-y-[2px] animate-pulse bg-wasp-yellow"
  />
);

// Angular open padlock (sharp corners, no arcs) — matches the neo-brutalist set.
const Padlock = () => (
  <svg
    viewBox="0 0 38 42"
    aria-hidden="true"
    className="h-[38px] w-[34px] shrink-0"
  >
    <path
      d="M26 18 V9 H12 V14"
      className="fill-none stroke-wasp-black"
      strokeWidth={2.6}
      strokeLinecap="square"
      strokeLinejoin="miter"
    />
    <rect
      x="5"
      y="18"
      width="26"
      height="20"
      className="fill-wasp-yellow stroke-wasp-black"
      strokeWidth={2.6}
      strokeLinejoin="miter"
    />
    <rect x="17" y="24" width="4" height="4" className="fill-wasp-black" />
    <rect x="18" y="27" width="2" height="6" className="fill-wasp-black" />
  </svg>
);

const Strong = ({ children }: { children: ReactNode }) => (
  <strong className="font-bold text-wasp-black">{children}</strong>
);

export default NoLockIn;
