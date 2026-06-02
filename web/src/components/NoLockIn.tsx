import { ReactNode } from "react";

import CodeHighlight from "./CodeHighlight";
import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const badges = [
  "MIT License",
  "No Cloud Lock-In",
  "No Provider Lock-In",
  "Self-Host Anywhere",
];

const deploymentTypeSource = `type OneCmdDeploy = Fly | Railway
type BigThree     = AWS | GCP | Azure
type Other        = DigitalOcean | Hetzner
type Deployment =
  | OneCmdDeploy
  | BigThree
  | Other
  | SelfHost
  | Custom<string>`;

const NoLockIn = () => (
  <SectionContainer>
    <SectionLabel text="no lock-in" />

    <h2 className="mb-4 font-mono text-2xl font-extrabold tracking-tight text-wasp-black lg:text-4xl">
      No lock-in. No catch. Ship it your way.
    </h2>
    <p className="max-w-2xl text-pretty font-mono text-sm leading-relaxed text-wasp-g6 lg:text-base">
      Open source. Not a BaaS, but a standalone framework. Not tied to any cloud
      platform, ours or anyone else's.
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

    <div className="mt-8 grid grid-cols-1 gap-3 lg:mt-10 lg:grid-cols-2 lg:items-start">
      <div className="border-2 border-wasp-black bg-wasp-bg">
        <CodeHighlight language="typescript" source={deploymentTypeSource} />
      </div>

      <div className="flex flex-col justify-center gap-4 lg:p-2">
        <Point>
          <Code>wasp deploy fly/railway/...</Code>: one command deploy to popular
          providers.
        </Point>
        <Point>
          Or take the generated <Strong>Dockerfile + static files</Strong> and
          deploy them however you want. No hidden/external dependencies, no
          platform lock-in.
        </Point>
        <Point>
          Your <Strong>auth</Strong>, your <Strong>db</Strong>. Or not. All up
          to you.
        </Point>
      </div>
    </div>
  </SectionContainer>
);

const Point = ({ children }: { children: ReactNode }) => (
  <p className="flex gap-2.5 font-mono text-sm leading-relaxed text-wasp-g6">
    <span
      aria-hidden="true"
      className="select-none font-bold text-wasp-yellow-dark"
    >
      ›
    </span>
    <span className="text-pretty">{children}</span>
  </p>
);

const Strong = ({ children }: { children: ReactNode }) => (
  <strong className="font-bold text-wasp-black">{children}</strong>
);

const Code = ({ children }: { children: ReactNode }) => (
  <code className="bg-[#f0ede6] px-1 py-0.5 font-mono text-[0.95em] text-wasp-black">
    {children}
  </code>
);

export default NoLockIn;
