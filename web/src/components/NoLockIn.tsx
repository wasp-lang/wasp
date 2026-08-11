import { ReactNode } from "react";

import CodeHighlight from "./CodeHighlight";
import InlineCode from "./InlineCode";
import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";
import TextLink from "./TextLink";

const badges = ["MIT License", "No Provider Lock-In", "Self-Host Anywhere"];

const deploymentTypePseudocode = `type OneCmdDeploy = Fly | Railway
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
      Open source. A standalone framework, not tied to any cloud platform.
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
        <CodeHighlight
          language="typescript"
          source={deploymentTypePseudocode}
        />
      </div>

      <ul className="flex flex-col justify-center gap-4 lg:p-2">
        <Item>
          <InlineCode>wasp deploy fly/railway/...</InlineCode>: one-command
          deploy to popular providers.
        </Item>
        <Item>
          Or take the generated <Strong>Dockerfile + static files</Strong> and
          deploy them{" "}
          <TextLink to="/docs/deployment/deployment-methods/self-hosted">
            however you want
          </TextLink>
          .
        </Item>
        <Item>
          No need to reach for third-party providers till you want them: Wasp
          comes with{" "}
          <Strong>
            built-in Auth and Jobs, and works with any Postgres database
          </Strong>
          .
        </Item>
      </ul>
    </div>
  </SectionContainer>
);

const Item = ({ children }: { children: ReactNode }) => (
  <li className="flex gap-2.5 font-mono text-sm leading-relaxed text-wasp-g6">
    <span
      aria-hidden="true"
      className="select-none font-bold text-wasp-yellow-dark"
    >
      ›
    </span>
    <span className="text-pretty">{children}</span>
  </li>
);

const Strong = ({ children }: { children: ReactNode }) => (
  <strong className="font-bold text-wasp-black">{children}</strong>
);

export default NoLockIn;
