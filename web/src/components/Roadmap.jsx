import Link from "@docusaurus/Link";
import { ArrowRight } from "react-feather";

import SectionContainer from "./Layouts/SectionContainer";

// TODO(matija): this is duplication from HowItWorks section.
const GhIssueLink = ({ url, label }) => (
  <Link to={url}>
    <span
      className={`cursor-pointer rounded-full bg-neutral-600 px-2.5 py-1 text-xs text-white`}
    >
      <div className="group inline-flex items-center gap-1">
        <span>{label}</span>
        <div className="transition-all group-hover:ml-0.5">
          <span className="text-yellow-400">
            <ArrowRight size={14} strokeWidth={2} />
          </span>
        </div>
      </div>
    </span>
  </Link>
);

const Section = ({ features }) => (
  <ul className="space-y-6">
    {features.map((f) => (
      <li className="grid grid-cols-12" key={f[0]}>
        <div className="col-span-8 col-start-3 flex items-center">
          <span>
            <span className="text-neutral-600">{f[0]}</span>
            {f[1] && (
              <>
                &nbsp;
                <GhIssueLink
                  url={"https://github.com/wasp-lang/wasp/issues/" + f[1]}
                  label={f[1]}
                />
              </>
            )}
          </span>
        </div>
      </li>
    ))}
  </ul>
);

const Roadmap = () => (
  <SectionContainer className="lg:py-18 space-y-16" id="roadmap">
    <div className="grid grid-cols-12">
      <div className="col-span-12 text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          🚧 Roadmap 🚧
        </h2>
        <p className="text-neutral-500">
          Work on Wasp never stops: get a glimpse of what is coming next!
        </p>
      </div>
    </div>

    <div className="grid grid-cols-1 md:gap-16 lg:grid-cols-2">
      <div
        className={`rounded-lg border border-yellow-500/25 bg-yellow-500/5 p-5`}
      >
        <div className="mb-6 text-center font-bold text-neutral-700">
          Right behind the corner
        </div>
        <Section
          features={[
            ["Improve Prisma support (more features, IDE)", 641],
            ["Add TS eDSL, next to Wasp DSL", 551],
            ["Make Wasp Auth usable in external services", 1973],
            ["Add more social providers to Wasp Auth", 2016],
            ["Support for SSR / SSG", 911],
            ["Full-Stack Modules (aka FSMs: think RoR Engines)"],
          ]}
        />
      </div>

      <div
        className={`mt-6 rounded-lg border border-yellow-500/25 bg-yellow-500/20 p-5 lg:mt-0`}
      >
        <div className="mb-6 text-center font-bold text-neutral-700">
          Further down the road
        </div>
        <Section
          features={[
            ["Multiple targets (e.g. mobile)", 1088],
            ["Automatic generation of API for Operations", 863],
            ["Top-level data schema", 642],
            ["Complex arch (multiple servers, clients, serverless)"],
            ["Polyglot (Python, Rust, Go, ...)", 1940],
            ["Multiple frontend libraries (Vue, Svelte, ...)"],
          ]}
        />
      </div>
    </div>
  </SectionContainer>
);

export default Roadmap;
