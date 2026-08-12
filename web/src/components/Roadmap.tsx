import classNames from "classnames";
import { GitHub } from "react-feather";

import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";
import TextLink from "./TextLink";

const roadmapUrl = "https://github.com/orgs/wasp-lang/projects/5";

interface RoadmapItem {
  title: string;
  issue: number;
}

const upcoming: RoadmapItem[] = [
  { title: "Wasp Studio / Dev toolbar", issue: 2892 },
  { title: "Advanced access control (RBAC)", issue: 2896 },
  { title: "Multi-tenancy", issue: 2129 },
  { title: "Modular Wasp (Auth, Db, etc.)", issue: 2893 },
  { title: "Polyglot", issue: 2894 },
  { title: "Complex architectures (e.g. serverless)", issue: 2895 },
];

const next: RoadmapItem[] = [
  { title: "Operations improvements", issue: 2876 },
  { title: "DB / Prisma / Entities improvements", issue: 2877 },
  { title: "Rendering improvements (e.g. SSR)", issue: 2874 },
  { title: "Jobs improvements", issue: 2878 },
  { title: "Custom API improvements", issue: 2880 },
  { title: "Email sender improvements", issue: 2881 },
  { title: "Web sockets improvements", issue: 2882 },
  { title: "Better testing story", issue: 2885 },
  { title: "Production readiness", issue: 2888 },
  { title: "Mobile support", issue: 2889 },
];

const inDevelopment: RoadmapItem[] = [
  { title: "Full-Stack Modules (FSMs)", issue: 2873 },
  { title: "Rework Auth", issue: 2875 },
  { title: "Deployment improvements", issue: 2879 },
  { title: "All-around DX improvements (CLI)", issue: 2884 },
  { title: "Windows support", issue: 2890 },
];

const Roadmap = () => (
  <SectionContainer id="roadmap">
    <SectionLabel text="roadmap" />

    <h2 className="mb-4 font-mono text-2xl font-extrabold tracking-tight text-wasp-black lg:text-4xl">
      Work on Wasp never stops
    </h2>
    <p className="max-w-2xl text-pretty font-mono text-sm leading-relaxed text-wasp-g6 lg:text-base">
      Below is a glimpse at our current roadmap. For more details, check our
      Github: all our work is public!
    </p>

    <div className="mt-8 grid grid-cols-1 gap-3 lg:mt-10 lg:grid-cols-3">
      <RoadmapColumn marker="[?]" title="upcoming" items={upcoming} />
      <RoadmapColumn marker="[ ]" title="next" items={next} />
      <RoadmapColumn
        accent
        marker="[~]"
        title="in development"
        items={inDevelopment}
      />
    </div>

    <div className="mt-8 text-center">
      <TextLink
        to={roadmapUrl}
        className="inline-flex items-center gap-2 font-mono text-sm font-bold"
      >
        Check the full roadmap on GitHub <GitHub size={14} />
      </TextLink>
    </div>
  </SectionContainer>
);

const RoadmapColumn = ({
  marker,
  title,
  items,
  accent = false,
}: {
  marker: string;
  title: string;
  items: RoadmapItem[];
  accent?: boolean;
}) => (
  <article
    className={classNames(
      "border-2 p-6",
      accent
        ? "border-wasp-black bg-wasp-yellow-light"
        : "border-wasp-g3 bg-wasp-white",
    )}
  >
    <h3 className="font-mono text-base font-extrabold leading-tight text-wasp-black lg:text-lg">
      <span aria-hidden="true">{marker}</span> {title}
    </h3>
    <ul className="mt-4 space-y-2.5">
      {items.map((item, i) => (
        <li key={i}>
          <a
            href={`https://github.com/wasp-lang/wasp/issues/${item.issue}`}
            className={classNames(
              "group flex gap-2.5 font-mono text-sm leading-relaxed",
              "hover:text-wasp-black",
              accent ? "text-wasp-black" : "text-wasp-g6",
            )}
          >
            <span
              aria-hidden="true"
              className="select-none font-bold text-wasp-yellow-dark"
            >
              ›
            </span>
            <span className="text-pretty decoration-wasp-yellow decoration-2 underline-offset-4 group-hover:underline">
              {item.title}
            </span>
          </a>
        </li>
      ))}
    </ul>
  </article>
);

export default Roadmap;
