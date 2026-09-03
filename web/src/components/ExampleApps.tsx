import Link from "@docusaurus/Link";
import useBrokenLinks from "@docusaurus/useBrokenLinks";
import { ArrowUpRight, GitHub, Monitor } from "react-feather";

import CtaLink from "./CtaLink";
import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const EXAMPLES_ID = "examples";

interface ExampleApp {
  name: string;
  description: string;
  linkUrl: string;
  repoName: string;
  demoUrl?: string;
  glyphType: "checkbox" | "letter" | "board";
}

const ExampleApps = () => {
  // Register anchor so Docusaurus's broken-link checker sees it at build time.
  useBrokenLinks().collectAnchor(EXAMPLES_ID);

  return (
    <SectionContainer id={EXAMPLES_ID}>
      <SectionLabel text="example apps" />
      <p className="mb-4 max-w-2xl text-base leading-relaxed text-wasp-g6">
        Reference implementations to learn from, fork, or deploy. Each one shows
        a complete Wasp app. Read the code, run it locally, ship it your way.
      </p>
      <div className="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-3">
        {exampleApps.slice(0, 3).map((p, i) => (
          <ExampleAppCard key={i} project={p} />
        ))}
      </div>
      <div className="mt-8">
        <CtaLink to="https://github.com/wasp-lang/wasp/tree/release/examples">
          <span>See all examples</span>
          <ArrowUpRight size={16} />
        </CtaLink>
      </div>
    </SectionContainer>
  );
};

/* Bottom action zone of the example-app card: repo link + Demo + See the code. */
const ExampleAppActions = ({ project }: { project: ExampleApp }) => {
  const { repoName, linkUrl, demoUrl } = project;
  return (
    <div className="border-t-2 border-wasp-black p-5">
      <Link
        to={linkUrl}
        className="mb-3 inline-flex items-center gap-1.5 font-mono text-xs text-wasp-g6 no-underline hover:text-wasp-black hover:no-underline"
      >
        <GitHub size={12} />
        <span>{repoName}</span>
      </Link>
      <div className="flex flex-wrap items-center gap-2">
        {demoUrl && (
          <Link
            to={demoUrl}
            className="inline-flex items-center gap-1.5 border-2 border-wasp-black bg-wasp-yellow px-3 py-1.5 font-mono text-xs font-bold uppercase tracking-wider text-wasp-black no-underline transition-colors hover:bg-wasp-yellow-dark hover:text-wasp-black hover:no-underline"
          >
            <Monitor size={12} />
            <span>Demo</span>
          </Link>
        )}
        <Link
          to={linkUrl}
          className="inline-flex items-center gap-1.5 border border-wasp-g7 bg-transparent px-3 py-1.5 font-mono text-xs font-semibold uppercase tracking-wider text-wasp-g7 no-underline transition-colors hover:border-wasp-black hover:text-wasp-black hover:no-underline"
        >
          <span>See the code</span>
          <ArrowUpRight size={12} />
        </Link>
      </div>
    </div>
  );
};

/* Detailed SVG illustration occupying the right third of the example-app
   card. Yellow fill + black stroke to match the brand. */
const BigIllustration = ({ type }: { type: ExampleApp["glyphType"] }) => {
  if (type === "checkbox") {
    return (
      <svg
        aria-hidden="true"
        viewBox="0 0 100 100"
        className="h-full w-full max-w-[120px]"
      >
        <rect
          x="10"
          y="14"
          width="18"
          height="18"
          fill="#F5C842"
          stroke="#111"
          strokeWidth="2.5"
        />
        <polyline
          points="13,23 18,28 25,17"
          fill="none"
          stroke="#111"
          strokeWidth="3"
          strokeLinecap="square"
          strokeLinejoin="miter"
        />
        <line x1="36" y1="23" x2="90" y2="23" stroke="#111" strokeWidth="2.5" />
        <rect
          x="10"
          y="42"
          width="18"
          height="18"
          fill="#F5C842"
          stroke="#111"
          strokeWidth="2.5"
        />
        <polyline
          points="13,51 18,56 25,45"
          fill="none"
          stroke="#111"
          strokeWidth="3"
          strokeLinecap="square"
          strokeLinejoin="miter"
        />
        <line x1="36" y1="51" x2="80" y2="51" stroke="#111" strokeWidth="2.5" />
        <rect
          x="10"
          y="70"
          width="18"
          height="18"
          fill="none"
          stroke="#111"
          strokeWidth="2.5"
        />
        <line x1="36" y1="79" x2="85" y2="79" stroke="#111" strokeWidth="2.5" />
      </svg>
    );
  }
  if (type === "letter") {
    return (
      <svg
        aria-hidden="true"
        viewBox="0 0 100 100"
        className="h-full w-full max-w-[120px]"
      >
        <rect
          x="22"
          y="20"
          width="50"
          height="68"
          fill="#F5C842"
          stroke="#111"
          strokeWidth="2.5"
        />
        <line x1="30" y1="32" x2="64" y2="32" stroke="#111" strokeWidth="2" />
        <line x1="30" y1="42" x2="64" y2="42" stroke="#111" strokeWidth="2" />
        <line x1="30" y1="52" x2="58" y2="52" stroke="#111" strokeWidth="2" />
        <line x1="30" y1="62" x2="64" y2="62" stroke="#111" strokeWidth="2" />
        <line x1="30" y1="72" x2="50" y2="72" stroke="#111" strokeWidth="2" />
        <polygon
          points="84,16 87,24 95,27 87,30 84,38 81,30 73,27 81,24"
          fill="#111"
        />
        <polygon
          points="14,68 16,72 20,74 16,76 14,80 12,76 8,74 12,72"
          fill="#111"
        />
      </svg>
    );
  }
  return (
    <svg
      aria-hidden="true"
      viewBox="0 0 100 100"
      className="h-full w-full max-w-[120px]"
    >
      <rect
        x="16"
        y="14"
        width="30"
        height="72"
        fill="#F5C842"
        stroke="#111"
        strokeWidth="2.5"
      />
      <rect
        x="54"
        y="14"
        width="30"
        height="52"
        fill="#F5C842"
        stroke="#111"
        strokeWidth="2.5"
      />
    </svg>
  );
};

const ExampleAppCard = ({ project }: { project: ExampleApp }) => (
  <article className="flex h-full min-h-[340px] flex-col border-2 border-wasp-black bg-wasp-yellow-light">
    <div className="flex flex-1 flex-col lg:flex-row">
      <div className="flex-1 p-5">
        <h4 className="mb-2 font-mono text-base font-bold uppercase text-wasp-black">
          {project.name}
        </h4>
        <p className="text-sm leading-relaxed text-wasp-g7">
          {project.description}
        </p>
      </div>
      <div className="flex items-center justify-center border-t-2 border-wasp-black p-5 lg:w-1/3 lg:border-l-2 lg:border-t-0">
        <BigIllustration type={project.glyphType} />
      </div>
    </div>
    <ExampleAppActions project={project} />
  </article>
);

const exampleApps: ExampleApp[] = [
  {
    name: "Todo App",
    description: "A famous To-Do list app, implemented in TypeScript.",
    linkUrl:
      "https://github.com/wasp-lang/wasp/tree/release/examples/tutorials/TodoAppTs",
    repoName: "TodoAppTs",
    glyphType: "checkbox",
  },
  {
    name: "CoverLetterGPT",
    description:
      "Generate cover letters based on your CV and the job description. Powered by ChatGPT.",
    linkUrl: "https://github.com/vincanger/coverlettergpt",
    repoName: "coverlettergpt",
    demoUrl: "https://coverlettergpt.xyz/",
    glyphType: "letter",
  },
  {
    name: "Waspello",
    description:
      "A Trello-style kanban board with lists, draggable cards, and user auth.",
    linkUrl: "https://github.com/wasp-lang/wasp/tree/release/examples/waspello",
    repoName: "waspello",
    demoUrl: "https://waspello-demo.netlify.app/",
    glyphType: "board",
  },
];

export default ExampleApps;
