import Link from "@docusaurus/Link";
import useBrokenLinks from "@docusaurus/useBrokenLinks";
import classNames from "classnames";
import { ReactNode } from "react";
import { ArrowUpRight, GitHub, Monitor } from "react-feather";

import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const EXAMPLES_ID = "examples";
const SHOWCASES_ID = "showcases";
const TESTIMONIALS_ID = "testimonials";

interface Testimonial {
  text: ReactNode;
  name: string;
  role?: string;
  img?: string;
  url?: string;
}

/* Wrap a slice of testimonial text to give it subtle bold emphasis. */
const Highlight = ({ children }: { children: ReactNode }) => (
  <span className="font-bold text-wasp-black">{children}</span>
);

interface ExampleApp {
  name: string;
  description: string;
  linkUrl: string;
  repoName: string;
  demoUrl?: string;
  glyphType: "checkbox" | "letter" | "board";
}

interface RealApp {
  name: string;
  description: string;
  linkText: string;
  linkUrl: string;
  imageSrc: string;
}

const WaspOutThere = () => {
  // Register anchors so Docusaurus's broken-link checker sees them at build time.
  // Old LP sections used the same ids; an existing blog post links to them.
  const brokenLinks = useBrokenLinks();
  brokenLinks.collectAnchor(EXAMPLES_ID);
  brokenLinks.collectAnchor(SHOWCASES_ID);
  brokenLinks.collectAnchor(TESTIMONIALS_ID);

  return (
    <>
      <SectionContainer id={EXAMPLES_ID}>
        <SectionLabel text="example apps" />
        <p className="mb-4 max-w-2xl text-base leading-relaxed text-wasp-g6">
          Reference implementations to learn from, fork, or deploy. Each one
          shows a complete Wasp app. Read the code, run it locally, ship it your
          way.
        </p>
        <div className="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-3">
          {exampleApps.slice(0, 3).map((p, i) => (
            <ExampleAppCard key={i} project={p} />
          ))}
        </div>
        <div className="mt-8">
          <Link
            to="https://github.com/wasp-lang/wasp/tree/release/examples"
            className="inline-flex items-center gap-1 font-mono text-sm font-semibold text-wasp-black underline decoration-wasp-yellow decoration-2 underline-offset-2 hover:text-wasp-black hover:decoration-wasp-yellow-dark"
          >
            <span>See all examples</span>
            <ArrowUpRight size={14} />
          </Link>
        </div>
      </SectionContainer>

      <SectionContainer id={SHOWCASES_ID}>
        <SectionLabel text="shipped with wasp" />
        <p className="mb-6 max-w-2xl text-base leading-relaxed text-wasp-g6">
          Live apps the community built and shipped on top of Wasp, running in
          production today.
        </p>
        <div className="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-3">
          {realApps.slice(0, 3).map((p, i) => (
            <ProjectCard key={i} project={p} />
          ))}
        </div>
      </SectionContainer>

      <div className="bg-wasp-yellow-light">
        <SectionContainer id={TESTIMONIALS_ID}>
          <SectionLabel text="join the community" />
          <p className="mb-10 max-w-2xl text-base leading-relaxed text-wasp-g7">
            Real builders, real apps, real conversations.{" "}
            <Link
              to="https://discord.gg/rzdnErX"
              className="bg-wasp-yellow px-1 font-semibold text-wasp-black underline decoration-wasp-black decoration-2 underline-offset-2 hover:text-wasp-black hover:decoration-wasp-black"
            >
              Drop into Discord
            </Link>{" "}
            to ask questions, share what you're working on, or just hang out.
          </p>
          <CommunityGrid />
        </SectionContainer>
      </div>
    </>
  );
};

/* Community grid: landscape featured testimonial center + 2 side columns of
   2 testimonials each, separated by 1px solid dividers. */
const CommunityGrid = () => (
  <div className="grid grid-cols-1 items-stretch gap-10 lg:grid-cols-12 lg:gap-8">
    <SideColumn side="left" t1={testimonials[0]} t2={testimonials[1]} />
    <div className="lg:col-span-6 lg:self-center">
      <FeaturedTestimonial />
    </div>
    <SideColumn side="right" t1={testimonials[2]} t2={testimonials[3]} />
  </div>
);

/* Side column with two testimonials, separated by a horizontal divider that
   sits at the column's 50% mark. Both columns stretch to the same height (via
   grid `items-stretch`), so the dividers line up across the section. */
const SideColumn = ({
  side,
  t1,
  t2,
}: {
  side: "left" | "right";
  t1: Testimonial;
  t2: Testimonial;
}) => {
  const isLeft = side === "left";
  const innerPadding = isLeft ? "lg:pr-8" : "lg:pl-8";
  const sideBorder = isLeft ? "lg:border-r" : "lg:border-l";
  return (
    <div
      className={classNames(
        "relative flex flex-col gap-10 border-wasp-g3 lg:col-span-3 lg:h-full lg:justify-between lg:gap-0",
        sideBorder,
      )}
    >
      <div className={innerPadding}>
        <SideTestimonial testimonial={t1} />
      </div>
      <div className={innerPadding}>
        <SideTestimonial testimonial={t2} />
      </div>
      <div className="pointer-events-none absolute left-0 right-0 top-1/2 hidden border-t border-wasp-g3 lg:block" />
    </div>
  );
};

/* ─────────── Card components ─────────── */

const Avatar = ({
  img,
  name,
  size = "h-10 w-10",
}: {
  img?: string;
  name: string;
  size?: string;
}) =>
  img ? (
    <img
      src={img}
      alt={name}
      className={`${size} flex-shrink-0 rounded-full object-cover`}
    />
  ) : (
    <div
      className={`${size} flex flex-shrink-0 items-center justify-center rounded-full bg-wasp-white font-mono text-sm font-bold text-wasp-g7`}
    >
      {name.charAt(0)}
    </div>
  );

const SideTestimonial = ({ testimonial }: { testimonial: Testimonial }) => {
  const { name, role, img, text, url } = testimonial;
  const Inner = (
    <article>
      <p className="text-sm leading-relaxed text-wasp-g7">“{text}”</p>
      <div className="mt-4 flex items-center gap-3">
        <Avatar img={img} name={name} />
        <div className="min-w-0 flex-1">
          <div className="font-mono text-xs font-bold text-wasp-black">
            {name}
          </div>
          {role && (
            <div className="font-mono text-[11px] text-wasp-g5">{role}</div>
          )}
        </div>
      </div>
    </article>
  );
  return url ? (
    <Link
      to={url}
      className="block text-wasp-g7 no-underline hover:text-wasp-g7 hover:no-underline"
    >
      {Inner}
    </Link>
  ) : (
    Inner
  );
};

const FeaturedTestimonial = () => (
  <article className="border border-[#1E1F22] bg-[#313338] p-8 lg:py-12">
    <div className="mb-4 flex items-center gap-1.5">
      <span className="font-sans text-2xl font-light leading-none text-[#80848E]">
        #
      </span>
      <span className="text-lg leading-none">🏠</span>
      <span className="font-sans text-base font-bold leading-none text-[#F2F3F5]">
        made-with-wasp
      </span>
    </div>
    <p className="font-sans text-base leading-relaxed text-[#DBDEE1] lg:text-lg">
      Wasp is as{" "}
      <span className="bg-wasp-yellow px-1 text-wasp-black">
        game-changing for me as React has been many years back
      </span>
      . Its simplicity, and how well [it] captures most full-stack engineering
      tasks is{" "}
      <span className="bg-wasp-yellow px-1 text-wasp-black">pure genius</span>.
      I believe Wasp will become{" "}
      <span className="bg-wasp-yellow px-1 text-wasp-black">
        the #1 web technology in just a couple years
      </span>
      . It has everything that most web devs are looking for.
    </p>
    <div className="mt-6 flex items-center gap-3">
      <div className="flex h-12 w-12 flex-shrink-0 items-center justify-center rounded-full bg-[#5865F2] font-sans text-base font-bold text-white">
        W
      </div>
      <div>
        <div className="font-sans text-sm font-bold text-[#F2F3F5]">
          Wasp builder
        </div>
        <div className="font-sans text-xs text-[#949BA4]">via Discord</div>
      </div>
    </div>
  </article>
);

const ProjectCard = ({ project }: { project: RealApp }) => (
  <Link
    to={project.linkUrl}
    className="block h-full overflow-hidden border border-wasp-black bg-wasp-white no-underline transition-shadow hover:shadow-md"
  >
    <img
      src={project.imageSrc}
      alt={project.name}
      className="aspect-video w-full object-cover object-top"
    />
    <div className="p-4">
      <h4 className="mb-1 font-mono text-base font-bold text-wasp-black">
        {project.name}
      </h4>
      <p className="mb-3 text-sm leading-relaxed text-wasp-g6">
        {project.description}
      </p>
      <span className="font-mono text-xs font-semibold text-wasp-black underline decoration-wasp-yellow decoration-2 underline-offset-2">
        {project.linkText}
      </span>
    </div>
  </Link>
);

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

/* ─────────── Data ─────────── */

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

const realApps: RealApp[] = [
  {
    name: "Scribeist",
    description:
      "AI writing platform with workspaces for novels, blogs, essays, and notes.",
    linkText: "visit site →",
    linkUrl: "https://scribeist.com/",
    imageSrc: "/img/lp/examples/scribeist.webp",
  },
  {
    name: "Microinfluencer Club",
    description:
      "Connect brands with micro-influencers to run marketing campaigns.",
    linkText: "visit site →",
    linkUrl: "https://microinfluencer.club/",
    imageSrc: "/img/lp/examples/microinfluencers.webp",
  },
  {
    name: "Searchcraft",
    description:
      "AI-powered search engine for product teams, with keyword precision plus semantic understanding.",
    linkText: "visit site →",
    linkUrl: "https://www.searchcraft.io/",
    imageSrc: "/img/lp/examples/searchcraft.webp",
  },
];

// Order matters: [leftColTop, leftColBottom, rightColTop, rightColBottom]
const testimonials: Testimonial[] = [
  {
    text: (
      <>
        Wasp is <Highlight>by far the best for AI coding</Highlight>. The
        high-level Wasp file keeps me on top of everything.
      </>
    ),
    name: "Marcel Coetzee",
    role: "Founder @ Hireveld",
    img: "https://github.com/Pipboyguy.png",
    url: "/blog/2026/03/29/hireveld-from-10-stacks-to-production-with-wasp",
  },
  {
    text: (
      <>
        If you start with Wasp,{" "}
        <Highlight>80% of the pains of vibe coding are taken care of</Highlight>{" "}
        for you already.
      </>
    ),
    name: "Kenny Rogers",
    role: "Dev Rel & AI-First Educator",
    img: "https://github.com/kenrogers.png",
    url: "https://x.com/KenTheRogers",
  },
  {
    text: (
      <>
        <Highlight>Everything just works on the first try</Highlight> — minimal
        intervention needed for anything.
      </>
    ),
    name: "Hrvoje Pavlinovic",
    role: "Senior Engineer @ Memoato",
    img: "https://github.com/hrvojepavlinovic.png",
    url: "https://memoato.com/",
  },
  {
    text: (
      <>
        Just using AI would make it harder to sleep at night. With Wasp{" "}
        <Highlight>I feel secure</Highlight>, like I'm{" "}
        <Highlight>not cutting any corners</Highlight>.
      </>
    ),
    name: "Robbie Artress",
    role: "Founder @ PeakMastering",
    img: "https://pbs.twimg.com/profile_images/1938395157109342208/PtnrrFe7_400x400.jpg",
    url: "https://peakmastering.com",
  },
];

export default WaspOutThere;
