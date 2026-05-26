import Link from "@docusaurus/Link";
import classNames from "classnames";

import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./SectionLabel";

import { caseByKind } from "../lib/utils";

type Showcase = Testimonial | Project;

interface Testimonial {
  kind: "testimonial";
  text: string;
  attribution: string;
}

interface Project {
  kind: "real-app" | "example-app";
  name: string;
  tag?: string;
  description: string;
  linkText: string;
  linkUrl: string;
}

const WaspOutThere = () => (
  <SectionContainer className="pt-6 lg:pt-12 lg:pb-24">
    <SectionLabel text="wasp out there" />
    <div className="gap-3 columns-1 sm:columns-2 lg:columns-3">
      {showcases.map((showcase, idx) =>
        <div key={idx} className="mb-3 inline-block w-full break-inside-avoid">
          {caseByKind(showcase, {
            "testimonial": (t) => <TestimonialCard key={idx} testimonial={t} />,
            "real-app": (p) => <ProjectCard key={idx} project={p} />,
            "example-app": (p) => <ProjectCard key={idx} project={p} />
          })}
        </div>
      )}
    </div>
  </SectionContainer>
);

const TestimonialCard = ({ testimonial }: { testimonial: Testimonial }) => {
  return (
    <article
      className="border-l-4 border-wasp-black py-3 pl-4">
      <p className="mb-2 text-sm italic leading-relaxed text-wasp-g7">
        “{testimonial.text}”
      </p>
      <div className="font-mono text-xs text-wasp-g6">{testimonial.attribution}</div>
    </article>
  );
};

const ProjectCard = ({ project }: { project: Project }) => {
  return (
    <article
      className={classNames(
        "border border-wasp-black p-4",
        caseByKind(project, { "real-app": () => "bg-wasp-yellow-light", "example-app" : () => "bg-wasp-white" }),
      )}
    >
      <h4 className="mb-1 flex flex-wrap items-baseline gap-x-2 font-mono text-base font-bold text-wasp-black">
        {project.name}
        <span className="font-mono text-sm text-wasp-g6 font-normal ml-2">
          {commentLabel(project)}
        </span>
      </h4>
      <p className="mb-2 text-sm leading-relaxed text-wasp-g6">
        {project.description}
      </p>
      <Link
        to={project.linkUrl}
        className="font-mono text-xs font-semibold text-wasp-black underline decoration-wasp-yellow decoration-2 underline-offset-2 hover:text-wasp-black hover:decoration-wasp-yellow-dark"
      >
        {project.linkText}
      </Link>
    </article>
  );
}

const commentLabel = (project: Project) => (
  "// " +
    caseByKind(project, { "real-app": () => "real app", "example-app" : () => "example app" }) +
    (project.tag ? ': ' + project.tag : '')
);

const showcases: Showcase[] = [
  {
    kind: "testimonial",
    text: "Two years ago I couldn't imagine building an application. Now I have a production app with users. It's kind of magic for me.",
    attribution: "Leo Golubyov · MeSync",
  },
  {
    kind: "real-app",
    name: "Hireveld",
    tag: "startup",
    description:
      "Job board platform — from idea to production in weeks, not months.",
    linkText: "read case study →",
    linkUrl:
      "https://wasp.sh/blog/2026/03/29/hireveld-from-10-stacks-to-production-with-wasp",
  },
  {
    kind: "real-app",
    name: "Searchcraft",
    tag: "saas",
    description:
      "AI-powered search engine for product teams — keyword precision plus semantic understanding.",
    linkText: "visit site →",
    linkUrl: "https://www.searchcraft.io/",
  },
  {
    kind: "testimonial",
    text: "It just works. I spent more time worrying about deployment than about Wasp infrastructure, because it works. It is solid.",
    attribution: "Josh Nathan · EasyPay",
  },
  {
    kind: "testimonial",
    text: "I probably wouldn't even have launched CTOBox without Wasp. That's the truth.",
    attribution: "Sergio · CTOBox",
  },
  {
    kind: "real-app",
    name: "Microinfluencer Club",
    tag: "saas",
    description:
      "Connect brands with micro-influencers to run marketing campaigns.",
    linkText: "visit site →",
    linkUrl: "https://microinfluencer.club/",
  },
  {
    kind: "testimonial",
    text: "Things are configured for me, tools are there, and I can focus on building.",
    attribution: "Stefan Vitoria",
  },
  {
    kind: "real-app",
    name: "Scribeist",
    tag: "saas",
    description:
      "AI writing platform with workspaces for novels, blogs, essays, and notes.",
    linkText: "visit site →",
    linkUrl: "https://scribeist.com/",
  },
  {
    kind: "example-app",
    name: "CoverLetterGPT",
    description:
      "Generate cover letters from CV + job description. Powered by ChatGPT.",
    linkText: "see the code →",
    linkUrl: "https://github.com/vincanger/coverlettergpt",
  },
  {
    kind: "testimonial",
    text: "Everything just works on the first try — minimal intervention needed for anything.",
    attribution: "Hrvoje Pavlinovic · Memoato",
  },
  {
    kind: "testimonial",
    text: "If you start with Wasp, 80% of the pains of vibe coding are taken care of for you already.",
    attribution: "Kenny Rogers",
  },
  {
    kind: "real-app",
    name: "Amicus",
    tag: "saas",
    description:
      "Legal task and workflow management for law firms. Full SaaS with billing, multi-tenant auth, and audit logs — built and shipped by a two-person team.",
    linkText: "read case study →",
    linkUrl: "https://wasp.sh/blog/2022/11/26/erlis-amicus-usecase",
  },
  {
    kind: "testimonial",
    text: "Just using AI would make it harder to sleep at night. With Wasp I feel secure, like I'm not cutting any corners — I can sleep knowing it's all OK.",
    attribution: "Robbie · PeakMastering",
  },
  {
    kind: "testimonial",
    text: "It's the cleanest, most structured and tidy way. I feel safe - security-wise, I'm in control.",
    attribution: "Dimitrios · Kivo",
  },
  {
    kind: "example-app",
    name: "AskDocs",
    description: "Chat with your documents using RAG. Deployed with one command.",
    linkText: "see the code →",
    linkUrl:
      "https://github.com/wasp-lang/wasp/tree/release/examples/ask-the-documents",
  },
  {
    kind: "example-app",
    name: "WaspVote",
    description: "Real-time voting with WebSockets.",
    linkText: "see the code →",
    linkUrl:
      "https://github.com/wasp-lang/wasp/tree/release/examples/websockets-realtime-voting",
  },
  {
    kind: "testimonial",
    text: "Wasp & Cursor cut our work from idea to almost production by literal months.",
    attribution: "Valentin Lazureanu",
  },
  {
    kind: "testimonial",
    text: "I tried 10 different stacks, and Wasp is by far the best for AI coding. The high-level Wasp file lets me stay on top of everything without getting lost in the file tree.",
    attribution: "Marcel Coetzee",
  },
  {
    kind: "example-app",
    name: "Waspello",
    description: "A Trello clone, implemented in Wasp.",
    linkText: "see the code →",
    linkUrl: "https://github.com/wasp-lang/wasp/tree/release/examples/waspello",
  },
  {
    kind: "example-app",
    name: "Todo App",
    description: "A famous Todo App, implemented in Wasp.",
    linkText: "see the code →",
    linkUrl:
      "https://github.com/wasp-lang/wasp/tree/release/examples/tutorials/TodoAppTs",
  },
];

export default WaspOutThere;
