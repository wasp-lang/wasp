import Link from "@docusaurus/Link";
import useBrokenLinks from "@docusaurus/useBrokenLinks";

import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";
import TextLink from "./TextLink";

const SHOWCASES_ID = "showcases";

interface RealApp {
  name: string;
  description: string;
  linkText: string;
  linkUrl: string;
  imageSrc: string;
  storyText?: string;
  storyUrl?: string;
}

const ShippedWithWasp = () => {
  // Register anchor so Docusaurus's broken-link checker sees it at build time.
  useBrokenLinks().collectAnchor(SHOWCASES_ID);

  return (
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
  );
};

const ProjectCard = ({ project }: { project: RealApp }) => (
  <div className="flex h-full flex-col overflow-hidden border border-wasp-black bg-wasp-white transition-shadow hover:shadow-md">
    <Link
      to={project.linkUrl}
      className="block no-underline hover:no-underline"
    >
      <img
        src={project.imageSrc}
        alt={project.name}
        className="aspect-video w-full object-cover object-top"
      />
      <div className="px-4 pb-3 pt-4">
        <h4 className="mb-1 font-mono text-base font-bold text-wasp-black">
          {project.name}
        </h4>
        <p className="text-sm leading-relaxed text-wasp-g6">
          {project.description}
        </p>
      </div>
    </Link>
    <div className="mt-auto flex flex-wrap items-center gap-x-4 gap-y-1 px-4 pb-4">
      <TextLink
        to={project.linkUrl}
        className="font-mono text-xs font-semibold"
      >
        {project.linkText}
      </TextLink>
      {project.storyUrl && (
        <Link
          to={project.storyUrl}
          className="inline-flex items-center gap-1 bg-wasp-yellow px-1.5 py-0.5 font-mono text-xs font-semibold text-wasp-black no-underline transition-colors hover:bg-wasp-yellow-dark hover:text-wasp-black hover:no-underline"
        >
          {project.storyText ?? "read the story →"}
        </Link>
      )}
    </div>
  </div>
);

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
    storyText: "read the story →",
    storyUrl: "/blog/2026/07/20/made-with-wasp-searchcraft",
  },
];

export default ShippedWithWasp;
