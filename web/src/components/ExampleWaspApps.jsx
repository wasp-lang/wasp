import Link from "@docusaurus/Link";
import { ArrowUpRight, GitHub, Monitor } from "react-feather";
import SectionContainer from "./Layouts/SectionContainer";

const examples = [
  {
    title: "Open SaaS",
    description:
      "A feature-rich, open-source SaaS boilerplate template. Powered by Wasp.",
    authorName: "wasp",
    authorImg: "https://avatars.githubusercontent.com/u/55102317",
    repoName: "open-saas",
    repoUrl: "https://github.com/wasp-lang/open-saas/tree/main/template/app",
    demoUrl: "https://opensaas.sh",
  },
  {
    title: "Roke ⛰️",
    description: "A Wasp + ShadCN + Framer Motion Starter Template.",
    authorName: "wardbox",
    authorImg: "https://avatars.githubusercontent.com/u/31261302",
    repoName: "roke",
    repoUrl: "https://github.com/wardbox/roke",
    demoUrl: "https://roke.dev",
  },
  {
    title: "Todo App (TypeScript) ✅",
    description: "A famous To-Do list app, implemented in TypeScript.",
    authorName: "wasp",
    authorImg: "https://avatars.githubusercontent.com/u/55102317",
    repoName: "TodoAppTs",
    repoUrl:
      "https://github.com/wasp-lang/wasp/tree/release/examples/tutorials/TodoAppTs",
    //demoUrl: 'https://waspello-demo.netlify.app/',
    // todo: try in GitPod/Replit url
  },
];

const SeeTheCodeButton = ({ repoUrl }) => (
  <Link to={repoUrl}>
    <button
      className={`flex items-center rounded border border-yellow-500 bg-transparent px-2.5 py-1 text-xs text-neutral-500 transition duration-200 ease-out hover:text-neutral-400`}
    >
      <span>See the code</span>
      <ArrowUpRight className="ml-2" size={14} />
    </button>
  </Link>
);

const DemoButton = ({ demoUrl }) => (
  <Link to={demoUrl}>
    <button
      className={`flex items-center rounded bg-yellow-500 px-2.5 py-1 text-xs text-white transition duration-200 ease-out hover:bg-yellow-400`}
    >
      <span>Demo</span>
      <Monitor className="ml-2" size={14} />
    </button>
  </Link>
);

const ExampleCard = (props) => (
  <>
    {/* Top half */}
    <div
      className={`flex h-40 flex-col rounded rounded-b-none border-l border-r border-t border-yellow-500/25 bg-yellow-500/5 p-5`}
    >
      <div className="mb-4">
        <h4 className="mb-4 text-neutral-700">{props.title}</h4>
        <p className="mb-4 text-sm text-neutral-500">{props.description}</p>
        <div>
          <img
            className="inline w-6 rounded-full"
            src={props.authorImg}
            alt={props.authorName + " GitHub profile picture"}
          />
          <span className="ml-2 text-sm text-neutral-700">
            {props.authorName}
          </span>
        </div>
      </div>
    </div>

    {/* Bottom half */}
    <div
      className={`flex flex-col rounded rounded-t-none border-b border-l border-r border-yellow-500/25 bg-yellow-500/20 p-5`}
    >
      <Link to={props.repoUrl}>
        <span className="flex items-center text-sm text-neutral-500 hover:text-neutral-400">
          <span className="">{props.repoName}</span>
          <span className="ml-1 inline-block">
            <GitHub className="" size={14} />
          </span>
        </span>
      </Link>

      {/* Action buttons */}
      <div className="mt-3 flex items-center gap-2">
        <SeeTheCodeButton repoUrl={props.repoUrl} />
        {/* Demo apps are not mobile-friendly yet so hiding them on mobile for now. */}
        <span className="hidden md:block">
          {props.demoUrl && <DemoButton demoUrl={props.demoUrl} />}
        </span>
      </div>
    </div>
  </>
);

const ExampleWaspApps = () => {
  return (
    <SectionContainer className="space-y-16" id="examples">
      <div className="grid grid-cols-12">
        <div className="col-span-12 text-center">
          <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
            Show, don't tell.
          </h2>
          <p className="text-neutral-500">
            Take a look at examples - see how things work and get inspired for
            your next project.
          </p>
        </div>
      </div>

      <div className="mt-16 grid grid-cols-12 gap-5">
        {examples.slice(0, 6).map((e, idx) => (
          <div className="col-span-12 lg:col-span-6 xl:col-span-4" key={idx}>
            <ExampleCard {...e} />
          </div>
        ))}
      </div>

      <div className="flex justify-center">
        <Link to="https://github.com/wasp-lang/wasp/tree/release/examples">
          <span
            className={`flex items-center font-medium text-neutral-500 underline decoration-yellow-500 decoration-2 transition duration-200 ease-out hover:text-neutral-400`}
          >
            <span>See all examples</span>
            <ArrowUpRight className="ml-1" size={14} />
          </span>
        </Link>
      </div>
    </SectionContainer>
  );
};

export default ExampleWaspApps;
