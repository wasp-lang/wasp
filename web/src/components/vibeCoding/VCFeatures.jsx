import Link from "@docusaurus/Link";
import {
  ArrowRight,
  BookOpen,
  Eye,
  FileText,
  Grid,
  Link2,
  Send,
  Settings,
  Unlock,
} from "react-feather";
import SectionContainer from "../Layouts/SectionContainer";

const TextLink = ({ url, label }) => (
  <Link to={url}>
    <span className="mt-3 block cursor-pointer text-sm text-neutral-500 hover:text-neutral-400">
      <div className="group flex items-center gap-1">
        <span>{label}</span>
        <div className="transition-all group-hover:ml-0.5">
          <span className="text-yellow-600">
            <ArrowRight size={14} strokeWidth={2} />
          </span>
        </div>
      </div>
    </span>
  </Link>
);

const Feature = ({ Icon, title, description, url }) => (
  <div className="mb-10 space-y-4 md:mb-0">
    <div className="flex items-center">
      <div className="inline-flex h-8 w-8 items-center justify-center rounded-md bg-neutral-700 text-yellow-500">
        <Icon size={20} />
      </div>
      <dt className="ml-4 text-neutral-700">{title}</dt>
    </div>
    <p className="text-neutral-700">{description}</p>
    {url && <TextLink url={url} label="Learn more" />}
  </div>
);

const features = [
  {
    Icon: Unlock,
    title: "Full-stack Auth",
    description:
      "Google, GitHub, email auth — declared in config. No middleware, no session handling, no OAuth callbacks. That's 500-800 lines your AI never has to write.",
    url: "/docs/auth/overview",
  },
  {
    Icon: Grid,
    title: "Automatic CRUD",
    description:
      "Define a data model, get typesafe create/read/update/delete operations with client-side cache invalidation. AI skips straight to custom logic.",
    url: "/docs/data-model/crud",
  },
  {
    Icon: Link2,
    title: "Typesafe Operations",
    description:
      "Queries and actions with automatic RPC between client and server. Your AI doesn't wire API endpoints — it just writes the function.",
    url: "/docs/data-model/operations/overview",
  },
  {
    Icon: FileText,
    title: "TypeScript Config",
    description:
      "Write your app config in main.wasp.ts with full IDE autocomplete and type checking. Your AI already speaks TypeScript. Split across files as your app grows.",
    url: "/docs/general/wasp-ts-config",
  },
  {
    Icon: Settings,
    title: "Background Jobs",
    description:
      "Recurring tasks, one-off jobs, retries — declared in config. AI doesn't build a job queue from scratch.",
    url: "/docs/advanced/jobs",
  },
  {
    Icon: BookOpen,
    title: "LLM-friendly Docs",
    description:
      "Wasp provides llms.txt — optimized documentation that gives AI 10x more context per token than traditional docs or MCP servers.",
    url: "pathname:///llms.txt",
  },
  {
    Icon: Send,
    title: "One-command Deploy",
    description:
      "Deploy to any platform with CLI helpers. No Dockerfiles, no CI/CD pipelines to set up.",
    url: "/docs/deployment/intro",
  },
  {
    Icon: Eye,
    title: "Human-reviewable Output",
    description:
      "Declarative config means consistent patterns from the first feature to the fiftieth. AI-generated code follows the same architecture — easy to review, easy to trust.",
  },
];

const VCFeatures = () => {
  return (
    <SectionContainer className="lg:py-18 space-y-16">
      <div className="mx-auto max-w-3xl text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          Built-in features that{" "}
          <span className="underline decoration-yellow-500">
            eliminate AI busywork
          </span>
        </h2>
      </div>
      <dl className="grid grid-cols-1 md:gap-16 lg:grid-cols-4 lg:gap-x-8 xl:gap-x-16">
        {features.map((feature, idx) => (
          <Feature key={idx} {...feature} />
        ))}
      </dl>
    </SectionContainer>
  );
};

export default VCFeatures;
