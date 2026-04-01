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
import { vc, VCSection } from "./vcVariant";

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

const Feature = ({ Icon, title, description, v3Description, url, variant }) => (
  <div className="mb-10 space-y-4 md:mb-0">
    <div className="flex items-center">
      <div className={`inline-flex h-8 w-8 items-center justify-center ${vc(variant, { base: "rounded-md bg-neutral-700", v1: "rounded-none bg-neutral-700", v2: "rounded-none bg-neutral-700", v3: "rounded-none border border-neutral-600 bg-neutral-700" })} text-yellow-500`}>
        <Icon size={20} />
      </div>
      <dt className="ml-4 text-neutral-700">{title}</dt>
    </div>
    <p className="text-neutral-700">{variant === "v3" ? v3Description : description}</p>
    {url && <TextLink url={url} label="Learn more" />}
  </div>
);

const features = [
  {
    Icon: Unlock,
    title: "Full-stack Auth",
    description:
      "Google, GitHub, email auth — declared in config. No middleware, no session handling, no OAuth callbacks. That's 500-800 lines your AI never has to write.",
    v3Description:
      "Google login, email login — just say which ones you want. Wasp sets it all up.",
    url: "/docs/auth/overview",
  },
  {
    Icon: Grid,
    title: "Automatic CRUD",
    description:
      "Define a data model, get typesafe create/read/update/delete operations with client-side cache invalidation. AI skips straight to custom logic.",
    v3Description:
      "Describe your data and Wasp creates all the basic operations — create, read, update, delete — automatically.",
    url: "/docs/data-model/crud",
  },
  {
    Icon: Link2,
    title: "Typesafe Operations",
    description:
      "Queries and actions with automatic RPC between client and server. Your AI doesn't wire API endpoints — it just writes the function.",
    v3Description:
      "Your frontend and backend talk to each other automatically. No wiring API endpoints by hand.",
    url: "/docs/data-model/operations/overview",
  },
  {
    Icon: FileText,
    title: "TypeScript Config",
    description:
      "Write your app config in main.wasp.ts with full IDE autocomplete and type checking. Your AI already speaks TypeScript. Split across files as your app grows.",
    v3Description:
      "Your app config is written in TypeScript — your AI already speaks it fluently.",
    url: "/docs/general/wasp-ts-config",
  },
  {
    Icon: Settings,
    title: "Background Jobs",
    description:
      "Recurring tasks, one-off jobs, retries — declared in config. AI doesn't build a job queue from scratch.",
    v3Description:
      "Need something to run on a schedule? Just describe when and what. Wasp handles the rest.",
    url: "/docs/advanced/jobs",
  },
  {
    Icon: BookOpen,
    title: "LLM-friendly Docs",
    description:
      "Wasp provides llms.txt — optimized documentation that gives AI 10x more context per token than traditional docs or MCP servers.",
    v3Description:
      "Wasp provides special documentation files optimized for AI, so your agent always has the best context.",
    url: "pathname:///llms.txt",
  },
  {
    Icon: Send,
    title: "One-command Deploy",
    description:
      "Deploy to any platform with CLI helpers. No Dockerfiles, no CI/CD pipelines to set up.",
    v3Description:
      "When you're ready to go live, one command gets your app deployed. No DevOps degree needed.",
    url: "/docs/deployment/intro",
  },
  {
    Icon: Eye,
    title: "Human-reviewable Output",
    description:
      "Declarative config means consistent patterns through the entire app. AI-generated code is easy to review and trust.",
    v3Description:
      "Because Wasp enforces consistent patterns, the code your AI generates is easy to read and review.",
      url: "/docs"
  },
];

const VCFeatures = ({ variant }) => {
  return (
    <VCSection variant={variant} className="lg:py-18 space-y-16">
      <div className="mx-auto max-w-3xl text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          {variant === "v3" ? (
            <>Everything your app needs,{" "}
              <span className="underline decoration-yellow-500">
                built in
              </span>
            </>
          ) : (
            <>Built-in features that{" "}
              <span className="underline decoration-yellow-500">
                eliminate AI busywork
              </span>
            </>
          )}
        </h2>
      </div>
      <dl className="grid grid-cols-1 md:gap-16 lg:grid-cols-4 lg:gap-x-8 xl:gap-x-16">
        {features.map((feature, idx) => (
          <Feature key={idx} {...feature} variant={variant} />
        ))}
      </dl>
    </VCSection>
  );
};

export default VCFeatures;
