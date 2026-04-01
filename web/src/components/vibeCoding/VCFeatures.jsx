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
import { VCSection } from "./vcWrappers";

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
      <div className="inline-flex h-8 w-8 items-center justify-center rounded-none border border-neutral-600 bg-neutral-700 text-yellow-500">
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
      "Google, Slack, email login, etc. Just say which ones you want. Wasp sets it all up and even handles the UI for you.",
    url: "/docs/auth/overview",
  },
  {
    Icon: Grid,
    title: "Automatic CRUD",
    description:
      "Describe your data and Wasp creates all the basic create, read, update, delete operations automatically.",
    url: "/docs/data-model/crud",
  },
  {
    Icon: Link2,
    title: "Typesafe Operations",
    description:
      "Your frontend and backend safely talk to each other automatically. No need to wire API endpoints and full-stack type safety by hand.",
    url: "/docs/data-model/operations/overview",
  },
  {
    Icon: FileText,
    title: "TypeScript Config",
    description:
      "Your app config is written in TypeScript, a safe choice your AI already speaks fluently.",
    url: "/docs/general/wasp-ts-config",
  },
  {
    Icon: Settings,
    title: "Background Jobs",
    description:
      "Need something to run on a schedule? Just describe when and what. Wasp handles the rest without relying on expensive third-party services.",
    url: "/docs/advanced/jobs",
  },
  {
    Icon: BookOpen,
    title: "LLM-friendly Docs",
    description:
      "Wasp provides special documentation files optimized for AI, so your agent always has the best context.",
    url: "/llms.txt",
  },
  {
    Icon: Send,
    title: "One-command Deploy",
    description:
      "When you're ready to go live, one command gets your app deployed to your favorite platform. No DevOps degree needed.",
    url: "/docs/deployment/intro",
  },
  {
    Icon: Eye,
    title: "Human-reviewable Output",
    description:
      "Because Wasp uses consistent patterns, the code your AI generates is easy to read and review (for humans and AI).",
    url: "/docs",
  },
];

const VCFeatures = () => {
  return (
    <VCSection className="lg:py-18 space-y-16">
      <div className="mx-auto max-w-3xl text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          Everything your app needs,{" "}
          <span className="underline decoration-yellow-500">built in</span>
        </h2>
      </div>
      <dl className="grid grid-cols-1 md:gap-16 lg:grid-cols-4 lg:gap-x-8 xl:gap-x-16">
        {features.map((feature, idx) => (
          <Feature key={idx} {...feature} />
        ))}
      </dl>
    </VCSection>
  );
};

export default VCFeatures;
