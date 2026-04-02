import Link from "@docusaurus/Link";
import { ChevronDown, Terminal } from "react-feather";
import { InstallBlock } from "./vcWrappers";

const ActionButtons = () => (
  <div className="flex items-center justify-center gap-4">
    <Link to="/docs/quick-start">
      <button className="inline-flex items-center space-x-2 rounded-none border border-neutral-300 bg-neutral-200 px-5 py-3 font-mono text-sm font-medium uppercase tracking-wider text-neutral-700 transition duration-200 ease-out hover:border-neutral-400">
        <Terminal size={16} />
        <span>Get Started</span>
      </button>
    </Link>
    <a href="#how-wasp-works">
      <button className="inline-flex items-center space-x-2 rounded-none border border-neutral-300 bg-neutral-100 px-5 py-3 font-mono text-sm font-medium uppercase tracking-wider text-neutral-700 transition duration-200 ease-out hover:border-neutral-400 hover:text-neutral-900">
        <ChevronDown size={16} />
        <span>See How It Works</span>
      </button>
    </a>
  </div>
);

const GridBackground = () => {
  return (
    <div
      className="pointer-events-none absolute inset-0 overflow-hidden"
      style={{
        maskImage: "linear-gradient(to bottom, black 40%, transparent 100%)",
        WebkitMaskImage:
          "linear-gradient(to bottom, black 40%, transparent 100%)",
      }}
    >
      <div
        className="absolute inset-0 opacity-[0.07]"
        style={{
          backgroundImage:
            "linear-gradient(to right, #000 1px, transparent 1px), linear-gradient(to bottom, #000 1px, transparent 1px)",
          backgroundSize: "200px 200px",
        }}
      />
    </div>
  );
};

const VCHero = () => {
  return (
    <div className="relative">
      <GridBackground />
      <div
        className="pointer-events-none absolute inset-0 z-[1]"
        style={{
          background:
            "radial-gradient(ellipse 50% 60% at 50% 50%, #f5f5f5 40%, transparent 100%)",
        }}
      />
      <div className="relative z-[2] mx-auto max-w-4xl px-6 pb-36 pt-28 text-center lg:pb-52 lg:pt-40">
        <h1 className="text-5xl font-extrabold tracking-tight text-neutral-800 sm:text-6xl lg:text-7xl">
          The framework for building{" "}
          <span className="underline decoration-yellow-500 underline-offset-4">
            real apps
          </span>{" "}
          with AI
        </h1>
        <p className="mx-auto mt-8 max-w-2xl text-lg text-neutral-500 sm:text-xl">
          Whether you're a pro or a first-time builder, Wasp keeps your agents
          on track, handles complex features, and lowers token usage and cost.
        </p>
        <div className="mt-10">
          <ActionButtons />
        </div>
        <div className="mt-6 flex justify-center">
          <InstallBlock />
        </div>
      </div>
    </div>
  );
};

export default VCHero;
