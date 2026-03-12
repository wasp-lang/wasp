import { useState } from "react";
import Link from "@docusaurus/Link";
import { Terminal, ChevronDown } from "react-feather";
import CodeHighlight from "../CodeHighlight";

const ActionButtons = () => (
  <div className="flex items-center justify-center gap-4">
    <Link to="/docs/quick-start">
      <button
        className="inline-flex items-center space-x-2 rounded-lg border border-yellow-500 bg-yellow-500 px-5 py-3 text-base font-medium text-white transition duration-200 ease-out hover:border-yellow-400 hover:bg-yellow-400"
      >
        <Terminal size={16} />
        <span>Get Started</span>
      </button>
    </Link>
    <a href="#how-wasp-works">
      <button
        className="inline-flex items-center space-x-2 rounded-lg border border-neutral-300 px-5 py-3 text-base font-medium text-neutral-700 transition duration-200 ease-out hover:border-neutral-400 hover:text-neutral-900"
      >
        <ChevronDown size={16} />
        <span>See How It Works</span>
      </button>
    </a>
  </div>
);

const AIToolLogos = () => (
  <div className="flex flex-col items-center gap-4">
    <small className="text-xs text-neutral-400">Works with</small>
    <div className="flex items-center gap-6">
      <span className="text-sm text-neutral-400">Claude Code</span>
      <span className="text-sm text-neutral-400">Cursor</span>
      <span className="text-sm text-neutral-400">GitHub Copilot</span>
      <span className="text-sm text-neutral-400">Windsurf</span>
    </div>
    <span className="mt-4 flex items-center">
      <small className="text-xs text-neutral-400">Backed by</small>
      <img
        className="ml-2 w-24"
        src="img/lp/yc-logo-rounded.webp"
        alt="Y Combinator"
      />
    </span>
  </div>
);

const GridBackground = () => (
  <div className="pointer-events-none absolute inset-0 overflow-hidden" style={{ maskImage: "linear-gradient(to bottom, black 40%, transparent 100%)", WebkitMaskImage: "linear-gradient(to bottom, black 40%, transparent 100%)" }}>
    <div
      className="absolute inset-0 opacity-[0.07]"
      style={{
        backgroundImage:
          "linear-gradient(to right, #000 1px, transparent 1px), linear-gradient(to bottom, #000 1px, transparent 1px)",
        backgroundSize: "200px 200px",
      }}
    />
    <div className="absolute left-[10%] top-[10%] h-24 w-24 rounded-full border border-neutral-300 opacity-50" />
    <div className="absolute right-[15%] top-[8%] h-20 w-20 rounded-full border border-neutral-300 opacity-40" />
    <div className="absolute bottom-[15%] right-[10%] h-28 w-28 rounded-full border border-neutral-300 opacity-45" />
    <div className="absolute bottom-[20%] left-[5%] h-16 w-16 rounded-full border border-neutral-300 opacity-40" />
  </div>
);

const VCHero = () => {
  return (
    <div className="relative">
      <GridBackground />
      <div className="relative mx-auto max-w-4xl px-6 py-28 text-center lg:py-40">
        <h1 className="text-5xl font-extrabold tracking-tight text-neutral-800 sm:text-6xl lg:text-7xl">
          The{" "}
          <span className="underline decoration-yellow-500 underline-offset-4">
            higher-level
          </span>{" "}
          full-stack framework for AI.
        </h1>
        <p className="mx-auto mt-6 max-w-2xl text-lg text-neutral-500 sm:text-xl">
          Auth, CRUD, routes, and deployment are just config — making Wasp the
          most{" "}
          <strong className="text-neutral-700">
            context-efficient framework
          </strong>{" "}
          for AI-assisted development.
        </p>
        <div className="mt-10">
          <ActionButtons />
        </div>
        <div className="mt-8 flex justify-center">
          <code className="rounded bg-neutral-100 px-4 py-2 text-sm text-neutral-500">
            <span className="text-yellow-500">%</span> npm i -g @wasp.sh/wasp-cli
          </code>
        </div>
        <div className="mt-12">
          <AIToolLogos />
        </div>
      </div>
    </div>
  );
};

export default VCHero;
