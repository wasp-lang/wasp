import { useState } from "react";
import Link from "@docusaurus/Link";
import { Terminal, ChevronDown } from "react-feather";
import CodeHighlight from "../CodeHighlight";
import { vc, InstallBlock } from "./vcVariant";

const ActionButtons = ({ variant }) => (
  <div className="flex items-center justify-center gap-4">
    <Link to="/docs/quick-start">
      <button
        className={vc(variant, {
          base: "inline-flex items-center space-x-2 rounded-lg border border-yellow-500 bg-yellow-500 px-5 py-3 text-base font-medium text-white transition duration-200 ease-out hover:border-yellow-400 hover:bg-yellow-400",
          v1: "inline-flex items-center space-x-2 rounded-none border border-yellow-500 bg-yellow-500 px-5 py-3 text-base font-medium text-white transition duration-200 ease-out hover:border-yellow-400 hover:bg-yellow-400",
          v2: "inline-flex items-center space-x-2 rounded-none border border-yellow-500 bg-yellow-500 px-5 py-3 text-base font-medium text-white transition duration-200 ease-out hover:border-yellow-400 hover:bg-yellow-400",
          v3: "inline-flex items-center space-x-2 rounded-none border border-neutral-300 bg-neutral-200 text-neutral-700 px-5 py-3 font-mono text-sm font-medium uppercase tracking-wider transition duration-200 ease-out hover:border-neutral-400",
        })}
      >
        <Terminal size={16} />
        <span>Get Started</span>
      </button>
    </Link>
    <a href="#how-wasp-works">
      <button
        className={vc(variant, {
          base: "inline-flex items-center space-x-2 rounded-lg border border-neutral-300 px-5 py-3 text-base font-medium text-neutral-700 transition duration-200 ease-out hover:border-neutral-400 hover:text-neutral-900",
          v1: "inline-flex items-center space-x-2 rounded-none border border-neutral-300 px-5 py-3 text-base font-medium text-neutral-700 transition duration-200 ease-out hover:border-neutral-400 hover:text-neutral-900",
          v2: "inline-flex items-center space-x-2 rounded-none border border-neutral-300 px-5 py-3 text-base font-medium text-neutral-700 transition duration-200 ease-out hover:border-neutral-400 hover:text-neutral-900",
          v3: "inline-flex items-center space-x-2 rounded-none border border-neutral-300 bg-neutral-100 px-5 py-3 font-mono text-sm font-medium uppercase tracking-wider text-neutral-700 transition duration-200 ease-out hover:border-neutral-400 hover:text-neutral-900",
        })}
      >
        <ChevronDown size={16} />
        <span>See How It Works</span>
      </button>
    </a>
  </div>
);

const AIToolLogos = () => (
  <div className="flex flex-col items-center gap-4">
    {/* <small className="text-xs text-neutral-400">Works with</small>
    <div className="flex items-center gap-6">
      <span className="text-sm text-neutral-400">Claude Code</span>
      <span className="text-sm text-neutral-400">Cursor</span>
      <span className="text-sm text-neutral-400">GitHub Copilot</span>
      <span className="text-sm text-neutral-400">Windsurf</span>
    </div> */}
    {/* <span className="mt-4 flex items-center">
      <small className="text-xs text-neutral-400">Backed by</small>
      <img
        className="ml-2 w-24"
        src="img/lp/yc-logo-rounded.webp"
        alt="Y Combinator"
      />
    </span> */}
  </div>
);

const GridBackground = ({ variant }) => {
  const isV2Only = variant === "v2";
  return (
    <div className="pointer-events-none absolute inset-0 overflow-hidden" style={{ maskImage: "linear-gradient(to bottom, black 40%, transparent 100%)", WebkitMaskImage: "linear-gradient(to bottom, black 40%, transparent 100%)" }}>
      <div
        className={vc(variant, {
          base: "absolute inset-0 opacity-[0.07]",
          v1: "absolute inset-0 opacity-[0.07]",
          v2: "absolute inset-0 opacity-[0.10]",
          v3: "absolute inset-0 opacity-[0.07]",
        })}
        style={{
          backgroundImage:
            "linear-gradient(to right, #000 1px, transparent 1px), linear-gradient(to bottom, #000 1px, transparent 1px)",
          backgroundSize: isV2Only ? "120px 120px" : "200px 200px",
        }}
      />
    </div>
  );
};

const VCHero = ({ variant }) => {
  return (
    <div className="relative">
      <GridBackground variant={variant} />
      {variant === "v3" && (
        <div
          className="pointer-events-none absolute inset-0 z-[1]"
          style={{
            background: "radial-gradient(ellipse 50% 60% at 50% 50%, #f5f5f5 40%, transparent 100%)",
          }}
        />
      )}
      <div className={`relative mx-auto max-w-4xl px-6 py-28 text-center lg:py-30 ${variant === "v3" ? "z-[2] lg:py-40" : ""}`}>
        <h1 className="text-5xl font-extrabold tracking-tight text-neutral-800 sm:text-6xl lg:text-7xl">
          {variant === "v3" ? (
            <>The framework for building{" "}
              <span className="underline decoration-yellow-500 underline-offset-4">
                real apps
              </span>{" "} with AI
            </>
          ) : (
            <>The{" "}
              <span
                className={vc(variant, {
                  base: "underline decoration-yellow-500 underline-offset-4",
                  v1: "underline decoration-yellow-500 underline-offset-4",
                  v2: "underline decoration-yellow-500 underline-offset-4",
                })}
              >
                higher-level
              </span>{" "}
              full-stack framework for AI
            </>
          )}
        </h1>
        <p className="mx-auto mt-8 max-w-2xl text-lg text-neutral-500 sm:text-xl">
          {variant === "v3" ? (
            <>Built for the agentic era, Wasp keeps your agents on track, handles complex features, and lowers token usage and cost.</>
          ) : (
            <>Define features as config, forget boilerplate, and give your agents the
              most{" "}
              <strong className="text-neutral-600">
                context-efficient framework
              </strong>{" "}
              out there.</>
          )}
        </p>
        <div className="mt-10">
          <ActionButtons variant={variant} />
        </div>
        <div className="mt-6 flex justify-center">
          <InstallBlock variant={variant} />
        </div>
        <div className="mt-12">
          <AIToolLogos />
        </div>
      </div>
    </div>
  );
};

export default VCHero;
