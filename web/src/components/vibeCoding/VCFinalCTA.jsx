import Link from "@docusaurus/Link";
import { Star, Terminal } from "react-feather";
import { vc, InstallBlock, VCSection } from "./vcVariant";

const VCFinalCTA = ({ variant }) => {
  const wrapperClass = vc(variant, {
    base: "mx-auto max-w-2xl rounded-lg bg-yellow-500/10 p-12 text-center",
    v1: "mx-auto max-w-2xl rounded-none border border-yellow-500/20 bg-yellow-500/10 p-12 text-center",
    v2: "mx-auto max-w-2xl rounded-none border border-neutral-300 bg-yellow-500/10 p-12 text-center",
    v3: "mx-auto max-w-2xl rounded-none border border-neutral-300 bg-yellow-500/10 p-12 text-center",
  });


  const primaryBtnClass = vc(variant, {
    base: "inline-flex items-center space-x-2 rounded border border-yellow-500 bg-yellow-500 px-3 py-2 text-sm leading-4 text-white transition duration-200 ease-out hover:border-yellow-400 hover:bg-yellow-400",
    v1: "inline-flex items-center space-x-2 rounded-none border border-yellow-500 bg-yellow-500 px-3 py-2 text-sm leading-4 text-white transition duration-200 ease-out hover:border-yellow-400 hover:bg-yellow-400",
    v2: "inline-flex items-center space-x-2 rounded-none border border-yellow-500 bg-yellow-500 px-3 py-2 text-sm leading-4 text-white transition duration-200 ease-out hover:border-yellow-400 hover:bg-yellow-400",
    v3: "inline-flex items-center space-x-2 rounded-none uppercase tracking-wider font-mono border border-yellow-500 bg-yellow-500 px-3 py-2 text-sm leading-4 text-white transition duration-200 ease-out hover:border-yellow-400 hover:bg-yellow-400",
  });

  const secondaryBtnClass = vc(variant, {
    base: "inline-flex items-center space-x-2 rounded border border-neutral-500 px-3 py-2 text-sm leading-4 text-neutral-700 transition duration-200 ease-out hover:border-neutral-400 hover:text-neutral-400",
    v1: "inline-flex items-center space-x-2 rounded-none border border-neutral-500 px-3 py-2 text-sm leading-4 text-neutral-700 transition duration-200 ease-out hover:border-neutral-400 hover:text-neutral-400",
    v2: "inline-flex items-center space-x-2 rounded-none border border-neutral-500 px-3 py-2 text-sm leading-4 text-neutral-700 transition duration-200 ease-out hover:border-neutral-400 hover:text-neutral-400",
    v3: "inline-flex items-center space-x-2 rounded-none border border-neutral-500 px-3 py-2 text-sm leading-4 text-neutral-700 transition duration-200 ease-out hover:border-neutral-400 hover:text-neutral-400",
  });

  return (
    <VCSection variant={variant}>
      <div className={wrapperClass}>
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          {variant === "v3"
            ? "Ready to build something real?"
            : "Your AI is ready. Give it a framework that keeps up."}
        </h2>
        <p className="text-neutral-500">
          {variant === "v3"
            ? "Wasp is free, open-source, and takes 30 seconds to install. Your AI handles the code. Wasp handles the architecture. You focus on your idea."
            : "Wasp is free, open-source, and takes 30 seconds to install. Architecture decided. Auth built-in. CRUD auto-generated. Your AI just writes features."}
        </p>

        <InstallBlock variant={variant} command="npm i -g @wasp.sh/wasp-cli@latest" className="my-8 block" />

        <div className="flex items-center justify-center gap-3">
          <Link to="/docs/quick-start">
            <button className={primaryBtnClass}>
              <Terminal size={16} />
              <span>Get Started</span>
            </button>
          </Link>
          <a
            href="https://github.com/wasp-lang/wasp"
            target="_blank"
            rel="noreferrer"
          >
            <button className={secondaryBtnClass}>
              <Star size={16} />
              <span>Star on GitHub</span>
            </button>
          </a>
        </div>

        <p className="mt-6 text-sm text-neutral-400">
          Free and open-source. No account required. Works with React, Node.js,
          and Prisma.
        </p>
      </div>
    </VCSection>
  );
};

export default VCFinalCTA;
