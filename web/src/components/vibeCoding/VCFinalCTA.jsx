import Link from "@docusaurus/Link";
import { Star, Terminal } from "react-feather";
import { InstallBlock, VCSection } from "./vcVariant";

const VCFinalCTA = () => {
  const wrapperClass =
    "mx-auto max-w-2xl rounded-none border border-neutral-300 bg-yellow-500/10 p-12 text-center";

  const primaryBtnClass =
    "inline-flex items-center space-x-2 rounded-none uppercase tracking-wider font-mono border border-yellow-500 bg-yellow-500 px-3 py-2 text-sm leading-4 text-white transition duration-200 ease-out hover:border-yellow-400 hover:bg-yellow-400";

  const secondaryBtnClass =
    "inline-flex items-center space-x-2 rounded-none border border-neutral-500 px-3 py-2 text-sm leading-4 text-neutral-700 transition duration-200 ease-out hover:border-neutral-400 hover:text-neutral-400";

  return (
    <VCSection>
      <div className={wrapperClass}>
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          Ready to build something real?
        </h2>
        <p className="text-neutral-500">
          Wasp is free, open-source, and takes 30 seconds to install. Your AI
          handles the code. Wasp handles the architecture. You focus on your
          idea.
        </p>

        <InstallBlock
          command="npm i -g @wasp.sh/wasp-cli@latest"
          className="my-8 block"
        />

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
