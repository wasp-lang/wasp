import Link from "@docusaurus/Link";
import { Star, Terminal } from "react-feather";
import SectionContainer from "../Layouts/SectionContainer";

const VCFinalCTA = () => {
  return (
    <SectionContainer>
      <div className="mx-auto max-w-2xl rounded-lg bg-yellow-500/10 p-12 text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          Your AI is ready. Give it a framework that keeps up.
        </h2>
        <p className="text-neutral-500">
          Wasp is free, open-source, and takes 30 seconds to install.
          Architecture decided. Auth built-in. CRUD auto-generated. Your AI just
          writes features.
        </p>

        <code className="my-8 block rounded bg-neutral-100 px-4 py-3 text-sm text-neutral-600">
          npm i -g @wasp.sh/wasp-cli@latest
        </code>

        <div className="flex items-center justify-center gap-3">
          <Link to="/docs/quick-start">
            <button className="inline-flex items-center space-x-2 rounded border border-yellow-500 bg-yellow-500 px-3 py-2 text-sm leading-4 text-white transition duration-200 ease-out hover:border-yellow-400 hover:bg-yellow-400">
              <Terminal size={16} />
              <span>Get Started</span>
            </button>
          </Link>
          <a
            href="https://github.com/wasp-lang/wasp"
            target="_blank"
            rel="noreferrer"
          >
            <button className="inline-flex items-center space-x-2 rounded border border-neutral-500 px-3 py-2 text-sm leading-4 text-neutral-700 transition duration-200 ease-out hover:border-neutral-400 hover:text-neutral-400">
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
    </SectionContainer>
  );
};

export default VCFinalCTA;
