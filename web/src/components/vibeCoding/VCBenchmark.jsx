import Link from "@docusaurus/Link";
import { ArrowRight } from "react-feather";
import { VCSection } from "./vcWrappers";

const stats = [
  {
    value: "45%",
    label: "lower cost",
    detail: "$2.87 vs $5.17 per feature",
  },
  {
    value: "40%",
    label: "fewer tokens",
    detail: "in the codebase for AI to read, understand, and write",
  },
  {
    value: "31%",
    label: "fewer API calls",
    detail: "66 vs 96",
  },
];

const VCBenchmark = () => {
  return (
    <VCSection>
      <div className="mx-auto max-w-3xl text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          Get more done for{" "}
          <span className="underline decoration-yellow-500">less money.</span>
        </h2>
        <p className="text-neutral-500">
          Next.js vs. Wasp: same app, same prompt, same model. Big difference.
        </p>
      </div>

      <div className="mt-16 grid grid-cols-1 gap-6 md:grid-cols-3">
        {stats.map((stat, idx) => (
          <div
            key={idx}
            className="h-full rounded-none border border-t-2 border-neutral-300 border-t-yellow-500 bg-yellow-500/5 p-6 text-center"
          >
            <div className="font-mono text-3xl font-extrabold text-neutral-700">
              {stat.value}
            </div>
            <div className="mt-1 text-sm font-semibold text-neutral-700">
              {stat.label}
            </div>
            <div className="mt-2 text-sm text-neutral-500">{stat.detail}</div>
          </div>
        ))}
      </div>

      <div className="mt-8 text-center">
        <Link
          to="/blog/2026/03/26/nextjs-vs-wasp-40-percent-less-tokens-same-app"
          target="_blank"
        >
          <span className="group inline-flex items-center gap-1 text-sm text-neutral-500 hover:text-neutral-400">
            <span>Read the full article</span>
            <span className="text-yellow-600 transition-all group-hover:ml-0.5">
              <ArrowRight size={14} strokeWidth={2} />
            </span>
          </span>
        </Link>
      </div>
    </VCSection>
  );
};

export default VCBenchmark;
