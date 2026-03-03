import Link from "@docusaurus/Link";
import { ArrowRight } from "react-feather";
import SectionContainer from "../Layouts/SectionContainer";

const stats = [
  {
    value: "45%",
    label: "lower cost",
    detail: "$2.87 vs $5.17 per feature",
  },
  {
    value: "43%",
    label: "fewer tokens",
    detail: "in the codebase for AI to read",
  },
  {
    value: "30%",
    label: "faster",
    detail: "2.6 min vs 3.7 min to implement",
  },
  {
    value: "Same",
    label: "output",
    detail: "AI wrote the same amount of code",
  },
];

const VCBenchmark = () => {
  return (
    <SectionContainer>
      <div className="mx-auto max-w-3xl text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          Don't take our word for it.{" "}
          <span className="underline decoration-yellow-500">
            We benchmarked it.
          </span>
        </h2>
        <p className="text-neutral-500">
          Same SaaS app. Same features. Same AI. One built with Next.js, one
          with Wasp.
        </p>
      </div>

      <div className="mt-16 grid grid-cols-2 gap-6 md:grid-cols-4">
        {stats.map((stat, idx) => (
          <div
            key={idx}
            className="rounded-md border border-yellow-500/25 bg-yellow-500/5 p-6 text-center"
          >
            <div className="text-3xl font-extrabold text-neutral-700">
              {stat.value}
            </div>
            <div className="mt-1 text-sm font-semibold text-neutral-700">
              {stat.label}
            </div>
            <div className="mt-2 text-sm text-neutral-500">{stat.detail}</div>
          </div>
        ))}
      </div>

      <div className="mx-auto mt-12 max-w-2xl rounded-md border border-yellow-500/25 bg-yellow-500/5 p-6">
        <p className="text-neutral-700">
          The AI wrote nearly identical code for both frameworks. The entire cost
          and speed difference comes from what the AI had to{" "}
          <em>read and understand</em> — not what it wrote. Fewer boilerplate
          files = faster, cheaper AI-assisted development.
        </p>
      </div>

      <div className="mt-8 text-center">
        <Link to="/blog">
          <span className="group inline-flex items-center gap-1 text-sm text-neutral-500 hover:text-neutral-400">
            <span>Read the full benchmark</span>
            <span className="text-yellow-600 transition-all group-hover:ml-0.5">
              <ArrowRight size={14} strokeWidth={2} />
            </span>
          </span>
        </Link>
      </div>
    </SectionContainer>
  );
};

export default VCBenchmark;
