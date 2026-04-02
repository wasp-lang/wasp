import Link from "@docusaurus/Link";
import { ArrowRight } from "react-feather";
import { VCSection } from "./vcWrappers";

const stats = [
  {
    value: "45%",
    label: "lower cost",
    detail: "per feature",
    wasp: 2.87,
    other: 5.17,
    prefix: "$",
  },
  {
    value: "40%",
    label: "fewer tokens",
    detail: "for AI to read & write",
    wasp: 2.5,
    other: 4.0,
    suffix: "M",
  },
  {
    value: "31%",
    label: "fewer API calls",
    detail: "to build the same feature",
    wasp: 66,
    other: 96,
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
        {stats.map((stat, idx) => {
          const maxVal = Math.max(stat.wasp, stat.other);
          const otherPct = (stat.other / maxVal) * 100;
          const waspPct = (stat.wasp / maxVal) * 100;
          const formatVal = (v) =>
            `${stat.prefix || ""}${v}${stat.suffix || ""}`;

          return (
            <div
              key={idx}
              className="h-full rounded-none border border-t-2 border-neutral-300 border-t-yellow-500 bg-white p-6"
            >
              <div className="text-center">
                <div className="font-mono text-6xl font-extrabold text-neutral-700">
                  {stat.value}
                </div>
                <div className="mt-1 text-lg font-semibold text-neutral-700">
                  {stat.label}
                </div>
                <div className="mt-1 text-xs text-neutral-400">
                  {stat.detail}
                </div>
              </div>

              <div>
                <div className="mb-1 flex items-center justify-between text-xs">
                  <span className="font-medium text-neutral-700">Wasp</span>
                  <span className="font-mono font-medium text-neutral-700">
                    {formatVal(stat.wasp)}
                  </span>
                </div>
                <div className="h-2.5 w-full rounded-full bg-neutral-100">
                  <div
                    className="h-2.5 rounded-full bg-yellow-400"
                    style={{ width: `${waspPct}%` }}
                  />
                </div>
              </div>

              <div className="mt-5 space-y-2.5">
                <div>
                  <div className="mb-1 flex items-center justify-between text-xs">
                    <span className="text-neutral-400">Next.js</span>
                    <span className="font-mono text-neutral-400">
                      {formatVal(stat.other)}
                    </span>
                  </div>
                  <div className="h-2.5 w-full rounded-full bg-neutral-100">
                    <div
                      className="h-2.5 rounded-full bg-neutral-300"
                      style={{ width: `${otherPct}%` }}
                    />
                  </div>
                </div>
              </div>
            </div>
          );
        })}
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
