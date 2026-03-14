import Link from "@docusaurus/Link";
import { Terminal } from "react-feather";
import SectionContainer from "../Layouts/SectionContainer";

const steps = [
  {
    number: "1",
    title: "Install Wasp",
    command: "npm i -g @wasp.sh/wasp-cli@latest",
    description: "One command. 30 seconds. Free and open-source.",
  },
  {
    number: "2",
    title: "Point your AI at the project",
    description:
      "Open in Cursor, fire up Claude Code, or use any AI assistant. The main.wasp.ts config gives it immediate, full-stack context.",
  },
  {
    number: "3",
    title: "Build features, not infrastructure",
    description:
      "Your AI understands the architecture from day one. Just describe what you want to build.",
  },
];

const VCWorkflow = () => {
  return (
    <SectionContainer>
      <div className="mx-auto max-w-3xl text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          Three steps.{" "}
          <span className="underline decoration-yellow-500">That's it.</span>
        </h2>
      </div>

      <div className="mt-16 grid grid-cols-1 gap-8 md:grid-cols-3">
        {steps.map((step) => (
          <div key={step.number} className="text-center">
            <div className="mx-auto mb-4 inline-flex h-10 w-10 items-center justify-center rounded-full bg-yellow-500 text-lg font-bold text-white">
              {step.number}
            </div>
            <h3 className="mb-2 text-lg font-semibold text-neutral-700">
              {step.title}
            </h3>
            {step.command && (
              <code className="mb-3 block rounded bg-neutral-100 px-3 py-2 text-sm text-neutral-600">
                {step.command}
              </code>
            )}
            <p className="text-neutral-500">{step.description}</p>
          </div>
        ))}
      </div>

      <div className="mt-12 text-center">
        <Link to="/docs/quick-start">
          <button className="inline-flex items-center space-x-2 rounded border border-yellow-500 bg-yellow-500 px-3 py-2 text-sm leading-4 text-white transition duration-200 ease-out hover:border-yellow-400 hover:bg-yellow-400">
            <Terminal size={16} />
            <span>Start Building</span>
          </button>
        </Link>
      </div>
    </SectionContainer>
  );
};

export default VCWorkflow;
