import Link from "@docusaurus/Link";
import { Terminal } from "react-feather";
import { InstallBlock, VCSection } from "./vcVariant";

const steps = [
  {
    number: "1",
    title: "Install Wasp",
    command: "npm i -g @wasp.sh/wasp-cli@latest",
  },
  {
    number: "2",
    title: "Add agent skills",
    command: "npx skills add wasp-lang/wasp-agent-plugins",
  },
  {
    number: "3",
    title: "Describe what you want",
    command: "Create a new Todo app with email login and a Postgres db",
  },
];

const VCWorkflow = () => {
  const circleClass =
    "mx-auto mb-4 inline-flex h-10 w-10 items-center justify-center rounded-none bg-yellow-500 text-lg font-bold text-white";

  const codeClass =
    "mb-3 block rounded-none bg-neutral-100 border border-neutral-300 text-neutral-500 font-mono px-3 py-2 text-sm";

  const buttonClass =
    "inline-flex items-center space-x-2 rounded-none uppercase tracking-wider font-mono border border-yellow-500 bg-yellow-500 px-3 py-2 text-sm leading-4 text-white transition duration-200 ease-out hover:border-yellow-400 hover:bg-yellow-400";

  return (
    <VCSection>
      <div className="mx-auto max-w-3xl text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          Three <span className="underline decoration-yellow-500">easy</span>{" "}
          steps.
        </h2>
      </div>

      <div className="mt-16 grid grid-cols-1 gap-8 md:grid-cols-3">
        {steps.map((step) => (
          <div key={step.number} className="text-center">
            <div className={circleClass}>{step.number}</div>
            <h3 className="mb-2 text-lg font-semibold text-neutral-700">
              {step.title}
            </h3>
            {step.command && (
              <InstallBlock
                command={step.command}
                className="mb-3 block border-2 border-neutral-300"
              />
            )}
            <p className="text-neutral-500">{step.description}</p>
          </div>
        ))}
      </div>

      <div className="mt-12 text-center">
        <Link to="/docs/quick-start">
          <button className={buttonClass}>
            <Terminal size={16} />
            <span>Start Building</span>
          </button>
        </Link>
      </div>
    </VCSection>
  );
};

export default VCWorkflow;
