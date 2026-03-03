import { Search, Shield, FileText } from "react-feather";
import SectionContainer from "../Layouts/SectionContainer";

const problemCards = [
  {
    Icon: Search,
    title: '"Where does this code go?"',
    description:
      "AI wastes turns debating architecture, file structure, and patterns with every new feature.",
  },
  {
    Icon: Shield,
    title: '"Let me set up auth first..."',
    description:
      "AI generates 500+ lines of boilerplate before writing a single line of your actual feature.",
  },
  {
    Icon: FileText,
    title: '"I need to understand your codebase"',
    description:
      "AI reads thousands of tokens of infrastructure code just to build context.",
  },
];

const VCProblem = () => {
  return (
    <SectionContainer>
      <div className="mx-auto max-w-3xl text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          AI coding tools are powerful.{" "}
          <span className="underline decoration-yellow-500">
            Your framework is holding them back.
          </span>
        </h2>
        <p className="text-neutral-500">
          When you ask your AI to "add a payment system," here's what actually
          happens with most frameworks:
        </p>
        <p className="mt-4 text-neutral-500">
          First, it spends several turns figuring out your project structure.
          Where does auth live? How are routes organized? What ORM are you using?
          How are client and server connected?
        </p>
        <p className="mt-4 text-neutral-500">
          Then it generates hundreds of lines of boilerplate — middleware, API
          routes, session handling, database migrations — just to set up the
          scaffolding.
        </p>
        <p className="mt-4 text-neutral-500">
          Only <em>then</em> does it write the actual business logic you asked
          for.
        </p>
        <p className="mt-4 text-neutral-500">
          The problem isn't your AI. It's that your framework forces it to make
          dozens of architectural decisions and write infrastructure code before
          it can do what you actually want.
        </p>
      </div>

      <div className="mt-16 grid grid-cols-1 gap-6 md:grid-cols-3">
        {problemCards.map((card, idx) => (
          <div
            key={idx}
            className="rounded-md border border-yellow-500/25 bg-yellow-500/5 p-6"
          >
            <div className="mb-3 inline-flex h-8 w-8 items-center justify-center rounded-md bg-neutral-700 text-yellow-500">
              <card.Icon size={20} />
            </div>
            <h3 className="mb-2 text-base font-semibold text-neutral-700">
              {card.title}
            </h3>
            <p className="text-neutral-500">{card.description}</p>
          </div>
        ))}
      </div>
    </SectionContainer>
  );
};

export default VCProblem;
