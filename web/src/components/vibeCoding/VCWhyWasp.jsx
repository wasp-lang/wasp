import { Layout, Eye, Code, Layers } from "react-feather";
import SectionContainer from "../Layouts/SectionContainer";

const benefits = [
  {
    Icon: Layout,
    title: "Architecture is already decided",
    description:
      "Wasp is a full-stack, opinionated framework. When your AI needs to add a feature, it doesn't negotiate patterns or debate file structure. It already knows where client code goes, how server operations work, and how data flows. It just writes the logic.",
  },
  {
    Icon: Eye,
    title: "Instant full-app understanding",
    description:
      "The main.wasp.ts config is a complete blueprint of your app. Your AI reads it and immediately understands your auth setup, all your routes, every data model, and how operations connect. No spelunking through 47 files to build context.",
  },
  {
    Icon: Code,
    title: "AI writes features, not plumbing",
    description:
      "Auth? Declared in config, not coded. CRUD operations? Auto-generated from your data model. Email? Configured, not built. Deployment? One CLI command. Your AI spends its time on what makes your app unique — the actual business logic.",
  },
  {
    Icon: Layers,
    title: "Stays coherent as complexity grows",
    description:
      "With other frameworks, AI-generated code gets harder to review as your app grows. Wasp's declarative config enforces consistent architecture — so the 50th feature follows the same patterns as the first. Easier for AI to generate, easier for you to review and validate.",
  },
];

const VCWhyWasp = () => {
  return (
    <SectionContainer id="how-wasp-works">
      <div className="mx-auto max-w-3xl text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          Declarative config. Zero architectural debates.{" "}
          <span className="underline decoration-yellow-500">
            All business logic.
          </span>
        </h2>
        <p className="text-neutral-500">
          Wasp takes a fundamentally different approach. Instead of generating
          boilerplate, you <em>declare</em> your app's architecture in TypeScript
          config files (<code className="text-neutral-600">main.wasp.ts</code>).
          Auth, routes, data models, server operations, background jobs —
          described declaratively with full IDE support and TypeScript
          autocomplete.
        </p>
        <p className="mt-4 text-neutral-500">
          As your app grows, split config across multiple files. The declarative
          structure stays coherent. This changes everything about how AI
          interacts with your codebase:
        </p>
      </div>

      <div className="mt-16 grid grid-cols-1 gap-8 md:grid-cols-2">
        {benefits.map((benefit, idx) => (
          <div key={idx} className="rounded-md border border-yellow-500/25 bg-yellow-500/5 p-6">
            <div className="mb-4 inline-flex h-10 w-10 items-center justify-center rounded-md bg-neutral-700 text-yellow-500">
              <benefit.Icon size={24} />
            </div>
            <h3 className="mb-2 text-lg font-semibold text-neutral-700">
              {benefit.title}
            </h3>
            <p className="text-neutral-500">{benefit.description}</p>
          </div>
        ))}
      </div>
    </SectionContainer>
  );
};

export default VCWhyWasp;
