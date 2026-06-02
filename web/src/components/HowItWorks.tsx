import EditorDemo from "./EditorDemo";
import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const HowItWorks = () => (
  <SectionContainer>
    <SectionLabel text="how it works" />

    {/* Explanation */}
    <div className="space-y-4">
      <p className="text-base leading-relaxed text-wasp-g7">
        You write your app as high-level config in <code>*.wasp.ts</code> files,
        alongside the usual React, Node, and Prisma code.
      </p>
      <p className="text-base leading-relaxed text-wasp-g7">
        Wasp compiles it and generates the whole full-stack app for you — ready
        to deploy anywhere, because it's just code.
      </p>
      <p className="text-base leading-relaxed text-wasp-g7">
        You can read the generated code whenever you like, but treat it as any
        compiler output: you change the source, not the output.
      </p>
    </div>

    {/* Compilation diagram */}
    <div className="mt-3 border-2 border-wasp-black bg-wasp-bg p-4 lg:p-8">
      <img
        src="img/lp/how-it-works-diagram.webp"
        alt="Wasp compiles your spec into an intermediate representation, then generates the full-stack code (frontend, backend, database) and deploys it."
        className="mx-auto block w-full max-w-5xl"
      />
    </div>

    {/* Animated editor demo */}
    <div className="mt-3">
      <EditorDemo />
    </div>
  </SectionContainer>
);

export default HowItWorks;
