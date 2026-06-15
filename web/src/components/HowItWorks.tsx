import HowItWorksDiagram from "./HowItWorksDiagram";
import InlineCode from "./InlineCode";
import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const HowItWorks = () => (
  <SectionContainer>
    <SectionLabel text="how it works" />

    <p className="mb-6 max-w-2xl text-base leading-relaxed text-wasp-g7">
      You write your app as high-level config in{" "}
      <InlineCode>*.wasp.ts</InlineCode> files, alongside the usual React,
      Node, and Prisma code. Wasp compiles it into a full-stack app, ready to
      deploy anywhere.
    </p>

    <HowItWorksDiagram />

    <p className="mt-4 max-w-2xl text-sm leading-relaxed text-wasp-g6">
      You can read the generated code, but treat it like any compiler output.
      Change the source, not the output.
    </p>
  </SectionContainer>
);

export default HowItWorks;
