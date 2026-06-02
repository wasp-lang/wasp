import { ReactNode } from "react";

import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const comments = [
  "you write your code as high level config in *.wasp.ts files + the usual (react, node, prisma, ...)",
  "wasp then compiles it and generates the whole full stack app for you, that you can deploy anywhere, it's just code",
  "you can look at the generated code if you wish, but you should treat it as any compiler output -> you change the source, not the output",
];

const HowItWorks = () => (
  <SectionContainer>
    <SectionLabel text="how it works" />

    {/* Browser/terminal demo placeholder */}
    <figure className="overflow-hidden border-2 border-wasp-black">
      <div className="flex items-center gap-2 border-b-2 border-wasp-black bg-wasp-g7 px-4 py-2.5">
        <span className="h-3 w-3 rounded-full bg-wasp-g5" />
        <span className="h-3 w-3 rounded-full bg-wasp-g5" />
        <span className="h-3 w-3 rounded-full bg-wasp-g5" />
        <span className="ml-3 truncate font-mono text-xs text-wasp-g3">
          wasp new my-app &nbsp;·&nbsp; localhost:3000 &nbsp;·&nbsp; wasp deploy
          fly
        </span>
      </div>
      <div className="bg-wasp-black px-5 py-10 font-mono text-sm lg:py-16">
        <div className="mx-auto max-w-xl">
          <div>
            <span className="text-wasp-yellow">$</span>{" "}
            <span className="text-wasp-white">wasp new</span>
          </div>
          <div className="mt-2 flex items-center gap-1 text-wasp-g4">
            RECORDING COMING SOON
            <span
              aria-hidden="true"
              className="inline-block h-4 w-2 animate-pulse bg-wasp-yellow"
            />
          </div>
          <div className="mt-2 text-wasp-g5">
            // build → run → deploy, all from your terminal
          </div>
        </div>
      </div>
    </figure>

    {/* Explanation as code comments */}
    <div className="mt-3 space-y-2 border-2 border-wasp-black bg-wasp-bg p-5 lg:p-6">
      {comments.map((comment, i) => (
        <Comment key={i}>{comment}</Comment>
      ))}
    </div>

    {/* Compilation diagram */}
    <div className="mt-3 border-2 border-wasp-black bg-wasp-bg p-4 lg:p-8">
      <img
        src="img/lp/how-it-works-diagram.webp"
        alt="Wasp compiles your spec into an intermediate representation, then generates the full-stack code (frontend, backend, database) and deploys it."
        className="mx-auto block w-full max-w-5xl"
      />
    </div>
  </SectionContainer>
);

const Comment = ({ children }: { children: ReactNode }) => (
  <p className="font-mono text-sm leading-relaxed text-wasp-g5">
    <span className="text-wasp-g4">// </span>
    {children}
  </p>
);

export default HowItWorks;
