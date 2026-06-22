import Link from "@docusaurus/Link";
import classNames from "classnames";
import { ReactNode } from "react";
import { BookOpen } from "react-feather";

import InlineCode from "./InlineCode";
import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const Philosophy = () => {
  return (
    <SectionContainer>
      <SectionLabel text="our philosophy" />
      <h2 className="mb-8 font-mono text-2xl font-extrabold tracking-tight text-wasp-black lg:text-4xl">
        The Way of the Wasp
      </h2>

      <div className="grid grid-cols-1 gap-3 lg:grid-cols-12">
        <BentoCard
          title={'"Onion" architecture'}
          diagram={<OnionRingsDiagram />}
          diagramWidth="max-w-[200px] sm:w-2/5"
          className="lg:col-span-6 lg:row-start-1"
        >
          <p>
            We aim to make every surface of Wasp a layer of APIs: highest level
            covers 80% of the use cases, and then you can peel layer after layer
            as you need more, trading simplicity for flexibility.
          </p>
          <p>
            Rely on curated best practice solutions by default, assume more
            control where/when you need it.
          </p>
        </BentoCard>

        <BentoCard
          title="Truly full-stack"
          className="lg:col-span-6 lg:row-start-1"
        >
          <p>
            In Wasp, full-stack is not an after-thought, it is the initial and
            central point of our design since inception of Wasp as an idea.
          </p>
          <p>
            The very way Wasp is designed, with unique specification layer to
            capture the full-stack logic, and with code generation focused
            approach in the background, is to allow us to capture all of what a
            web app is into one solution, to take the definition of "full-stack"
            as far as it goes.
          </p>
        </BentoCard>

        <BentoCard
          accent
          title="A powerful, unique full-stack specification layer"
          diagram={<SpecLayerDiagram />}
          diagramWidth="max-w-[190px] sm:w-2/5"
          diagramSide="right"
          className="lg:col-span-full lg:row-start-2"
        >
          <p>
            Wasp stands for{" "}
            <strong className="font-bold text-wasp-black">
              Web App SPecification
            </strong>
            : you describe the full-stack behaviour (specification) of your app
            in <InlineCode>*.wasp.ts</InlineCode> files, at the level of your
            thoughts (<InlineCode>route</InlineCode>,{" "}
            <InlineCode>auth</InlineCode>, <InlineCode>job</InlineCode>, ...),
            then implement the details in the underlying stack (React, Node,
            Prisma, ...).
          </p>
          <p>
            Every full-stack framework has a way to be configured, but it is
            usually a mix of file conventions, naming rules, mini-DSLs, and JSON
            files. Wasp pulls all of that into a single cohesive SDK with the
            full power of TypeScript, making the specification a first-class
            citizen, and giving Wasp its superpowers.
          </p>
        </BentoCard>

        <BentoCard
          title="Greatest > latest"
          className="lg:col-span-5 lg:row-start-3"
        >
          <p>
            We are not chasing the bleeding edge. We critically track and curate
            the latest web dev trends to provide you with a deeply integrated
            and cohesive experience that we stand behind.
          </p>
        </BentoCard>

        <BentoCard
          title="Managed experience over DIY"
          diagram={<CubeBlockDiagram />}
          diagramWidth="max-w-[160px] sm:w-2/5"
          className="lg:col-span-7 lg:row-start-3"
        >
          <p>
            With Wasp, you don't build your framework up, you build it down: you
            start with everything working out of the box, then take more control
            where you need it. You start with big building blocks, then replace
            them with smaller ones and your own logic as needed.
          </p>
        </BentoCard>
      </div>

      {/* Hidden until the actual Wasp Manifesto is written. */}
      <div className="mt-8 hidden text-center">
        <Link
          to="#"
          className={classNames(
            "inline-flex items-center gap-2",
            "border-2 border-wasp-black px-5 py-2",
            "font-mono text-sm font-bold",
            "bg-wasp-yellow text-wasp-black",
            "hover:bg-wasp-yellow-dark hover:text-wasp-black",
          )}
        >
          Read the full Wasp Manifesto
          <BookOpen size={14} strokeWidth={1.75} aria-hidden="true" />
        </Link>
      </div>
    </SectionContainer>
  );
};

const BentoCard = ({
  title,
  children,
  className,
  accent = false,
  diagram,
  diagramWidth,
  diagramSide = "left",
}: {
  title: string;
  children: ReactNode;
  className?: string;
  accent?: boolean;
  diagram?: ReactNode;
  diagramWidth?: string;
  diagramSide?: "left" | "right";
}) => {
  const diagramColumn = diagram && (
    <div
      className={classNames(
        "order-first mx-auto w-full",
        "sm:order-none sm:mx-0 sm:flex-none sm:self-center",
        diagramWidth,
      )}
    >
      {diagram}
    </div>
  );
  const { diagramLeft, diagramRight } = {
    left: { diagramLeft: diagramColumn, diagramRight: null },
    right: { diagramLeft: null, diagramRight: diagramColumn },
  }[diagramSide];

  return (
    <article
      className={classNames(
        "border-2 p-6",
        accent ? "bg-wasp-yellow-light border-wasp-black" : "bg-wasp-white border-wasp-g3",
        className,
      )}
    >
      <div className="flex flex-col gap-5 sm:h-full sm:flex-row sm:items-start">
        {diagramLeft}
        <div className="min-w-0 flex-1">
          <h4 className="font-mono text-base font-extrabold leading-tight text-wasp-black lg:text-lg">
            {title}
          </h4>
          <div
            className={classNames(
              "mt-2.5 space-y-2.5 font-mono text-sm leading-relaxed",
              accent ? "text-wasp-black" : "text-wasp-g6",
            )}
          >
            {children}
          </div>
        </div>
        {diagramRight}
      </div>
    </article>
  );
};

const OnionRingsDiagram = () => {
  const rd = 10;
  const pad = 5;
  const circles = [
    // From outer to inner.
    { fill: "fill-wasp-yellow" },
    { fill: "fill-wasp-yellow-somewhat-light" },
    { fill: "fill-wasp-yellow-quite-light" },
    { fill: "fill-wasp-yellow-light" },
  ];
  const vbSize = (rd * circles.length + pad) * 2;
  return (
    <svg
      viewBox={`0 0 ${vbSize} ${vbSize}`}
      role="img"
      aria-label="Onion architecture: concentric layers"
      className="block h-auto w-full"
    >
      {circles.map((c, i) => (
        <circle
          key={i}
          cx={vbSize / 2}
          cy={vbSize / 2}
          r={(circles.length - i) * rd}
          strokeWidth="1.0"
          className={classNames(c.fill, "stroke-wasp-black")}
        />
      ))}
    </svg>
  );
};

const CubeBlockDiagram = () => (
  <svg
    viewBox="16 12 116 114"
    role="img"
    aria-label="Managed experience: a building block"
    className="block h-auto w-full"
  >
    {[
      "M72 60 L22 36 L76 18 L126 42 Z", // top
      "M72 60 L22 36 L22 96 L72 120 Z", // left
      "M72 60 L126 42 L126 102 L72 120 Z", // right
    ].map((d) => (
      <path
        key={d}
        d={d}
        strokeWidth="1.8"
        strokeLinejoin="round"
        className="fill-wasp-yellow stroke-wasp-black"
      />
    ))}
  </svg>
);

const SpecLayerDiagram = () => {
  const rectCount = 4;
  const rectW = 150;
  const rectH = 35;
  const pad = 5;
  const rectDist = 3;
  const vbWidth = rectW + pad * 2;
  const vbHeight = rectCount * rectH + (rectCount - 1) * rectDist + pad * 2;
  return (
    <svg
      viewBox={`0 0 ${vbWidth} ${vbHeight}`}
      role="img"
      aria-label="The specification layer sits on top of the stack"
      className="block h-auto w-full"
    >
      {Array.from({ length: rectCount }, (_, i) => (
        <rect
          key={i}
          x={pad}
          y={pad + i * (rectH + rectDist)}
          width={rectW}
          height={rectH}
          strokeWidth="2.0"
          className={classNames(
            "stroke-wasp-black",
            i === 0 ? "fill-wasp-yellow" : "fill-none",
          )}
        />
      ))}
    </svg>
  );
};

export default Philosophy;
