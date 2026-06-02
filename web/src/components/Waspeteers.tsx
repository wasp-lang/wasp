import Link from "@docusaurus/Link";

import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const DISCORD_URL = "https://discord.gg/rzdnErX";

const coreTeam = [
  { handle: "cprecioso", name: "Carlos" },
  { handle: "FranjoMindek", name: "Franjo" },
  { handle: "infomiho", name: "Miho" },
  { handle: "Licto", name: "Licto" },
  { handle: "Martinsos", name: "Martin" },
  { handle: "matijasos", name: "Matija" },
  { handle: "sodic", name: "Sodic" },
  { handle: "vincanger", name: "Vince" },
];

const Waspeteers = () => (
  <SectionContainer>
    <SectionLabel text="community" />
    <h2 className="mb-8 font-mono text-2xl font-extrabold tracking-tight text-wasp-black lg:text-4xl">
      The Waspeteers
    </h2>

    <blockquote className="border-2 border-wasp-black bg-wasp-yellow-light p-6 lg:p-8">
      <p className="text-pretty font-mono text-base leading-relaxed text-wasp-g7 lg:text-lg">
        "Wasp community f****n rocks! I ask a question and I get genuinely
        useful information…{" "}
        <strong className="text-wasp-black">
          Community is the second best thing about Wasp, after Wasp itself.
        </strong>
        "
      </p>
      <footer className="mt-4 flex items-center gap-2.5 font-mono text-sm font-bold text-wasp-black">
        <span className="flex h-8 w-8 items-center justify-center border-2 border-wasp-black bg-wasp-yellow text-xs">
          JS
        </span>
        Joe Slater
      </footer>
    </blockquote>

    <div className="mt-3 grid grid-cols-1 gap-3 lg:grid-cols-[260px_1fr]">
      <div className="flex flex-col items-center border-2 border-wasp-black bg-wasp-bg p-5 text-center">
        <img
          src="img/lp/daboi.webp"
          alt="Da Boi — Wasp's mascot"
          className="w-40 max-w-full"
        />
        <div className="mt-3 font-mono text-sm font-bold text-wasp-black">
          // meet da boi
        </div>
        <div className="mt-1 font-mono text-xs text-wasp-g5">
          our resident <s>bee</s> wasp, hangs out on discord
        </div>
      </div>

      <div className="flex flex-col justify-center gap-4 border-2 border-wasp-black bg-wasp-yellow-light p-5 lg:p-6">
        <p className="font-mono text-sm leading-relaxed text-wasp-g6">
          The whole Wasp team is on{" "}
          <Link
            to={DISCORD_URL}
            className="font-bold text-wasp-black underline"
          >
            Discord
          </Link>{" "}
          every day — answering questions, sharing the details about the daily
          work, discussing web development, engineering, and life, and just
          having a good time.
        </p>
        <p className="font-mono text-sm leading-relaxed text-wasp-g6">
          All our development is done on our{" "}
          <Link
            to="https://github.com/wasp-lang/wasp"
            className="font-bold text-wasp-black underline"
          >
            public GitHub repo
          </Link>
          , so you can also easily see what we are working on currently, what we
          are planning (most of the issues are our future plans!), and
          participate as you wish.
        </p>
        <p className="font-mono text-sm leading-relaxed text-wasp-g6">
          We love hearing your feedback and ideas and figuring out things
          together, learning more about what you are building, so come join us!
        </p>
        <Link
          to={DISCORD_URL}
          className="inline-flex w-fit items-center gap-2 border-2 border-wasp-black bg-wasp-yellow px-5 py-2 font-mono text-sm font-bold text-wasp-black transition duration-200 ease-out hover:bg-wasp-yellow-dark hover:text-wasp-black"
        >
          → Join us on Discord
        </Link>
      </div>
    </div>

    <div className="mt-8">
      <div className="mb-4 font-mono text-sm font-bold text-wasp-black">
        // the core team
      </div>
      <div className="flex flex-wrap gap-3">
        {coreTeam.map((member) => (
          <Link
            key={member.handle}
            to={`https://github.com/${member.handle}`}
            aria-label={member.name}
            className="block border-2 border-wasp-black transition hover:-translate-y-0.5"
          >
            <img
              src={`https://github.com/${member.handle}.png`}
              alt={member.name}
              width={64}
              height={64}
              className="block h-16 w-16 object-cover"
            />
          </Link>
        ))}
      </div>
    </div>
  </SectionContainer>
);

export default Waspeteers;
