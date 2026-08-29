import Link from "@docusaurus/Link";

import { GitHub, MessageCircle } from "react-feather";
import CtaLink from "./CtaLink";
import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";
import TextLink from "./TextLink";
import useBrokenLinks from "@docusaurus/useBrokenLinks";

const DISCORD_URL = "https://discord.gg/rzdnErX";

const coreTeam = [
  { handle: "Martinsos", name: "Martin" },
  { handle: "matijasos", name: "Matija" },
  { handle: "sodic", name: "Sodic" },
  { handle: "infomiho", name: "Miho" },
  { handle: "vincanger", name: "Vince" },
  { handle: "cprecioso", name: "Carlos" },
  { handle: "FranjoMindek", name: "Franjo" },
  { handle: "Licto", name: "Licto" },
].sort((a, b) => a.handle.localeCompare(b.handle));

const WASPETEERS_ID = "waspeteers";

const Waspeteers = () => {
  // Register anchor so Docusaurus's broken-link checker sees it at build time.
  useBrokenLinks().collectAnchor(WASPETEERS_ID);

  return (
    <SectionContainer id={WASPETEERS_ID}>
      <SectionLabel text="waspeteers" />
      <h2 className="mb-8 font-mono text-2xl font-extrabold tracking-tight text-wasp-black lg:text-4xl">
        The team & the community
      </h2>

      <blockquote className="border-l-4 border-wasp-black py-2 pl-6">
        <p className="text-pretty font-mono text-base leading-relaxed text-wasp-g7 lg:text-lg">
          “Wasp community f****n rocks! I ask a question and I get genuinely
          useful information…{" "}
          <strong className="text-wasp-black">
            Community is the second best thing about Wasp, after Wasp itself.
          </strong>
          ”
        </p>
        <footer className="mt-4 font-mono text-sm font-bold text-wasp-black">
          Joe Slater
        </footer>
      </blockquote>

      <div className="mt-6 grid grid-cols-1 gap-3 lg:grid-cols-[260px_1fr]">
        <div className="flex flex-col border-2 border-wasp-black bg-wasp-yellow-light p-5 text-center">
          <div className="self-start font-mono text-sm font-bold text-wasp-black">
            // meet da boi
          </div>
          <div className="flex flex-1 items-center justify-center">
            <img
              src="/img/lp/daboi.webp"
              loading="lazy"
              alt="Da Boi, Wasp's mascot"
              className="w-40 max-w-full"
            />
          </div>
          <div className="font-mono text-xs text-wasp-g7">
            our resident <s>bee</s> wasp, hangs out on discord
          </div>
        </div>

        <div className="flex flex-col justify-center gap-4 p-5 lg:p-6">
          <p className="font-mono text-base leading-relaxed text-wasp-g6">
            The whole Wasp team is on{" "}
            <TextLink to={DISCORD_URL}>Discord</TextLink> every day, together with
            the rest of the Wasp community of builders and engineers (the
            Waspeteers!): answering questions, showing off what we're building,
            discussing web development, engineering, and life.
          </p>
          <p className="font-mono text-base leading-relaxed text-wasp-g6">
            All our development is done on our{" "}
            <TextLink to="https://github.com/wasp-lang/wasp" variant="purple">
              public GitHub repo
            </TextLink>
            , so you can also easily see what we are working on currently, what we
            are planning (most of the issues are our future plans!), and
            participate as you wish.
          </p>
          <p className="font-mono text-base leading-relaxed text-wasp-g6">
            We love hearing your feedback and ideas and figuring out things
            together, learning more about what you are building, so come join us!
          </p>
          <div className="flex flex-wrap gap-5">
            <CtaLink to={DISCORD_URL}>
              Join us on Discord <MessageCircle size={18} />
            </CtaLink>
            <CtaLink to="https://github.com/wasp-lang/wasp" variant="purple">
              Follow development on GitHub <GitHub size={18} />
            </CtaLink>
          </div>
        </div>
      </div>

      <div className="mt-8">
        <div className="mb-6 font-mono text-sm font-bold text-wasp-black">
          // the core team
        </div>
        <div className="flex flex-wrap justify-center gap-10">
          {coreTeam.map((member) => (
            <Link
              key={member.handle}
              to={`https://github.com/${member.handle}`}
              className="flex flex-col items-center gap-2 transition hover:-translate-y-0.5"
            >
              <img
                src={`https://github.com/${member.handle}.png?size=128`}
                loading="lazy"
                alt={member.name}
                width={64}
                height={64}
                className="block h-16 w-16 border-2 border-wasp-black object-cover"
              />
              <span className="font-mono text-xs text-wasp-g6">
                @{member.handle}
              </span>
            </Link>
          ))}
        </div>
      </div>
    </SectionContainer>
  );
};

export default Waspeteers;
