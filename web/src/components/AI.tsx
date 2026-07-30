import Link from "@docusaurus/Link";
import classNames from "classnames";
import { ReactNode } from "react";
import { Zap } from "react-feather";

import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const AI = () => (
  // scroll-mt offsets the sticky nav when jumping here via the #ai anchor.
  <SectionContainer id="ai" className="scroll-mt-24">
    <SectionLabel
      text="ai"
      bgColorClassName="bg-wasp-purple"
      textColorClassName="text-wasp-white"
    />

    <h2 className="mb-4 font-mono text-2xl font-extrabold tracking-tight text-wasp-black lg:text-4xl">
      Perfect for AI, by design
    </h2>
    <p className="max-w-2xl text-pretty font-mono text-sm leading-relaxed text-wasp-g6 lg:text-base">
      By making Wasp as easy as possible for humans (opinionated, high-level,
      batteries-included) we made it the ideal framework for AI too.
    </p>

    <div className="mt-8 grid grid-cols-1 gap-3 lg:mt-10 lg:grid-cols-2">
      <CliCard question="why AI loves Wasp" className="lg:row-span-2">
        <List>
          <Item>
            <Strong>Clear best practices and structure</Strong> keep AI's focus
            on what is important.
          </Item>
          <Item>
            <Strong>Less boilerplate, less code,</Strong> less space for bugs
            and security issues.
          </Item>
          <Item>
            <Strong>No "connection" points</Strong> where things break - Wasp
            glues everything together.
          </Item>
          <Item>
            <Strong>Less code = smaller context,</Strong> saving tokens and
            keeping your AI smart.
          </Item>
          <Item>
            <Strong>Batteries included</Strong> means bigger building blocks,
            keeping AI on point, codebase tidy, and reducing chance of mistakes.
          </Item>
        </List>
      </CliCard>

      <CliCard question="how does Wasp keep me in control">
        <List>
          <Item>
            <Strong>Wasp's spec files</Strong> give you a unique high-level
            overview of your app.
          </Item>
          <Item>
            <Strong>High-level code</Strong> keeps you in the loop even when AI
            does the heavy lifting.
          </Item>
        </List>
      </CliCard>

      <CliCard question="first-party AI tooling?">
        <List>
          <Item>
            <Strong>Rule files, skills, and plugins</Strong> crafted by the Wasp
            team and shaped by community experience, so you get the most out of
            your AI.
          </Item>
          <Item>
            <Strong>llms.txt</Strong> built directly from the docs and always up
            to date.
          </Item>
        </List>
      </CliCard>
    </div>

    <div className="mt-8 text-center">
      <Link
        to="/vibe-coding"
        className="inline-flex items-center gap-2 border-2 border-wasp-purple bg-wasp-white px-5 py-2 font-mono text-sm font-bold text-wasp-purple hover:bg-wasp-purple hover:text-wasp-white"
      >
        See how Wasp supercharges vibe coding <Zap size={14} />
      </Link>
    </div>
  </SectionContainer>
);

const CliCard = ({
  question,
  children,
  className,
}: {
  question: string;
  children: ReactNode;
  className?: string;
}) => (
  <article
    className={classNames(
      "bg-wasp-code-bg-purple-dark p-5 font-mono lg:p-6",
      className,
    )}
  >
    <p className="text-sm leading-relaxed">
      <span className="font-bold text-wasp-yellow">$</span>{" "}
      <span className="text-wasp-white">ask</span>{" "}
      <span className="text-wasp-purple-quite-light">"{question}"</span>
    </p>
    {children}
  </article>
);

const List = ({ children }: { children: ReactNode }) => (
  <ul className="mt-4 space-y-3">{children}</ul>
);

const Item = ({ children }: { children: ReactNode }) => (
  <li className="flex gap-2.5 text-sm leading-relaxed text-wasp-g3">
    <span
      aria-hidden="true"
      className="select-none font-bold text-wasp-purple-somewhat-light"
    >
      ›
    </span>
    <span className="text-pretty">{children}</span>
  </li>
);

const Strong = ({ children }: { children: ReactNode }) => (
  <strong className="font-bold text-wasp-white">{children}</strong>
);

export default AI;
