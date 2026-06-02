import Link from "@docusaurus/Link";
import useBrokenLinks from "@docusaurus/useBrokenLinks";
import { ReactNode, useState } from "react";
import { Minus, Plus } from "react-feather";

import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const FAQ_ID = "faq";
const DISCORD_URL = "https://discord.gg/rzdnErX";

const Code = ({ children }: { children: ReactNode }) => (
  <code className="bg-wasp-yellow-light px-1 py-0.5 font-mono text-[0.95em] text-wasp-black">
    {children}
  </code>
);

interface FaqItemData {
  question: string;
  answer: ReactNode;
}

const faqs: FaqItemData[] = [
  {
    question: "Who is Wasp for?",
    answer: (
      <p>
        Wasp is for developers building full-stack web apps who want to move
        fast without wiring all of it together by hand. Instead of assembling
        and maintaining your own stack, you lean on the choices and best
        practices Wasp bakes in, and spend your time on what actually makes your
        product yours.
      </p>
    ),
  },
  {
    question: "How is Wasp different from Next.js / TanStack Start / ... ?",
    answer: (
      <>
        <p>
          Wasp doesn't conceptually start from frontend and extend to
          (serverless) backend, but instead arrives from the truly full-stack
          position, capturing frontend, backend, database, deployment, all in
          one tight package.
        </p>
        <p>
          Wasp is more opinionated, which allows it to go further in the scope
          of the value it aims to provide, if you are ok sticking with these
          pre-made choices.
        </p>
      </>
    ),
  },
  {
    question: "How does Wasp compare to Rails / Laravel / ... ?",
    answer: (
      <>
        <p>
          Same as them, Wasp is an opinionated, full-stack, battery-included
          framework, and in many ways, Wasp is aiming for the same fully managed
          experience, but more in the spirit of the modern JS/TS ecosystem.
        </p>
        <p>
          Unlike them, Wasp's config is not embedded into the host language
          (Ruby, PHP, ...), but is instead a separate, powerful specification
          layer (<Code>*.wasp.ts</Code> files) that is agnostic from the
          underlying tech stack (React, NodeJS, ...). This means Wasp can
          support that same config in different languages, support different
          backend languages, can compile it and reason about it upfront, you can
          configure it in a turing complete language, ... .
        </p>
      </>
    ),
  },
  {
    question: "Which tech stack does Wasp use?",
    answer: (
      <>
        <p>
          Wasp uses React, Node.js (Express) and Prisma as the primary tech
          stack.
        </p>
        <p>
          That said, Wasp is built as a tech-agnostic framework at its core. Its
          config layer (<Code>*.wasp.ts</Code>) is standalone and designed to be
          combineable with different code generators in the future, that go
          beyond the primary tech stack we are focusing on right now: e.g.
          Drizzle next to Prisma, Vue or Svelte next to React, ... .
        </p>
      </>
    ),
  },
  {
    question: "Can I use X in Wasp?",
    answer: (
      <>
        <p>
          If <Code>X</Code> is a library in the JS ecosystem, then answer is
          almost always "yes".
        </p>
        <p>
          If <Code>X</Code> is something you would like to use in the place of
          the primary part of the Wasp's stack (React, Node.js, Prisma, Wasp's
          Auth, Wasp's jobs), and not next to it, then the answer is usually
          "not yet".
        </p>
        <p>
          Why "Not yet", and not "no"? Because Wasp is built as a tech-agnostic
          framework, and supporting multiple frontend frameworks / database
          layers / backend languages / ... is a big part of our vision, but at
          this moment focus is on refining the design on a single stack.
        </p>
      </>
    ),
  },
  {
    question: "Is Wasp production-ready?",
    answer: (
      <>
        <p>
          Wasp is still in Beta, but has already seen a lot of usage in
          production (in indie projects, startups, big corporations).
        </p>
        <p>
          The reason we are still sticking to the "Beta" label is to allow us
          faster iteration with some of the critical Wasp parts, but that said,
          we are already honoring many of the promises expected from the stable
          version.
        </p>
        <p>
          We are moving fast toward 1.0, which is the point when we will
          officially proclaim it to be "production ready".
        </p>
      </>
    ),
  },
  {
    question: "What about performance / scaling?",
    answer: (
      <p>
        There are many aspects to performance and scaling, and we will be
        covering these in much more detail in our docs as we get closer to 1.0,
        but in its essence, due to heavily relying on code generation, when you
        deploy a Wasp app, you are really deploying just an app in Wasp's tech
        stack (React / Node.js / Prisma), and most properties are inherited
        directly.
      </p>
    ),
  },
  {
    question: "Is there vendor lock-in?",
    answer: (
      <>
        <p>
          No! Wasp is not a BaaS, there is no runtime that needs to be deployed
          with Wasp, or a platform to which Wasp needs to be deployed.
        </p>
        <p>
          Wasp app compiles to its primary tech stack (React / Node.js / Prisma)
          and can therefore be deployed to any hosting solution. We do support
          "single click" deployments to Fly and Railway, but those are merely
          here for your convenience, and we will be adding more.
        </p>
        <p>
          Wasp also brings in-house features like Auth and Jobs, meaning you
          don't have to reach out for external providers unless/until you wish
          to.
        </p>
      </>
    ),
  },
  {
    question: "How does Wasp earn money?",
    answer: (
      <>
        <p>
          Wasp is VC funded by carefully picked investors that understand and
          support us in our vision.
        </p>
        <p>
          This gives us unique freedom in pursuing the ambitious vision we have
          for Wasp with all he focus it deserves while also staying true to our
          values.
        </p>
        <p>
          At the moment we are not yet trying to earn any money, but we have a
          clear vision how to get there. Wasp framework will always stay
          open-source and free, while we plan to offer additional paid services
          on top.
        </p>
      </>
    ),
  },
  {
    question: "My question is not here",
    answer: (
      <p>
        In that case, come to our{" "}
        <Link to={DISCORD_URL} className="font-bold text-wasp-black underline">
          Discord
        </Link>{" "}
        and shoot us a question! Whole team is there and we would love to hear
        from you.
      </p>
    ),
  },
];

const Faq = () => {
  useBrokenLinks().collectAnchor(FAQ_ID);

  return (
    <SectionContainer id={FAQ_ID}>
      <SectionLabel text="faq" />
      <h2 className="mb-8 font-mono text-2xl font-extrabold tracking-tight text-wasp-black lg:text-4xl">
        Frequently asked questions
      </h2>

      <div className="mx-auto max-w-3xl space-y-3">
        {faqs.map((faq, idx) => (
          <FaqItem key={idx} faq={faq} />
        ))}
      </div>
    </SectionContainer>
  );
};

const FaqItem = ({ faq }: { faq: FaqItemData }) => {
  const [isOpen, setIsOpen] = useState(false);

  return (
    <div className="border-2 border-wasp-black bg-wasp-bg">
      <button
        type="button"
        onClick={() => setIsOpen((v) => !v)}
        aria-expanded={isOpen}
        className="flex w-full items-center justify-between gap-4 px-5 py-4 text-left font-mono text-sm font-bold text-wasp-black"
      >
        <span>{faq.question}</span>
        <span className="shrink-0 text-wasp-yellow-dark" aria-hidden="true">
          {isOpen ? <Minus size={18} /> : <Plus size={18} />}
        </span>
      </button>
      {isOpen && (
        <div className="space-y-3 border-t-2 border-wasp-black/15 px-5 py-4 font-mono text-sm leading-relaxed text-wasp-g6">
          {faq.answer}
        </div>
      )}
    </div>
  );
};

export default Faq;
