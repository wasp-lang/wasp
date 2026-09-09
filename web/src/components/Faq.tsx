import Link from "@docusaurus/Link";
import useBrokenLinks from "@docusaurus/useBrokenLinks";
import { ReactNode, useEffect, useState } from "react";
import { Minus, Plus } from "react-feather";

import InlineCode from "./InlineCode";
import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const FAQ_ID = "faq";
const DISCORD_URL = "https://discord.gg/rzdnErX";
const CAREERS_URL =
  "https://wasp-lang.notion.site/Wasp-Careers-59fd1682c80d446f92be5fa65cc17672";

const FaqLink = ({ to, children }: { to: string; children: ReactNode }) => (
  <Link to={to} className="font-bold text-wasp-black underline">
    {children}
  </Link>
);

const slugify = (text: string) =>
  text
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");

interface FaqItemData {
  question: string;
  answer: ReactNode;
}

interface FaqGroupData {
  title: string;
  items: FaqItemData[];
}

const waspFaqs: FaqItemData[] = [
  {
    question: "Who is Wasp for?",
    answer: (
      <p>
        Wasp is for developers building full-stack web apps who want to move
        fast without wiring it all together by hand (either theirs or their
        agent's). Instead of assembling and maintaining your own stack, lean on
        the choices and best practices Wasp bakes in, and spend your time on
        what actually makes your product yours.
      </p>
    ),
  },
  {
    question: "How is Wasp different from Next.js / TanStack Start / ... ?",
    answer: (
      <>
        <p>
          Wasp doesn't conceptually start from the frontend and extend to
          (serverless) backend, but instead starts from a truly full-stack
          position, capturing frontend, backend, database, deployment, all in
          one tight package.
        </p>
        <p>
          Wasp is more opinionated, which allows it to go further in the scope
          of the value it aims to provide, if you are willing to stick with
          these pre-made choices.
        </p>
      </>
    ),
  },
  {
    question: "How does Wasp compare to Rails / Laravel / ... ?",
    answer: (
      <>
        <p>
          Like them, Wasp is an opinionated, full-stack, batteries-included
          framework. It aims for the same fully managed experience, but in the
          spirit of the modern JS/TS ecosystem.
        </p>
        <p>
          Unlike them, Wasp doesn't embed its config into the host language
          (Ruby, PHP, ...). Instead, it lives in its own specification layer:{" "}
          <InlineCode>*.wasp.ts</InlineCode> files, agnostic to the underlying
          tech stack.
        </p>
        <p>
          This gives Wasp a lot of leverage. The spec is written in a real
          programming language (TypeScript), and Wasp can compile it and reason
          about your whole app upfront. Since the spec is decoupled from the
          stack, Wasp can later support other frontend frameworks and backend
          languages.
        </p>
      </>
    ),
  },
  {
    question: "Which tech stack does Wasp use?",
    answer: (
      <>
        <p>Wasp uses React, Node.js (Express) and Prisma.</p>
        <p>
          At its core, though, Wasp is tech-agnostic. The config layer (
          <InlineCode>*.wasp.ts</InlineCode>) is standalone and can be paired
          with different code generators in the future: Drizzle next to Prisma,
          Vue or Svelte next to React, ...
        </p>
      </>
    ),
  },
  {
    question: "Can I use X/Y/Z in Wasp?",
    answer: (
      <>
        <p>
          If <InlineCode>X</InlineCode> is a library from the JS ecosystem, the
          answer is almost always "yes".
        </p>
        <p>
          If <InlineCode>X</InlineCode> would replace a core part of Wasp's
          stack (React, Node.js, Prisma, Wasp's Auth or jobs) rather than sit
          next to it, the answer is usually "not yet".
        </p>
        <p>
          Why "not yet" and not "no"? Supporting multiple frontend frameworks,
          databases and backend languages is a big part of our vision. Right
          now, though, we are focused on refining the design on a single stack.
        </p>
      </>
    ),
  },
  {
    question: "Is Wasp production-ready?",
    answer: (
      <>
        <p>
          Wasp is still in Beta, but it is already used in production by indie
          hackers, startups and big corporations.
        </p>
        <p>
          We keep the "Beta" label because it lets us iterate faster on some
          critical parts of the framework. That said, we already honor many of
          the promises expected from a stable version.
        </p>
        <p>
          We are moving fast toward 1.0. That is when we will officially call
          Wasp production-ready.
        </p>
      </>
    ),
  },
  {
    question: "What about performance / scaling?",
    answer: (
      <>
        <p>
          Since Wasp relies on code generation, a deployed Wasp app is just an
          app in its tech stack (React, Node.js, Prisma). It performs and scales
          the same way any such app does - there is no Wasp runtime on top.
        </p>
        <p>
          We will cover this in more detail in our docs as we get closer to 1.0.
        </p>
      </>
    ),
  },
  {
    question: "Is there vendor lock-in?",
    answer: (
      <>
        <p>
          No! Wasp is not a BaaS. There is no runtime that ships with your app,
          and no platform you have to deploy to.
        </p>
        <p>
          A Wasp app compiles to its tech stack (React, Node.js, Prisma) and can
          be deployed to any hosting solution. Deploying to Fly or Railway takes
          a single CLI command, but that is just a convenience, with more
          providers to come.
        </p>
        <p>
          Wasp also brings features like Auth and Jobs in-house, so you don't
          need external providers unless you want them.
        </p>
      </>
    ),
  },
];

const companyFaqs: FaqItemData[] = [
  {
    question: "Are you hiring?",
    answer: (
      <p>
        Open positions are listed on our{" "}
        <FaqLink to={CAREERS_URL}>careers page</FaqLink>. If there is nothing
        there at the moment, you can still come say hi on{" "}
        <FaqLink to={DISCORD_URL}>Discord</FaqLink> - we love meeting people
        excited about Wasp!
      </p>
    ),
  },
  {
    question: "Is Wasp a for-profit company?",
    answer: (
      <>
        <p>
          Yes! Behind the framework is Wasp, Inc. - a company backed by Y
          Combinator and other carefully picked investors who understand and
          support our vision.
        </p>
        <p>
          Being a company is what lets us have a full-time team working on Wasp,
          while staying true to our values.
        </p>
      </>
    ),
  },
  {
    question: "How does Wasp earn money?",
    answer: (
      <>
        <p>
          Right now, it doesn't - we are not yet trying to earn money, and our
          funding lets us focus fully on making Wasp great.
        </p>
        <p>
          The framework will always be open-source and free. The plan is to earn
          by offering paid services on top of it.
        </p>
      </>
    ),
  },
];

const faqGroups: FaqGroupData[] = [
  { title: "Wasp", items: waspFaqs },
  { title: "Company", items: companyFaqs },
];

const Faq = () => {
  useBrokenLinks().collectAnchor(FAQ_ID);

  return (
    <SectionContainer id={FAQ_ID}>
      <SectionLabel text="faq" />
      <div className="grid gap-8 lg:grid-cols-3 lg:gap-16">
        {/* Nudges the title up a bit so its visible top lines up with the
            "Wasp" group label on the right. Text has a few pixels of built-in
            whitespace above it (from line-height), so without this the title
            would look slightly lower than the label. */}
        <div className="lg:-mt-2">
          <h2 className="font-mono text-2xl font-extrabold tracking-tight text-wasp-black lg:text-4xl">
            Frequently asked questions
          </h2>
          <p className="mt-4 font-mono text-sm leading-relaxed text-wasp-g6">
            If your question is not covered here, come ask us on{" "}
            <FaqLink to={DISCORD_URL}>Discord</FaqLink> - the whole team is
            there!
          </p>
        </div>

        <div className="space-y-10 lg:col-span-2">
          {faqGroups.map((group) => (
            <div key={group.title}>
              <h3 className="mb-4 font-mono text-lg font-extrabold uppercase tracking-wide text-wasp-black lg:text-xl">
                {group.title}
              </h3>
              <div className="border-t-2 border-wasp-black">
                {group.items.map((faq) => (
                  <FaqItem key={faq.question} faq={faq} />
                ))}
              </div>
            </div>
          ))}
        </div>
      </div>
    </SectionContainer>
  );
};

const FaqItem = ({ faq }: { faq: FaqItemData }) => {
  const anchorId = `faq-${slugify(faq.question)}`;
  const buttonId = `${anchorId}-button`;
  const panelId = `${anchorId}-panel`;
  useBrokenLinks().collectAnchor(anchorId);

  const [isOpen, setIsOpen] = useState(false);

  // Open the answer when this question's anchor is visited, both on page
  // load and on later in-page hash changes.
  useEffect(() => {
    const openIfTargeted = () => {
      if (window.location.hash === `#${anchorId}`) {
        setIsOpen(true);
        // Also scroll to the question ourselves: the browser's own anchor
        // scroll can run before React renders this element and miss it.
        document.getElementById(anchorId)?.scrollIntoView();
      }
    };
    openIfTargeted();
    window.addEventListener("hashchange", openIfTargeted);
    return () => window.removeEventListener("hashchange", openIfTargeted);
  }, [anchorId]);

  // Reflect the open question in the URL. replaceState (instead of setting
  // location.hash) avoids scroll jumps and back-button history entries.
  const toggle = () => {
    // setIsOpen doesn't update isOpen within this function run (state changes
    // only apply on the next render), so to know which way we are toggling we
    // have to compute the upcoming value ourselves.
    const isAboutToOpen = !isOpen;
    setIsOpen(isAboutToOpen);
    if (isAboutToOpen) {
      window.history.replaceState(null, "", `#${anchorId}`);
    } else if (window.location.hash === `#${anchorId}`) {
      window.history.replaceState(
        null,
        "",
        window.location.pathname + window.location.search,
      );
    }
  };

  return (
    // scroll-mt keeps the question below the sticky nav when jumping to its
    // anchor.
    <div id={anchorId} className="scroll-mt-24 border-b-2 border-wasp-black">
      <button
        type="button"
        id={buttonId}
        onClick={toggle}
        aria-expanded={isOpen}
        aria-controls={panelId}
        className="flex w-full items-center justify-between gap-4 py-4 text-left font-mono text-sm font-bold text-wasp-black"
      >
        <span>{faq.question}</span>
        <span className="shrink-0 text-wasp-yellow-dark" aria-hidden="true">
          {isOpen ? <Minus size={18} /> : <Plus size={18} />}
        </span>
      </button>
      <div
        id={panelId}
        role="region"
        aria-labelledby={buttonId}
        hidden={!isOpen}
        className="space-y-3 pb-5 font-mono text-sm leading-relaxed text-wasp-g6"
      >
        {faq.answer}
      </div>
    </div>
  );
};

export default Faq;
