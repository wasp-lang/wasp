import { useState } from "react";
import { FiChevronDown, FiChevronRight } from "react-icons/fi";

function l(title, overrideTitle) {
  const links = {
    Wasp: "https://wasp.sh/",
    "web app": "https://github.com/wasp-lang/wasp/tree/main/mage",
    "GPT code agent":
      "https://github.com/wasp-lang/wasp/tree/main/waspc/src/Wasp/AI",
    "blog post": "https://wasp.sh/blog/2023/07/10/gpt-web-app-generator",
  };

  return (
    <a
      href={links[title]}
      className="underline decoration-yellow-500 decoration-2"
    >
      {overrideTitle || title}
    </a>
  );
}

const faqs = [
  {
    question: "What is GPT Web App Generator?",
    answer: (
      <p>
        GPT Web App Generator is an experiment by the {l("Wasp")} team: it is a
        web app where you can shortly describe the web app you would like to
        create, and in a matter of minutes, a full-stack web app codebase,
        written in React, Node.js, Prisma, and Wasp, will be generated right in
        front of you, and made available to download and run locally.
        <br />
        <br />
        All the code behind GPT Web App Generator is open source: {l(
          "web app",
        )}{" "}
        and {l("GPT code agent")}.
        <br />
        <br />
        To learn more about the GPT Web App Generator and how exactly it works,
        check out our {l("blog post")}.
      </p>
    ),
  },
  {
    question: "What is Wasp?",
    answer: (
      <p>
        {l("Wasp")} is an open-source, full-stack framework for React & Node.js.
        It covers everything from front-end, back-end, database, to deployment.
        You can think of it as a modern version of Ruby on Rails for JS/TS -
        opinionated, provides best practices and helps you move faster.
      </p>
    ),
  },
  {
    question: "How well does it work?",
    answer: (
      <p>
        Due to GPT being non-deterministic, it sometimes introduces (small)
        mistakes, especially for more complex apps, but altogether it works
        better than we expected! The code it generates is often very reasonable,
        and for very simple apps, it can even produce a working app out of the
        box, while for a bit more complex apps it currently serves more like a
        super-intelligent starter that needs a couple of tweaks to get it going.
        <br />
        <br />
        With LLMs improving in general, the quality of generated code will only
        get better!
      </p>
    ),
  },
  {
    question: "What kind of apps can it generate?",
    answer: (
      <p>
        It can generate full stack web apps written in React, Node.js, Prisma,
        and Wasp. You can download the generated app, run it locally on your
        machine, even easily deploy if you wish.
        <br />
        <br />
        In order to keep things simpler for this first version, we enforced some
        limitations: no additional npm deps, no additional files, no TS, no
        advanced Wasp features.
        <br />
        <br />
        We believe most of these limitations can be removed in the future with
        more work!
      </p>
    ),
  },
  {
    question: "How does it work?",
    answer: (
      <p>
        GPT Web App Generator (aka Generator) does its work in 3 main phases:
        planning, generating and fixing.
        <br />
        <br />
        During each of these steps, we heavily guide it by providing it with doc
        snippets, examples and guidelines that are most relevant to its current
        task.
        <br />
        <br />
        We use GPT4o for all the phases at the moment of writing, as it gives
        the best ratio of quality / cost.
      </p>
    ),
  },
  {
    question: "[Advanced] Can I choose GPT model? / Can I run Mage locally?",
    answer: (
      <p>
        If you have access yourself to the OpenAI API, you can choose GPT model
        for the whole app, or play with adjusting the temperature, by running
        the Wasp GPT code agent locally! So same thing like Mage, but via CLI.
        <br />
        <br />
        To run Wasp AI (Mage) locally, make sure you have wasp installed and
        just run:
        <br />
        <span className="rounded bg-slate-800 p-1 text-slate-200">
          wasp new
        </span>
        <br />
        When asked, choose AI generation, answer some questions, and your app
        will start generating!
        <br />
        <br />
        There is also a command for running the same thing programmatically,
        without interactive questions:
        <br />
        <span className="rounded bg-slate-800 p-1 text-slate-200">
          wasp new:ai
        </span>
        <br />
        Run it with no arguments (as above) to see its usage instructions.
      </p>
    ),
  },
];

function FaqItem({ keyP, faq }) {
  const [isExpanded, setIsExpanded] = useState(false);

  return (
    <div className="py-6">
      <dt key={keyP} className="text-base text-neutral-700">
        <button
          className="flex w-full items-center justify-between text-left"
          onClick={() => {
            setIsExpanded(!isExpanded);
          }}
        >
          <span>{faq.question}</span>
          <div className="ml-6 text-yellow-500">
            {isExpanded ? (
              <FiChevronDown size={20} />
            ) : (
              <FiChevronRight size={20} />
            )}
          </div>
        </button>
      </dt>
      {isExpanded && <dd className="mt-2 text-neutral-500">{faq.answer}</dd>}
    </div>
  );
}

export function Faq() {
  return (
    <>
      <div className="grid grid-cols-12" id="faq">
        <div className="col-span-12 text-center">
          <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
            Frequently asked questions
          </h2>
          <p className="text-neutral-500">
            For anything not covered here, join&nbsp;
            <a
              href="https://discord.gg/rzdnErX"
              className="font-medium underline decoration-yellow-500 decoration-2"
            >
              our Discord
            </a>
            !
          </p>
        </div>
      </div>

      <dl className="mx-auto mt-6 max-w-3xl divide-y divide-neutral-300">
        {faqs.map((faq, idx) => (
          <FaqItem keyP={idx} key={idx} faq={faq} />
        ))}
      </dl>
    </>
  );
}
