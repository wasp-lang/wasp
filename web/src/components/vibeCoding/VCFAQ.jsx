import { useState } from "react";
import { ChevronDown, ChevronRight } from "react-feather";
import { VCSection } from "./vcWrappers";

const FaqItem = ({ faq }) => {
  const [isExpanded, setIsExpanded] = useState(false);

  return (
    <div className="py-6">
      <dt className="text-base text-neutral-700">
        <button
          className="flex w-full items-center justify-between text-left"
          onClick={() => setIsExpanded(!isExpanded)}
        >
          <span>{faq.question}</span>
          <div className="ml-6 text-yellow-500">
            {isExpanded ? (
              <ChevronDown size={20} />
            ) : (
              <ChevronRight size={20} />
            )}
          </div>
        </button>
      </dt>
      {isExpanded && <dd className="mt-2 text-neutral-500">{faq.answer}</dd>}
    </div>
  );
};

const faqs = [
  {
    question: "Do I need to know how to code?",
    answer: (
      <p>
        No. Wasp is designed for AI to do the heavy lifting. You describe what
        you want, your AI builds it. The main.wasp.ts config lets you see what
        your app does in plain English even if you've never written code before.
      </p>
    ),
  },
  {
    question: "Is this just for AI coding?",
    answer: (
      <p>
        No. Wasp is a great framework on its own. It's Laravel / Rails-like
        productivity for React and Node.js. The AI advantages are a natural
        byproduct of its declarative, low-boilerplate architecture. You get a
        better framework <em>and</em> a better AI coding experience.
      </p>
    ),
  },
  {
    question: "Why not just use Lovable, Bolt, or Replit?",
    answer: (
      <>
        <p>
          Those tools are great for quick prototypes. But if you want something
          that stays manageable and affordable as it grows, you'll eventually
          want to own your code, control your infrastructure, and customize
          beyond what a hosted builder allows.
        </p>
        <p className="mt-2">
          With Wasp, you get the same ability to describe what you want and AI
          builds it, but the code lives on your machine, deploys wherever you
          choose, and uses standard React + Node.js you can always modify by
          hand. Auth, database, email sending, cron jobs, and deployment are all
          built in and production-hardened, so you're not stitching together
          third-party services which can get expensive and complex.
        </p>
      </>
    ),
  },
  {
    question: "Do I have to learn a new language?",
    answer: (
      <p>
        Nope. Wasp config is written in TypeScript (<code>main.wasp.ts</code>)
        with full IDE support and autocomplete. It takes about 30 minutes to
        learn. 90% of your code is still React and Node.js. Your AI already
        understands it.
      </p>
    ),
  },
  {
    question: "What happens as my app gets more complex?",
    answer: (
      <p>
        Wasp's config can be split across multiple files as your app grows. The
        declarative architecture keeps your codebase coherent and every new
        feature follows the same patterns, whether it's the 5th or the 50th.
        This makes AI-generated code consistent and easy for you to review.
      </p>
    ),
  },
  {
    question: "What if I outgrow Wasp?",
    answer: (
      <p>
        Wasp compiles to standard React + Node.js + Prisma. You can eject
        anytime. Open-source, free, nothing to cancel.
      </p>
    ),
  },
  {
    question:
      "How does this compare to using AI with Next.js / Rails / Django?",
    answer: (
      <p>
        Beyond the benchmark numbers, the key difference is architectural. Wasp
        is opinionated and covers the entire stack, from client to database and
        even deployments. Something no other framework does. It's focus on
        higher-level abstractions also make it the most context-efficient
        framework out there.
      </p>
    ),
  },
  {
    question: "Is Wasp production-ready?",
    answer: (
      <p>
        Yes. YC-backed, 13k+ GitHub stars, used in production by Fortune
        500companies and solo developers. Free and open-source.
      </p>
    ),
  },
];

const VCFAQ = () => {
  const dividerClass =
    "mx-auto mt-6 max-w-3xl divide-y divide-dotted divide-neutral-300";

  return (
    <VCSection>
      <div className="grid grid-cols-12">
        <div className="col-span-12 text-center">
          <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
            Straight talk
          </h2>
        </div>
      </div>

      <dl className={dividerClass}>
        {faqs.map((faq, idx) => (
          <FaqItem key={idx} faq={faq} />
        ))}
      </dl>
    </VCSection>
  );
};

export default VCFAQ;
