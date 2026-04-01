import Link from "@docusaurus/Link";
import { vc, VCSection } from "./vcVariant";

const PH = "ph";

const testimonials = [
  {
    text: "I used Wasp to win a hackathon this year - it was such a pleasure to use! I've done plenty of hackathons before where I've built small SaaS apps, and there's just so much time wasted setting up common utilities - stuff like user management, databases, routing, etc. Wasp handled all that for me and let me build out our web app in record time.",
    url: "https://www.producthunt.com/posts/wasp-lang-beta?comment=2048039",
    name: "Julian LaNeve",
    handle: "@julian_laneve",
    img: "img/lp/tm/jlaneve.webp",
    source: PH,
  },
  {
    text: "I spent the one weekend building with Wasp and it was amazing, a real pleasure. I normally develop in Vue.js, but in a weekend I had time to learn Wasp, React and finish a full-stack app (only missing styling). This would have been impossible before.",
    url: "https://www.producthunt.com/posts/wasp-lang-beta?comment=2048094",
    name: "Joan Reyero",
    handle: "@joanreyero",
    img: "img/lp/tm/reyero.webp",
    source: PH,
  },
  {
    text: "If it weren't for Wasp, my app Amicus would probably have never been finished. I estimate it saved me 100+ hours from the start and I'm still amazed that I did all this work as a team-of-one. Being able to quickly change existing features and add the new ones is the biggest advantage of Wasp for me.",
    url: "https://www.producthunt.com/posts/wasp-lang-beta?comment=2048472",
    name: "Erlis Kllogjri",
    handle: "@erlis_kllogjri",
    img: "img/lp/tm/erlis.webp",
    source: PH,
  },
  {
    text: "When I first learned about Wasp on HN I was really excited about its DSL approach. It was amazing how fast I could get things running with Wasp - I had the first version within an hour! The language is also fairly simple and straightforward and plays well with React & Node.js + it removes a ton of boilerplate.",
    url: "https://www.producthunt.com/posts/wasp-lang-beta?comment=2048168",
    name: "Michael Curry",
    handle: "@michael_curry1",
    img: "img/lp/tm/cursorial.webp",
    source: PH,
  },
];

const TestimonialCard = ({ url, text, name, handle, img, variant }) => {
  const cardClass = vc(variant, {
    base: "rounded-md border border-yellow-500/25 bg-yellow-500/5 p-6 shadow-sm drop-shadow-sm",
    v1: "rounded-none border border-yellow-500/25 bg-yellow-500/5 p-6",
    v2: "rounded-none border border-neutral-300 bg-yellow-500/5 p-6",
    v3: "rounded-none border-l-2 border-yellow-500 bg-white p-6",
  });

  return (
    <Link to={url}>
      <div className={cardClass}>
        <div className="flex">
          <img className="rounded-full" src={img} width={45} height={45} />
          <div className="flex w-full justify-between pl-3">
            <div>
              <h6 className="text-md font-semibold text-neutral-700">{name}</h6>
              <p className="text-sm text-neutral-500">{handle}</p>
            </div>
            <div>
              <img
                className="h-5 w-5 rounded-full"
                src="img/lp/ph-logo.webp"
                alt="Product Hunt"
              />
            </div>
          </div>
        </div>
        <div className="mt-2 whitespace-pre-wrap text-neutral-700">{text}</div>
      </div>
    </Link>
  );
};

const VCTestimonials = ({ variant }) => {
  return (
    <VCSection variant={variant} className="space-y-16">
      <div className="grid grid-cols-12">
        <div className="col-span-12 text-center">
          <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
            Developers building with Wasp
          </h2>
          <p className="text-neutral-500">
            Join{" "}
            <a
              href="https://discord.gg/rzdnErX"
              className="font-medium underline decoration-yellow-500 decoration-2"
            >
              our Discord
            </a>{" "}
            community for more!
          </p>
        </div>
      </div>

      <div className="grid grid-cols-1 gap-4 md:grid-cols-2">
        {testimonials.map((t, idx) => (
          <TestimonialCard key={idx} {...t} variant={variant} />
        ))}
      </div>

      <div className="flex flex-wrap items-center justify-center gap-6">
        <a
          href="https://github.com/wasp-lang/wasp"
          target="_blank"
          rel="noreferrer"
          className="text-sm text-neutral-500 hover:text-neutral-400"
        >
          13k+ GitHub Stars
        </a>
        <span className="h-4 border-l border-neutral-300" />
        <span className="flex items-center text-sm text-neutral-500">
          Backed by{" "}
          <img
            className="ml-2 w-20"
            src="img/lp/yc-logo-rounded.webp"
            alt="Y Combinator"
          />
        </span>
        <span className="h-4 border-l border-neutral-300" />
        <a
          href="https://discord.gg/rzdnErX"
          target="_blank"
          rel="noreferrer"
          className="text-sm text-neutral-500 hover:text-neutral-400"
        >
          Active Discord Community
        </a>
      </div>
    </VCSection>
  );
};

export default VCTestimonials;
