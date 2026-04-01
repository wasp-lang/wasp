import Link from "@docusaurus/Link";
import { useRef, useState } from "react";
import { ArrowRight, ChevronLeft, ChevronRight } from "react-feather";
import { VCSection } from "./vcVariant";

const examples = [
  {
    name: "Microinfluencers",
    description: "microinfluencer.club",
    imageSrc: "img/lp/examples/microinfluencers.webp",
    href: "https://microinfluencer.club",
  },
  {
    name: "Kivo",
    description: "kivo.dev",
    imageSrc: "img/lp/examples/kivo.webp",
    href: "https://kivo.dev",
  },
  {
    name: "Searchcraft",
    description: "searchcraft.io",
    imageSrc: "img/lp/examples/searchcraft.webp",
    href: "https://www.searchcraft.io",
  },
  {
    name: "Scribeist",
    description: "scribeist.com",
    imageSrc: "img/lp/examples/scribeist.webp",
    href: "https://scribeist.com",
  },
  {
    name: "Messync",
    description: "messync.com",
    imageSrc: "img/lp/examples/messync.webp",
    href: "https://messync.com",
  },
  {
    name: "Prompt Panda",
    description: "promptpanda.io",
    imageSrc: "img/lp/examples/promptpanda.webp",
    href: "https://promptpanda.io",
  },
  {
    name: "Review Radar",
    description: "reviewradar.ai",
    imageSrc: "img/lp/examples/reviewradar.webp",
    href: "https://reviewradar.ai",
  },
];

const ExampleCard = ({ example }) => (
  <a
    href={example.href}
    target="_blank"
    rel="noopener noreferrer"
    className="shrink-0"
  >
    <div className="w-[280px] overflow-hidden rounded-none border border-yellow-500/25 bg-yellow-500/5 transition-all duration-200 hover:scale-105 hover:border-yellow-500/50 hover:shadow-md sm:w-[320px] md:w-[350px]">
      <img
        src={example.imageSrc}
        alt={example.name}
        className="aspect-video h-auto w-full object-cover object-top"
      />
      <div className="p-4">
        <p className="font-bold text-neutral-700">{example.name}</p>
        <p className="text-xs text-neutral-500">{example.description}</p>
      </div>
    </div>
  </a>
);

const VCExamples = () => {
  const scrollRef = useRef(null);
  const [isHovering, setIsHovering] = useState(false);
  const [canScrollLeft, setCanScrollLeft] = useState(false);
  const [canScrollRight, setCanScrollRight] = useState(true);

  const updateScrollState = () => {
    const el = scrollRef.current;
    if (!el) return;
    setCanScrollLeft(el.scrollLeft > 10);
    setCanScrollRight(el.scrollLeft < el.scrollWidth - el.clientWidth - 10);
  };

  const scroll = (direction) => {
    const el = scrollRef.current;
    if (!el) return;
    const amount = 370;
    el.scrollBy({
      left: direction === "left" ? -amount : amount,
      behavior: "smooth",
    });
    setTimeout(updateScrollState, 350);
  };

  return (
    <VCSection className="space-y-6">
      <div className="text-center">
        <h2 className="mb-2 text-xl text-neutral-700 lg:text-2xl">
          Vibe. Ship.{" "}
          <span className="underline decoration-yellow-500">SaaS.</span>{" "}
        </h2>
        <p className="text-neutral-500">
          Real businesses shipped by the community on top of Wasp's free,
          built-in SaaS template.
        </p>
      </div>

      <div
        className="relative left-1/2 flex w-screen -translate-x-1/2 flex-col items-center"
        onMouseEnter={() => {
          setIsHovering(true);
          updateScrollState();
        }}
        onMouseLeave={() => setIsHovering(false)}
      >
        {/* Left arrow */}
        {isHovering && canScrollLeft && (
          <button
            onClick={() => scroll("left")}
            className="absolute left-4 top-1/2 z-10 flex h-12 w-12 -translate-y-1/2 items-center justify-center bg-white/80 text-neutral-600 shadow-lg transition-all hover:bg-white hover:text-neutral-900"
          >
            <ChevronLeft size={28} />
          </button>
        )}

        {/* Right arrow */}
        {isHovering && canScrollRight && (
          <button
            onClick={() => scroll("right")}
            className="absolute right-4 top-1/2 z-10 flex h-12 w-12 -translate-y-1/2 items-center justify-center bg-white/80 text-neutral-600 shadow-lg transition-all hover:bg-white hover:text-neutral-900"
          >
            <ChevronRight size={28} />
          </button>
        )}

        <div className="w-full max-w-full overflow-hidden">
          <div
            className="flex gap-4 overflow-x-hidden px-4 pb-10 pt-4"
            ref={scrollRef}
            onScroll={updateScrollState}
          >
            {examples.map((example, index) => (
              <ExampleCard key={index} example={example} />
            ))}
          </div>
        </div>
      </div>

      <div className="mt-8 text-center">
        <Link to="https://opensaas.sh" target="_blank">
          <span className="group inline-flex items-center gap-1 text-sm text-neutral-500 hover:text-neutral-400">
            <span>Learn more about Open SaaS</span>
            <span className="text-yellow-600 transition-all group-hover:ml-0.5">
              <ArrowRight size={14} strokeWidth={2} />
            </span>
          </span>
        </Link>
      </div>
    </VCSection>
  );
};

export default VCExamples;
