import { Ref, useEffect, useRef, useState } from "react";
import { Card, CardContent } from "../../client/components/ui/card";

const EXAMPLES_CAROUSEL_INTERVAL = 3000;
const EXAMPLES_CAROUSEL_SCROLL_TIMEOUT = 200;

interface ExampleApp {
  name: string;
  description: string;
  imageSrc: string;
  href: string;
}

export function ExamplesCarousel({ examples }: { examples: ExampleApp[] }) {
  const [currentExample, setCurrentExample] = useState(0);
  const [isInView, setIsInView] = useState(false);
  const containerRef = useRef<HTMLDivElement>(null);
  const scrollContainerRef = useRef<HTMLDivElement>(null);
  const intervalRef = useRef<NodeJS.Timeout | null>(null);
  const observerRef = useRef<IntersectionObserver | null>(null);
  const scrollTimeoutRef = useRef<NodeJS.Timeout | null>(null);

  useEffect(() => {
    observerRef.current = new IntersectionObserver(
      ([entry]) => setIsInView(entry.isIntersecting),
      {
        threshold: 0.5,
        rootMargin: "-200px 0px -100px 0px",
      },
    );

    if (containerRef.current) {
      observerRef.current.observe(containerRef.current);
    }

    return () => {
      if (observerRef.current) {
        observerRef.current.disconnect();
      }
    };
  }, []);

  useEffect(() => {
    if (intervalRef.current) {
      clearInterval(intervalRef.current);
    }

    if (isInView && examples.length > 1) {
      intervalRef.current = setInterval(() => {
        setCurrentExample((prev) => (prev + 1) % examples.length);
      }, EXAMPLES_CAROUSEL_INTERVAL);
    }

    if (scrollTimeoutRef.current) {
      clearTimeout(scrollTimeoutRef.current);
    }

    scrollTimeoutRef.current = setTimeout(() => {
      if (scrollContainerRef.current) {
        const scrollContainer = scrollContainerRef.current;
        const targetCard = scrollContainer.children[currentExample] as
          | HTMLElement
          | undefined;

        if (targetCard) {
          const containerRect = scrollContainer.getBoundingClientRect();
          const cardRect = targetCard.getBoundingClientRect();
          const scrollLeft =
            targetCard.offsetLeft -
            scrollContainer.offsetLeft -
            containerRect.width / 2 +
            cardRect.width / 2;

          scrollContainer.scrollTo({
            left: scrollLeft,
            behavior: "smooth",
          });
        }
      }
    }, EXAMPLES_CAROUSEL_SCROLL_TIMEOUT);

    return () => {
      if (intervalRef.current) {
        clearInterval(intervalRef.current);
      }
      if (scrollTimeoutRef.current) {
        clearTimeout(scrollTimeoutRef.current);
      }
    };
  }, [isInView, examples.length, currentExample]);

  const handleMouseEnter = (index: number) => {
    setCurrentExample(index);

    if (intervalRef.current) {
      clearInterval(intervalRef.current);
    }

    if (isInView && examples.length > 1) {
      intervalRef.current = setInterval(() => {
        setCurrentExample((prev) => (prev + 1) % examples.length);
      }, EXAMPLES_CAROUSEL_INTERVAL);
    }
  };

  return (
    <div
      ref={containerRef}
      className="relative left-1/2 my-16 flex w-screen -translate-x-1/2 flex-col items-center"
    >
      <h2 className="text-muted-foreground mb-6 text-center font-semibold tracking-wide">
        Used by:
      </h2>
      <div className="w-full max-w-full overflow-hidden">
        <div
          className="no-scrollbar flex snap-x snap-mandatory gap-4 overflow-x-auto scroll-smooth px-4 pb-10 pt-4"
          ref={scrollContainerRef}
        >
          {examples.map((example, index) => (
            <ExampleCard
              key={index}
              example={example}
              index={index}
              isCurrent={index === currentExample}
              onMouseEnter={handleMouseEnter}
            />
          ))}
        </div>
      </div>
    </div>
  );
}

interface ExampleCardProps {
  example: ExampleApp;
  index: number;
  isCurrent: boolean;
  onMouseEnter: (index: number) => void;
  ref?: Ref<HTMLDivElement>;
}

function ExampleCard({
  example,
  index,
  isCurrent,
  onMouseEnter,
  ref,
}: ExampleCardProps) {
  return (
    <a
      href={example.href}
      target="_blank"
      rel="noopener noreferrer"
      className="shrink-0 snap-center"
      onMouseEnter={() => onMouseEnter(index)}
    >
      <Card
        ref={ref}
        className="w-[280px] overflow-hidden transition-all duration-200 hover:scale-105 sm:w-[320px] md:w-[350px]"
        variant={isCurrent ? "default" : "faded"}
      >
        <CardContent className="h-full p-0">
          <img
            src={example.imageSrc}
            alt={example.name}
            className="aspect-video h-auto w-full object-cover object-top"
          />
          <div className="p-4">
            <p className="font-bold">{example.name}</p>
            <p className="text-muted-foreground text-xs">
              {example.description}
            </p>
          </div>
        </CardContent>
      </Card>
    </a>
  );
}
