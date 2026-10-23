import { type ReactNode, useState } from "react";
import { twJoin } from "tailwind-merge";

export type WalkthroughStep = {
  title: string;
  content: ReactNode;
  media: ReactNode;
};

// A list of steps where one step is open at a time, and its media is shown
// next to the list. Inspired by VS Code's extension walkthroughs.
export function Walkthrough({ steps }: { steps: WalkthroughStep[] }) {
  const [openIndex, setOpenIndex] = useState(0);

  return (
    <div className="card grid grid-cols-1 lg:grid-cols-12">
      <ol className="divide-y-2 divide-black lg:col-span-5 lg:border-r-2 lg:border-black">
        {steps.map((step, index) => {
          const isOpen = index === openIndex;
          return (
            <li key={step.title} className={twJoin(isOpen && "bg-cream")}>
              <button
                type="button"
                aria-expanded={isOpen}
                onClick={() => setOpenIndex(index)}
                className="flex w-full cursor-pointer items-center gap-3 px-5 py-4 text-left hover:bg-cream"
              >
                <StepMarker number={index + 1} isOpen={isOpen} />
                <span className="text-sm font-bold uppercase">
                  {step.title}
                </span>
              </button>
              {isOpen && (
                <div className="flex flex-col gap-4 pr-5 pb-5 pl-14 text-sm">
                  {step.content}
                  <div className="lg:hidden">{step.media}</div>
                </div>
              )}
            </li>
          );
        })}
      </ol>
      <div className="hidden items-center bg-cream p-8 lg:col-span-7 lg:flex">
        {steps[openIndex].media}
      </div>
    </div>
  );
}

function StepMarker({ number, isOpen }: { number: number; isOpen: boolean }) {
  return (
    <span
      className={twJoin(
        "flex size-6 shrink-0 items-center justify-center rounded-full border-2 border-black text-xs font-bold",
        isOpen && "bg-yellow",
      )}
    >
      {number}
    </span>
  );
}
