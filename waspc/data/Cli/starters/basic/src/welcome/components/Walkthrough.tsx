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
    <div className="grid grid-cols-1 border border-neutral-900 bg-white lg:grid-cols-12">
      <ol className="lg:col-span-5 lg:border-r lg:border-neutral-900">
        {steps.map((step, index) => {
          const isOpen = index === openIndex;
          return (
            <li
              key={step.title}
              className={twJoin(
                "border-b border-neutral-200 last:border-b-0",
                isOpen && "bg-primary-50",
              )}
            >
              <button
                type="button"
                aria-expanded={isOpen}
                onClick={() => setOpenIndex(index)}
                className="hover:bg-primary-50 flex w-full items-center gap-3 px-5 py-4 text-left"
              >
                <StepMarker number={index + 1} />
                <span className="font-mono text-sm font-bold tracking-wide uppercase">
                  {step.title}
                </span>
              </button>
              {isOpen && (
                <div className="flex flex-col gap-4 pr-5 pb-5 pl-14 text-sm text-neutral-700">
                  {step.content}
                  <div className="lg:hidden">{step.media}</div>
                </div>
              )}
            </li>
          );
        })}
      </ol>
      <div className="hidden items-center bg-neutral-100 p-8 lg:col-span-7 lg:flex">
        {steps[openIndex].media}
      </div>
    </div>
  );
}

function StepMarker({ number }: { number: number }) {
  return (
    <span className="flex size-6 shrink-0 items-center justify-center rounded-full border border-neutral-900 font-mono text-xs">
      {number}
    </span>
  );
}
