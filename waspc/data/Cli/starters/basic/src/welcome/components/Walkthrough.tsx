import type { ReactNode } from "react";

export type WalkthroughStep = {
  title: string;
  content: ReactNode;
  example: ReactNode;
};

export function Walkthrough({ steps }: { steps: WalkthroughStep[] }) {
  return (
    <ol role="list" className="card divide-y-2 divide-black p-0">
      {steps.map((step, index) => (
        <li
          key={step.title}
          className="grid grid-cols-1 items-start gap-4 p-4 sm:p-6 lg:grid-cols-2 lg:gap-6"
        >
          <div className="flex min-w-0 flex-col gap-3 text-sm">
            <h3 className="flex items-center gap-3 font-bold">
              <span
                aria-hidden="true"
                className="flex size-6 shrink-0 items-center justify-center rounded-full border-2 border-black bg-yellow text-xs"
              >
                {index + 1}
              </span>
              {step.title}
            </h3>
            {step.content}
          </div>
          <div className="min-w-0">{step.example}</div>
        </li>
      ))}
    </ol>
  );
}
