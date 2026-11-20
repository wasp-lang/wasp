import { type ReactNode } from "react";

export type WalkthroughStep = {
  title: string;
  content: ReactNode;
  media: ReactNode;
};

// Keep every step visible so reading and copying examples never shifts the page.
export function Walkthrough({ steps }: { steps: WalkthroughStep[] }) {
  return (
    <ol className="flex flex-col gap-6">
      {steps.map((step, index) => (
        <li
          key={step.title}
          className="card grid min-w-0 gap-6 p-5 sm:p-6 lg:grid-cols-2 lg:gap-8"
        >
          <div className="flex min-w-0 flex-col gap-4 text-sm leading-relaxed">
            <h3 className="flex items-center gap-3 text-base font-semibold">
              <span className="bg-yellow text-on-accent flex size-7 shrink-0 items-center justify-center rounded-md text-sm">
                {index + 1}
              </span>
              {step.title}
            </h3>
            {step.content}
          </div>
          <div className="min-w-0 self-center">{step.media}</div>
        </li>
      ))}
    </ol>
  );
}
