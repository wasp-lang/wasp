import { type ReactNode } from "react";

export function InlineCode({ children }: { children: ReactNode }) {
  return (
    <code className="bg-neutral-100 px-1 py-0.5 font-mono text-[0.9em] text-neutral-900">
      {children}
    </code>
  );
}
