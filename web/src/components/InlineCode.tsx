import { ReactNode } from "react";

const InlineCode = ({ children }: { children: ReactNode }) => (
  <code className="bg-[var(--wasp-code-bg)] px-1 align-baseline font-mono text-[0.95em] text-wasp-black">
    {children}
  </code>
);

export default InlineCode;
