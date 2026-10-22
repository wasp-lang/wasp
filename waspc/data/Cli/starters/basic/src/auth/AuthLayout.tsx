import { ReactNode } from "react";

export function AuthLayout({ children }: { children: ReactNode }) {
  return (
    // Auth UI has margin-top on title, so we lower the top padding
    <div className="card mx-auto my-12 w-[min(32rem,100%-2rem)] px-8 pt-4 pb-8">
      {children}
    </div>
  );
}
