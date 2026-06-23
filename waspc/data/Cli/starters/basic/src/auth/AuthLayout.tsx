import { ReactNode } from "react";

export function AuthLayout({ children }: { children: ReactNode }) {
  return (
    <div className="flex justify-center">
      <div className="card mt-32 h-fit w-full max-w-md px-8 py-10">
        {children}
      </div>
    </div>
  );
}
