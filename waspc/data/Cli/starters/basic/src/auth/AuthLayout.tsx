import { ReactNode } from "react";

export function AuthLayout({ children }: { children: ReactNode }) {
  return <main className="card">{children}</main>;
}
