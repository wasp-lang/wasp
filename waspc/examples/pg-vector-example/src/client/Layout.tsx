import { NextUIProvider } from "@nextui-org/react";
import "./Main.css";

export function Layout({ children }: React.PropsWithChildren<{}>) {
  return (
    <NextUIProvider>
      <div className="grid place-content-center min-h-screen">{children}</div>
    </NextUIProvider>
  );
}
