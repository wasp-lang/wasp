import { NextUIProvider } from "@nextui-org/react";
import "./Main.css";

export function Layout({ children }: React.PropsWithChildren<{}>) {
  return (
    <NextUIProvider>
      <div className="dark text-foreground bg-background min-h-screen">
        {children}
      </div>
    </NextUIProvider>
  );
}
