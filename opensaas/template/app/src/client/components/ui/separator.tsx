import * as SeparatorPrimitive from "@radix-ui/react-separator";
import * as React from "react";

import { cn } from "../../utils";

function Separator({
  className,
  orientation = "horizontal",
  decorative = true,
  ...props
}: React.ComponentProps<typeof SeparatorPrimitive.Root>) {
  return (
    <SeparatorPrimitive.Root
      data-slot="separator"
      decorative={decorative}
      orientation={orientation}
      className={cn(
        "bg-border shrink-0",
        orientation === "horizontal" ? "h-[1px] w-full" : "h-full w-[1px]",
        className,
      )}
      {...props}
    />
  );
}

export { Separator };
